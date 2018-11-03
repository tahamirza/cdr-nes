(use-modules (ice-9 binary-ports))
(use-modules (ice-9 iconv))
(use-modules (ice-9 format))
(use-modules (rnrs bytevectors))

(define A 0)
(define X 0)
(define Y 0)
(define PC 0)
(define S #xFD)
(define P #x34)

(define memory-opcodes
  '((#b000 'ORA)
    (#b001 'AND)
    (#b010 'EOR)
    (#b011 'ADC)
    (#b100 'STA)
    (#b101 'LDA)
    (#b110 'CMP)
    (#b111 'SBC)))

(define memory-address-modes
  '((#b000 'indirectx)
    (#b001 'zero)
    (#b010 'immediate)
    (#b011 'absolute)
    (#b100 'indirecty)
    (#b101 'zerox)
    (#b110 'absolutey)
    (#b111 'absolutex)))

(define two-opcodes
  '((#b000 'ASL)
    (#b001 'ROL)
    (#b010 'LSR)
    (#b011 'ROR)
    (#b100 'STX)
    (#b101 'LDX)
    (#b110 'DEC)
    (#b111 'INC)))

(define two-address-modes
  '((#b000 'immediate)
    (#b001 'zero)
    (#b010 'accumulator)
    (#b011 'absolute)
    (#b101 'zerox)
    (#b111 'absolutex)))

(define zero-opcodes
  '((#b001 'BIT)
    (#b010 'JMP)
    (#b011 'JMP) 			; absolute
    (#b100 'STY)
    (#b101 'LDY)
    (#b110 'CPY)
    (#b111 'CPX)))

(define zero-address-modes
  '((#b000 'immediate)
    (#b001 'zero)
    (#b011 'absolute)
    (#b101 'zerox)
    (#b111 'absolutex)))

(define misc-opcodes
  '((#x00 'BRK)
    (#x20 'JSR)				; absolute
    (#x40 'RTI)
    (#x60 'RTS)
    (#x08 'PHP)
    (#x28 'PLP)
    (#x48 'PHA)
    (#x68 'PLA)
    (#x88 'DEY)
    (#xA8 'TAY)
    (#xC8 'INY)
    (#xE8 'INX)
    (#x18 'CLC)
    (#x38 'SEC)
    (#x58 'CLI)
    (#x78 'SEI)
    (#x98 'TYA)
    (#xB8 'CLV)
    (#xD8 'CLD)
    (#xF8 'SED)
    (#x8A 'TXA)
    (#x9A 'TXS)
    (#xAA 'TAX)
    (#xBA 'TSX)
    (#xCA 'DEX)
    (#xEA 'NOP)))

(define (parse-opcode op)
  ;; the first two bits determine which table to check
  (case (bit-extract op 0 2)
    ;; if they are 01 or 10 then the opcode has the form
    ;; aaabbbcc
    ;; 76543210
    ((#b01)
     (display (assoc (bit-extract op 5 8) memory-opcodes))
     (display (assoc (bit-extract op 2 5) memory-address-modes)))
    ((#b10)
     (display (assoc (bit-extract op 5 8) two-opcodes))
     (display (assoc (bit-extract op 2 5) two-address-modes)))
    ((#b00)
     (let* ((bbb (bit-extract op 2 5))
	    (address-mode (assoc bbb zero-address-modes)))
       (if address-mode
	   (begin
	     (display address-mode)
	     (display (assoc (bit-extract op 5 8) zero-opcodes)))
	   (if (eq? bbb #b100)
	       ;; conditional branch
	       (let ((y (bit-extract op 5 6)))
		 (case (bit-extract op 6 8)
		   ((#b00)
		    ;;negative
		    (if (eq? y 0)
			(display 'BPL)
			(display 'BMI)))
		   ((#b01)
		    ;;overflow
		    (if (eq? y 0)
			(display 'BVC)
			(display 'BVS)))
		   ((#b10)
		    ;;carry
		    (if (eq? y 0)
			(display 'BCC)
			(display 'BCS)))
		   ((#b11)
		    ;;zero
		    (if (eq? y 0)
			(display 'BNE)
			(display 'BEQ)))))
	       ;; just look up the opcode directly
	       (display (assoc op misc-opcodes))))))
    (else (display "unknown opcode"))))

(define (print-file file)
  (let ((char (get-u8 file)))
    (if (not (eof-object? char))
        (begin
          (display char)
          (display " ")
          (print-file file))
        (newline))))

(define (print-header header)
  (let ((info (make-bytevector 4)))
    (display "constant: ")
    (bytevector-copy! header 0 info 0 4)
    (display (bytevector->string info "ascii"))
    (newline))

  (let ((prg-len (bytevector-u8-ref header 4)))
    (display "PRG ROM: ")
    (display (* 16 prg-len))
    (display " kb")
    (newline))

  (let ((chr-len (bytevector-u8-ref header 5)))
    (display "CHR ROM: ")
    (display (* 8 chr-len))
    (display " kb")
    (newline))

  (let ((flags-6 (bytevector-u8-ref header 6)))
    (display "Flags 6: ")
    (display flags-6)
    (newline))

  (let ((flags-7 (bytevector-u8-ref header 7)))
    (display "Flags 7: ")
    (display flags-7)
    (newline))

  (let ((prg-ram-len (bytevector-u8-ref header 8)))
    (display "PRG RAM: ")
    (if (= prg-ram-len 0)
        (display "8")
        (display (* 8 prg-ram-len)))
    (display " kb")
    (newline))

  (let ((flags-9 (bytevector-u8-ref header 9)))
    (display "Flags 9: ")
    (display flags-9)
    (newline)))

;; (let ((file (open-input-file "nestest.nes" #:binary #t)))
;;   (print-header (get-bytevector-n file 16))
;;   (close-port file))

(display (parse-opcode #x68))
