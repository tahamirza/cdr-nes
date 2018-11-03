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
     (let ((address-mode (assoc (bit-extract op 2 5) zero-address-modes)))
       (if address-mode
	   (begin
	     (display address-mode)
	     (display (assoc (bit-extract op 5 8) zero-opcodes)))
	   (display "some other opcode"))))
    (else (display "unknown opcode"))))

(define zeroops
  '(BCC
    BCS
    BEQ
    BIT
    BMI
    BNE
    BPL
    BRK
    BVC
    BVS
    CLC
    CLD
    CLI
    CLV
    CPX
    CPY
    DEY
    INX
    INY
    JMP
    JSR
    LDY
    PHA
    PHP
    PLA
    PLP
    RTI
    RTS
    STY
    TAY
    TYA))

(define accumulatorops
  '(ADC
    AND
    CMP
    EOR
    LDA
    ORA
    SBC
    STA))

(define twoops
  '(ASL
    DEC
    DEX
    INC
    LDX
    LSR
    NOP
    ROL
    ROR
    STX
    TAX
    TSX
    TXA
    TXS))

(define opcodes
  '((#x69 'ADC 'immediate)
    (#x65 'ADC 'zero)
    (#x75 'ADC 'zerox)
    (#x6D 'ADC 'absolute)
    (#x7D 'ADC 'absolutex)
    (#x79 'ADC 'absolutey)
    (#x61 'ADC 'indirectx)
    (#x71 'ADC 'indirecty)

    (#x29 'AND 'immediate)
    (#x25 'AND 'zero)
    (#x35 'AND 'zerox)
    (#x2D 'AND 'absolute)
    (#x3D 'AND 'absolutex)
    (#x39 'AND 'absolutey)
    (#x21 'AND 'indirectx)
    (#x31 'AND 'indirecty)

    (#x0A 'ASL 'accumulator)
    (#x06 'ASL 'zero)
    (#x16 'ASL 'zerox)
    (#x0E 'ASL 'absolute)
    (#x1E 'ASL 'absolutex)

    (#x90 'BCC 'relative)

    (#xB0 'BCS 'relative)

    (#xF0 'BEQ 'relative)

    (#x24 'BIT 'zero)
    (#x2C 'BIT 'absolute)

    (#x30 'BMI 'relative)

    (#xD0 'BNE 'relative)

    (#x10 'BPL 'relative)

    (#x00 'BRK 'implied)

    (#x50 'BVC 'relative)

    (#x70 'BVS 'relative)

    (#x18 'CLC 'implied)

    (#xD8 'CLD 'implied)

    (#x58 'CLI 'implied)

    (#xB8 'CLV 'implied)

    (#xC9 'CMP 'immediate)
    (#xC5 'CMP 'zero)
    (#xD5 'CMP 'zerox)
    (#xCD 'CMP 'absolute)
    (#xDD 'CMP 'absolutex)
    (#xD9 'CMP 'absolutey)
    (#xC1 'CMP 'indirectx)
    (#xD1 'CMP 'indirecty)

    (#xE0 'CPX 'immediate)
    (#xE4 'CPX 'zero)
    (#xEC 'CPX 'absolute)

    (#xC0 'CPY 'immediate)
    (#xC4 'CPY 'zero)
    (#xCC 'CPY 'absolute)

    (#xC6 'DEC 'zero)
    (#xD6 'DEC 'zerox)
    (#xCE 'DEC 'absolute)
    (#xDE 'DEC 'absolutex)

    (#xCA 'DEX 'implied)

    (#x88 'DEY 'implied)

    (#x49 'EOR 'immediate)
    (#x45 'EOR 'zero)
    (#x55 'EOR 'zerox)
    (#x4D 'EOR 'absolute)
    (#x5D 'EOR 'absolutex)
    (#x59 'EOR 'absolutey)
    (#x41 'EOR 'indirectx)
    (#x51 'EOR 'indirecty)

    (#xE6 'INC 'zero)
    (#xF6 'INC 'zerox)
    (#xEE 'INC 'absolute)
    (#xFE 'INC 'absolutex)

    (#xE8 'INX 'implied)

    (#xC8 'INY 'implied)

    (#x4C 'JMP 'absolute)
    (#x6C 'JMP 'indirect)

    (#x20 'JSR 'absolute)

    (#xA9 'LDA 'immediate)
    (#xA5 'LDA 'zero)
    (#xB5 'LDA 'zerox)
    (#xAD 'LDA 'absolute)
    (#xBD 'LDA 'absolutex)
    (#xB9 'LDA 'absolutey)
    (#xA1 'LDA 'indirectx)
    (#xB1 'LDA 'indirecty)

    (#xA2 'LDX 'immediate)
    (#xA6 'LDX 'zero)
    (#xB6 'LDX 'zeroy)
    (#xAE 'LDX 'absolute)
    (#xBE 'LDX 'absolutey)

    (#xA0 'LDY 'immediate)
    (#xA4 'LDY 'zero)
    (#xB4 'LDY 'zerox)
    (#xAC 'LDY 'absolute)
    (#xBC 'LDY 'absolutex)

    (#x4A 'LSR 'accumulator)
    (#x46 'LSR 'zero)
    (#x56 'LSR 'zerox)
    (#x4E 'LSR 'absolute)
    (#x5E 'LSR 'absolutex)

    (#xEA 'NOP 'implied)

    (#x09 'ORA 'immediate)
    (#x05 'ORA 'zero)
    (#x15 'ORA 'zerox)
    (#x0D 'ORA 'absolute)
    (#x1D 'ORA 'absolutex)
    (#x19 'ORA 'absolutey)
    (#x01 'ORA 'indirectx)
    (#x11 'ORA 'indirecty)

    (#x48 'PHA 'implied)

    (#x08 'PHP 'implied)

    (#x68 'PLA 'implied)

    (#x28 'PLP 'implied)

    (#x2A 'ROL 'accumulator)
    (#x26 'ROL 'zero)
    (#x36 'ROL 'zerox)
    (#x2E 'ROL 'absolute)
    (#x2E 'ROL 'absolutex)

    (#x6A 'ROR 'accumulator)
    (#x66 'ROR 'zero)
    (#x76 'ROR 'zerox)
    (#x6E 'ROR 'absolute)
    (#x7E 'ROR 'absolutex)

    (#x40 'RTI 'implied)

    (#x60 'RTS 'implied)

    (#xE9 'SBC 'immediate)
    (#xE5 'SBC 'zero)
    (#xF5 'SBC 'zerox)
    (#xED 'SBC 'absolute)
    (#xFD 'SBC 'absolutex)
    (#xF9 'SBC 'absolutey)
    (#xE1 'SBC 'indirectx)
    (#xF1 'SBC 'indirecty)

    (#x38 'SEC 'implied)

    (#xF8 'SED 'implied)

    (#x78 'SEI 'implied)

    (#x85 'STA 'zero)
    (#x95 'STA 'zerox)
    (#x8D 'STA 'absolute)
    (#x9D 'STA 'absolutex)
    (#x99 'STA 'absolutey)
    (#x81 'STA 'indirectx)
    (#x91 'STA 'indirecty)

    (#x86 'STX 'zero)
    (#x96 'STX 'zeroy)
    (#x8E 'STX 'absolute)

    (#x84 'STY 'zero)
    (#x94 'STY 'zerox)
    (#x8C 'STY 'absolute)

    (#xAA 'TAX 'implied)

    (#xA8 'TAY 'implied)

    (#xBA 'TSX 'implied)

    (#x8A 'TXA 'implied)

    (#x9A 'TXS 'implied)

    (#x98 'TYA 'implied)))

(display A)
(newline)

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

(define (print-opcode opcode-list)
  (format #t "~8,'0b " (car opcode-list))
  (display (cdr opcode-list))
  (newline))


(define (print-optable table)
  (print-opcode (car table))
  (if (not (null? (cdr table)))
      (print-optable (cdr table))))


;(print-optable opcodes)
(display (parse-opcode #xEC))
