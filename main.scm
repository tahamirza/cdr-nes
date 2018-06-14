(define A 0)
(define X 0)
(define Y 0)
(define PC 0)
(define S #xFD)
(define P #x34)

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
