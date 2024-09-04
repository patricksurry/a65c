.comment

    64tass --nostart --list=expr.lst --output expr.bin --labels=expr.sym expr.asm

Run like:
    c65 -gg -r expr.bin -a 0x200 -l expr.sym

.endcomment

        .cpu "65c02"
        .enc "none"

.section zp

radix   .byte ?
delim   .byte ?
status  .byte ?
tok     .byte ?
literal .word ?
opstkp  .byte ?
vstkp   .byte ?

STK_PIVOT = $80             ; vstk descends from here, opstk ascends

; STK_PIVOT divides the value stack and the operator stack
; vstkp points to the LSB of the "top" word on the value stack
; and is equal to STK_PIVOT when the value stack is empty
; opstkp points to the top operator token and is equal to
; STK_PIVOT-1 when the operator stack is empty (though it
; normally has a TOK_NONE sigil in the bottommost slot)
;
;    vstkp ----v                 |           v---- opstkp
;   ----+----+----+----+----+----+----+----+----+----+----
;  <-- grows | 1L | 1H | 0L | 0H | c0 | c1 | c2 | grows -->
;   ----+----+----+----+----+----+----+----+----+----+----

.endsection


.section test

test_expr:
        jsr get_line
        lda #<linebuf
        sta cursor
        lda #>linebuf
        sta cursor+1
        jsr parse_expr
        jsr prbyte
        jsr prnl
        ldy STK_PIVOT-2
        lda STK_PIVOT-1
        jsr prword
        jsr prnl
        bra test_expr

.endsection



.section code

parse_expr:
        ldx #STK_PIVOT
        stx vstkp               ; descending stack of words
        stx opstkp              ; ascending stack of operators (bytes)
        stz 0,x                 ; start with TOK_NONE sigil

        ; fall through, leaving result at vstkp

match_expr:
        ; match <term> (<bop> <term>)*
_terms:
        jsr match_term          ; match first term
        bne _err

        ldx #2                  ; check for binary op
        jsr next_token
        cpx #0
        beq _resolve            ; no op so we're done

        jsr push_op             ; otherwise stack and continue
        bne _err
        bra _terms

_resolve:
        ldx opstkp              ; resolve operators until we hit TOK_NONE
        lda 0,x
        beq _done
        jsr pop_op
        bra _resolve
        ; fall thru with error status A=0 on success, err otherwise
_done:
_err:
        rts

match_term:
        ldx #1
        jsr next_token
        cpx #TOK_LIT
        bne _chk_paren

        ; stack the literal
        ldx vstkp
        dex
        dex
        lda literal
        sta 0,x
        lda literal+1
        sta 1,x
        stx vstkp
        lda #0
        bra _done
_chk_paren:
        cpx #TOK_LPAREN
        bne _chk_tok

        ldx #TOK_NONE
        jsr push_op             ; create a new context
        bne _err
        jsr match_expr          ; resolve new expression
        bne _err
        ldx #1
        jsr next_token
        cpx #TOK_RPAREN
        beq +

        lda #E_PAREN
        bra _err
+
        dec opstkp              ; drop the context
        lda #0
        bra _done
_chk_tok:
        cpx #0
        bne +

        lda #E_TERM
        bra _err
+
        jsr push_op
        bne _err
        jsr match_term
        bne _err
_err:
_done:
        rts


pop_op:
    ; process the top operator on the stack, consuming its input value(s) to produce an output value
    ; X has the current opstkp value which points to the next free slot
        dex                     ; drop the token
        stx opstkp
        lda 1,x                 ; get the token
        tax
        and #$f                 ; low nibble is operator
        asl                     ; double for jump table
        cpx #$10                ; high-nibble (prec) nonzero means binary
        tax                     ; X is jump offset
        bcs _binary
        jmp (ftbl_unary,x)
_binary:
        jmp (ftbl_binary,x)


push_op:
    ; add op X to the stack, first resolving any higher precedence operators already there
        stx tok

_chk_prec:
        ldx opstkp
        lda 0,x                 ; top token @ opstkp
        beq _ok                 ; the sentinel has lowest precedence

        ; op in A has greater or equal precedence if top nibble is less or equal
        and #$f0                ; ignore the bottom nibble
        cmp tok
        bcs _ok                 ; A >= tok means lower precedence

        jsr pop_op              ; otherwise resolve higher precedence op
        bne _err
        bra _chk_prec

_ok:
        lda tok
        sta 1,x
        inc opstkp
        lda #0
_err:
        rts



parse_string:
    ; consume up to 256 characters until delim
    ; X contains number of characters
    ;TODO escapes
    ;TODO end of line?
        ldx #0
-
        lda (cursor)
        cmp delim
        beq _done
        sta strbuf,x
        jsr next_chr
        inx
        bne -
_done:
        rts

parse_literal:
        ; initialize storage and status=0
        stz literal
        stz literal+1
        stz status

        ; check for explicit radix
        ldx #0
-
        lda radix_table,x
        beq _implied_radix      ; reach end of table
        cmp (cursor)
        beq _explicit_radix
        inx
        bra -

_explicit_radix:
        sta tmp                 ; stash in case it's a quote
        jsr next_chr            ; consume it
_implied_radix:
        ldy radix_value,x
        sty radix
        bne _digit

        ; radix 0 is ' or "
        sta delim
        jsr parse_string
        ldy #2
-
        lda literal
        sta literal+1
        lda strbuf-1,x
        sta literal
        inc status
        dey
        beq _finish
        dex
        beq _finish
        bra -

        ; start parsing number characters
_digit:
        lda (cursor)
        cmp #$60
        bcc +
        and #$5f                ; a-z => A-Z
+
        sec
        sbc #"0"
        bcc _finish
        cmp #10
        bcc +
        sbc #"A"-"9"-1          ; C=1 already
        cmp #10
        bcc _finish
+
        cmp radix
        bcs _finish

        inc status

        ; we have a digit to include!

        ldx literal
        stx tmp
        ldx literal+1
        stx tmp+1
        sta literal             ; store the digit
        stz literal+1

        ; add current * radix
        ldx radix
_mult:
        txa
        bne +
        jsr next_chr           ; consume it
        bra _digit
+
        lsr
        tax
        bcc +
        clc
        lda tmp
        adc literal
        sta literal
        lda tmp+1
        adc literal+1
        sta literal+1
+
        asl tmp
        rol tmp+1
        bra _mult

_finish:
        rts


next_token:
    ; input: X=1 for unary, =2 for binary
        jsr skipws
        lda (cursor)
        bne +
        ldx #0
        bra _done
+
        dex
        bne _binary

        ; look for unary operator, parenthesis, literal value or symbol

        ldx #n_unary
-
        cmp op_unary-1,x        ; note we're using 1-based index for unary
        beq _found
        dex
        bne -

        jsr parse_literal
        ldx status              ; success?
        beq _done

        ldx #TOK_LIT
        bra _done

_binary:
        ldx #n_binary-1
-
        cmp op_binary,x         ; 0-based index for binary
        beq _hit
        dex
        bne -

        bra _done               ; fail with X=0

;TODO check special ! which needs trailing =

_hit:
        ldy #1
        cmp #'='                ; '=' could also be '=='
        bne _chk_gtlt

        cmp (cursor),y          ; found ==
        beq _found2

_chk_gtlt:
        eor #%00111100          ; is A '<' or '>', ie. $3c or $3e ?
        bit #%11111101          ; zero if %00111100 or %00111110
        bne _foundb

        ; we have a '<', consider next character

        lda (cursor),y
        cmp #'='
        bne +

        ; token is <= or >=
        txa                     ; add offset to ge/le, note C=1
        adc #tok_gele - tok_gtlt - 1
        tax
        bra _found2

+       cmp (cursor)            ; doubled << or >> ?
        bne +

        ; token is << or >>
        txa
        adc #tok_lsrs - tok_gtlt - 1
        txa
        bra _found2

+       eor (cursor)            ; <> (or ><)
        cmp #%10
        bne _foundb             ; plain old <, >

        ; token is <>
        ldx #tok_neq - tok_binary

        ; fall through

_found2:
        jsr next_chr            ; consume two characters
_foundb:
        lda tok_binary,x        ; get the representation
        tax
_found:
        jsr next_chr            ; consume character
_done:
        ; X=0 if no token;
        ; X=1-based index for unary with high nibble (precedence) of 0
        ; else high-nibble >=1 for binary with low nibble disambiguating
        rts

.endsection


.section data

radix_table:
        .text "%&$'""", 0
radix_value:
        .text 2, 8, 16, 0, 0, 10

E_OK    = 0
E_TERM  = 1
E_PAREN = 2


TOK_NONE = 0
TOK_LIT = $ff

op_unary:
        .text "()+-!~<>"
n_unary = * - op_unary

TOK_LPAREN = 1
TOK_RPAREN = 2
TOK_UPLS = 3
TOK_UMNS = 4
TOK_UNOT = 5
TOK_UINV = 6
TOK_ULO = 7
TOK_UHI = 8

ftbl_unary = *-6                ; omit LPAREN/RPAREN, and 1-based table
    .word f_upls, f_umns, f_unot, f_uinv, f_ulo, f_uhi

op_binary:
        .text "*/%+-<>=&^|"
n_binary = * - op_binary

TOK_MUL = $10
TOK_DIV = $11
TOK_MOD = $12

TOK_PLS = $23
TOK_MNS = $24

TOK_LSHFT = $35
TOK_RSHFT = $36

TOK_LT  = $47
TOK_GT  = $48
TOK_LE  = $49
TOK_GE  = $4a

TOK_EQ  = $5b
TOK_NE  = $5c

TOK_AND = $6d

TOK_EOR = $7e

TOK_OR  = $8f

ftbl_binary:
    .word f_mul, f_div, f_mod, f_pls, f_mns, f_lshft, f_rshft
    .word f_lt, f_gt, f_le, f_ge, f_eq, f_ne, f_and, f_eor, f_or

; unary operators are all precedence 0 (highest)
; and are represented by their 1-based index in the op_unary list
; binary operators are represented with a high nibble
; containing their precedence and low nibble
; to disambiguate.  they're ordered according to the list above
; with some extra specials at the end.
tok_binary:
        .byte TOK_MUL, TOK_DIV, TOK_MOD     ; *, /, %
        .byte TOK_PLS, TOK_MNS              ; +, -
tok_gtlt:
        .byte TOK_LT, TOK_GT                ; <, >
        .byte TOK_EQ, TOK_AND, TOK_EOR, TOK_OR; = or ==, &, ^, |

; multi-character specials
tok_lsrs:
        .byte TOK_LSHFT, TOK_RSHFT          ; <<, >>
tok_gele:
        .byte TOK_LE, TOK_GE                ; <=, >=
tok_neq:
        .byte TOK_NE                        ; != or <> or ><


.endsection

.section code

; unary operators
f_umns:
        ldx vstkp
        lda #0
        sec
        sbc 0,x         ; LSB
        sta 0,x

        lda #0
        sbc 1,x         ; MSB
        sta 1,x
        bra f_ok1
f_unot:
        ldx vstkp
    ; not 0 is 1; not other is 0
        lda 0,x
        ora 1,x
        beq +
        lda #1
+       eor #1
        sta 0,x
        stz 1,x
        bra f_ok1
f_uinv:
        ldx vstkp
        lda #$FF
        eor 0,x         ; LSB
        sta 0,x

        lda #$FF
        eor 1,x         ; MSB
        sta 1,x
        bra f_ok1
f_uhi:
        ldx vstkp
        lda 1,x
        sta 0,x
        ; fall thru
f_ulo:
        ldx vstkp
        stz 1,x
        ; fall thru
        bra f_ok1
f_ok2:
        inx
        inx
        stx vstkp
f_ok1:
f_upls:
        lda #0
        rts

; binary operators

;TODO
f_mul:
f_div:
f_mod:

f_pls:
        ldx vstkp
        clc
        lda 0,x         ; LSB
        adc 2,x
        sta 2,x

        lda 1,x         ; MSB. No CLC, conserve carry bit
        adc 3,x
        sta 3,x
        bra f_ok2

f_mns:
        ldx vstkp
        sec
        lda 2,x         ; LSB
        sbc 0,x
        sta 2,x

        lda 3,x         ; MSB
        sbc 1,x
        sta 3,x
        bra f_ok2

f_lshft:
        ldx vstkp
        ldy 0,x
        cpy #16
        bcc _loop
        ldy #16
_loop:
        lsr 3,x
        ror 2,x
        dey
        bne _loop
        bra f_ok2

f_rshft:
        ldx vstkp
        ldy 0,x
        cpy #16
        bcc _loop
        ldy #16
_loop:
        asl 2,x
        rol 3,x
        dey
        bne _loop
        bra f_ok2

; TODO
f_lt:
f_gt:
f_le:
f_ge:

f_eq:
        ldx vstkp
        ldy #0                  ; default false

        lda 0,x                 ; LSB
        cmp 2,x
        bne _neq

        lda 1,x                 ; MSB
        cmp 3,x
        bne _neq

        iny
_neq:
        sty 2,x
        stz 3,x
jmp_ok2:
        jmp f_ok2

f_ne:
        ldx vstkp
        ldy #1                  ; default true

        lda 0,x                 ; LSB
        cmp 2,x
        bne _neq

        lda 1,x                 ; MSB
        cmp 3,x
        bne _neq

        dey
_neq:
        sty 2,x
        stz 3,x
        bra jmp_ok2

f_and:
        ldx vstkp
        lda 0,x
        and 2,x
        sta 2,x

        lda 1,x
        and 3,x
        sta 3,x
        bra jmp_ok2

f_eor:
        ldx vstkp
        lda 0,x
        eor 2,x
        sta 2,x

        lda 1,x
        eor 3,x
        sta 3,x
        bra jmp_ok2

f_or:
        ldx vstkp
        lda 0,x
        ora 2,x
        sta 2,x

        lda 1,x
        ora 3,x
        sta 3,x
        bra jmp_ok2


.endsection

; ---------------------------------------------------------------------
; eof
; ---------------------------------------------------------------------
