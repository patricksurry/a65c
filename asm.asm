        .cpu "65c02"
        .enc "none"

* = 0
.dsection zp

* = $200
.dsection code
.dsection data
.align
.dsection test

* = $1000
.dsection bss


.section zp

cursor  .word ?

.endsection

.section bss

linebuf = *
    .fill $100
strbuf = *
    .fill $100

.endsection

.section code

main:
        jsr get_line
        lda #<linebuf
        sta cursor
        lda #>linebuf
        sta cursor+1
        jsr match_mnemonic
        bra main


match_mnemonic:
        ; pack "ABC" as (MSB) %aaa aabbb  bbc ccccf (LSB)
        stz tmp
        stz tmp+1
        ldx #3
_pack:
        lda (cursor)
        cmp #$40
        bcc _fail
        jsr next_chr
        asl
        asl
        asl
        ldy #5
-
        asl a
        rol tmp
        rol tmp+1
        dey
        bne -

        dex
        bne _pack
        asl tmp
        rol tmp+1

        jsr prnl
        ldy tmp
        lda tmp+1
        jsr prword
        jsr prnl

        ldx (n_mnemonics - 1) * 2
        ; TODO there is a gap we could avoid if we wanted
_search:
        lda mnemonics+1,x
        cmp tmp+1
        bne +
        lda mnemonics,x
        and #$fe
        cmp tmp
        bne +
        txa
        lsr
        jsr prbyte
        jsr prnl
+
        dex
        dex
        bne _search
_fail:  ;TODO
        rts


; ---------------------------------------------------------------------
; helpers (dup)


skipws:
        lda (cursor)
        beq +
        cmp #' '+1
        bcs +
        jsr next_chr
        bra skipws
+
        rts

next_chr:
    ;TODO end of line?

        inc cursor
        bne +
        inc cursor+1
+       rts





kernel_putc:
        sta $f001
        rts


get_line:
        ldx #0
-
        jsr kernel_getc
        beq -
        cmp #$0d
        bne +
        lda #$0a
+
        sta linebuf,x
        jsr putc
        inx
        cmp #$0a
        bne -
        rts

kernel_getc:
        lda $f004
        rts

.endsection

.include 'expr.asm'
.include 'dasm.asm'