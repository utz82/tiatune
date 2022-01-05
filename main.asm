; Ideas
; + get rid of initial CLC
; + keep variables in X/Y
; + inline waveform code into RAM
; + precalulate waveforms
;   - poly4_5
; - rowLen via TIMxxT
; TODOs
; + adapt FreqDiv tables to faster loop
; - adapt tone lengths to faster loop

;TIAtune
;Atari 2600 music player
;by utz 10'2017 * irrlichtproject.de
;updated by Thomas Jentzsch 01'2021

;wave  AUDCx      range         type
;0     4,5,C,D    c-0..g-8      square div2
;1     8          c-0..g-8      poly9  div2
;2     1          c-0..gis-5    poly4  div15
;3     6,A        c-0..gis-4    r1813  div31
;4     7,9        c-0..gis-4    poly5  div31
;5     3 (TODO)

NTSC    = 1
;PAL     = !NTSC ;TODO

; define which waveforms should be excluded
NO_POLY9    = 0
NO_POLY4_5  = 0 ;TODO

        !to "test.bin", plain
        !sl "test.sym"
        !cpu 6510

        !source "vcs.h"
        !source "notes.h"

        * = $f080, invisible
        !pseudopc $80 {
seqOffs     !byte 0     ;seqence offset
ptnPtrL     !byte 0
ptnPtrH     !byte 0
ptnOffs     !byte 0
rowLenL     !byte 0     ;do Rowlen via Timer
rowLenH     !byte 0
VAR_END
        }

    !macro CreatePoly .init, .count, .tap1, .tap2 {
        !set .val = .init
        !set .idx = 0
        !for .x, 0, .count {
            !if (.val & .tap1) = .tap1 XOR (.val & .tap2) = .tap2 {
                !set .add = 1
            } else {
                !set .add = 0
            }
            !set .val = (.val << 1) + .add
            !set .pat = .pat << 1
            !if (.val & 1) = 1 {
                !set .pat = .pat | 1
            }
            !set .idx = .idx + 1
            !if .idx = 8 {
              !set .idx = 0
              !byte <.pat
              !set .pat = 0
            }
        }
        !set .pat = .pat << 1
        !byte <.pat
    }

    *       = $f000
    !zone main

PatternTbl
;Note: 1st bit of a pattern MUST always be set!
SquarePtn
    !byte   %01010101
Poly9Ptn
  !ifndef NO_POLY9 {
    +CreatePoly 1, 510, 8, 256
  }
Poly4Ptn
    +CreatePoly 1, 14, 1, 8
;    !byte   %01110000, %10100110
R1813Ptn
    !byte   %00000000, %00000111, %11111111, %11111110
Poly5Ptn
    +CreatePoly 1, 30, 2, 16
;    !byte   %00001010, %11101100, %01111100, %11010010
Poly4_5Ptn  ;TODO
  !ifndef NO_POLY4_5 {
  }

PatternPtr
    !byte   <SquarePtn, <Poly9Ptn, <Poly4Ptn, <R1813Ptn, <Poly5Ptn
InitVal
    !byte   $ea, $0a, $0a, $0a, $0a ; NOP, ASL
ResetVal
    !byte     0,  63,   1,   3,   3

reset
    cld
    ldx     #0                  ;clear TIA regs, RAM, set SP to $00ff
    txa                         ;alternatively just use CLEAN_START macro
-
    dex
    txs
    pha
    bne     -

;relocate player to zeropage
    ldx     #PlayerCode_End - PlayerCode - 1
-
    lda     PlayerCode,x
    sta     VAR_END,x
    dex
    bpl     -

.readSeq                        ;read next entry in sequence
    ldy     seqOffs
    lda     sequence_hi,y
    beq     reset               ;if hi-byte = 0, loop
    sta     ptnPtrH
    lda     sequence_lo,y
    sta     ptnPtrL
    inc     seqOffs
    ldy     #0
    sty     ptnOffs

ReadPtn
    ldy     ptnOffs
    lda     (ptnPtrL),y         ;ctrl byte
    beq     .readSeq            ;0-end marker

    sta     rowLenH
    bmi     .noCh0Reload

    iny
    lax     (ptnPtrL),y         ;wave0/vol0
    lsr
    lsr
    lsr
    sta+1   Vol0
    txa
    and     #$7
    tax
    lda     PatternPtr,x
    sta+1   Pattern0
    lda     InitVal,x
    sta+1   Init0
    lda     ResetVal,x
    sta+1   Reset0

    iny
    cpx     #2                  ;look up freq divider depending on waveform used
    bcs     +                   ;0,1 -> square/poly9
    lax     (ptnPtrL),y         ;note
    lda     FreqDiv2Lsb,x
    sta+1   Freq0L
    lda     FreqDiv2Msb,x
    bcc     .contCh0

+
    bne     +                   ;2 -> poly4
    lax     (ptnPtrL),y
    lda     FreqDiv15Lsb,x
    sta+1   Freq0L
    lda     FreqDiv15Msb,x
    bcs     .contCh0

+
    lax     (ptnPtrL),y         ;3,4 -> 1813/poly5
    lda     FreqDiv31Lsb,x
    sta+1   Freq0L
    lda     FreqDiv31Msb,x
.contCh0
    sta+1   Freq0H
.noCh0Reload

    bit     rowLenH
    bvs     .noCh1Reload

    iny
    lax     (ptnPtrL),y         ;wave1/vol1
    lsr
    lsr
    lsr
    sta+1   Vol1
    txa
    and     #$7
    tax
    lda     PatternPtr,x
    sta+1   Pattern1
    lda     InitVal,x
    sta+1   Init1
    lda     ResetVal,x
    sta+1   Reset1

    iny
    cpx     #2                  ;look up freq divider depending on waveform used
    bcs     +                   ;0,1 -> square/poly9
    lax     (ptnPtrL),y         ;note
    lda     FreqDiv2Lsb,x
    sta+1   Freq1L
    lda     FreqDiv2Msb,x
    bcc     .contCh1

+
    bne     +                   ;2 -> poly4
    lax     (ptnPtrL),y
    lda     FreqDiv15Lsb,x
    sta+1   Freq1L
    lda     FreqDiv15Msb,x
    bcs     .contCh1

+
    lax     (ptnPtrL),y         ;3,4 -> 1813/poly5
    lda     FreqDiv31Lsb,x
    sta+1   Freq1L
    lda     FreqDiv31Msb,x
.contCh1
    sta+1   Freq1H
.noCh1Reload

    iny
    sty     ptnOffs

    lda     rowLenH
    and     #$3f
    sta     rowLenH
    lsr
    lsr
    adc     rowLenH
    sta     rowLenH

    ldx     #$ff
    stx+1   Mask0
    stx+1   Mask1
    inx
    ldy     #0
    jmp     PlayNote


PlayerCode = *

    !pseudopc VAR_END {         ;actual player runs on zeropage

.waitCh0                        ;3
    jmp     WaitCh0             ;25  = 28

PlayNote
Sum0L   = *+1
    lda     #0                  ;2
Freq0L  = *+1
    adc     #0                  ;2           CF==1!
    sta     Sum0L               ;3
Sum0H   = *+1
    lda     #0                  ;2
Freq0H  = *+1
    adc     #0                  ;2
    sta     Sum0H               ;3   = 14

    bcc     .waitCh0            ;2/3 = 2/3
;create waveform from table
;assumes 1st bit of waveform alwaws set
Mask0   = *+1
    lda     #0                  ;2
    bmi     .nextMask0          ;2/3
    asl                         ;2
    sec                         ;2
    bne     .cont0              ;3

.nextMask0                      ;5
    lda     #$01                ;2
    dex                         ;2
    bmi     .resetIdx0          ;2/3
.cont0
    sta+1   Mask0               ;3   = 14
Pattern0 = *+1
    and     PatternTbl,x        ;4
    bne     .loadV0             ;2/3
    beq     .zeroV0             ;3

.resetIdx0                      ;12          assumes 1st bit set
Reset0  = *+1
    ldx     #3                  ;2           0,1,3,63(,57)
Init0   = *
    asl                         ;2           or NOP (square,poly4->5)
;    lda     #$02                ;2           or $01 (square,poly4->5)
    sta+1   Mask0               ;3
    sec                         ;2   (= 14 = 21-7)
.loadV0
Vol0    = *+1
    lda     #0                  ;2
.zeroV0
    sta     AUDV0               ;3   = 12
ContinueCh0

Sum1L   = *+1
    lda     #0                  ;2
Freq1L  = *+1
    adc     #0                  ;2           CF==1!
    sta     Sum1L               ;3
Sum1H   = *+1
    lda     #0                  ;2
Freq1H  = *+1
    adc     #0                  ;2
    sta     Sum1H               ;3   = 14

    bcc     .waitCh1            ;2/3 =  2/3
Mask1   = *+1
    lda     #0                  ;2
    bmi     .nextMask1          ;2/3
    asl                         ;2
    sec                         ;2
    bne     .cont1              ;3

.nextMask1                      ;5
    lda     #$01                ;2
    dex                         ;2
    bmi     .resetIdx1          ;2/3
.cont1
    sta+1   Mask1               ;3   = 14
Pattern1 = *+1
    and     PatternTbl,x        ;4
    bne     .loadV1             ;2/3
    beq     .zeroV1             ;3

.resetIdx1                      ;12          assumes 1st bit set
Reset1  = *+1
    ldx     #3                  ;2           0,1,3,63,57
Init1   = *
    asl                         ;2           or NOP (square,poly4->5)
    sta+1   Mask1               ;3
    sec                         ;2   (= 14 = 21-7)
.loadV1
Vol1    = *+1
    lda     #0                  ;2
.zeroV1
    sta     AUDV1               ;3   = 12
ContinueCh1

    dec     rowLenL             ;5          always loops 256 times here
    bne     PlayNote            ;3/2 =  8/7
                                ;           avg 92 cycles (was 114)
    dec     rowLenH             ;5
    bne     PlayNote            ;3/2 =  8/7
    jmp     ReadPtn             ;3

.waitCh1                        ;3
    jmp     WaitCh1             ;25 = 28
}

PlayerCode_End = *


WaitCh0                         ;3
    jsr     Wait19              ;19
    jmp     ContinueCh0         ;3  = 25

WaitCh1                         ;3
    jsr     Wait19              ;19
    jmp     ContinueCh1         ;3  = 25

Wait19                          ;6
    bit    $ea                  ;3
    nop                         ;2
    sec                         ;2
    rts                         ;6  = 19

  !if NTSC { ; {
FreqDiv31Lsb
    !byte   $00, $98, $3a, $e6, $9a, $5b, $25, $fd, $e2, $d4, $d5, $e4  ;c-0..b-0
    !byte   $02, $34, $77, $cc, $36, $b6, $4e, $fc, $c4, $a9, $a9, $c9  ;c-1..b-1
    !byte   $08, $69, $ef, $9a, $70, $6f, $9d, $fa, $8c, $53, $54, $91  ;c-2..b-2
    !byte   $10, $d3, $de, $36, $df, $df, $3b, $f6, $18, $a7, $a9, $24  ;c-3..b-3
    !byte   $22, $a7, $bd, $6f, $c1, $c1, $76, $ee, $31                 ;c-4..gis-4
FreqDiv31Msb
    !byte   $0a, $0a, $0b, $0b, $0c, $0d, $0e, $0e, $0f, $10, $11, $12  ;c-0..b-0
    !byte   $14, $15, $16, $17, $19, $1a, $1c, $1d, $1f, $21, $23, $25  ;c-1..b-1
    !byte   $28, $2a, $2c, $2f, $32, $35, $38, $3b, $3f, $43, $47, $4b  ;c-2..b-2
    !byte   $50, $54, $59, $5f, $64, $6a, $71, $77, $7f, $86, $8e, $97  ;c-3..b-3
    !byte   $a0, $a9, $b3, $be, $c9, $d5, $e2, $ef, $fe                 ;c-4..gis-4

FreqDiv15Lsb
    !byte   $d6, $20, $6e, $c1, $18, $76, $d7, $40, $af, $24, $a0, $23  ;c-0..b-0
    !byte   $ae, $42, $de, $83, $32, $ec, $b1, $82, $5e, $49, $41, $48  ;c-1..b-1
    !byte   $5e, $85, $bd, $08, $67, $da, $64, $05, $bf, $93, $83, $90  ;c-2..b-2
    !byte   $bc, $0a, $7b, $11, $ce, $b5, $c9, $0b, $7e, $27, $07, $21  ;c-3..b-3
    !byte   $7b, $16, $f8, $24, $9f, $6d, $93, $18, $fe, $4f, $0f, $44  ;c-4..b-4
    !byte   $f7, $2f, $f2, $49, $3f, $db, $28, $30, $ff                 ;c-5..gis-5
FreqDiv15Msb
    !byte   $04, $05, $05, $05, $06, $06, $06, $07, $07, $08, $08, $09  ;c-0..b-0
    !byte   $09, $0a, $0a, $0b, $0c, $0c, $0d, $0e, $0f, $10, $11, $12  ;c-1..b-1
    !byte   $13, $14, $15, $17, $18, $19, $1b, $1d, $1e, $20, $22, $24  ;c-2..b-2
    !byte   $26, $29, $2b, $2e, $30, $33, $36, $3a, $3d, $41, $45, $49  ;c-3..b-3
    !byte   $4d, $52, $56, $5c, $61, $67, $6d, $74, $7a, $82, $8a, $92  ;c-4..b-4
    !byte   $9a, $a4, $ad, $b8, $c3, $ce, $db, $e8, $f5                 ;c-5..gis-5

FreqDiv2Lsb
    !byte   $a4, $ae, $b8, $c3, $cf, $db, $e8, $f6, $05, $15, $25, $37  ;c-0..b-0
    !byte   $49, $5d, $72, $88, $9f, $b8, $d2, $ee, $0b, $2b, $4c, $6f  ;c-1..b-1
    !byte   $94, $bb, $e5, $11, $40, $71, $a6, $dd, $18, $57, $99, $df  ;c-2..b-2
    !byte   $29, $78, $cb, $23, $81, $e4, $4d, $bc, $32, $af, $33, $bf  ;c-3..b-3
    !byte   $53, $f1, $97, $48, $03, $c9, $9b, $79, $65, $5f, $67, $7f  ;c-4..b-4
    !byte   $a8, $e3, $30, $91, $07, $93, $37, $f4, $cb, $bf, $d0, $00  ;c-5..b-5
    !byte   $52, $c7, $61, $24, $10, $28, $70, $ea, $98, $7f, $a1, $02  ;c-6..b-6
    !byte   $a5, $8f, $c4, $49, $21, $52, $e2, $d5, $32, $ff, $43, $05  ;c-7..b-7
    !byte   $4c, $20, $8a, $93, $43, $a6, $c5, $ab                      ;c-8..g-8
FreqDiv2Msb
    !byte   $00, $00, $00, $00, $00, $00, $00, $00, $01, $01, $01, $01  ;c-0..b-0
    !byte   $01, $01, $01, $01, $01, $01, $01, $01, $02, $02, $02, $02  ;c-1..b-1
    !byte   $02, $02, $02, $03, $03, $03, $03, $03, $04, $04, $04, $04  ;c-2..b-2
    !byte   $05, $05, $05, $06, $06, $06, $07, $07, $08, $08, $09, $09  ;c-3..b-3
    !byte   $0a, $0a, $0b, $0c, $0d, $0d, $0e, $0f, $10, $11, $12, $13  ;c-4..b-4
    !byte   $14, $15, $17, $18, $1a, $1b, $1d, $1e, $20, $22, $24, $27  ;c-5..b-5
    !byte   $29, $2b, $2e, $31, $34, $37, $3a, $3d, $41, $45, $49, $4e  ;c-6..b-6
    !byte   $52, $57, $5c, $62, $68, $6e, $74, $7b, $83, $8a, $93, $9c  ;c-7..b-7
    !byte   $a5, $af, $b9, $c4, $d0, $dc, $e9, $f7                      ;c-8..g-8
  } ;} NTSC

    !zone musicdata
musicData
    !source "music.asm"


    * = $fffc

    !word   reset          ; RESET
    !word   reset          ; IRQ

;       END
