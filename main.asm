; Ideas
; + get rid of initial CLC
; + keep variables in X/Y
; + inline waveform code into RAM
; + precalulate waveforms
;   - poly4_5
; - rowLen via TIMxxT
; TODOs
; - adapt FreqDiv tables to faster loop
; - adapt tone lengths to faster loop

;TIAtune
;Atari 2600 music player
;by utz 10'2017 * irrlichtproject.de
;updated by Thomas Jentzsch 01'2021

;wave  AUDCx      range         type
;0     4,5,C,D    c-0..dis8     square div2
;1     8          c-0..dis8     poly9  div2
;2     1          c-0..e-4      poly4  div15
;3     6,A        c-0..e-3      r1813  div15
;4     7,9        c-0..e-3      poly5  div31
;5     3

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
    dec     rowLenH
    bne     PlayNote

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

FreqDiv15Lsb
    !byte    $9-1,$63-1,$bd-1,$26-1,$8f-1, $7-1,$7f-1, $6-1,$8d-1,$23-1,$b9-1,$5e-1
    !byte    $3-1,$c6-1,$7a-1,$4c-1,$2d-1, $e-1,$fe-1, $c-1,$1a-1,$37-1,$72-1,$bc-1
    !byte   $15-1,$7d-1, $3-1,$98-1,$4b-1,$1c-1, $b-1, $9-1,$34-1,$7d-1,$e4-1,$69-1
    !byte   $1b-1,$fa-1, $6-1,$3f-1,$a5-1,$38-1, $7-1,$21-1,$68-1,$eb-1,$b9-1,$d2-1
    !byte   $45-1,$f4-1, $c-1,$6f-1,$3b-1,$7f-1,$1d-1,$33-1,$c1-1,$d6-1,$81-1,$b3-1
    !byte   $7b-1,$e8-1, $9-1,$ed-1,$85

FreqDiv15Msb
    !byte   $6,$6,$6,$7,$7,$8,$8,$9,$9,$a,$a,$b
    !byte   $c,$c,$d,$e,$f,$10,$10,$12,$13,$14,$15,$16
    !byte   $18,$19,$1b,$1c,$1e,$20,$22,$24,$26,$28,$2a,$2d
    !byte   $30,$32,$36,$39,$3c,$40,$44,$48,$4c,$50,$55,$5a
    !byte   $60,$65,$6c,$72,$79,$80,$88,$90,$98,$a1,$ab,$b5
    !byte   $c0,$cb,$d8,$e4,$f2

FreqDiv31Lsb
    !byte   $79-1,$33-1,$ed-1,$c6-1,$9f-1,$97-1,$8f-1,$a6-1,$bd-1,$f3-1,$29-1,$7e-1
    !byte   $d3-1,$66-1,$da-1,$8c-1,$5d-1,$2e-1,$1e-1,$4c-1,$7a-1,$c7-1,$52-1,$fc-1
    !byte   $c5-1,$ad-1,$d3-1,$18-1,$9b-1,$5c-1,$5b-1,$79-1,$f4-1,$ad-1,$a4-1,$d9-1
    !byte   $6b-1,$5a-1,$a6-1,$4f-1,$55-1,$b8-1,$97-1,$11-1,$e8-1,$3b-1,$29-1,$b2-1
    !byte   $f5-1,$b4-1,$4c-1,$7f-1,$8b-1

FreqDiv31Msb
    !byte   $c,$d,$d,$e,$f,$10,$11,$12,$13,$14,$16,$17
    !byte   $18,$1a,$1b,$1d,$1f,$21,$23,$25,$27,$29,$2c,$2e
    !byte   $31,$34,$37,$3b,$3e,$42,$46,$4a,$4e,$53,$58,$5d
    !byte   $63,$69,$6f,$76,$7d,$84,$8c,$95,$9d,$a7,$b1,$bb
    !byte   $c6,$d2,$df,$ec,$fa

FreqDiv2Lsb
    !byte   $ce-1,$da-1,$e6-1,$f4-1, $2-1,$12-1,$22-1,$34-1,$46-1,$5a-1,$6e-1,$84-1
    !byte   $9a-1,$b4-1,$cc-1,$e8-1, $6-1,$24-1,$44-1,$68-1,$8c-1,$b2-1,$dc-1, $8-1
    !byte   $36-1,$66-1,$9a-1,$d0-1, $a-1,$48-1,$8a-1,$ce-1,$18-1,$66-1,$b8-1, $e-1
    !byte   $6a-1,$cc-1,$34-1,$a2-1,$16-1,$90-1,$12-1,$9e-1,$30-1,$ca-1,$6e-1,$1c-1
    !byte   $d6-1,$98-1,$68-1,$42-1,$2a-1,$22-1,$26-1,$3a-1,$5e-1,$94-1,$de-1,$3a-1
    !byte   $aa-1,$30-1,$ce-1,$86-1,$56-1,$42-1,$4c-1,$74-1,$be-1,$2a-1,$ba-1,$72-1
    !byte   $54-1,$62-1,$9e-1, $a-1,$ac-1,$84-1,$98-1,$e8-1,$7c-1,$54-1,$76-1,$e6-1
    !byte   $a8-1,$c4-1,$3c-1,$16-1,$58-1, $8-1,$2e-1,$d0-1,$f6-1,$a6-1,$ea-1,$cc-1
    !byte   $52-1,$86-1,$76-1,$2a-1

FreqDiv2Msb
    !byte   $0,$0,$0,$0,$1,$1,$1,$1,$1,$1,$1,$1
    !byte   $1,$1,$1,$1,$2,$2,$2,$2,$2,$2,$2,$3
    !byte   $3,$3,$3,$3,$4,$4,$4,$4,$5,$5,$5,$6
    !byte   $6,$6,$7,$7,$8,$8,$9,$9,$a,$a,$b,$c
    !byte   $c,$d,$e,$f,$10,$11,$12,$13,$14,$15,$16,$18
    !byte   $19,$1b,$1c,$1e,$20,$22,$24,$26,$28,$2b,$2d,$30
    !byte   $33,$36,$39,$3d,$40,$44,$48,$4c,$51,$56,$5b,$60
    !byte   $66,$6c,$73,$7a,$81,$89,$91,$99,$a2,$ac,$b6,$c1
    !byte   $cd,$d9,$e6,$f4

    !zone musicdata
musicData
    !source "music.asm"


    * = $fffc

    !word   reset          ; RESET
    !word   reset          ; IRQ

;       END
