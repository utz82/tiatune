; Ideas:
; + get rid of initial CLC
; + keep variables in X/Y
; + inline waveform code into RAM
; + precalulate waveforms
;   - poly4_5
; - rowLen via TIMxxT

; TODOs:
; + adapt FreqDiv tables to faster loop
; - adapt tone lengths to faster loop
; + sounds rough, maybe preserve X/Y between notes

;TIAtune
;Atari 2600 music player
;by utz 10'2017 * irrlichtproject.de
;improved by Thomas Jentzsch 01'2021

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

        !to "tiatune.bin", plain
        !sl "tiatune.sym"
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
saveX       !byte 0
saveY       !byte 0
VAR_END
        }

    !macro CreatePoly .init, .count, .tap1, .tap2 {
        !set .val = .init
        !set .idx = 0
        !for .x, 0, .count {
            !if (.val & .tap1) != 0 XOR (.val & .tap2) != 0 {
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

Reset
    cld
    ldx     #0                  ;clear TIA regs, RAM, set SP to $00ff
    txa                         ;alternatively just use CLEAN_START macro
-
    dex
    txs
    pha
    bne     -

;relocate player to zeropage
    ldx     #PlayerLength - 1
-
    lda     PlayerCode,x
    sta     VAR_END,x
    dex
    bpl     -

.readSeq                        ;read next entry in sequence
    ldy     seqOffs
    lda     sequence_hi,y
    beq     Reset               ;if hi-byte = 0, loop
    sta     ptnPtrH
    lda     sequence_lo,y
    sta     ptnPtrL
    inc     seqOffs
    ldy     #0
    sty     ptnOffs
    beq     .enterPtn

ReadPtn
    stx     saveX
    sty     saveY

    ldy     ptnOffs
.enterPtn
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
    cmp     saveX
    bcs     .xOK
    sta     saveX
.xOK

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
    cmp     saveY
    bcs     .yOK
    sta     saveY
.yOK

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
;;hack: coarse slow down to adapt to faster loops
;    lsr
;    lsr
;    adc     rowLenH
;    sta     rowLenH

    ldx     saveX
    ldy     saveY
;    sec
    jmp     PlayNote

PlayerCode = *

    !pseudopc VAR_END {         ;actual player runs on zeropage

.loopH                          ;8
    dec     rowLenH             ;5
    bne     PlayNote            ;3/2 =  8/7
    jmp     ReadPtn             ;3
;---------------------------------------
.waitCh0                        ;3
    jmp     WaitCh0             ;25  = 28

.resetIdx1                      ;12          assumes 1st bit set
Reset1  = *+1
    ldy     #3                  ;2           0,1,3,63,57
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
    beq     .loopH              ;2/3 =  7/8
                                ;           avg 91 cycles (was 114)
;---------------------------------------
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
;--------------------------------------------------
  !if 0 { ;{
Mask0   = *+1
    lda     #1                  ;2
    bpl     .contMask0          ;2/3
    lda     #$01                ;2
    dex                         ;2
    bmi     .resetIdx0          ;2/3
    sta+1   Mask0               ;3   = 13
Pattern0 = *+1
    and     PatternTbl,x        ;4
    bne     .loadV0             ;2/3
    beq     .zeroV0             ;3

.contMask0                      ;5
    asl                         ;2
    sec                         ;2
    sta+2   Mask0               ;4   = 13
    and     PatternTbl,x        ;4
    bne     .loadV0             ;2/3
    beq     .zeroV0             ;3

.resetIdx0                      ;11          assumes 1st bit set
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
; 40 bytes
  } ;}
;--------------------------------------------------
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
; 32 bytes
ContinueCh0
;---------------------------------------
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
    dey                         ;2
    bmi     .resetIdx1          ;2/3
.cont1
    sta+1   Mask1               ;3   = 14
Pattern1 = *+1
    and     PatternTbl,y        ;4
    bne     .loadV1             ;2/3
    beq     .zeroV1             ;3

.waitCh1                        ;3
    jmp     WaitCh1             ;25  = 28
}
PlayerLength = * - PlayerCode

WaitCh0                         ;3
    jsr     Wait19              ;19
    jmp     ContinueCh0         ;3   = 25

WaitCh1                         ;3
    jsr     Wait19              ;19
    jmp     ContinueCh1         ;3   = 25

Wait19                          ;6
    asl    $2e                  ;5
    sec                         ;2
    rts                         ;6   = 19

  !if NTSC { ; {
FreqDiv2Lsb
    !byte   $A2, $AC, $B6, $C1, $CC, $D9, $E6, $F3, $02, $11, $22, $33 ;c-0..b-0
    !byte   $45, $59, $6E, $83, $9A, $B3, $CD, $E8, $06, $24, $45, $68 ;c-1..b-1
    !byte   $8D, $B3, $DD, $08, $37, $68, $9B, $D2, $0D, $4A, $8C, $D1 ;c-2..b-2
    !byte   $1B, $68, $BB, $12, $6F, $D1, $38, $A6, $1B, $96, $19, $A4 ;c-3..b-3
    !byte   $37, $D2, $77, $26, $DF, $A3, $72, $4E, $37, $2E, $34, $49 ;c-4..b-4
    !byte   $6F, $A6, $F0, $4D, $BF, $47, $E6, $9E, $70, $5E, $6A, $94 ;c-5..b-5
    !byte   $DF, $4E, $E1, $9B, $7F, $8F, $CE, $3E, $E2, $BE, $D5, $2A ;c-6..b-6
    !byte   $C0, $9D, $C3, $38, $00, $20, $9E, $7E, $C6, $7E, $AB, $55 ;c-7..b-7
    !byte   $82, $3B, $88, $72, $02, $42, $3D, $FD                     ;c-8..g-8
FreqDiv2Msb
    !byte   $00, $00, $00, $00, $00, $00, $00, $00, $01, $01, $01, $01 ;c-0..b-0
    !byte   $01, $01, $01, $01, $01, $01, $01, $01, $02, $02, $02, $02 ;c-1..b-1
    !byte   $02, $02, $02, $03, $03, $03, $03, $03, $04, $04, $04, $04 ;c-2..b-2
    !byte   $05, $05, $05, $06, $06, $06, $07, $07, $08, $08, $09, $09 ;c-3..b-3
    !byte   $0A, $0A, $0B, $0C, $0C, $0D, $0E, $0F, $10, $11, $12, $13 ;c-4..b-4
    !byte   $14, $15, $16, $18, $19, $1B, $1C, $1E, $20, $22, $24, $26 ;c-5..b-5
    !byte   $28, $2B, $2D, $30, $33, $36, $39, $3D, $40, $44, $48, $4D ;c-6..b-6
    !byte   $51, $56, $5B, $61, $67, $6D, $73, $7A, $81, $89, $91, $9A ;c-7..b-7
    !byte   $A3, $AD, $B7, $C2, $CE, $DA, $E7, $F4                     ;c-8..g-8

FreqDiv15Lsb
    !byte   $C9, $11, $5F, $B1, $07, $64, $C4, $2C, $99, $0D, $88, $0A ;c-0..b-0
    !byte   $93, $25, $C0, $63, $10, $C8, $8B, $59, $34, $1B, $11, $15 ;c-1..b-1
    !byte   $28, $4C, $81, $C8, $23, $93, $18, $B4, $6A, $38, $23, $2B ;c-2..b-2
    !byte   $51, $99, $03, $91, $47, $26, $31, $6A, $D4, $72, $47, $57 ;c-3..b-3
    !byte   $A4, $33, $07, $25, $90, $4E, $63, $D6, $AA, $E6, $90, $AF ;c-4..b-4
    !byte   $4A, $68, $10, $4B, $22, $9E, $C9, $AD, $55                ;c-5..gis-5
FreqDiv15Msb
    !byte   $04, $05, $05, $05, $06, $06, $06, $07, $07, $08, $08, $09 ;c-0..b-0
    !byte   $09, $0A, $0A, $0B, $0C, $0C, $0D, $0E, $0F, $10, $11, $12 ;c-1..b-1
    !byte   $13, $14, $15, $16, $18, $19, $1B, $1C, $1E, $20, $22, $24 ;c-2..b-2
    !byte   $26, $28, $2B, $2D, $30, $33, $36, $39, $3C, $40, $44, $48 ;c-3..b-3
    !byte   $4C, $51, $56, $5B, $60, $66, $6C, $72, $79, $80, $88, $90 ;c-4..b-4
    !byte   $99, $A2, $AC, $B6, $C1, $CC, $D8, $E5, $F3                ;c-5..gis-5

FreqDiv31Lsb
    !byte   $E5, $7B, $1B, $C5, $77, $36, $FE, $D4, $B6, $A5, $A3, $AF ;c-0..b-0
    !byte   $CB, $F9, $38, $8A, $F0, $6C, $FF, $A9, $6C, $4B, $46, $60 ;c-1..b-1
    !byte   $99, $F4, $72, $16, $E4, $DB, $00, $54, $DC, $98, $8E, $C0 ;c-2..b-2
    !byte   $32, $E8, $E5, $2E, $C8, $B7, $01, $AA, $B8, $32, $1D, $82 ;c-3..b-3
    !byte   $67, $D1, $CB, $5F, $92, $70, $02, $56, $71                ;c-4..gis-4
FreqDiv31Msb
    !byte   $09, $0A, $0B, $0B, $0C, $0D, $0D, $0E, $0F, $10, $11, $12 ;c-0..b-0
    !byte   $13, $14, $16, $17, $18, $1A, $1B, $1D, $1F, $21, $23, $25 ;c-1..b-1
    !byte   $27, $29, $2C, $2F, $31, $34, $38, $3B, $3E, $42, $46, $4A ;c-2..b-2
    !byte   $4F, $53, $58, $5E, $63, $69, $70, $76, $7D, $85, $8D, $95 ;c-3..b-3
    !byte   $9E, $A7, $B1, $BC, $C7, $D3, $E0, $ED, $FB                ;c-4..gis-4
  } ;} NTSC

    !zone debug
    !warn * - $f000, " bytes free"

    !zone musicdata
musicData
    !source "music.asm"

    * = $fffc

    !word   Reset          ; RESET
    !word   Reset          ; IRQ

;       END
