;TIAtune
;Atari 2600 music player
;by utz 10'2017 * irrlichtproject.de
;improved by Thomas Jentzsch 01'2021

; Ideas:
; + get rid of initial CLC
; + keep variables in X/Y
; + inline waveform code into RAM
; + precalulate waveforms
;   - poly4_5
; - rowLen via TIMxxT
; - variable tempo (using fractional math here too)
; - use wait cycles (e.g. prepare next loop)

; TODOs:
; + adapt FreqDiv tables to faster loop
; - adapt tone lengths to faster loop
; + sounds rough, maybe preserve X/Y between notes

; Legend: + done, o partially done, - todo, x canceled

;wave  AUDCx      range         type
;0     4,5,C,D    c-0..gis-8    square div2
;1     8          c-0..gis-8    poly9  div2
;2     1          c-0..a-5      poly4  div15
;3     6,A        c-0..gis-4    r1813  div31
;4     7,9        c-0..gis-4    poly5  div31
;5     3 (TODO)

;Assembler switches
NTSC    = 1 ; else PAL (TODO)
VISUALS = 1 ; add some visuals (from channel 0 only) (+75 bytes)

;Define which waveforms should be excluded
;NO_POLY9
;NO_POLY4_5 ;TODO

    !to "tiatune.bin", plain
    !sl "tiatune.sym"
    !cpu 6510

    !source "vcs.h"
    !source "notes.h"

    * = $f000, invisible
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
;    clc
    jmp     PlayNote

PlayerCode = *

    !pseudopc VAR_END {         ;actual player runs on zeropage

.loopH                          ;8
    dec     rowLenH             ;5
    bne     PlayNote            ;3/2 =  8/7
    jmp     ReadPtn             ;3
;---------------------------------------
  !ifndef VISUALS {
.waitCh0                        ;3
    jmp     WaitCh0             ;24  = 27
  }

.resetIdx1                      ;11          assumes 1st bit set
Reset1  = *+1
    ldy     #3                  ;2   = 13    0,1,3,63(,57)
Init1   = *
    asl                         ;2           or NOP (square,poly4->5)
    sta+1   Mask1               ;3
    nop                         ;2
.loadV1
Vol1    = *+1
    lda     #0                  ;2
.zeroV1
    sta     AUDV1               ;3   = 12
ContinueCh1

    dec     rowLenL             ;5          always loops 256 times here
    beq     .loopH              ;2/3 =  7/8
                                ;           avg 89 cycles (was 114)
;---------------------------------------
PlayNote
.sum0L  = *+1
    lda     #0                  ;2
Freq0L  = *+1
    adc     #0                  ;2           CF==0!
    sta     .sum0L              ;3
Sum0H  = *+1
    lda     #0                  ;2
Freq0H  = *+1
    adc     #0                  ;2
    sta     Sum0H               ;3   = 14

    bcs     .waitCh0            ;2/3 = 2/3
;create waveform from table
;assumes 1st bit of waveform always set
Mask0   = *+1
    lda     #1                  ;2
    bpl     .contMask0          ;2/3
    lda     #$01                ;2
    dex                         ;2
    bmi     .resetIdx0          ;2/3
.cont0
    sta+1   Mask0               ;3   = 13
Pattern0 = *+1
    and     PatternTbl,x        ;4
    bne     .loadV0             ;2/3
    beq     .zeroV0             ;3

.contMask0                      ;5
    asl                         ;2
    bcc     .cont0              ;3

.resetIdx0                      ;11          assumes 1st bit set
Reset0  = *+1
    ldx     #3                  ;2   = 13    0,1,3,63(,57)
Init0   = *
    asl                         ;2           or NOP (square,poly4->5)
;    lda     #$02                ;2           or $01 (square,poly4->5)
    sta+1   Mask0               ;3
    nop                         ;2
.loadV0
Vol0    = *+1
    lda     #0                  ;2
.zeroV0
    sta     AUDV0               ;3   = 12
; 31 bytes
ContinueCh0
;---------------------------------------
.sum1L  = *+1
    lda     #0                  ;2
Freq1L  = *+1
    adc     #0                  ;2           CF==0!
    sta     .sum1L              ;3
Sum1H  = *+1
    lda     #0                  ;2
Freq1H  = *+1
    adc     #0                  ;2
    sta     Sum1H               ;3   = 14

    bcs     .waitCh1            ;2/3 =  2/3
Mask1   = *+1
    lda     #1                  ;2
    bpl     .contMask1          ;2/3
    lda     #$01                ;2
    dey                         ;2
    bmi     .resetIdx1          ;2/3
.cont1
    sta+1   Mask1               ;3   = 13
Pattern1 = *+1
    and     PatternTbl,y        ;4
    bne     .loadV1             ;2/3
    beq     .zeroV1             ;3

.contMask1                      ;5
    asl                         ;2
    bcc     .cont1              ;3

.waitCh1                        ;3
    jmp     WaitCh1             ;24  = 27

  !ifdef VISUALS {
.waitCh0
    txs                         ;2
    lda+1   Sum0H               ;3
    lsr                         ;2
    lsr                         ;2
    tax                         ;2
    lda     WaveGfx,x           ;4
    sta+2   PF2                 ;4
    tsx                         ;2
;Note: carry is random
    bpl     ContinueCh0         ;3   = 24
  }
}
PlayerLength = * - PlayerCode

  !ifdef VISUALS {
WaitCh1                         ;3
    lda+1   Freq0H              ;3
    sta     COLUPF              ;3
    lda     #1                  ;2
    sta+2   CTRLPF              ;4
    nop                         ;2
    nop                         ;2
    clc                         ;2
    jmp     ContinueCh1         ;3   = 24
  } else {
WaitCh0                         ;3
    jsr     Wait18              ;18
    jmp     ContinueCh0         ;3   = 24

WaitCh1                         ;3
    jsr     Wait18              ;18
    jmp     ContinueCh1         ;3   = 24

Wait18                          ;6
    nop                         ;2
    nop                         ;2
    clc                         ;2
    rts                         ;6   = 18
  }

  !if NTSC { ; {
;$10000 - Frequency * 256 * 256 / (1193181.67 / (89 + 8/256)) * div (div = 2, 15, 31)
FreqDiv2Lsb
    !byte   $60, $56, $4C, $41, $36, $2A, $1D, $10, $02, $F3, $E3, $D2 ;c-0..b-0
    !byte   $C0, $AD, $98, $83, $6D, $55, $3B, $20, $04, $E6, $C6, $A4 ;c-1..b-1
    !byte   $80, $5A, $31, $07, $DA, $AA, $77, $41, $08, $CC, $8C, $48 ;c-2..b-2
    !byte   $00, $B4, $63, $0E, $B4, $54, $EE, $83, $11, $98, $18, $90 ;c-3..b-3
    !byte   $01, $69, $C7, $1D, $68, $A8, $DD, $06, $22, $30, $30, $21 ;c-4..b-4
    !byte   $02, $D2, $8F, $3A, $D0, $50, $BA, $0C, $44, $61, $61, $43 ;c-5..b-5
    !byte   $05, $A4, $1F, $74, $A0, $A2, $75, $18, $89, $C2, $C3, $87 ;c-6..b-6
    !byte   $0A, $48, $3F, $E8, $41, $43, $EB, $31, $12, $85, $86, $0D ;c-7..b-7
    !byte   $14, $91, $7E, $D2, $83, $87, $D6, $63, $24                ;c-8..gis-8
FreqDiv2Msb
    !byte   $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FE, $FE, $FE ;c-0..b-0
    !byte   $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FD, $FD, $FD ;c-1..b-1
    !byte   $FD, $FD, $FD, $FD, $FC, $FC, $FC, $FC, $FC, $FB, $FB, $FB ;c-2..b-2
    !byte   $FB, $FA, $FA, $FA, $F9, $F9, $F8, $F8, $F8, $F7, $F7, $F6 ;c-3..b-3
    !byte   $F6, $F5, $F4, $F4, $F3, $F2, $F1, $F1, $F0, $EF, $EE, $ED ;c-4..b-4
    !byte   $EC, $EA, $E9, $E8, $E6, $E5, $E3, $E2, $E0, $DE, $DC, $DA ;c-5..b-5
    !byte   $D8, $D5, $D3, $D0, $CD, $CA, $C7, $C4, $C0, $BC, $B8, $B4 ;c-6..b-6
    !byte   $B0, $AB, $A6, $A0, $9B, $95, $8E, $88, $81, $79, $71, $69 ;c-7..b-7
    !byte   $60, $56, $4C, $41, $36, $2A, $1D, $10, $02                ;c-8..gis-8

FreqDiv15Lsb
    !byte   $50, $09, $BE, $6D, $18, $BE, $60, $FA, $8F, $1E, $A6, $27 ;c-0..b-0
    !byte   $A1, $12, $7B, $DB, $31, $7E, $BF, $F5, $20, $3D, $4D, $4F ;c-1..b-1
    !byte   $42, $24, $F6, $B6, $63, $FB, $7F, $EB, $3F, $7B, $9B, $9F ;c-2..b-2
    !byte   $84, $4A, $ED, $6D, $C6, $F8, $FE, $D7, $80, $F6, $37, $3E ;c-3..b-3
    !byte   $09, $94, $DB, $DA, $8D, $EF, $FC, $AE, $01, $ED, $6E, $7D ;c-4..b-4
    !byte   $12, $28, $B6, $B5, $1B, $DF, $F8, $5D, $01, $DA           ;c-5..a-5
FreqDiv15Msb
    !byte   $FB, $FB, $FA, $FA, $FA, $F9, $F9, $F8, $F8, $F8, $F7, $F7 ;c-0..b-0
    !byte   $F6, $F6, $F5, $F4, $F4, $F3, $F2, $F1, $F1, $F0, $EF, $EE ;c-1..b-1
    !byte   $ED, $EC, $EA, $E9, $E8, $E6, $E5, $E3, $E2, $E0, $DE, $DC ;c-2..b-2
    !byte   $DA, $D8, $D5, $D3, $D0, $CD, $CA, $C7, $C4, $C0, $BD, $B9 ;c-3..b-3
    !byte   $B5, $B0, $AB, $A6, $A1, $9B, $95, $8F, $89, $81, $7A, $72 ;c-4..b-4
    !byte   $6A, $61, $57, $4D, $43, $37, $2B, $1F, $12, $03           ;c-5..a-5

FreqDiv31Lsb
    !byte   $51, $BE, $22, $7B, $CD, $12, $4F, $7D, $A0, $B7, $BE, $B8 ;c-0..b-0
    !byte   $A2, $7B, $43, $F8, $9A, $26, $9C, $FB, $42, $6E, $7E, $70 ;c-1..b-1
    !byte   $44, $F6, $86, $F1, $33, $4C, $39, $F7, $84, $DC, $FD, $E2 ;c-2..b-2
    !byte   $8A, $EE, $0D, $E2, $68, $9A, $73, $EF, $09, $B9, $FA, $C5 ;c-3..b-3
    !byte   $12, $DD, $1B, $C3, $CE, $33, $E8, $DF, $13                ;c-4..gis-4
FreqDiv31Msb
    !byte   $F6, $F5, $F5, $F4, $F3, $F3, $F2, $F1, $F0, $EF, $EE, $ED ;c-0..b-0
    !byte   $EC, $EB, $EA, $E8, $E7, $E6, $E4, $E2, $E1, $DF, $DD, $DB ;c-1..b-1
    !byte   $D9, $D6, $D4, $D1, $CF, $CC, $C9, $C5, $C2, $BE, $BA, $B6 ;c-2..b-2
    !byte   $B2, $AD, $A9, $A3, $9E, $98, $92, $8B, $85, $7D, $75, $6D ;c-3..b-3
    !byte   $65, $5B, $52, $47, $3C, $31, $24, $17, $0A                ;c-4..gis-4
  } ;} NTSC

  !ifdef VISUALS {
WaveGfx
    !fill   7, %00000000
    !fill   7, %10000000
    !fill   7, %11000000
    !fill   7, %01100000
    !fill   8, %00110000
    !fill   7, %00011000
    !fill   7, %00001100
    !fill   7, %00000110
    !fill   7, %00000011
  }

    !zone debug
    !warn * - $f000, " bytes used"

    !zone musicdata
musicData
    !source "music.asm"

    * = $fffc

    !word   Reset          ; RESET
    !word   $0000          ; IRQ

;       END
