;TIAtune
;Atari 2600 music player
;by utz 10'2017 * irrlichtproject.de

	!to "test.bin", plain
	!sl "test.sym"
	!cpu 6510

	!source "vcs.h"
	!source "notes.h"
	
SEQOFFS	= $d0			;s=ence offset
PTNPTRL	= $d1
PTNPTRH	= $d2
PTNOFFS	= $d3
ROWLENL	= $d4			;do Rowlen via Timer
ROWLENH	= $d5
POLY4VAL1 = $e0
POLY5VAL1 = $e1
POLY9VAL1L = $e2
POLY9VAL1H = $e3
SQUAREVAL1 = $e4
R1813_1 = $e5
T1813_1 = $e6
VOL1 = $e7
POLY4VAL2 = $e8
POLY5VAL2 = $e9
POLY9VAL2L = $ea
POLY9VAL2H = $eb
SQUAREVAL2 = $ec
R1813_2 = $ed
T1813_2 = $ee
VOL2 = $ef
CONST80 = $f0
CONST0 = $f1
CONST18 = $f2

	* = $f000
	!zone main
reset
        ldx #0			;clear TIA regs, RAM, set SP to $00ff
        txa			;alternatively just use CLEAN_START macro
-   
	dex
        txs
        pha
        bne -
	
relocate_player			;relocate to zeropage
	ldx #0
-
	lda playerCode,x
	sta $80,x
	inx
	cpx #playerCode_End - playerCode
	bne -
	
	lda #$f
	sta POLY4VAL1
	sta POLY4VAL2
	lda #18
	sta T1813_1
	sta T1813_2
	sta CONST18
	lda #$1f
	sta POLY5VAL1
	sta POLY5VAL2
	lda #$ff
	sta POLY9VAL1L
	sta POLY9VAL2L
	sta SQUAREVAL1
	sta SQUAREVAL2
	sta R1813_1
	sta R1813_2
	lda #$80
	sta POLY9VAL1H
	sta POLY9VAL2H
	sta CONST80

rdSeq				;read next entry in sequence
	ldx SEQOFFS
	lda sequence_hi,x
	beq reset		;if hi-byte = 0, loop
	sta PTNPTRH
	lda sequence_lo,x		
	sta PTNPTRL
	inx
	stx SEQOFFS	
	ldy #0
	
rdPtn	
	lda (PTNPTRL),y		;ctrl byte
	beq rdSeq		;0-end marker
	
	sta ROWLENH
	bmi no_ch1_reload
	
	iny
	lda (PTNPTRL),y		;wave1/vol1
	tax
	lsr
	lsr
	lsr
	sta+1 VOL1
	txa
	and #$7
	tax
	lda wave_sub_lut_ch1,x
	sta+1 WAVEP1

	iny
	txa			;look up freq divider depending on waveform used
	cmp #2	
	bcs +			;0,1 -> square/poly9
	lda (PTNPTRL),y		;note
	tax
	lda freq_div2_lsb,x
	sta+1 FREQ1L
	lda freq_div2_msb,x
	sta+1 FREQ1H
	jmp .cont
+		
	bne +			;2 -> poly4
	lda (PTNPTRL),y
	tax
	lda freq_div15_lsb,x
	sta+1 FREQ1L
	lda freq_div15_msb,x
	sta+1 FREQ1H
	jmp .cont
+
	lda (PTNPTRL),y		;3,4 -> 1813/poly5
	tax
	lda freq_div31_lsb,x
	sta+1 FREQ1L
	lda freq_div31_msb,x
	sta+1 FREQ1H	
.cont	
no_ch1_reload
	bit ROWLENH
	bvs no_ch2_reload
		
	iny
	lda (PTNPTRL),y		;wave2/vol
	tax
	lsr
	lsr
	lsr
	sta+1 VOL2
	txa
	and #$7
	tax
	lda wave_sub_lut_ch2,x
	sta+1 WAVEP2

	iny
	txa			;look up freq divider depending on waveform used
	cmp #2	
	bcs +
	lda (PTNPTRL),y		;note
	tax
	lda freq_div2_lsb,x
	sta+1 FREQ2L
	lda freq_div2_msb,x
	sta+1 FREQ2H
	jmp .cont2
+			
	bne +
	lda (PTNPTRL),y
	tax
	lda freq_div15_lsb,x
	sta+1 FREQ2L
	lda freq_div15_msb,x
	sta+1 FREQ2H
	jmp .cont2
+
	lda (PTNPTRL),y
	tax
	lda freq_div31_lsb,x
	sta+1 FREQ2L
	lda freq_div31_msb,x
	sta+1 FREQ2H	
.cont2
no_ch2_reload
	iny

	lda ROWLENH
	and #$3f
	sta ROWLENH	
	jmp playNote


playerCode = *

	!pseudopc $80 {			;actual player runs on zeropage
playNote
	clc			;2
SUM1L = *+1
	lda #0			;2
FREQ1L = *+1
	adc #0			;2
	sta SUM1L		;3
SUM1H = *+1
	lda #0			;2
FREQ1H = *+1
	adc #0			;2
	sta SUM1H		;3
	
	bcc wait_ch1		;3/2
WAVEP1 = *+1
	jmp poly4_ch1		;3 (+32 = 37)

continue_ch1
	clc			;2
SUM2L = *+1	
	lda #0			;2	;carry should always be reset at this point
FREQ2L = *+1
	adc #0			;2
	sta SUM2L		;3
SUM2H = *+1
	lda #0			;2
FREQ2H = *+1
	adc #0			;2
	sta SUM2H		;3
	
	bcc wait_ch2		;3/2
WAVEP2 = *+1
	jmp poly4_ch2		;3

continue_ch2
	dec ROWLENL		;5
	bne playNote		;3/2
				;avg 114
	dec ROWLENH
	bne playNote
		
	jmp rdPtn		;3
	
wait_ch1
	lda (0,x)		;6
	lda (0,x)		;6
	lda (0,x)		;6
wait_ch1x
	lda (0,x)		;6
	lda 0			;3
	nop			;2
	nop			;2
	jmp continue_ch1	;3

wait_ch2
	lda (0,x)		;6
	lda (0,x)		;6
	lda (0,x)		;6
wait_ch2x
	lda (0,x)		;6
	lda 0			;3
	nop			;2
	nop			;2
	jmp continue_ch2	;3
}

playerCode_End = *

freq_div15_lsb
	!byte $9,$63,$bd,$26,$8f,$7,$7f,$6,$8d,$23,$b9,$5e
	!byte $3,$c6,$7a,$4c,$2d,$e,$fe,$c,$1a,$37,$72,$bc
	!byte $15,$7d,$3,$98,$4b,$1c,$b,$9,$34,$7d,$e4,$69
	!byte $1b,$fa,$6,$3f,$a5,$38,$7,$21,$68,$eb,$b9,$d2
	!byte $45,$f4,$c,$6f,$3b,$7f,$1d,$33,$c1,$d6,$81,$b3
	!byte $7b,$e8,$9,$ed,$85

freq_div15_msb
	!byte $6,$6,$6,$7,$7,$8,$8,$9,$9,$a,$a,$b
	!byte $c,$c,$d,$e,$f,$10,$10,$12,$13,$14,$15,$16
	!byte $18,$19,$1b,$1c,$1e,$20,$22,$24,$26,$28,$2a,$2d
	!byte $30,$32,$36,$39,$3c,$40,$44,$48,$4c,$50,$55,$5a
	!byte $60,$65,$6c,$72,$79,$80,$88,$90,$98,$a1,$ab,$b5
	!byte $c0,$cb,$d8,$e4,$f2

freq_div31_lsb
	!byte $79,$33,$ed,$c6,$9f,$97,$8f,$a6,$bd,$f3,$29,$7e
	!byte $d3,$66,$da,$8c,$5d,$2e,$1e,$4c,$7a,$c7,$52,$fc
	!byte $c5,$ad,$d3,$18,$9b,$5c,$5b,$79,$f4,$ad,$a4,$d9
	!byte $6b,$5a,$a6,$4f,$55,$b8,$97,$11,$e8,$3b,$29,$b2
	!byte $f5,$b4,$4c,$7f,$8b
ft1end

	* = $f200
; square				;TODO watch out for code crossing page -> lookup table
; 18:13
; mute possibly needs its own core
	
square_ch1
	lda (0,x)		;6
	lda (0,x)		;6
	lda 0			;3
	lda SQUAREVAL1		;3
	eor #$ff		;2
	sta SQUAREVAL1		;3
	and VOL1		;3
	sta AUDV0		;3
	jmp continue_ch1	;3
				;32
	
	!zone r1813_1			
r1813_ch1
	dec T1813_1		;5
	bne .noupd		;3/2
	lda R1813_1		;3
	eor #$ff		;2
	sta R1813_1		;3
	beq +			;3/2
	and VOL1		;3
	sta AUDV0		;5
	lda CONST18		;3
	sta T1813_1		;3
	jmp continue_ch1	;3
				;32
+
	lda CONST0		;3
	sta AUDV0		;3
	lda #13			;2
	sta T1813_1		;3
	jmp continue_ch1	;3
				;22

.noupd
	lda 0			;3
	nop			;2
	jmp wait_ch1x		;3	
	
	!zone poly4_1
poly4_ch1				;init POLY4VAL1 with $f
	nop			;2
	lda POLY4VAL1		;3
	lsr			;2
	tax			;2
	eor POLY4VAL1		;3
	lsr			;2
	txa			;2
	bcc .noc		;3/2
	ora #8			;2
	sta POLY4VAL1		;3
	lda VOL1		;3
	sta AUDV0		;3	;could be omitted (handle in main sound loop)
	jmp continue_ch1	;3
				;32 + 3 (jump to)

.noc
	and #7
	sta POLY4VAL1
	lda #0
	sta AUDV0
	jmp continue_ch1	;3 32 bytes	

	!zone poly5_1
poly5_ch1				;init with POLY5VAL1 = $1f
	lda POLY5VAL1
	lsr
	tax
	lsr
	eor POLY5VAL1
	lsr
	txa
	bcc .noc
	ora #$10
	sta POLY5VAL1
	lda VOL1
	sta AUDV0
	jmp continue_ch1	;3 32t (+3 jump to)

.noc
	and #$f
	sta POLY5VAL1
	lda #0
	sta AUDV0
	jmp continue_ch1	;3

	!zone poly9_1
poly9_ch1				;init VALxH with 0, VALxL with $ff
	lda POLY9VAL1L		;3
	lsr			;2
	ora POLY9VAL1H		;3
	sta POLY9VAL1L		;3
	and #8			;2	;FAIL: needs to be XOR'D
	bcc .noc		;3/2
	beq .cz			;3/2
	lda CONST0		;3
	sta POLY9VAL1H		;3
	lda VOL1		;3
	sta AUDV0		;3
	jmp continue_ch1	;3
				;32t	;can be aligned by loading constant from zp
	
.cz
	lda #$80		;2
	sta POLY9VAL1H		;3
	lda VOL1		;3
	sta AUDV0		;3
	jmp continue_ch1	;3
				;32t

.noc
	beq .nocz		;3/2
	lda CONST80		;3
	sta POLY9VAL1H		;3
	lda #0			;2
	sta AUDV0		;3
	jmp continue_ch1	;3
				;32t

.nocz
	nop			;2
	nop			;2
	sta POLY9VAL1H		;3
	sta AUDV0		;3
	jmp continue_ch1	;3
				;32t

wave_sub_lut_ch1
	!byte square_ch1&$ff,poly9_ch1&$ff,poly4_ch1&$ff,r1813_ch1&$ff,poly5_ch1&$ff
	
wave_sub_lut_ch2
	!byte square_ch2&$ff,poly9_ch2&$ff,poly4_ch2&$ff,r1813_ch2&$ff,poly5_ch2&$ff

freq_div31_msb
	!byte $c,$d,$d,$e,$f,$10,$11,$12,$13,$14,$16,$17
	!byte $18,$1a,$1b,$1d,$1f,$21,$23,$25,$27,$29,$2c,$2e
	!byte $31,$34,$37,$3b,$3e,$42,$46,$4a,$4e,$53,$58,$5d
	!byte $63,$69,$6f,$76,$7d,$84,$8c,$95,$9d,$a7,$b1,$bb
	!byte $c6,$d2,$df,$ec,$fa
ffffffff
	* = $f300
	
square_ch2
	lda (0,x)		;6
	lda (0,x)		;6
	lda 0			;3
	lda SQUAREVAL2		;3
	eor #$ff		;2
	sta SQUAREVAL2		;3
	and VOL2		;3
	sta AUDV1		;3
	jmp continue_ch2	;3
				;32
				
	!zone r1813_2			
r1813_ch2
	dec T1813_2		;5
	bne .noupd		;3/2
	lda R1813_2		;3
	eor #$ff		;2
	sta R1813_2		;3
	beq +			;3/2
	and VOL2		;3
	sta AUDV1		;3
	lda CONST18		;3
	sta T1813_2		;3
	jmp continue_ch2	;3
				;32
+
	lda CONST0		;3
	sta AUDV1		;3
	lda #13			;2
	sta T1813_2		;3
	jmp continue_ch2	;3
				;32
.noupd
	lda 0			;3
	nop			;2
	jmp wait_ch2x

	!zone poly4_2
poly4_ch2				;init POLY4VAL1 with $f
	nop			;2
	lda POLY4VAL2		;3
	lsr			;2
	tax			;2
	eor POLY4VAL2		;3
	lsr			;2
	txa			;2
	bcc .noc		;3/2
	ora #8			;2
	sta POLY4VAL2		;3
	lda VOL2		;3
	sta AUDV1		;3	;could be omitted (handle in main sound loop)	
	jmp continue_ch2	;3
				;32 + 3 (jump to)

.noc
	and #7
	sta POLY4VAL2
	lda #0
	sta AUDV1
	jmp continue_ch2	;3 32t

	!zone poly5_2
poly5_ch2				;init with POLY5VAL = $1f
	lda POLY5VAL2
	lsr
	tax
	lsr
	eor POLY5VAL2
	lsr
	txa
	bcc .noc
	ora #$10
	sta POLY5VAL2
	lda VOL2
	sta AUDV1
	jmp continue_ch2	;3 32t (+3 jump to)

.noc
	and #$f
	sta POLY5VAL2
	lda #0
	sta AUDV1
	jmp continue_ch2	;3

	!zone poly9_2
poly9_ch2				;init VALxH with 0, VALxL with $ff
	lda POLY9VAL2L		;3
	lsr			;2
	ora POLY9VAL2H		;3
	sta POLY9VAL2L		;3
	and #8			;2
	bcc .noc		;3/2
	beq .cz			;3/2
	lda CONST0		;3	;lda #0, but wasting 1 extra cycle
	sta POLY9VAL2H		;3
	lda VOL2		;3
	sta AUDV1		;3
	jmp continue_ch2	;3
				;32t	;can be aligned by loading constant from zp
	
.cz
	lda #$80		;2
	sta POLY9VAL2H		;3
	lda VOL2		;3
	sta AUDV1		;3
	jmp continue_ch2	;3
				;32t

.noc
	beq .nocz		;3/2
	lda CONST80		;3	;lda #$80, but wasting 1 extra cycle
	sta POLY9VAL2H		;3
	lda #0			;2
	sta AUDV1		;3
	jmp continue_ch2	;3
				;31t

.nocz
	nop			;2
	nop			;2
	sta POLY9VAL2H		;3
	sta AUDV1		;3
	jmp continue_ch2	;3
				;32t



freq_div2_lsb
	!byte $ce,$da,$e6,$f4,$2,$12,$22,$34,$46,$5a,$6e,$84
	!byte $9a,$b4,$cc,$e8,$6,$24,$44,$68,$8c,$b2,$dc,$8
	!byte $36,$66,$9a,$d0,$a,$48,$8a,$ce,$18,$66,$b8,$e
	!byte $6a,$cc,$34,$a2,$16,$90,$12,$9e,$30,$ca,$6e,$1c
	!byte $d6,$98,$68,$42,$2a,$22,$26,$3a,$5e,$94,$de,$3a
	!byte $aa,$30,$ce,$86,$56,$42,$4c,$74,$be,$2a,$ba,$72
	!byte $54,$62,$9e,$a,$ac,$84,$98,$e8,$7c,$54,$76,$e6
	!byte $a8,$c4,$3c,$16,$58,$8,$2e,$d0,$f6,$a6,$ea,$cc
	!byte $52,$86,$76,$2a

freq_div2_msb
	!byte $0,$0,$0,$0,$1,$1,$1,$1,$1,$1,$1,$1
	!byte $1,$1,$1,$1,$2,$2,$2,$2,$2,$2,$2,$3
	!byte $3,$3,$3,$3,$4,$4,$4,$4,$5,$5,$5,$6
	!byte $6,$6,$7,$7,$8,$8,$9,$9,$a,$a,$b,$c
	!byte $c,$d,$e,$f,$10,$11,$12,$13,$14,$15,$16,$18
	!byte $19,$1b,$1c,$1e,$20,$22,$24,$26,$28,$2b,$2d,$30
	!byte $33,$36,$39,$3d,$40,$44,$48,$4c,$51,$56,$5b,$60
	!byte $66,$6c,$73,$7a,$81,$89,$91,$99,$a2,$ac,$b6,$c1
	!byte $cd,$d9,$e6,$f4



	!zone musicdata
musicData
	!source "music.asm"

	
	* = $fffc

	!word reset          ; RESET
	!word reset          ; IRQ

;    	END
