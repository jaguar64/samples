;**************************************************************************
; (C)1993 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;				     CDBIOS
;
;                                REVISION HISTORY
;
; REV.  DATE       BY            DESCRIPTION OF EDIT
; """"  """"       ""            """""""""""""""""""
; 0.01	30 Mar 94  LJT		 First stab
; 0.02	12 Apr 94  LJT		 Added CD_ptr,CD_osamp added comment blocks
; 0.03	14 Apr 94  LJT		 Added more voodoo (delay at start of ack)
; 0.10	   May 94  LJT		 Added CD_getoc from Dave Staugas (Called Release 2)
; 3.0				 Added initf and initm
; 3.1				 Fixed a bug in initm
; 4	16 Feb 95  LJT		 Added CD_switch
; 4.01	21 Feb 95  LJT		 Removed Stop from CD_switch
; 4.02	14 Mar 95  LJT		 Added a Spinup call in GETOC to stop data flow
; 4.03	11 Apr 95  LJT		 Stopped the GPU code (ALL OF IT) from resetting upper bits in BUTCH
;**************************************************************************
	.include 'jaguar.inc'	; Jaguar equates
	.include 'cdbios.inc'	; CD-related equates

; Jump table to ALL of the functions

CD_init::
	bra.b	init
	nop
BIOS_VER::
	dc.w	$0403
CD_mode::
	bra	mode
	nop
CD_ack::
	bra	ack
	nop
CD_jeri::
	bra	jeri
	nop
CD_spin::
	bra	spin
	nop
CD_stop::
	bra	stop
	nop
CD_mute::
	bra	mute
	nop
CD_umute::
	bra	umute
	nop
CD_paus::
	bra	paus
	nop
CD_upaus::
	bra	upaus
	nop
CD_read::
	bra	read
	nop
CD_uread::
	bra	uread
	nop
CD_setup::
	bra	setup
	nop
CD_ptr::
	bra	ptr
	nop
CD_osamp::
	bra	osamp
	nop
CD_getoc::
	bra	getoc
	nop
CD_initm::
	bra	initm
	nop
CD_initf::
	bra	initf
	nop
CD_switch::
	bra	switch
	nop

INITTYPE:
	dc.b	0
	.long
PTRLOC:
	dc.l	0




;****************************************************************
;		CD_init						*
;								*
; This call loads support code into the GPU to allow data to 	*
; be stored in DRAM.						*
;								*
;	Input:	a0.l	The address of a LONG aligned block	*
; 			of GPURAM at 224 bytes			*
;								*
;	Uses:	a1,a0,d0					*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

init:

; Save the address of the DATA BLOCK for later

	move.l	a0,PTRLOC

; Set up the Jerry interrupt jump in the GPU

	move.l	a0,d0
	add.l	#12,d0
	and.l	#$ffff,d0
	or.l	#$981e0000,d0
	move.l	d0,G_RAM+16
	move.l	#$00f0d3c0,G_RAM+20
	move.l	#$e400e400,G_RAM+24

; Load up the block o' code that follows
	move.l	#GPUSTART,a1
	move.l	#GPUEND-GPUSTART,d0
	asr.l	#2,d0
	subq.l	#1,d0
.loop:
	move.l	(a1)+,(a0)+
        dbra    d0,.loop

	move.b	#0,INITTYPE

; Clear pending DSA interrupts

	move.w	DS_DATA,d0
	move.l	DSCNTRL,d0

; Allow Jerry interrupts

	move.l	G_FLAGS,d0
	or.l	#%100000,d0
	move.l	d0,G_FLAGS

	rts

; This block of code is the produced from the source file
; FIFO_R1.GAS

GPUSTART:
.if 0
; This is the first release version
	dc.w	$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$981E,$2100,$00F0,$A7DD,$189F,$BFF9
	dc.w	$189F,$BFF4,$9814,$FF00,$00DF,$189F,$BFFB,$189F
	dc.w	$BFFA,$189F,$BFE0,$CC19,$981B,$007C,$0000,$0379
	dc.w	$A69B,$35BB,$D5E2,$8C7A,$BE9A,$0A14,$A69B,$385B
	dc.w	$BE9B,$1994,$A69A,$08D4,$A29B,$355B,$D561,$2B5A
	dc.w	$D320,$E400,$CC00,$981C,$005C,$0000,$1380,$35DB
	dc.w	$D522,$3BFB,$0A14,$A69C,$2B9C,$1A14,$A41C,$0900
	dc.w	$BC1C,$1900,$A41A,$0880,$A41C,$1880,$7B5C,$D474
	dc.w	$E400,$3C1B,$BE9B,$981B,$FF24,$00DF,$8C74,$A77C
	dc.w	$A699,$A77E,$089A,$BF5C,$1834,$0C9A,$D714,$BF5E
	dc.w	$BC1A,$9814,$0020,$00F1,$8C3C,$391C,$BA9C,$A7E0
	dc.w	$089F,$A7FA,$089F,$A7FB,$089F,$A7F4,$089F,$A7F9
	dc.w	$089F,$981E,$2100,$00F0,$3C7D,$395D,$A7FC,$085C
	dc.w	$089F,$D380,$BFDD,$0000
.else
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$981E,$2100
	dc.w	$00F0,$A7DD,$189F,$BFF9,$189F,$BFF8,$9818,$FF00
	dc.w	$00DF,$189F,$BFFB,$189F,$BFFA,$189F,$BFF7,$CC17
	dc.w	$981C,$002E,$0000,$1397,$CC19,$981B,$0072,$0000
	dc.w	$0379,$A71B,$35BB,$D5E2,$3CBB,$383B,$BF1B,$0A18
	dc.w	$A71B,$385B,$BF1B,$1998,$A71A,$08D8,$A31B,$355B
	dc.w	$D4A1,$2B5A,$D320,$35DB,$D522,$3BFB,$0A18,$A71C
	dc.w	$2B9C,$1A18,$A6FC,$0917,$BEFC,$1917,$A6FA,$0897
	dc.w	$A6FC,$1897,$7B5C,$D454,$3C1B,$BF1B,$981B,$FF24
	dc.w	$00DF,$8B79,$089B,$8C78,$A77C,$A73E,$089A,$1838
	dc.w	$BF5C,$0C9A,$D734,$BF5E,$BEFA,$9818,$0020,$00F1
	dc.w	$8C3C,$391C,$BB1C,$A7F7,$089F,$A7FA,$089F,$A7FB
	dc.w	$089F,$A7F8,$089F,$A7F9,$089F,$981E,$2100,$00F0
	dc.w	$3C7D,$395D,$A7FC,$085C,$089F,$D380,$BFDD,$0000
.endif
GPUEND:

;****************************************************************
;		CD_initf					*
;								*
; This call loads support code into the GPU to allow data to 	*
; be stored in DRAM.						*
;								*
;	Input:	a0.l	The address of a LONG aligned block	*
; 			of GPURAM at 224 bytes			*
;								*
;	Uses:	a1,a0,d0					*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

initf:

; Save the address of the DATA BLOCK for later

	move.l	a0,PTRLOC

; Set up the Jerry interrupt jump in the GPU

	move.l	a0,d0
	add.l	#12,d0
	and.l	#$ffff,d0
	or.l	#$981e0000,d0
	move.l	d0,G_RAM+16
	move.l	#$00f0d3c0,G_RAM+20
	move.l	#$e400e400,G_RAM+24

; Load up the block o' code that follows
	move.l	#GPUSF,a1
	move.l	#GPUEF-GPUSF,d0
	asr.l	#2,d0
	subq.l	#1,d0
.loop:
	move.l	(a1)+,(a0)+
        dbra    d0,.loop

	move.b	#1,INITTYPE

; Clear pending DSA interrupts

	move.w	DS_DATA,d0
	move.l	DSCNTRL,d0

; Allow Jerry interrupts

	move.l	G_FLAGS,d0
	or.l	#%100000,d0
	move.l	d0,G_FLAGS

	rts

; This block of code is the produced from the source file
; FIFO_R3.GAS

GPUSF:
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$981E,$2100
	dc.w	$00F0,$A7DD,$9818,$FF00,$00DF,$CC17,$981C,$001A
	dc.w	$0000,$1397,$CC19,$981B,$0090,$0000,$0379,$A71B
	dc.w	$35BB,$D5E2,$3CBB,$383B,$BF1B,$0A18,$A71B,$385B
	dc.w	$BF1B,$1998,$A71A,$08D8,$A31B,$355B,$D4A1,$2B5A
	dc.w	$D320,$35DB,$D522,$3BFB,$0A18,$A71C,$2B9C,$1A18
	dc.w	$A6FC,$0917,$BEFC,$1917,$A6FA,$0897,$A6FC,$1897
	dc.w	$7B5C,$D454,$3C1B,$BF1B,$981B,$FF24,$00DF,$8B79
	dc.w	$089B,$A77C,$A73E,$A775,$A736,$A778,$A734,$A773
	dc.w	$A732,$089A,$BF5C,$0C9A,$BF5E,$0C9A,$BF55,$0C9A
	dc.w	$BF56,$0C9A,$BF58,$0C9A,$BF54,$0C9A,$BF53,$0C9A
	dc.w	$BF52,$BEFA,$9818,$0020,$00F1,$8C3C,$391C,$BB1C
	dc.w	$981E,$2100,$00F0,$3C7D,$395D,$A7FC,$085C,$089F
	dc.w	$D380,$BFDD
GPUEF:

;****************************************************************
;		CD_initm					*
;								*
; This call loads support code into the GPU to allow data to 	*
; be stored in DRAM.						*
;								*
;	Input:	a0.l	The address of a LONG aligned block	*
; 			of GPURAM at 328 bytes			*
;								*
;	Uses:	a1,a0,d0					*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

initm:

; Save the address of the DATA BLOCK for later

	move.l	a0,PTRLOC

; Set up the Jerry interrupt jump in the GPU

	move.l	a0,d0
	add.l	#20,d0
	and.l	#$ffff,d0
	or.l	#$981e0000,d0
	move.l	d0,G_RAM+16
	move.l	#$00f0d3c0,G_RAM+20
	move.l	#$e400e400,G_RAM+24

; Load up the block o' code that follows
	move.l	#GPUSM,a1
	move.l	#GPUEM-GPUSM,d0
	asr.l	#2,d0
	subq.l	#1,d0
.loop:
	move.l	(a1)+,(a0)+
        dbra    d0,.loop

; Clear pending DSA interrupts

	move.w	DS_DATA,d0
	move.l	DSCNTRL,d0

	move.b	#-1,INITTYPE

; Allow Jerry interrupts

	move.l	G_FLAGS,d0
	or.l	#%100000,d0
	move.l	d0,G_FLAGS

	rts

; This block of code is the produced from the source file
; MATCH.GAS

GPUSM:
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$981E,$2100,$00F0,$A7DD,$189F,$BFF9
	dc.w	$189F,$BFF8,$9818,$FF00,$00DF,$189F,$BFFB,$A71B
	dc.w	$189F,$BFFA,$189F,$BFF7,$189F,$BFF6,$CC17,$981C
	dc.w	$003C,$0000,$1397,$CC19,$981A,$0088,$0000,$0359
	dc.w	$35BB,$D5E2,$3CBB,$383B,$BF1B,$0A18,$A71B,$385B
	dc.w	$BF1B,$1998,$A71A,$08D8,$A31B,$355B,$D5E1,$2B5A
	dc.w	$D320,$0997,$A6FA,$7C1A,$D4C2,$1997,$8B3C,$0B9C
	dc.w	$0997,$0B9C,$D380,$35DB,$D522,$3BFB,$0A18,$A71C
	dc.w	$2B9C,$1A18,$A6FC,$0917,$BEFC,$1917,$A6FA,$0897
	dc.w	$A6FC,$1897,$7B5C,$D454,$3C1B,$BF1B,$981B,$FF24
	dc.w	$00DF,$8B79,$089B,$8C78,$A77C,$A73E,$089A,$3C1A ; The last instruction pair is patched $089a 3c1a
	dc.w	$BF5C,$089A,$3C1A,$1838,$D6F4,$BF5E,$BEFA,$9818 ; The second instruction pair is patched $3c1a $1838
	dc.w	$0020,$00F1,$8C3C,$391C,$BB1C,$A7F6,$089F,$A7F7
	dc.w	$089F,$A7FA,$089F,$A7FB,$089F,$A7F8,$089F,$A7F9
	dc.w	$089F,$981E,$2100,$00F0,$3C7D,$395D,$A7FC,$085C
	dc.w	$089F,$D380,$BFDD,$981B,$FF24,$00DF,$8C7E,$8D36
	dc.w	$63DE,$0897,$A6F8,$1897,$D460,$E400,$8E1A,$BEFA
	dc.w	$1836,$D322,$E400,$2FDB,$A77C,$7B98,$D6E1,$E400
	dc.w	$183A,$D6C1,$BEFA,$1997,$A6FA,$2FDB,$0C9A,$A77C
	dc.w	$BF5C,$1836,$D741,$E400,$BEFA,$D320,$E400,$0000
GPUEM:

;****************************************************************
;		CD_mode						*
;								*
; This call sets the speed of the CD and allows either single	*
; or double speed						*
;	Input:	d0.w	Mode desired 				*
;			bit 0 -> speed 0=>single, 1=>double	*
;			bit 1 -> mode  0=>Audio, 1=>CDROM	*
; 								*
;	Uses: d1,d2						*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

mode:
	move.w	d0,d2
	and.w	#1,d2
	add.w	#1,d2

	btst	#1,d0
	beq.b	.audio
	bset	#3,d2
.audio:
	or.w	#$1500,d2
	move.w	d2,DS_DATA
	bsr.b	ack		; secretly, ack returns the code in d1
	bset	#9,d2
	cmp.w	d1,d2
	beq.b	.return
	move.w	#$f,d1		; This is VOODOO
.delay:
	dbra	d1,.delay
	bra	mode
.return:
	rts

;****************************************************************
;		CD_osamp					*
;								*
; This call sets the amount of oversampling in audio mode	*
;	Input:	d0.w	Oversample by 2^(d0)			*
;			0 => no oversample			*
;			1 => 2x oversample			*
;			2 => 4x oversample			*
;			3 => 8x oversample			*
; 								*
;	NOTE: This call will only perform the functions that	*
;	the mechanism can actually do. If the mechanism cannot	*
;	perform the oversampling requested it will do the next	*
;	best that it can.					*
;	IMPORTANT!! This call will cause the data rate to	*
;	Jerry's I2S to be multiplied by the oversample factor.	*
;	Whatever software is handling Jerry had better be able	*
;	to handle it.						*
;								*
;	Uses: NADA						*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

osamp:
	cmp.w	#3,d0
	bmi.b	.ok
	move.w	#2,d0
.ok:
	add.w	#1,d0
	or.w	#$7000,d0
	move.w	d0,DS_DATA
	bsr.b	ack
	rts

;****************************************************************
;		CD_spin						*
;								*
; This call sets the CD drive to a specific session		*
;								*
;	Input: d0.w	0 => return immediately			*
;			1 => wait for completion		*
;	       d1	Session to spin up on			*
;								*
;	Uses: NADA						*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

spin:
	or.w	#$1800,d1
	move.w	d1,DS_DATA
	tst.w	d0
	beq.b	.return
	bsr.b	ack
.return:
	rts


;****************************************************************
;		CD_stop						*
;								*
; This call stops the CD					*
;								*
;	Input: d0.w	0 => return immediately			*
;			1 => wait for completion		*
;								*
;	Uses: d1						*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

stop:
	move.w	#$200,DS_DATA
	tst.w	d0
	beq.b	.return
	bsr.b	ack
.return:
	rts


;****************************************************************
;		CD_ack						*
;								*
; If any call uses the "return immediately" option, this call	*
; may be used to ensure that the requested action completed	*
;								*
;	Input: NADA						*
;								*
;	Uses: d1						*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

; mode counts on d0 being retained and a return in d1!!!

ack:
	move.l	BUTCH,d1	; get Butch's ICR into d1
	and.w	#$2000,d1	; mask for d13, DSA RX Intr. pending
	beq.b	ack		; nothing here yet, so wait for bit to set 
	move.w	DS_DATA,d1	; flag set, so get the word from Butch
	move.w	d1,-(sp)	; save the current state
	tst.l	DSCNTRL		; clear the pending flag
	and.w	#$400,d1	; check for error
	bne	.error
	move.w	#0,err_flag.w
	bra.b	.return
.error:
	move.w	#1,err_flag
.return:
	move.w	(sp)+,d1	; retrieve the current state
	rts


;****************************************************************
;		CD_mute						*
;								*
; This call mutes the CD. It functions only in audio mode	*
;								*
;	Input: d0.w	0 => return immediately			*
;			1 => wait for completion		*
;								*
;	Uses: d1						*
;								*
;	Returns: NADA						*
;								*
;	Note: Works only in audio mode				*
;								*
;****************************************************************

mute:
	move.w	#$5100,DS_DATA
	tst.w	d0
	beq.b	.return
	bsr.b	ack
.return:
	rts


;****************************************************************
;		CD_umute					*
;								*
; This call unmutes the CD. It functions only in audio mode	*
;								*
;	Input: d0.w	0 => return immediately			*
;			1 => wait for completion		*
;								*
;	Uses: d1						*
;								*
;	Returns: NADA						*
;								*
;	Note: Works only in audio mode				*
;								*
;****************************************************************

umute:
	move.w	#$51ff,DS_DATA
	tst.w	d0
	beq.b	.return
	bsr.b	ack
.return:
	rts


;****************************************************************
;		CD_paus						*
;								*
; This call pauses the CD. When in CD-ROM mode data may still	*
; be sent but it will not be sensisble.				*
; This also puts the drive into a mode where it will not track	*
; after completeing a seek (pause mode). If a CD_read call is	*
; made while in pause mode the buffer will be filled with	*
; nonsense data							*
;								*
;	Input: d0.w	0 => return immediately			*
;			1 => wait for completion		*
;								*
;	Uses: d1						*
;								*
;	Returns: NADA						*
;								*
;								*
;****************************************************************

paus:
	move.w	#$400,DS_DATA
	tst.w	d0
	beq.b	.return
	bsr.b	ack
.return:
	rts


;****************************************************************
;		CD_upaus					*
;								*
; This call undoes the actions of CD_paus.			*
;								*
;	Input: d0.w	0 => return immediately			*
;			1 => wait for completion		*
;								*
;	Uses: d1						*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

upaus:
	move.w	#$500,DS_DATA
	tst.w	d0
	beq.b	.return
	bsr.b	ack
.return:
	rts


;****************************************************************
;		CD_jeri						*
;								*
; This call allows data to flow to the I2S port on Jerry	*
; This allows audio data to go into the system without taking	*
; main system bandwidth.					*
;								*
;	Input: d0.w	0 => no data to Jerry			*
;			!0 => send data to Jerry		*
;								*
;	Uses: d1						*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

jeri:
	move.l	I2CNTRL,d1
	tst.w	d0
	bne.b	.send
	bclr	#1,d1
	bra.b	.save
.send:
	bset	#1,d1
.save:
	bset	#2,d1
	move.l	d1,I2CNTRL
	rts


;****************************************************************
;		CD_setup					*
;								*
; This call MUST be used to initialize the CD system before ANY	*
; other calls can be made					*
;								*
;	Input:  NADA						*
;	 							*
;								*
;	Uses: NADA						*
;								*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

setup:
	move.l	#$180000,BUTCH	; enable BUTCH
;	move.l	#0,BUTCH	; enable BUTCH
	move.l	#$10000,DSCNTRL	; enable DSA
	move.l	#7,I2CNTRL	; Enable I2S
	move.l	#1,I2CNTRL	; Enable I2S
	move.w	#$7001,DS_DATA	; Set non oversampled audio
	rts


;****************************************************************
;		CD_ptr						*
;								*
; This call returns the address that the data transfers from	*
; the CD are currently at. This address will change in jumps.	*
; The size of these jumps may change at any time.		*
; This call also returns the position of the last detected read	*
; error since the start of the last CD_read command		*
;								*
;	Input:  NADA						*
;	 							*
;								*
;	Uses: NADA						*
;								*
;								*
;	Returns: a0	Address of last written data		*
;		 a1	Approximate address of most recent error*
;								*
;****************************************************************

ptr:
	move.l	PTRLOC,a0	; Get address of data
	move.l	a0,a1		; Get address of error
	add.l	#8,a1
	move.l	(a0),a0		; Get current address
	move.l	(a1),a1		; Get error address
	rts


;****************************************************************
;		CD_read						*
;								*
; This call transfers data from the CD, starting at a given	*
; time code to RAM in the Jaguar starting at a given location	*
; and ending at the other location.				*
; If the "Just seek" bit is set, no data is transfered but the	*
; CD will continue to advance at the current speed.		*
; This call always returns immediately. A CD_ack may follow	*
; ONLY if the "Just seek" bit is set. 				*
; Once this call is made use the call CD_ptr to find out where	*
; data is currently being stored.				*
;								*
;	Input: a0.l	Beginning of destination data buffer	*
;	       a1.l	End of destination data buffer		*
;								*
;	       d0.l	Time code to start read			*
;			bit 31 set => Just seek			*
;								*
; If CD_initm used 						*
; 	      d1.l	Pattern to match to			*
;	      d2.l	Bit to clear for circular buffer	*
;								*
;	Uses: a2						*
;								*
;	      d1						*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

read:
	btst.l	#31,d0
	bne.w	.play
	subq.l	#4,a0		; Make up for ISR pre-increment
	move.l	d0,-(sp)
	move.l	BUTCH,d0
	and.l	#$ffff0000,d0
	move.l	d0,BUTCH	; NO INTERRUPTS!!!!!!!!!!!
	move.l	(sp)+,d0
;	move.l	#0,BUTCH

	move.w	#$101,J_INT

	move.l	d1,-(sp)
	move.l	I2CNTRL,d1	;Read I2S Control Register
	bclr	#2,d1		; Stop data
	move.l	d1,I2CNTRL
	move.l	(sp)+,d1

	move.l	PTRLOC,a2
	move.l	a0,(a2)+
	move.l	a1,(a2)+
	move.l	#0,(a2)+

	btst.b	#7,INITTYPE
	beq	.not_bad
	move.l	PTRLOC,a0
	asl.l	#5,d2

	move.l	d2,-(sp)

	or.l	#$089a3c1a,d2		; These instructions include the bclr
	move.l	d2,188(a0)

	move.l	(sp)+,d2

	swap	d2
	or.l	#$3c1a1838,d2		; These instructions include the bclr
	move.l	d2,196(a0)

	move.l	#16,(a2)+
	move.l	d1,(a2)

.not_bad:

; Clear any pending DSARX states
	move.w	DS_DATA,d1

; Clear any pending errors
	move.l	I2CNTRL,d1

; Drain the FIFO so that we don't get overloaded

.dump:
	move.l	FIFO_DATA,d1
	move.l	I2CNTRL,d1
	btst	#4,d1
	bne.b	.dump

.butch_go:
	move.l	BUTCH,d1
	and.l	#$FFFF0000,d1
	or.l	#%000100001,d1 ;Enable DSARX interrupt
	move.l	d1,BUTCH
;	move.l	#%000100001,BUTCH	 ;Enable DSARX interrupt

; Do a play @

.play:	move.l	d0,d1		; mess with copy in d1
	lsr.l	#8,d1		; shift the byte over
	lsr.w	#8,d1		
	or.w	#$1000,d1	; format it for goto
	move.w	d1,DS_DATA	; DSA tx
        bsr.b	DSA_tx

	move.l	d0,d1		; mess with copy in d1
	lsr.w	#8,d1		
	or.w	#$1100,d1	; format it for goto
	move.w	d1,DS_DATA	; DSA tx
        bsr.b	DSA_tx

	move.l	d0,d1		; mess with copy in d1
	and.w	#$00FF,d1	; mask for minutes
	or.w	#$1200,d1	; format it for goto
	move.w	d1,DS_DATA	; DSA tx
        bsr.b	DSA_tx
	
	rts


; Internal use only
; waits for transmit to occur


DSA_tx:				; set up as a polling loop
	move.w	#$1000,d1	; This is VOODOO
.delay:
	dbra	d1,.delay
	move.l	BUTCH,d1	; get Butch's ICR into d1
	and.l	#$1000,d1	; mask for d12, DSA TX Intr. pending
	beq.b	DSA_tx		; nothing here yet, so wait for bit to set 
	move.l	DSCNTRL,d1	; read here to clear interrupt flag
	rts


;****************************************************************
;		CD_uread					*
;								*
; This call stops data recording started with a CD_read call	*
;								*
;	Input:  NADA						*
;	 							*
;								*
;	Uses: d0						*
;								*
;								*
;	Returns: NADA						*
;								*
;****************************************************************

uread:
	move.l	I2CNTRL,d0	;Read I2S Control Register
	bclr	#2,d0		; Stop data
	move.l	d0,I2CNTRL

	rts

;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
; 		The following code is from Dave Staugas		*
; I have added comments with " VOODOO" at delays in the code	*
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;**************************************************************************
; (C)1994 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;
;                                getoc.s
;
;       This MC68K program demos the "getoc" routine which returns
;	a buffer containing the Table of Contents (TOC) for the CD
;	found in the Jaguar CD-drive.  A description of the format
;	of that return buffer is found in the header comments of
;	"gettoc".
;
;
;                                REVISION HISTORY
;
; REV.  DATE       BY            DESCRIPTION OF EDIT
; """"  """"       ""            """""""""""""""""""
; 1.00  20 Apr 94  DJS		First version
; 1.01  04 May 94  LJT		Folded into CDBIOS
;
;
;--------------------------------------------------------------
;
; Butch's hardware registers
;
;
;BUTCH     equ  $DFFF00		;base of Butch=interrupt control register, R/W
;
;  When written (Long):
;
;  bit0 - set to enable interrupts
;  bit1 - enable CD data FIFO half full interrupt
;  bit2 - enable CD subcode frame-time interrupt (@ 2x spped = 7ms.)
;  bit3 - enable pre-set subcode time-match found interrupt
;  bit4 - CD module command transmit buffer empty interrupt
;  bit5 - CD module command receive buffer full
;  bit6 - CIRC failure interrupt
;
;  bit7-31  reserved, set to 0 
;
;
;  When read (Long):
;
;  bit0-8 reserved
;
;  bit9  - CD data FIFO half-full flag pending
;  bit10 - Frame pending
;  bit11 - Subcode data pending
;  bit12 - Command to CD drive pending (trans buffer empty if 1)
;  bit13 - Response from CD drive pending (rec buffer full if 1)
;  bit14 - CD uncorrectable data error pending
;
;
;   Offsets from BUTCH
;
O_DSCNTRL   equ  4		; DSA control register, R/W
O_DS_DATA   equ  $A		; DSA TX/RX data, R/W
;
;
;
O_I2CNTRL   equ  $10		; i2s bus control register, R/W
;
;  When read:
;
;  b0 - I2S data from drive is ON if 1
;  b1 - I2S path to Jerry is ON if 1
;  b2 - reserved
;  b3 - host bus width is 16 if 1, else 32
;  b4 - FIFO state is not empty if 1
;
;
;
;
O_SBCNTRL   equ  $14		; CD subcode control register, R/W
O_SUBDATA   equ  $18		; Subcode data register A
O_SUBDATB   equ  $1C		; Subcode data register B
O_SB_TIME   equ  $20		; Subcode time and compare enable (D24)
O_FIFODAT   equ  $24		; i2s FIFO data
O_I2SDAT2   equ  $28		; i2s FIFO data (old)
;
;


;
;
;-----------------------------------------
;
;
;   Get (multi-session) Table of Contents
;
;
;   entry:  a0 -> address of 1024 byte buffer for returned multi-session TOC
;
;
;   exit:  all regs preserved
;
;
;  The returned buffer will contain 8-byte records, one for each
;   track found on the CD in track/time order.  The very first
;   record (corresponding to the "0th" track) is the exception.
;
;   Format for the first record:
;
;    +0 - unused, reserved (0)
;    +1 - unused, reserved (0)
;    +2 - minimum track number
;    +3 - maximum track number
;    +4 - total number of sessions
;    +5 - start of last lead-out time, absolute minutes
;    +6 - start of last lead-out time, absolute seconds
;    +7 - start of last lead-out time, absolute frames
;
;   Format for the track records that follow:
;
;    +0 - track # (must be non-zero)
;    +1 - absolute minutes (0..99), start of track
;    +2 - absolute seconds (0..59), start of track
;    +3 - absolute frames, (0..74), start of track
;    +4 - session # (0..99)
;    +5 - track duration minutes
;    +6 - track duration seconds
;    +7 - track duration frames
;
;  Note that the track durations are computed by subtracting the
;   start time of track N by the start time of either track N+1 or by the
;   start of the lead-out for that session.  This may need to be further
;   adjusted by the customary 2 seconds of silence between tracks if necessary.
;
;

getoc:
	movem.l	a1-a6/d0-d7,-(sp)
	lea	BUTCH,a4		;we need this alot

;	move.l	#$0,(a4)		;enable BUTCH
;	move.l	#$10000,O_DSCNTRL(a4)	;turn on DSA bus


;  We may not succeed with setmode the 1st time,
;        so keep trying until success

	lea	900(a0),a3	;use for return codes from DS_DSA_tx
;remode:	
;	move.w	#$1501,d0	;set mode= 1x speed, audio mode
;	moveq	#1,d1		;# of return words
;	bsr	DS_DSA_tx
;
;	moveq	#15,d0		;this delay helps
;.redelay:
;	dbra	d0,.redelay
;
;	cmpi.w	#$1701,d2	;did we get the desired return code?
;	bne	remode		;if not, retry

;   setmode has now been performed...

;	move.w	#$7002,d0	;set DAC mode
;	moveq	#1,d1		;# of return words
;	bsr	DS_DSA_tx

	move.l	a0,a1
	moveq	#0,d0
	move.w	#255,d1		;clear out TOC
trackset:
	move.l	d0,(a1)+
	dbra	d1,trackset

	moveq	#-1,d6		;total sessions (-1 if unknown)
	moveq	#0,d7		;current session

	move.b	d6,6(a0)	;set min at max

resess:
	move.w	d7,d0		;get current session
	or.w	#$0300,d0	;read session TOC
	moveq	#5,d1		;to get disk turning...5 parms are returned
	bsr	DS_DSA_tx

	moveq	#4,d3
	move.l	a3,-(sp)	;preserve return codes ptr
getmaxT:
	move.w	(a3)+,d0	;get next return code
	move.w	d0,d1		;we want $20xx,$21xx,$22xx,$23xx, & $24xx
	lsr.w	#8,d1
	subi.w	#$20,d1
	bcs.s	notmaxT		;br if not what we expect
	cmpi.w	#5,d1
	bcc.s	notmaxT		;br if not what we expect
	move.b	d0,(a0,d1.w)	;save datum in correct slot of 0th record
notmaxT:
	dbra	d3,getmaxT	;go for all 5
	move.l	(sp)+,a3	;restore return code buf ptr

	move.b	1(a0),d3	;max track # in d3
	cmp.b	7(a0),d3	;is this larger than current champion?
	bls.s	notgmax	
	move.b	d3,7(a0)	;save new global max
 notgmax:	
	move.b	(a0),d5
	moveq	#0,d4
	move.b	d3,d4
	sub.b	d5,d4
	addq.l	#1,d4		;d4 is track count
	cmp.b	6(a0),d5	;min track--less than previous global min?
	bcc.s	notgmin
	move.b	d5,6(a0)
notgmin:
	move.b	d7,5(a0)	;save current session #

;  Get full (long) TOC, then sort

	move.w	d7,d0			;current session #
	or.w	#$1400,d0
	move.w	d0,O_DS_DATA(a4)		;send Full TOC command word
	moveq	#-1,d0			;time-out count


.txwait:
;	move.l	(a4),d2			;wait til transmit buffer empty
;	btst.l	#12,d2
;	beq	.txwait			;br if coast is clear
;
	tst.w	O_DS_DATA(a4)		;else,  
	moveq	#0,d5			;current track # (0=none yet)
	bra.s	.txcount		

.txwait1:
	move.l	(a4),d2			;wait til receive buffer full
	btst.l	#13,d2
	beq	.txwait1

	move.w	O_DS_DATA(a4),d2		;fetch next response
	tst.l	O_DSCNTRL(a4)		;read to clear interrupt flag


	move.w	d2,d1		;now, let's check that response
	lsr.w	#8,d1		;get hi byte to lo
	cmpi.w	#$60,d1		;   is it a track #?
	bne.s	.nottrk		;   br if not

;  track # given..

	moveq	#0,d5
	andi.w	#$ff,d2		;get argument from track
	cmp.b	d2,d3		;check for > max track
	bcs	.txcount	;greater than max track will be ignored
	move.w	d2,d5
	lsl.w	#3,d5		;*8 = index into track records
	tst.b	(a0,d5.w)	;  else, have we encountered already?
	bne.s	.txcount	;skip if so
;
	move.b	d2,(a0,d5.w)	;save track #
	move.b	d7,4(a0,d5.w)	;save session # too
	subq.w	#1,d4		;subtract max # of tracks
	bne.s	.txcount	; br if still need more tracks
	moveq	#4,d0		;just read 4 more items
	bra.s	.txcount

;  it wasn't a track--other track info perhaps

.nottrk:
	subi.w	#$61,d1
	bls.s	.txcount	;br if < $60
	cmpi.w	#5,d1		;>= $65 (not track info)
	bcc.s	.txcount	;br if so
	tst.w	d5		;have we been given a track?
	beq.s	.txcount
	add.w	d5,d1
	move.b	d2,(a0,d1.w)	;save track info
.txcount:
	dbra	d0,.txwait1	;loop until done

;  all tracks in on this session...

;     compute track durations

durat:
	move.w	(a0),d0		;get min/max
	move.w	d0,d1
	lsr.w	#8,d0		;d0=min
	andi.w	#$ff,d1		;d1=max
	sub.w	d0,d1		;d1= count
	lsl.w	#3,d0		;min * 8 = offset to 1st field
	move.l	a0,a1
	adda.w	d0,a1
	addq.l	#4,a1
	lea	8(a1),a2
	bra.s	subloopi
perTRK:
	move.b	-(a2),d5	;get frames for track(n+1)
	sub.b	-(a1),d5	;subtract frames for track(n)
	bcc.s	FRMok		;br if no borrow
	add.b	#75,d5		;else, frames roll over at 75
FRMok:
	move.b	d5,4(a1)	;save in duration frames for track(n)

;  CY set if borrow

	move.b	-(a2),d5	;do same for seconds 
	move.b	-(a1),d4
	subx.b	d4,d5		;adjust for borrow generated above
	bcc.s	SECok
	add.b	#60,d5		;seconds roll over at 60
SECok:
	move.b	d5,4(a1)	;save seconds

	move.b	-(a2),d5	;do same for minutes
	move.b	-(a1),d4
	subx.b	d4,d5
	move.b	d5,4(a1)

	addq.l	#8,a1		;advance to next record
	addq.l	#3,a1

	addq.l	#8,a2		;advance to next record
	addq.l	#3,a2

subloopi:
	dbra	d1,perTRK
	lea	13(a0),a5
	cmp.l	a2,a5
	bcc.s	nextse
	lea	5(a0),a2
	moveq	#0,d1
	bra	perTRK
nextse:

;   ...more sessions to read?

	tst.w	d6		;have we checked for # of sessions?
	bpl.s	sessisin	;br if session info has been done

	move.w	#$1800,d0	;issue spin-up (servo) command
	moveq	#1,d1		; to stop read long TOC command
	bsr	DS_DSA_tx

	moveq	#30,d0
.delay:
	dbra	d0,.delay	;this seems to help ;I call this VOODOO

	move.w	#$5400,d0	;get session info if we haven't before
	moveq	#1,d1		;# of return words
	bsr	DS_DSA_tx

	move.w	d2,d6
	andi.w	#$ff,d6
sessisin:
	addq.w	#1,d7
	cmp.w	d6,d7
	bcs	resess

audtest:
	move.w	#$1800,d0	;issue spin-up (servo) command
	moveq	#1,d1		; to stop read long TOC command
	bsr	DS_DSA_tx

;	move.w	#$200,O_DS_DATA(a4)	;stop the spindle

	move.l	2(a0),d0
	addq.l	#1,d0		;max sess # +1 = total sessions
	clr.w	(a0)
	ror.l	#8,d0
	move.w	6(a0),d1
	move.w	d1,2(a0)
	move.l	d0,4(a0)

	movem.l	(sp)+,a1-a6/d0-d7
	rts
;
;
;
;----------------------------------------------------
;
;  Send command to DSA bus
;   and receive 0,1 or multiple words in return
;
;	
;
;  entry:
;    d0 = command code to send
;    d1 = # of returned words expected
;
;    a3 -> buffer for returned words
;
;  exit:
;    d2 = last of returned words
;
;
DS_DSA_tx:
	move.l	a3,-(sp)
	move.l	(a4),d2			;check receive buffer full
	btst.l	#13,d2
	beq	.txready		;br if receive buffer clear
;
	move.w	O_DS_DATA(a4),d2		;else, get bogus receive stuff
	tst.l	O_DSCNTRL(a4)		;read to clear interrupt flag
.txready:
	move.w	d0,O_DS_DATA(a4)		;send command word
.txwait:
	bra.s	.txcount		
.txwait1:
	moveq	#15,d2			;this delay helps
.txdelay0:
	dbra	d2,.txdelay0		;I call this VOODOO
;
	move.l	(a4),d2			;wait til receive buffer full
	btst.l	#13,d2
	beq	.txwait1
;
	move.w	O_DS_DATA(a4),d2
	move.w	d2,(a3)+		;get response, stuff in return buffer
	tst.l	O_DSCNTRL(a4)		;read to clear interrupt flag
.txcount:
	dbra	d1,.txwait1
	move.l	(sp)+,a3

	rts


;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
; 		The preceding code is from Dave Staugas		*
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************
;****************************************************************


;****************************************************************
;		CD_switch					*
;								*
; This call waits for the CD lid to open and a new CD installed *
;								*
;	Input:  NADA						*
;	 							*
;								*
;	Uses: d0, d1						*
;	      a0						*
;								*
;								*
;	Returns: New TOC @ $2c00				*
;								*
;****************************************************************
switch:
	move.l	#$100000,BUTCH

not_open:
	move.w	#$5000,DS_DATA
	bsr	DSA_rx
	move.w	DS_DATA,d0
	move.w	d0,d1
	and.w	#$ff00,d1
	cmp.w	#$300,d1
	bne	not_open
	btst	#0,d0
	bne	not_open

not_closed:
	move.w	#$200,DS_DATA
	bsr	DSA_rx
	move.w	DS_DATA,d0
	cmp.w	#$402,d0
	bne	not_empty

	move.w	#$200,d1
frm_lop:
	move.w	VC,d0
	cmp.w	#1,d0
	bne	frm_lop
	dbra	d1,frm_lop
	bra	not_closed

not_empty:
	cmp.w	#$200,d0
	bne	not_closed

	move.l	#$180000,BUTCH

	move.l	#$2c00,a0
	bra	getoc
;	rts			; The previous ends with a RTS

DSA_rx:				; set up as a polling loop
	move.w	#$1000,d1	; This is VOODOO
.delay:
	dbra	d1,.delay
	move.l	BUTCH,d1	; get Butch's ICR into d1
	and.l	#$2000,d1	; mask for d13, DSA RX Intr. pending
	beq.b	DSA_rx		; nothing here yet, so wait for bit to set 
	move.l	DSCNTRL,d1	; read here to clear interrupt flag
	rts

	.data
err_flag:
	dc.w	0
