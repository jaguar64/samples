;**************************************************************************
; (C)1994 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;			Code to read CD data in thru Jerry
; 			based on Jeridata.gas
;
;**************************************************************************

		include		"jaguar.inc"

					; addresses of...

LRXD		equ	$F1A148		; i2s receive registers
RRXD		equ	$F1A14C

DSP_FLAGS       equ     $F1A100         ; flag register
STACKPOS	equ	$F1CF00		; stack in DSP SRAM

DSP_stop	equ	$F1C310		; Stop-DSP semaphore from M68K

DSP_BB		equ	$180000		; DSP storage buffer begin
DSP_BE		equ	$1f0000		; DSP storage (guess the rest)

		XDEF	JERI_B

ORG	$F1B000				; put this program at start of DSP SRAM

DSPORG:
	movei	JERI_B,r0		; (interrupt 0  - DSP Control)
	jump	(r0)
	nop
	nop
	nop
	nop
	movei	#i2s_isr,r29		; (interrupt 1 - I2S)
	jump	(r29)
	nop
	nop
	nop
	nop

JERI_B:
	movei	#$ffff,r20	; low_mask

	movei	#STACKPOS,r31	; Set up a stack

	movei	#DSP_BB,r21	; Set up pointer
	movei	#DSP_BE,r22	; Set up end position

	movei	#DSP_FLAGS,r1	; load dsp flags
	load	(r1),r0
	bset	5,r0		; enable I2S interrupt
	store	r0,(r1) 	; save dsp flags

	movei	#$f1b000,r3	; This is where the counter goes
mloop:
	moveq	#0,r23

delay:
	cmp	r23,r2
	jr	z,delay
	addqt	#1,r0

	jr	mloop
	store	r0,(r3)	
                 
i2s_isr:                        ; Sample pair-rate ISR
	movei	DSP_FLAGS,r30	; get flags ptr
	load	(r30),r29	; load flags

	movei	LRXD,r18	; address of left i2s input port
	load	(r18),r13	; data from left channel in r13
	store	r13,(r18)
	movei	RRXD,r17	; right input
	load	(r17),r12	; rt. in r12
	store	r12,(r17)

	moveq	#1,r2		; tell the main routine we got here

clean_up:			; do the housekeeping, per THE BOOK
	bclr	3,r29		; clear IMASK
	bset	10,r29		; set I2S interrupt clear bit
	load	(r31),r28	; get last instruction address
	addq	2,r28		; point at next to be executed
	addq	4,r31		; update the stack pointer
	jump	(r28)		; and return
	store	r29,(r30)	; restore flags

halt_DSP:			; do this to stop the damn thing
	movei	$DEADC0DE,r9	; breadcrumb

	moveq	#0,r1

	movei	#D_FLAGS,r0	; make sure it doesn't start 
	store	r1,(r0)		; with interrupts on

	movei	#D_CTRL,r0	; and halt it
	store	r1,(r0)

	nop
	nop
	end                                        

