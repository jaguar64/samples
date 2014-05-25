;---------------------------------------------------------------------------------------
;
; - Simple Jerry sample-replayer for Atari Jaguar (46KHz)
; - standard Plasma routine running on TOM using double-buffering (320x256, 3vbls)
;
;
; Sample: "Embrace the night" by Day-Mar
;
;
; Based on DOWNFALL.S by ReBoot
; _*.S by ReBoot
; Jerry code based on Sinister Development's modplayer
;
;
; (w)2013/2014 Carsten'lsl'Koeckritz  - lsl@quantentunnel.de
; http://checkpoint.atari.org
;---------------------------------------------------------------------------------------

                .include    "jaguar.inc"
                .text

SIN_MASK		EQU $FFE
COS				EQU $400


objs			equ 	32+32						; maximum number of objects in list

ob_list			equ		$200000-$4000				; address of list (shadow)
ob_list1		equ		ob_list+$2000				; address of read list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code entrypoint
;;

top_of_the_code:
				lea		$200000-4,a7				; set stack pointer

				moveq	#0,d0						;
				move.l	d0,$f00000+$1a148			; L_I2S
				move.l	d0,$f00000+$1a14c			; R_I2S
				move.l	d0,$f00000+$2114			; G_CTRL 
				move.l	d0,$f00000+$1a114			; D_CTRL
				move.l	d0,$f00000+$2100			; G_FLAGS
				move.l	d0,$f00000+$1a100			; D_FLAGS
				move.l	#$00070007,$f00000+$1a10c	; D_END
				move.l	#$00070007,$f00000+$210c	; G_END
;bclr	#0,DIV_OFFSET				;
;move.w	MEMCON1,d0					;
;bset	#1,d0						;
;bclr	#2,d0						; All this required for
;bset	#3,d0						; the Jagware cart
;bset	#4,d0						;
;bclr	#7,d0						;
;move.w	d0,MEMCON1					;
				
				lea		top_of_bss,a0				; clear BSS
				lea		_trashram,a1
				moveq	#0,d0
clear:			
				move.l	d0,(a0)+
				cmp.l	a1,a0
				bne.s	clear

				move.l	#'0000',mt_buffer
				move.l	#'1000',mt_buffer+4
				move.l	#1000,mt_buffer+8

				move.l #memory_track_work,work_area
				bsr		mt_init
;tst.l	mt_present
;bmi		.didnt_load
;lea		mt_buffer,a0
;lea		high_ascii,a1
;move.l	(a0)+,(a1)+					; load 1st half of ascii
;move.l	(a0)+,(a1)+					; load 2nd half of ascii
;move.l	(a0)+,(a1)+					; load highscore
;.didnt_load:


;				
;								 Varmod
;								 |	   0=CRY16 | 1=RGB24 | 2=DIRECT16 | 3=RGB16	
;								 |	   ||
; 								 |	   ||Vid enable
;								 |	   |||	
				move.w	#%0000011011000111,VMODE	; 320x256
				move.w	#$100,JOYSTICK				; turn off MUTE
                jsr     InitVideo               	; Setup our video registers.

				jsr     MakeObjectList          	; Create bit magled list for OP
				
                move.l  #VB_INTERUPT,LEVEL0     	; Install 68K LEVEL0 handler
                move.w  a_vde,d0                	; Must be ODD
                sub.w   #16,d0
                ori.w   #1,d0
                move.w  d0,VI

                move.w  #1,INT1                 	; Enable video interrupts
                and.w   #$f8ff,sr
			
				bsr		wait_frame
				bsr		set_title_list				; title splash				

				
				move.l	title_obj,a0				; position logo object
				moveq	#0,d0						; x
				moveq	#8,d1						; y  #80
				jsr		setup_object_xy			
				
				move.l	#ob_list,d0					; set the object list pointer
				swap	d0
				move.l	d0,OLP

;-----------------------------------------------------				
;-----------------------------------------------------

				bsr init_music

				bsr init_color_table
		
				bsr init_GPU_code

loop:
				st		enable_screen_swap
				bsr		wait_frame

				bsr		set_x_coeffs		; can be done on DSP too
				bsr		set_y_coeffs		; can be done on DSP too

				move.l	#$f03080,G_PC
				move.l  #RISCGO,G_CTRL		; start TOM
				
				bsr		wait_GPU			; wait until TOM done
				
				bsr		inc_angles

				add.l	#$14000,plasma_color_add

				bra	loop

;print vbls
				move.b	#64,text_color
				move.l	screen1,a0 		; screen
				lea		16+(16*320)(a0),a0
				move.l	vbl_counter,d0
				jsr		hex1_out


				bra loop
;-----------------------------------------------------
wait_GPU:
				move.l	G_CTRL,d0
;and.w	#1,d0
;bne.s	wait_GPU
				lsr.w	#1,d0
				bcs.s	wait_GPU
				rts
;-----------------------------------------------------


; x & y coefficients setup, not that optimised

set_x_coeffs:
				lea		x_coeffs,a6

				lea		sintab,a0
				lea		(a0),a1
				add.w	w0,a0
				add.w	w1,a1

				lea		$c001,a5			; x 16:16 inc
				moveq	#0,d3				; x 16:16
				
				moveq	#127,d5				; masker
				
				move.w	#SIN_MASK,d6				
				
				move.w	#320-1,d7
sxc:
				move.w	d7,d4				; x
				muls	#26,d4				; phase
				and.w	d6,d4				; SIN_MASK
				move.w	0(a0,d4.w),d0
				muls	#37*4,d0			; amplitude

				move.w	d7,d4				; x
				muls	#38,d4				; phase
				and.w	d6,d4				; SIN_MASK
				move.w	0(a1,d4.w),d1
				muls	#12*4,d1			; amplitude
				
				add.l	d1,d0

				add.l	d3,d0				;+x 16:16

				swap	d0
				
				and.l	d5,d0				; MASK

				move.l	d0,(a6)+

				add.l	a5,d3				; x 16:16 inc
				
				dbf		d7,sxc
				rts

set_y_coeffs:
				lea		y_coeffs,a6
				
				lea		sintab,a0
				lea		(a0),a1
				add.w	w2,a0
				add.w	w3,a1
				
				lea		$babe,a5			; y 16:16 inc
				move.l	plasma_color_add,d3	; y 16:16
				
				moveq	#127,d5				; masker
				
				move.w	#SIN_MASK,d6
				
				move.w #256-1,d7			; height
ly:
				move.w	d7,d4				; y
				muls	#-36,d4				; phase
				and.w	d6,d4				; SIN_MASK
				move.w	0(a0,d4.w),d0
				muls	#29*4,d0			; amplitude

				move.w	d7,d4				; y
				muls	#42,d4				; phase
				and.w	d6,d4				; SIN_MASK
				move.w	0(a1,d4.w),d1
				muls	#21*4,d1			; amplitude
				
				add.l	d1,d0
				
				add.l	d3,d0				;+y 16:16

				swap	d0
				
				and.l	d5,d0				; MASK

				move.l	d0,(a6)+

				add.l	a5,d3				; y 16:16 inc

				dbf		d7,ly
				rts

						.dphrase

plasma_color_add:		dc.l 0

						.dphrase

;**********************************************************************
;**********************************************************************
;***** GPU CODE (TOM) **************************************************
;**********************************************************************
;**********************************************************************

init_GPU_code:
				move.l  #0,G_CTRL

				lea		GPU_START_plasma,a0
				lea		$f03080,a1
				move.l	#GPU_END_plasma-GPU_START_plasma,d0
				bsr		copy_code_to_gpu
				rts
copy_code_to_gpu:
				lsr.l	#1,d0	; words
				subq.w	#1,d0
cctg:
				move.w	(a0)+,(a1)+
				dbf d0,cctg
				rts

				.dphrase
GPU_START_plasma:
				.gpu 
				.org $f03080
gpu_test_loop:
;-------------------------------------------				
	
				movei	#gpu_plasma_y,r20
				movei	#gpu_plasma_x,r21
				
				moveq	#16,r22				; x_coeff advance
				movei	#$000000ff,r23		; masker
	
				movei	#screen1,r14
				load	(r14),r15			; screen1
				
				movei	#x_coeffs,r0
				movei	#y_coeffs,r1
				
				subq	#4,r15				; screen--
								
				movei	#256,r3				; r3 ... height
gpu_plasma_y:
				move	r0,r14				; x_coeffs
				load	(r1),r4				; y_coeff
				addq	#4,r1

				moveq	#320/4/8,r5	; width
				
; r14 ... x_coeffs
; r15 ... screen

gpu_plasma_x:
				.rept 8

; register-scoreboarding optimized innerloop

				load	(r14),r6			; x_coeff0
				addqt	#4,r15				; screen++
				load	(r14+4/4),r7		; x_coeff1
				add		r4,r6				; +Y
				load	(r14+8/4),r8		; x_coeff2		
				add		r4,r7				; +Y
				load	(r14+12/4),r9		; x_coeff3
				add		r4,r8				; +Y	
				add		r4,r9				; +Y

				shlq	#24,r6	; 0
				shlq	#16,r7	; 1
				shlq	#8,r8	; 2
				or		r7,r6	; 01
				or		r9,r8	; 23

				add		r22,r14				; advance x_coeffs 4*4				
				or		r8,r6	; 0123

; only stall left, can be removed by the next coefficients prefetch!				
				store	r6,(r15)			; output 4 pixel

				.endr
				
				subq	#1,r5
				jump	ne,(r21)
				nop							; <-- can be optimised too!

				subq	#1,r3
				jump	ne,(r20)
				nop
;-------------------------------------------			
				movei	#G_CTRL,r1
				moveq	#0,r0
				store	r0,(r1)		; stop gpu
				nop
				nop
				nop

				.dphrase
x_coeffs:
				.rept 320			; since rmac only allows ds.l in BSS-segment
				.dc.l 0
				.endr
y_coeffs:
				.rept 256	
				.dc.l 0
				.endr

;----------------------------------------------------------------------
				.68000
				.text
GPU_END_plasma:
;**********************************************************************
;**********************************************************************
;**********************************************************************


;-----------------------------------------------------
; Red:   5 bits
; Blue:  5 bits
; Green: 6 bits
;                        7654321076543210 
;                        RRRRRbbbbbGGGGGG
init_color_table:
			
				lea		CLUT,a6				
				
				moveq	#0,d7
ict:
				move.w	d7,-(sp)
				
				lsl.w	#1,d7				; double! (0...510) because of 7bit plasma values!

;red
				move.w	d7,d0
				sub.w	#14,d0
				bsr		ict_colclip
				move	d0,d2
;green
				move.w	d7,d0
				bsr		ict_colclip
				move.w	d0,d1
;blue
				move.w	d7,d0
				add.w	#10,d0
				bsr		ict_colclip				
				move.w	d0,d3

; red,green,blue are now 8 bit values
; this code by Reboot converts them to 556 Jaguar palette
				
				moveq	#0,d4			; final Jaguar RGB value				
				
                lsr.w   #3,d2           ; div 8 for 5-bit colour  Red
                ror.w   #5,d2           ; shift to hi-bits
                or.w    d2,d4           ; mask RED!
                lsr.w   #3,d3           ; div 8 for 5-bit colour  Blue
                rol.w   #6,d3           ; shift to mid-bits
                or.w    d3,d4           ; mask BLUE!
                lsr.w   #2,d1           ; div 4 for 6-bit colour  Green
                or.w    d1,d4           ; mask GREEN!

				move.w	d4,(a6)+		; store in CLUT
	
				move.w	(sp)+,d7
				addq.b	#1,d7
				bne.s	ict

				rts

;in:  D0 ... color
;out: D0 in range (0...254)(254...0) - 128 arrays wraparound, because of 7 bit plasma values
ict_colclip:
				and.w	#255,d0
				cmp.w	#127,d0
				ble.s	ict_cc1
				neg.w	d0
				add.w	#255,d0
ict_cc1:
				add.w	d0,d0
				rts
;-----------------------------------------------------
				
inc_angles:
				moveq	#4-1,d7
				lea		w0,a0
				lea 	w0_inc,a1
				
				move.w #SIN_MASK,d6
ial:
				move.w	(a1)+,d0	; inc
				add.w	(a0),d0
				and.w	d6,d0
				move.w	d0,(a0)+
				dbf d7,ial
				rts
;-----------------------------------------------------				

				.data

w0:				dc.w 0
w1:				dc.w 0
w2:				dc.w 0
w3:				dc.w 0

w0_inc:			dc.w 16*2
w1_inc:			dc.w 23*2
w2_inc:			dc.w 14*2
w3_inc:			dc.w 20*2

				.dphrase

sintab:									; 16bit!, 2048 entries, 14 bit fractional, big endian
				.incbin "SIN2048.DAT"
				.incbin "SIN2048.DAT"	

				
				.dphrase

				.text

				.dphrase				
				
;-----------------------------------------------------
;;;;
;;;; SET new object list
;;;;

set_game_list:	move.l	skip_obj,a0					; branch to titles
				lea		obj_copy1,a1
				movem.l	(a1),d0-d7
				movem.l	d0-d7,(a0)
				rts

set_title_list:	move.l	skip_obj,a0
				move.l	title_list,a1
				move.l	#braobject,d0
				move.l	#always,d1
				move.l	#$7ff,d2
				bsr		CreateObject
				rts

set_blank_list:	move.l	skip_obj,a0
				move.l	blank_list,a1
				move.l	#braobject,d0
				move.l	#always,d1
				move.l	#$7ff,d2
				bsr		CreateObject
				rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; set object pointed to by (a0) to new x/y position and bitmap in d2
;;
;;
;; a0=object list data
;; d0=x
;; d1=y
;; d2=bitmap address
;;

setup_object_xyg: 
				add.l	#16,d0
				add.l	#8,d1

				move.l	#%1111111111,d3
				tst.w	ntsc_flag
				bpl		.pal_skip
				add.w	#28,d1
.pal_skip:		and.w	#%111111111111,d0			; mask x value only (for negatives)
				and.w	d3,d1
				move.w  6(a0),d4                	; get y-pos
				and.w   #$c007,d4
				move.w  14(a0),d6               	; get x-pos
				and.w   #$f000,d6
				lsl.w	#4,d1
                or.w    d1,d4
                or.w    d0,d6
                move.w  d4,6(a0)					; update in object data
                move.w  d6,14(a0)
				lsl.l	#8,d2
				move.l	(a0),d0
				and.l 	d3,d0
				or.l	d2,d0
				move.l	d0,(a0)						; update object GFX pointer
				rts

				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; set object pointed to by (a0) to new x/y position
;;
;;
;; a0=object list data
;; d0=x
;; d1=y
				
setup_object_xy:
				add.l	#16,d0
				add.l	#8,d1

				tst.w	ntsc_flag
				bpl		.pal_skip
				add.w	#28,d1
.pal_skip:		and.w	#%111111111111,d0
				and.w	#%1111111111,d1
				move.w  6(a0),d4                	; get y-pos
                and.w   #$c007,d4
                move.w  14(a0),d6               	; get x-pos
                and.w   #$f000,d6
				lsl.w	#4,d1
                or.w    d1,d4
                or.w    d0,d6
                move.w  d4,6(a0)					; update in object data
                move.w  d6,14(a0)
				rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Wait for a VBL
;;

wait_frame:		move.w	d7,-(a7)

				clr.l	vbl_counter
 
				move.w  ticks,d7
.wait:          cmp.w   ticks,d7
                beq.s   .wait

				move.w	(a7)+,d7
                rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Vertical Blank Interupt (At least it happens every frame somewhere......)
;;

				.dphrase

VB_INTERUPT:    movem.l d0-d7/a0-a6,-(a7)

				move.b	enable_screen_swap(pc),d0	; check semaphore
				beq.s	no_swap
				clr.b	enable_screen_swap			; lock
				bsr		swap_screens
no_swap:

				jsr     copy_olist              	; use Blitter to update active list from shadow
            	add.w	#7,ticks
				neg.l	_25hz						; flip 25hz flag
				
				move.w  #$101,INT1              	; Signal we're done
                move.w  #$0,INT2

				addq.l	#1,vbl_counter
.exit:			
				movem.l (a7)+,d0-d7/a0-a6
                rte

swap_screens:
				lea		screen0,a0
				movem.l (a0),d0-d1
				exg		d0,d1
				movem.l	d0-d1,(a0)

				move.l	title_obj,a0

				move.l	screen0,d0				; gfx address

				lsl.l   #8,d0                   ; shift address
				or.l	gfx_adr_array32,d0
				move.l	d0,(a0)                 ;

				rts

				.dphrase

vbl_counter:		dc.l 0
enable_screen_swap:	dc.l 0	
gfx_adr_array32:	dc.l 0
				
				.dphrase


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clear ram routine
;;

clear_memory:	clr.b	(a0)+
				cmp.l	a0,a1
				bne.s	clear_memory
				rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copy ram routine
;;

copy_ram:       move.w (a0)+,(a2)+
                cmp.l a0,a1
                bne.s copy_ram
                rts

				.dphrase

screen0:		dc.l screen_ram0
screen1:		dc.l screen_ram1

				.dphrase

				include "_initvid.s"
				include "_makeol.s"
				include "_joypad.s"
				include "_BMPCONV.S"
				include "_INITGFX.S"
				include "_MEMTRCK.S"

				include	"SAMPLE_REPLAY_JERRY.S"
				
				include "TEXTOUT.S"

				.dphrase
			

				.bss
						.dphrase
top_of_bss:
						.phrase

blank_list:				.ds.l	1
title_list:				.ds.l	1
skip_obj:				.ds.l	1
obj_copy1:				.ds.l	8
player_obj:				.ds.l	1
blocking_obj:			.ds.l	1
title_obj:				.ds.l	1


_25hz:					.ds.l	1

background_flag:		.ds.l	1

player_x:				.ds.l	1
						.ds.l	1
player_y:				.ds.l	1
						.ds.l	1
player_frame:			.ds.l	1
anim_delay:				.ds.l	1
direction:				.ds.l	1
at_stop:				.ds.l	1
falling:				.ds.l	1
falling_anim:			.ds.l	1
falling_offset:			.ds.l	1

ntsc_flag:				.ds.w	1
a_hdb:          		.ds.w   1
a_hde:          		.ds.w   1
a_vdb:          		.ds.w   1
a_vde:          		.ds.w   1
width:          		.ds.w   1
height:         		.ds.w   1
ticks:					.ds.w	1
temppal:				.ds.w	256

						.dphrase
memory_track_work:		.ds.b	16384						

blocking_jag:			.ds.b	40*16

						.dphrase

						.ds.b	16384
screen_ram0:			.ds.b	320*256
						.ds.b	16384
screen_ram1:			.ds.b	320*256
						.ds.b	16384

						.dphrase

_trashram:		
				.end
			
























