;---------------------------------------------------------------------------------------
;
; Simple Jerry sample-replayer for Atari Jaguar
; (w)2013/2014 Carsten'lsl'Koeckritz
;
; based on Sinister Development's modplayer
;
; 
; Sample: "Embrace the night" by Day-Mar
;
; http://checkpoint.atari.org
;---------------------------------------------------------------------------------------

SAMPLE_BITS		EQU 8
SAMPLE_ONE		EQU (1<<SAMPLE_BITS)

;---------------------------------------------------------------------------------------

				.dphrase
				.68000
				.dphrase

init_music:
				bsr		setup_dsp
				rts
setup_dsp:                                                      ;setup dsp, init some vars etc.
                movem.l         d0-a6,-(sp)
				
                move.l          #0,D_CTRL                       ;stop dsp
                move.l          #0,D_FLAGS                      ;set dsp flags
				
                lea             dsp_code_start(pc),a0
                lea             dsp_code_end(pc),a1
                lea             D_RAM,a2
copy_dsp:		move.l          (a0)+,(a2)+                     ;copy dsp code to dsp ram
                cmp.l           a0,a1
                bgt.s           copy_dsp

;--------------------------------------------------
; freq   = (26590906/32/(2*(x+1))
; freq   = (26590906/64) / (x+1)	| *(x+1) , /freq
; (x+1)  = (26590906/64) / freq
; x      = (26590906/64) / freq - 1
;--------------------------------------------------

; 22050 Hz: 17.84
; 44100 Hz:  8.42 --> 9 --> 46164 Hz

				moveq	#9,d0			; 46164 Hz
				
                move.l  d0,SCLK			; setup dsp timer

                move.l  #$15,SMODE
                move.l  #D_RAM,D_PC		;set dsp pc to start of code
                move.l	#1,D_CTRL		;start dsp

                movem.l (sp)+,d0-a6
                rts

				.dphrase
;--------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------
dsp_code_start:
                .dsp
                .org            D_RAM

                movei           #dsp_main,r0            ;point to start
                jump            (r0)                    ;go start
                nop
                nop
                nop
                nop
dsp_int:        load            (r28),r29               ;get dsp flags
                bclr            #3,r29                  ;interrupt serviced
                load            (r31),r30               ;get return from stack
                bset            #10,r29                 ;clear int source flag
                addq            #4,r31                  ;update stack
                addq            #2,r30                  ;update return address
                movei           #1,r27                  ;set int wait flag
                jump            (r30)                   ;return from int
                store           r29,(r28)               ;set dsp flags

dsp_main:
				movei           #dsp_stack,r31          ;point to stack
                moveta          r31,r31                 ;in both reg sets
                movei           #D_FLAGS,r28            ;point to DSP flags
                movei           #dsp_dummy,r26          ;empty sample pointer
                load            (r28),r0                ;get dsp flags
                bset            #5,r0                   ;enable timer int
                store           r0,(r28)                ;do it				
;------------------------------------------------------------------------
dsp_main_loop:
				movei	#sample0_pos,r20
				load	(r20),r2
				movei	#(sample_end-sample)<<SAMPLE_BITS,r21
				cmp		r2,r21				; check if end of sample reached
				jr		hi,not_sample_end
				nop
				moveq	#0,r2
not_sample_end:

				movei	#sample0_inc,r21
				load	(r21),r1
				add		r1,r2				; sample0_pos+=sample0_inc
				store	r2,(r20)			; sample0_pos
				shrq	#SAMPLE_BITS,r2		; get int
				
				movei	#sample,r19
				add		r2,r19				; sample offset

							
				loadb	(r19),r0			; load sample byte
				shlq	#32-8,r0			; 32 bit extend
				sharq	#32-16,r0			; 16 bit signed
				movei	#256+256+128,r1		; amp factor
				imult	r1,r0				; do amp
				sharq	#8,r0				; amp bits
				sat16s	r0					; 16 bit signed saturation(clipping)
				sharq	#2,r0				; 16 bit --> 14 bit !!

				move	r0,r1				; left=right

                moveq           #0,r27                  ;clear int wait flag
                movei           #L_I2S,r14              ;point to sound h/ware !!!*********!!!
                movei           #dsp_main_loop,r2		; do_chan_1,r2  ;point to start

s_wait_int:     cmpq            #1,r27                  ;wait for timer int
                jr              ne,s_wait_int           ;not yet?
                nop
                store           r0,(r14)                ;output left channel
                store           r1,(r14+4/4)            ;output right channel

                jump            (r2)                    ;and do it all again
                nop                                     ;this is the weird one!!!!
;---------------------------------------------------------------------------------------				
				.phrase

sample0_pos:	dc.l 0
sample0_inc:	dc.l SAMPLE_ONE
		
                .phrase
                .dc.l           0,0,0,0,0,0,0,0         ;stack w/space
dsp_stack:      .dc.l           0,0                     ;stack pointer
dsp_dummy:      .dc.l           0,0                     ;empty dummy sample

                .68000

dsp_code_end:   .dc.l           0

				.dphrase
				.text
				
;---------------------------------------------------------------------------------------				
;---------------------------------------------------------------------------------------				
;---------------------------------------------------------------------------------------				
;
; sample

			.dphrase
sample:
			incbin "MUSIC/Day-Mar_Embrace_the_night_8bit_44KHz.spl"
sample_end:
			.dphrase
				
				
				
				
				
				
				