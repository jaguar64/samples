;---------------------------------------------------------------------------------------
;
; Simple to screen text/value printing routines
; (w)2013/2014 Carsten'lsl'Koeckritz
;
; http://checkpoint.atari.org
;---------------------------------------------------------------------------------------


T_XW	equ 320			; width


text_color:	.dc.b	1,0

		.dphrase

; D0.l ... num
; A0   ... screen
hex8_out:
		move.l	d0,d1
		lea		hexasc(pc),a1
		moveq	#8-1,d7
h8o_l:
		rol.l	#4,d1
		moveq	#$0f,d0
		and.b	d1,d0
		move.b	0(a1,d0.w),d0
		bsr.s	print_char
		dbf		d7,h8o_l
		rts

; D0.l ... num
; A0   ... screen
hex1_out:
		move.l	d0,d1
		lea		hexasc(pc),a1
		moveq	#$0f,d0
		and.b	d1,d0
		move.b	0(a1,d0.w),d0
		bsr.s	print_char
		rts

hexasc:	.dc.b "0123456789ABCDEF"


		.dphrase


; D0...char
; A0...screen
print_char:
		movem.l	d0-a6,-(sp)

		lea		font88(pc),a1
		and.w	#$00ff,d0
		lsl.w	#3,d0
		adda.w	d0,a1
		
		move.b	text_color(pc),d6
		
		moveq	#8-1,d7
pc_y:
		move.b	(a1)+,d0	; 8 pixel, 1bpp

		.rept 8
		add.b	d0,d0
		scs		d1
		and.b	d6,d1		; color
		move.b	d1,(a0)+	; screen out
		.endr

		lea		T_XW-8(a0),a0
		dbf		d7,pc_y
		
		movem.l	(sp)+,d0-a6
		addq.l	#8,a0
		
		rts


		.dphrase
font88:	.incbin "88SYS.FNT"
		.dphrase
	