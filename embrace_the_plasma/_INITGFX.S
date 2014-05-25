;;
convert_bmp:

;lea		logo_bmp,a0
;lea		logo_jag,a1
;lea		temppal,a2
;moveq	#1,d5
;move.l	#320,d6
;move.l	#80,d7
;bsr		ConvertBMP


;lea		font16_bmp,a0
;lea		font16_jag,a1
;lea		temppal,a2
;moveq	#2,d5
;move.l	#160,d6
;move.l	#16,d7
;bsr		ConvertBMP
;movem.l	temppal,d0-d7
;movem.l	d0-d7,font16_pal
				
;lea		para1_pal,a0					;; and now copy the 16c (4bpp) pallettes into the CLUT
;lea		$f00400,a1
;move.l	#127,d7
;.writepal:		move.l	(a0)+,(a1)+
;dbra	d7,.writepal
				
				rts


				
				
				
				
				
				