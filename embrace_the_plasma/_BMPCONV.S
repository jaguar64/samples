;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Assumes 8bpp and RGB - Will not handle RLE8 Packed Files!!!
;;
;; a0 - address of Windows bitmap
;; a1 - address to place Jaguar bitmap at
;; a2 - address to place 16-bit Jaguar pallette
;; d5 - 0=8bit colour JAG dest. bitmap
;;		1=16bit colour JAG dest. bitmap
;;	    2=4bit colour JAG dest. bitmap
;; d6 - width (in PIXELS)
;; d7 - height (in PIXELS)
;;
;; returns GFX end in d0

ConvertBMP:     move.l  a2,-(a7)
                move.l  a0,a6
                lea     53(a6),a3       ; Start of xBGR table
                move.l  #255,d0         ; 256 colours
.clut:          moveq.l #0,d1           ;
                moveq.l #0,d2           ; make
                moveq.l #0,d3           ;  em
                moveq.l #0,d4           ;   empty
                move.b  (a3)+,d1        ; unused
                move.b  (a3)+,d3        ; (B)lue
                move.b  (a3)+,d1        ; (G)reen
                move.b  (a3)+,d2        ; (R)ed
                lsr.w   #3,d2           ; div 8 for 5-bit colour
                ror.w   #5,d2           ; shift to hi-bits
                or.w    d2,d4           ; mask RED!
                lsr.w   #3,d3           ; div 8 for 5-bit colour
                rol.w   #6,d3           ; shift to mid-bits
                or.w    d3,d4           ; mask BLUE!
                lsr.w   #2,d1           ; div 4 for 6-bit colour
                or.w    d1,d4           ; mask GREEN!
                move.w  d4,(a2)+        ; Store 16-BIT RBG Jag Colour!
                dbra    d0,.clut
                move.l  (a7)+,a2

				cmp.w	#2,d5
				beq		fourbit				
                tst.l   d5              ; check for colour depth
                beq     eightbit

sixteenbit:     lea     1024+54(a6),a0  ;
                move.w  d6,d0           ;
                muls    d7,d0           ;
                add.l   d0,a1           ;
                add.l   d0,a1           ;
                move.l  a1,-(a7)
                sub.l   d6,a1           ;
                sub.l   d6,a1           ; addr of last line

                move.l  d7,d0           ; copy the bitmap
                subq.l  #1,d0
.lines:         move.l  d6,d1           ; a line at a time
                subq.l  #1,d1
.acc:           move.b  (a0)+,d3        ; get colour value
                and.w   #$00FF,D3       ; and only that value
                add.w   d3,d3           ; mult by 2
                move.w  (a2,d3.w),(a1)+ ; store RBG Jag value from pallette
                dbra    d1,.acc         ; loop for pixels
                sub.l   d6,a1           ;
                sub.l   d6,a1           ; go back
                sub.l   d6,a1           ;  2 lines
                sub.l   d6,a1           ;
                dbra    d0,.lines       ; loop for lines

                move.l  (a7)+,d0        ; get end of gfx address
                rts

eightbit:       lea     1024+54(a6),a0  ;
                move.l  d7,d5
                subq    #1,d5
                muls    d6,d5
                add.l   d5,a1
                add.l   d6,a1
                move.l  a1,-(a7)
                sub.l   d6,a1

.lines:         move.l  d6,d1           ; a line at a time
.acc:           move.b  (a0)+,(a1)+
                subq    #1,d1
                bne.s   .acc            ; loop for pixels
                sub.l   d6,a1           ;
                sub.l   d6,a1           ; go back
                subq    #1,d7
                bne.s   .lines

                move.l  (a7)+,d0
                rts
				
fourbit:       	lea     $76(a6),a0  ;

				asr.l	#1,d6			; 2 pixels/byte
			
                move.l  d7,d5
                subq    #1,d5
                muls    d6,d5
                add.l   d5,a1
                add.l   d6,a1
                move.l  a1,-(a7)
                sub.l   d6,a1

.lines:         move.l  d6,d1           ; a line at a time

.acc:			move.b	(a0)+,(a1)+
                subq    #1,d1
                bne.s   .acc            ; loop for pixels
                sub.l   d6,a1           ;
                sub.l   d6,a1           ; go back
                subq    #1,d7
                bne.s   .lines

                move.l  (a7)+,d0
				add.l	#64,d0
				and.b	#%11111000,d0
                rts		
				