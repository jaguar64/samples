;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create the initial object list.
;;


bitmapobject    equ 0            ; object types
scaledobject    equ 1
gpuobject       equ 2
braobject       equ 3             
stopobject      equ 4

y_less          equ 0            ; branch object types
y_more          equ 1
always          equ 2

; D1=branch type
; D2=Ypos
; A1=address if branch TAKEN

				.dphrase
stoplist:		dc.l	0,4

MakeObjectList:	move.l	#stoplist,d0
				swap.w	d0
				move.l	d0,OLP

				lea     ob_list1,a0             			; start of OL
                jsr     build_header            			; build BRA header

				move.l	a0,skip_obj
				move.l	#braobject,d0
				move.l	#always,d1
				move.l	#$7ff,d2
				lea		32(a0),a1
				bsr		CreateObject

				movem.l	-32(a0),d0-d7
				lea		obj_copy1,a1
				movem.l	d0-d7,(a1)
				
	
				tst.w	ntsc_flag
				bpl.s	.ntsc_end

				move.l	a0,blocking_obj
				lea		blocking_jag,a1
				move.l	#bitmapobject,d0
				move.l	#16,d1
				move.l	#40,d2
				move.l	#0,d3
				move.l	#0,d4
				move.l	#40,d5
				jsr		CreateObject
				and.w	#%1110000000111111,-32+10(a0)	; mask out index
				or.w	#%0000110000000000,-32+10(a0)	; or in next index (Pal 6)				
		
.ntsc_end:		moveq.l #stopobject,d0          			; STOP Object
                jsr     CreateObject

				lea		24(a0),a0
				move.l	a0,title_list
				
;;; *** SCREEN ***
				
				move.l	a0,title_obj

				move.l	screen0,a1

				move.l  #bitmapobject,d0        			; type
				move.l  #256,d1								; 74,d1 ; height
                move.l  #320,d2      						; bytes to next line

; 4 for 256c and 5 for 16bit
; 4 ... 16bit			
                moveq  #3,d3
				
                moveq  #1,d4                   			; Transparent
;move.l  #640,d5          					; bytes/pixels*width / line;
				move.l d2,d5
				jsr     CreateObject

				move.l	a0,blank_list
				moveq.l #stopobject,d0          			; STOP Object
                jsr     CreateObject				
				
				rts
				
;; Set RMW flag on current object
;;
				
.rmw:			lea		-32(a0),a1							; pointer to object
				or.w	#%0100000000000000,10(a1)			; RMW_OBJECT
				rts
				
;; Set CLUT pallette to D0 (0-15) on current object
;;

.setpalt:		lea		-32(a0),a1
				rol.w	#4,d0
				rol.w	#8,d0
				and.w	#%1110000000111111,10(a1)			; mask out index
				or.w	d0,10(a1)
				rts

.reflect:		lea		-32(a0),a1
flipset:		or.w	#%0010000000000000,10(a1)	; REFLECT_OBJECT
				rts

flipclr:		and.w	#%1101111111111111,10(a1)
				rts
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copy Object List Routine
;;

copy_olist:
				move.l	#ob_list,A1_BASE
				move.l	#$0,A1_PIXEL
				move.l	#PIXEL16|XADDPHR|PITCH1,A1_FLAGS
				move.l	#ob_list1,A2_BASE
				move.l	#$0,A2_PIXEL
				move.l	#PIXEL16|XADDPHR|PITCH1,A2_FLAGS
				move.w	#1,d0
				swap	d0
				move.w	#$2000/2,d0
				move.l	d0,B_COUNT
				move.l	#LFU_REPLACE|SRCEN,B_CMD
				rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Build a header that'll STOP if VPOS not on screen.
;;

build_header:   move.l  a0,d0               ; address if bra not taken
                sub.l   #ob_list1,d0        ;
                add.l   #ob_list,d0         ;
                add.l   #32,d0              ; next BRA object
                lsr.l   #3,d0               ;
                lsl.l   #8,d0               ;
                clr.l   (a0)                ;
                move.l  #$00008003,4(a0)    ; bra if yp<0
				
.nocliptop:		or.l    d0,2(a0)            ; link to next BRA
                lea     8(a0),a0            ;
                bsr     stopob              ; write stop object (before top)
                lea     16(a0),a0           ; next object here

                move.l  a0,d0               ; calc address of 1st actual
                sub.l   #ob_list1,d0        ; object (after the next STOP)
                add.l   #ob_list,d0         ;
                add.l   #32,d0              ;
                lsr.l   #3,d0               ;
                lsl.l   #8,d0               ;
                clr.l   (a0)                ;
                move.l  #$00004003,4(a0)    ; bra is yp>value
                or.l    d0,2(a0)            ; link to first object!

; 560 correct for PAL
				move.w	CONFIG,d6
				andi.w	#VIDTYPE,d6
				beq		.pal
				move.l	#492,d0				; 492 lines = full NTSC frame
				move.w	#1,ntsc_flag
				bra		.ntsc
.pal:			move.l	#550,d0				; 492 lines = full PAL frame
				move.w	#-1,ntsc_flag
.ntsc:          lsl.l   #3,d0               ;
                or.l    d0,4(a0)            ;
                lea     8(a0),a0            ; next object
                bsr     stopob              ; is a STOP
				lea     16(a0),a0           ; address for next REAL object

                rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simple Object List Routines for a simple life.
;;
;; call with *d0=object type
;;           *d1=height
;;           *d2=data width
;;           *d3=colour depth
;;           *d4=transparent
;;           *d5=image width
;;           *a0=address of object (phrase alligned)
;;           *a1=address for GFX   (phrase alligned)
;;
;; exit with object built
;;           link=object address+32
;;           scaled objects will *NOT* be 1:1 in x/y
;;           x/y positions will be -500,2 (off screen)
;;

CreateObject:   lsl.w   #2,d0                   ; object type
                move.l  jmp_tab(pc,d0),a6       ; get routine address
                jmp (a6)                        ; call it!

jmp_tab:        .dc.l bitmp                     ; jump table
                .dc.l scaled
                .dc.l gpuob
                .dc.l braob
                .dc.l stopob

bitmp:          clr.l   (a0)                    ; template
                clr.l   4(a0)                   ;
                clr.l   8(a0)                   ;
                move.l  #$00008000,12(a0)       ;
                move.l  a0,d0                   ; link address
                sub.l   #ob_list1,d0            ;
                add.l   #ob_list,d0             ;
                add.l   #32,d0                  ;
                and.b   #%11111000,d0           ;
                lsl.l   #5,d0                   ;
                or.l    d0,2(a0)                ;
                move.l  a1,d0                   ; gfx address
                lsl.l   #8,d0                   ;

				move.l	(a0),gfx_adr_array32	; ***USED BY swap_screens***


                or.l    d0,(a0)                 ;
                move.l  d1,d0                   ; height
                swap    d0                      ;
                lsr.l   #2,d0                   ;
                or.l    d0,4(a0)                ;
                ror.w   #1,d4                   ; transparency
                or.w    d4,10(a0)               ;
                lsl.w   #8,d3                   ; depth (colour depth)
                lsl.w   #4,d3                   ;
                or.w    d3,14(a0)               ;
                lsr.w   #3,d2                   ; data width
                swap    d2                      ;
                lsl.l   #2,d2                   ;
                or.l    d2,12(a0)               ;
                lsr.w   #3,d5                   ; image width
                swap    d5                      ;
                lsr.l   #4,d5                   ;
                or.l    d5,10(a0)               ;
                move.w  #-500,d0                ; x-pos
                and.w   #$fff,d0
                or.w    d0,14(a0)
                or.w    #2*8,6(a0)              ; y-pos
                lea     32(a0),a0               ; BITMAP object!
                rts

scaled:         bsr     bitmp                   ; same as bitmap
                move.l  #$0,-16(a0)             ; clear it out
                move.l  #%00000000000000000010000000100000,-12(a0)
                or.l    #$1,-28(a0)             ; SCALED object!
                rts

gpuob:          move.l  #0,(a0)+
                move.l  #$3ffa,(a0)+            ; GPU object!
                rts

				
; D1=branch type
; D2=Ypos
; A1=address if branch TAKEN
				
braob:          add     d2,d2                   ; mult y-pos
                bsr     branchobject            ; make the object
make8into32:    move.l  #braobject,d0           ;
                move.l  #always,d1              ; even it out for 32-byte
                move.l  #$7ff,d2                ; positions by following with
                lea     24(a0),a1               ; a BRA+16 object
                bsr     branchobject            ;
                lea     16(a0),a0               ;
                rts

branchobject:   clr.l   (a0)
                move.l  #3,4(a0)
                add.w   d1,d1
                move.w  branchtypes(pc,d1.w),d1
                or.w    d1,6(a0)                ; branch TYPE!
                move.l  a1,d0
                sub.l   #ob_list1,d0
                add.l   #ob_list,d0
                and.l   #$fffffff8,d0
                lsl.l   #5,d0                   ; Link if branch **TAKEN!**
                move.l  d0,2(a0)                ; (< & > swapped!)
                lsl.w   #3,d2                   ; scanline to branch on
                or.w    d2,6(a0)                ; is VC/2
                lea     8(a0),a0                ; next object
                rts

branchtypes:    .dc.w    $4000,$8000,$0000       ; 

stopob:         move.l  #0,(a0)+
                move.l  #4,(a0)+                ; STOP object!
                rts
				
				
				