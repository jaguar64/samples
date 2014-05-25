;
; MT Track support for Superfly DX
; Most of the code here by Matthias Domin.
; Chopped up and adapted for Superfly DX by ggn (started 13/5/2010)
;

NVM_Bios    equ $2404
NVM_Magic   equ $2400

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mt_init: Call this routine once when your program initializes. The code will:
;          - Initialise the cart and, if needed, copy the ROM code to NVM_Bios
;          - Try to initialize the MT BIOS. You must find a 16k phrase aligned mem space for this.
;            Change "app_name" in the end of the source to suit your application.
;            App name must be up to 15 chars in length and should be composed only from the following chars:
;            A B C D E F G H I J K L M N O P Q R S T U V
;            W X Y Z 0 1 2 3 4 5 6 7 8 9 : ' . space
;          - Try to open the save file (change "save_file" in the end of the source to suit your needs)
;            If it exists, read the savecode and close the file.
;          - If the file in the previous step doesn't exist, then create it (file name length up to 9 chars)
;          If init or create file fails, then mt_present is set to -1.L
;          Read and close errors are not handled atm. (Superfly savecode has its own CRC, so it's not too much
;          of a problem anyway).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

mt_init:
	movem.l	d0-a6,-(sp)

	move.l	#1,gorffound
	
    jsr     InitializeMemoryTrackAccess  	; Test MemoryTrack-access
	
    move.l	work_area,-(sp)					; Call "Initialize"
    pea     app_name						; int NVM_Bios(short opcode = 0, char *app_name, char *work_area)
    move.w  #0,-(sp)
    jsr     NVM_Bios
    adda.l  #10,sp

    btst.l   #31,d0       					; 0 = success
    beq.s   .NVMInitOk

	move.l	#-1,mt_present					; nope, flag that the mt wasn't initalized, so it won't be used
	movem.l	(sp)+,d0-a6
	rts
	
.NVMInitOk:
	bsr		mt_load							; Load the savefile
.rts:
	movem.l	(sp)+,d0-a6
	rts				

	; and that's it

nvexit:	dc.l	0
dont_upd_txt:	dc.l	1
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mt_load: Loads a save file.
;          Creates the file if it doesn't exist.
;          Puts a -1 in mt_present if create fails (the cart prolly ran out of space)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
mt_load:
	movem.l	d0-a6,-(sp)

    move.l	work_area,-(sp)					; Call "Initialize"
    pea     gorf_name						; int NVM_Bios(short opcode = 0, char *app_name, char *work_area)
    move.w  #0,-(sp)
    jsr     NVM_Bios
    adda.l  #10,sp

    btst.l   #31,d0       					; 0 = success	
	bne.s	.notfound
	
	move.l	#-1,gorffound
				
.notfound:
    move.l	work_area,-(sp)					; Call "Initialize"
    pea     app_name						; int NVM_Bios(short opcode = 0, char *app_name, char *work_area)
    move.w  #0,-(sp)
    jsr     NVM_Bios
    adda.l  #10,sp
	
    pea     file_name						; Call "Open"
    move.w  #2,-(sp)						; int NVM_Bios( short opcode = 2, char *file_name)
    jsr     NVM_Bios
    adda.l  #6,sp

    cmp.l   #0,d0
    bmi     filenotfound

    move.w  d0,filehandle
	
    move.l  #12,-(sp)						; Call "Read"
    pea     mt_buffer						; long NVM_Bios( short opcode = 5, short handle, char * bufptr, long count)
    move.w  filehandle,-(sp)
    move.w  #5,-(sp)
    jsr     NVM_Bios
    adda.l  #12,sp

    move.w  filehandle,-(sp)				; Call "Close"
    move.w  #3,-(sp)						; int NVM_Bios( short opcode = 3, short handle)
    jsr     NVM_Bios
    adda.l  #4,sp
	
	bra		mt_load_exit					; All ok (we hope!), so exit!


filenotfound:
    move.l  #128,-(sp)						; Call "Create"
    move.l  #file_name,-(sp)				; int NVM_Bios( short opcode = 1, char *file_name, long file_size)
    move.w  #1,-(sp)
    jsr     NVM_Bios
    adda.l  #10,sp

    cmp.l   #0,d0
    bmi     filenotcreated

    move.l  #12,-(sp)						; Call "Write"
    pea     mt_buffer						; long NVM_Bios( short opcode = 6, short handle, char *bufptr, long count )
    move.w  filehandle,-(sp)
    move.w  #6,-(sp)
    jsr     NVM_Bios
    adda.l  #12,sp

    move.w  filehandle,-(sp)				; Call "Close"
    move.w  #3,-(sp)						; int NVM_Bios( short opcode = 3, short handle)
    jsr     NVM_Bios
    adda.l  #4,sp

mt_load_exit:
	movem.l	(sp)+,d0-a6
	rts

	
filenotcreated:
	move.l	#-1,mt_present					;File not created - don't use MT at all!
	bra.s mt_load_exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mt_save: saves a file. (Assumes that the file is already created. By the time someone calls this subroutine
;          the file should have been created anyway!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

mt_save:
	movem.l	d0-a6,-(sp)

    move.l	work_area,-(sp)					; Call "Initialize"
    pea     app_name						; int NVM_Bios(short opcode = 0, char *app_name, char *work_area)
    move.w  #0,-(sp)
    jsr     NVM_Bios
    adda.l  #10,sp

    pea     file_name						; Call "Open"
    move.w  #2,-(sp)						; int NVM_Bios( short opcode = 2, char *file_name)
    jsr     NVM_Bios
    adda.l  #6,sp
    cmp.l   #0,d0
    bmi     .filenotfound
    move.w  d0,filehandle

    move.l  #12,-(sp)						; Call "Write"
    pea     mt_buffer						; long NVM_Bios( short opcode = 6, short handle, char *bufptr, long count )
    move.w  filehandle,-(sp)
    move.w  #6,-(sp)
    jsr     NVM_Bios
    adda.l  #12,sp
	
    move.w  filehandle,-(sp)				; Call "Close"
    move.w  #3,-(sp)						; int NVM_Bios( short opcode = 3, short handle)
    jsr     NVM_Bios
    adda.l  #4,sp

.filenotfound:
	movem.l	(sp)+,d0-a6
	rts

InitializeMemoryTrackAccess:
    move.w  #$1865,MEMCON1              	; set cart up to be 32 bits
    nop                    					; wait for it to settle down
    nop
    nop
											; DO: switch off CD-BOOT-ROM
    move.l  #0,$dfff00 						; turn off BUTCH chip of the CDROM! in 32bit!!!
    
	;
    ; In case that there was no CD-Addon:
    ; Need to issue a reset-command after this for ATMEL 29C010 only!!!!
    ;

    move.w  #$00aa,d4						; reset the ROM
    move.w  #$0055,d5
    move.w  #$00f0,d6
    lea     $800000+(4*$5555),a4
    lea     $800000+(4*$2aaa),a5

    move.w  sr,d7
    or.w    #$0700,sr               		; interrupts off
    move.w  d4,(a4)
    move.w  d5,(a5)
    move.w  d6,(a4)
    move.w  d7,sr                   		; interrupts back on again
    bsr     pause10
	
	;
	; reset finished
	;

.dontreset1:

    move.l $f17000,d0 ; GPIO3    
    nop                     				; wait for it to settle down
    nop
    nop

    move.l  NVM_Magic,d0            		; save the old magic number for the cart
    move.l  d0,cart_magic

;
; figure out which type of NVRAM cart is plugged in
;
; first, try asking flash rom for product identification

    move.w  #$00aa,d4
    move.w  #$0055,d5
    move.w  #$0090,d6
    lea     $800000+(4*$5555),a4
    lea     $800000+(4*$2aaa),a5
    move.w  sr,d7
    or.w    #$0700,sr               		; interrupts off
    move.w  d4,(a4)
    move.w  d5,(a5)
    move.w  d6,(a4)
    move.w  d7,sr                   		; interrupts back on again
    bsr     pause10

    move.l  $800000,d2  					; read manufacturer code from the device
    swap    d2      						; get data into low word
    and.w   #$00FF,d2   					; mask out all but the byte we read
    move.b  d2,manu_id

    move.l  $800004,d3  					; read device id
    swap    d3      						; get data into low word
    and.w   #$00FF,d3   					; mask out all but the byte we read
    move.b  d3,prod_id
;
; reset the ROM
;
    move.w  #$00aa,d4
    move.w  #$0055,d5
    move.w  #$00f0,d6
    lea     $800000+(4*$5555),a4
    lea     $800000+(4*$2aaa),a5

    move.w  sr,d7
    or.w    #$0700,sr               		; interrupts off
    move.w  d4,(a4)
    move.w  d5,(a5)
    move.w  d6,(a4)
    move.w  d7,sr                   		; interrupts back on again
    bsr     pause10
    
    cmp.b   #$01,d2     					; AMD manufacturer ID == 01
    bne.b   .notAMD
    cmp.b   #$20,d3     					; check for device == AM29F010
    beq.s   .isAMD29F010
    cmp.b   #$AD,d3     					; check for device == AM29F016
    bne     unknown_device

.isAMD29F016:
    move.l  #nvmmyamd,a0
    move.l  #nvmmyamd_end,d0
    bra loadbios

.isAMD29F010:
    move.l  #nvmamd,a0
    move.l  #nvmamd_end,d0
    bra loadbios

.notAMD:
    cmp.b   #$C2,d2     					; Macronix manufacturer ID == $C2
    bne.b   .notMacronix
    cmp.b   #$AD,d3     					; check for device == MX29F016
    bne     unknown_device

.isMX29F016:
    move.l  #nvmmyamd,a0
    move.l  #nvmmyamd_end,d0
    bra loadbios


.notMacronix:
    cmp.b   #$1f,d2     					; ATMEL manufacturer ID == $1f
    bne.b   .notATMEL
    cmp.b   #$d5,d3     					; check for device == AT29C010
    bne     unknown_device

    move.l  #nvmat,a0
    move.l  #nvmat_end,d0
    bra loadbios

.notATMEL:
;
; next, check for ROMULATOR
;
;; ROMULATOR 0 SRAM of the Alpine -->  Check if one 
;; of the written comand-bytes is still there,
;; a FLASH-device would return something different.
;; 

    move.w  $800000+(4*$2aaa),d0
    cmp.w   #$0055,d0
    bne unknown_device
    move.l  #nvmrom,a0
    move.l  #nvmrom_end,d0
    bra loadbios
unknown_device:

    move.l  #nvmnone,a0
    move.l  #nvmnone_end,d0

    lea $2400,a1

; copy the BIOS over

.copy:
    move.l  (a0)+,(a1)+
    cmp.l   a0,d0
    bge.b   .copy

; now fall through to the program manager
;
domanager:
    rts

loadbios:
	lea	$2400,a1
	jsr	d_lz77
	rts

d_lz77:
		addq.l	#4,a0		; Skip original length
		bra.s	.loadtag

.literal:
		rept	8
		move.b	(a0)+,(a1)+     ; Copy 8 bytes literal string
		endr
         
.loadtag:
		move.b	(a0)+,d0	; Load compression TAG
		beq.s	.literal	; 8 bytes literal string?


		moveq.l	#8-1,d1         ; Process TAG per byte/string
.search:
		add.b	d0,d0		; TAG <<= 1
		bcs.s	.compressed

		move.b  (a0)+,(a1)+     ; Copy another literal byte
		dbra	d1,.search

		bra.s	.loadtag

.compressed:
		moveq.l	#0,d2
		move.b  (a0)+,d2        ; Load compression specifier
		beq.s	.break		; End of stream, exit

		moveq.l	#$0f,d3		; Mask out stringlength
		and.l	d2,d3

		lsl.w	#4,d2		; Compute string location
		move.b	(a0)+,d2
		movea.l	a1,a2
		suba.l	d2,a2


		add.w	d3,d3		; Jump into unrolled string copy loop
		neg.w	d3
		jmp     .unroll(pc,d3.w)

		rept	15
		move.b	(a2)+,(a1)+
		endr
.unroll:
		move.b	(a2)+,(a1)+
		move.b	(a2)+,(a1)+

		dbra	d1,.search

		bra.s	.loadtag

.break:
		rts

	
;#####################################################################


    ; pause 10 mS: on Jaguar I there are 13000 cycles/mS; let's be safe and use
    ; Jaguar II's 20000 cycles/mS, so pause for 200,000 cycles
    ; a divs instruction takes 122 cycles, so execute 1640 of them
pause10:
    movem.l d0-d1,-(sp)
    move.w  #409,d0     ; 4 divides per loop
    moveq   #1,d1
.delay:
    divs    d1,d1
    divs    d1,d1
    divs    d1,d1
    divs    d1,d1
    dbra    d0,.delay
    movem.l (sp)+,d0-d1
    rts

;
; the actual BIOSes come here
;
; ROMULATOR BIOS
nvmrom:
    .incbin "mt/nvmrom.lz"
nvmrom_end:

; AMD BIOS
    .long
nvmamd:
    .incbin "mt/nvmamd.lz"
nvmamd_end:

; my AMD BIOS for 29F016 chips
; is the same as my Macronix BIOS for 29F016 chips
    .long
nvmmyamd:
nvmmymacronix:
    .incbin "mt/nvmmyamd.lz"
nvmmyamd_end:
nvmmymacronix_end:


; ATMEL BIOS
    .long
nvmat:
    .incbin "mt/nvmat.lz"
nvmat_end:

; UNKNOWN DEVICE BIOS
    .long
nvmnone:
    dc.l    '_NVM'
    moveq.l #-1,d0
    rts
nvmnone_end:

gorffound:	dc.l	0
			.even
			
		; 15 chars  '012345678901234'
		
gorf_name:	dc.b	'3DSTOOGES',0
			.even
app_name:   dc.b    'DOWNFALL',0
			.even
file_name:	dc.b	'FIBLOCKER',0
			.even
mt_present:	dc.l	0	;-1=MT not present

    even
work_area:
	dc.l	0
cart_magic:
    dc.l    0       ; old contents of $2400
filehandle:
	dc.l	0
    even
manu_id:
    dc.b    0       ; manufacturer id ($01 = AMD, $1F = Atmel, $C2 = Macronix)
prod_id:
    dc.b    0       ; product id ($AD = 29F016, $D5 = 29C010)
    even
				.dphrase
mt_buffer:		dc.l	0,0,0,0,0,0
	