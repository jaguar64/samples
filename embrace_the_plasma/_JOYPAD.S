
dpad_left:      .dc.w   0
dpad_right:     .dc.w   0
dpad_up:        .dc.w   0
dpad_down:      .dc.w   0

dpad2_b:		.dc.w	0

dpad_b:         .dc.w   0
dpad_a:         .dc.w   0
dpad_c:			.dc.w	0

dpad_number:	.dc.w	0

PAUSE_ON:       .dc.w   0
pawslock:       .dc.w   0

exit_flag:      .dc.w   0

;================================================================================================
handle_input:
		move.l	joycur+4,d0
        btst.l  #FIRE_B,d0
        beq.b   .4
        move.w  #1,dpad2_b
        bra     pad2_done
.4:		move.w  #0,dpad2_b		
pad2_done:

        move.l  joycur,d0
ck_up:
        btst.l  #JOY_UP,d0
        beq.b   ck_down
        move.w #1,dpad_up
        move.w #0,dpad_down
        bra.b   ck_left
ck_down:
        move.w #0,dpad_up
        move.w #0,dpad_down
        btst.l  #JOY_DOWN,d0
        beq.b   ck_left
        move.w #1,dpad_down
ck_left:
        btst.l  #JOY_LEFT,d0
        beq.b   .1
        move.w  #1,dpad_left
        bra     ck_right
.1:
		move.w  #0,dpad_left
ck_right:
        btst.l  #JOY_RIGHT,d0
        beq.b   .2
        move.w  #1,dpad_right
        bra.b   ck_fireA
.2:
        move.w  #0,dpad_right

ck_fireA:
        btst.l  #FIRE_A,d0
        beq.b   .3
        move.w  #1,dpad_a
        bra     ck_fireB
.3:
        move.w  #0,dpad_a
ck_fireB:
        btst.l  #FIRE_B,d0
        beq.b   .4
        move.w  #1,dpad_b
        bra     ck_fireC
.4:
        move.w  #0,dpad_b
ck_fireC:
        btst.l  #FIRE_C,d0
        beq.b   .5
        move.w  #1,dpad_c
        bra     ck_Option
.5:
        move.w  #0,dpad_c

ck_Option:
        btst.l  #OPTION,d0
        beq.b   ck_Pause
        nop
ck_Pause:
        btst.l  #PAUSE,d0
        beq.b   .6
        tst.w   pawslock
        bne.s   fire_done
        eor.w   #1,PAUSE_ON
        move.w  #1,pawslock
        bra     fire_done
.6:
        move.w  #0,pawslock
fire_done:
        and.l   #ANY_FIRE,d0

;------------------------------------------------------------------------------------------------
ck_keypad:
;--------------------------------------------------------------
;  Evaluate each 12-key button from the keypad and execute
;  code independent to each button.
;--------------------------------------------------------------
;       lea     keys1,a1

		move.w	#-1,dpad_number

        move.l  joycur,d0               ; necessary to perform following modulo 32 BTST
ck_key1:
        btst.l  #KEY_1,d0
        beq.b   ck_key2
        move.w	#1,dpad_number
ck_key2:
        btst.l  #KEY_2,d0
        beq.b   ck_key3
        move.w	#2,dpad_number
ck_key3:
        btst.l  #KEY_3,d0
        beq.b   ck_key4
        move.w	#3,dpad_number
ck_key4:
        btst.l  #KEY_4,d0
        beq.b   ck_key5
        move.w	#4,dpad_number
ck_key5:
        btst.l  #KEY_5,d0
        beq.b   ck_key6
        move.w	#5,dpad_number
ck_key6:
        btst.l  #KEY_6,d0
        beq.b   ck_key7
        move.w	#6,dpad_number
ck_key7:
        btst.l  #KEY_7,d0
        beq.b   ck_key8
        move.w	#7,dpad_number
ck_key8:
        btst.l  #KEY_8,d0
        beq.b   ck_key9
        move.w	#8,dpad_number
ck_key9:
        btst.l  #KEY_9,d0
        beq.b   ck_keyS
        move.w	#9,dpad_number
ck_keyS:
        btst.l  #KEY_STAR,d0
        beq.b   ck_key0
        btst.l  #KEY_HASH,d0
        beq.b   ck_key0
        move.w  #1,exit_flag
ck_key0:
        btst.l  #KEY_0,d0
        beq.b   ck_keyH
        move.w	#0,dpad_number
ck_keyH:
        btst.l  #KEY_HASH,d0
        beq.b   keypad_done
        nop
keypad_done:
        and.l   #ANY_KEY,d0
        bne     pad_all_done
        nop

pad_all_done:
        rts

;================================================================================================
readpad:
;scan for player 1
        move.l  #$f0fffffc,d1           ; d1 = Joypad data mask
        moveq.l #-1,d2                  ; d2 = Cumulative joypad reading

        move.w  #$81fe,JOYSTICK
        move.l  JOYSTICK,d0             ; Read joypad,pause button,A button
        or.l    d1,d0                   ; Mask off unused bits
        ror.l   #4,d0
        and.l   d0,d2                   ; d2 = xxAPxxxx RLDUxxxx xxxxxxxx xxxxxxxx
        move.w  #$81fd,JOYSTICK
        move.l  JOYSTICK,d0             ; Read *741 keys,B button
        or.l    d1,d0                   ; Mask off unused bits
        ror.l   #8,d0
        and.l   d0,d2                   ; d2 = xxAPxxBx RLDU741* xxxxxxxx xxxxxxxx
        move.w  #$81fb,JOYSTICK
        move.l  JOYSTICK,d0             ; Read 2580 keys,C button
        or.l    d1,d0                   ; Mask off unused bits
        rol.l   #6,d0
        rol.l   #6,d0
        and.l   d0,d2                   ; d2 = xxAPxxBx RLDU741* xxCxxxxx 2580xxxx
        move.w  #$81f7,JOYSTICK
        move.l  JOYSTICK,d0             ; Read 369# keys,Option button
        or.l    d1,d0                   ; Mask off unused bits
        rol.l   #8,d0
        and.l   d0,d2                   ; d2 = xxAPxxBx RLDU741* xxCxxxOx 2580369# <== inputs active low

        moveq.l #-1,d1
        eor.l   d2,d1                   ; d1 = xxAPxxBx RLDU741* xxCxxxOx 2580369# <== now inputs active high

        move.l  joycur,d0               ; old joycur needed for determining the new joyedge
        move.l  joycur,d2               ; old joycur needed for determining the new joyedge
        and.l   #$00f00000,d2
        move.l  d1,joycur               ; Current joypad reading stored into joycur
        eor.l   d1,d0
        and.l   d1,d0
        or.l    d2,d0
        move.l  d0,joyedge              ;joypad,buttons,keys that were just pressed

;scan for player 2
        move.l  #$0ffffff3,d1           ; d1 = Joypad data mask
        moveq.l #-1,d2                  ; d2 = Cumulative joypad reading

        move.w  #$817f,JOYSTICK
        move.l  JOYSTICK,d0             ; Read joypad,pause button,A button
        or.l    d1,d0                   ; Mask off unused bits
        rol.b   #2,d0                   ; note the size of rol
        ror.l   #8,d0
        and.l   d0,d2                   ; d2 = xxAPxxxx RLDUxxxx xxxxxxxx xxxxxxxx
        move.w  #$81bf,JOYSTICK
        move.l  JOYSTICK,d0             ; Read *741 keys,B button
        or.l    d1,d0                   ; Mask off unused bits
        rol.b   #2,d0                   ; note the size of rol
        ror.l   #8,d0
        ror.l   #4,d0
        and.l   d0,d2                   ; d2 = xxAPxxBx RLDU741* xxxxxxxx xxxxxxxx
        move.w  #$81df,JOYSTICK
        move.l  JOYSTICK,d0             ; Read 2580 keys,C button
        or.l    d1,d0                   ; Mask off unused bits
        rol.b   #2,d0                   ; note the size of rol
        rol.l   #8,d0
        and.l   d0,d2                   ; d2 = xxAPxxBx RLDU741* xxCxxxxx 2580xxxx
        move.w  #$81ef,JOYSTICK
        move.l  JOYSTICK,d0             ; Read 369# keys,Option button
        or.l    d1,d0                   ; Mask off unused bits
        rol.b   #2,d0                   ; note the size of rol
        rol.l   #4,d0
        and.l   d0,d2                   ; d2 = xxAPxxBx RLDU741* xxCxxxOx 2580369# <== inputs active low

        moveq.l #-1,d1
        eor.l   d2,d1                   ; d1 = xxAPxxBx RLDU741* xxCxxxOx 2580369# <== now inputs active high

        move.l  joycur+4,d0             ; old joycur needed for determining the new joyedge
        move.l  joycur+4,d2             ; old joycur needed for determining the new joyedge
        and.l   #$00f00000,d2
        move.l  d1,joycur+4             ; Current joypad reading stored into joycur
        eor.l   d1,d0
        and.l   d1,d0
        or.l    d2,d0
        move.l  d0,joyedge+4            ;joypad,buttons,keys that were just pressed

        rts


joyedge:         dc.l            0,0
joycur:          dc.l            0,0

