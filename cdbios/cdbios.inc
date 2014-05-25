;**************************************************************************
; (C)1993 ATARI CORP.       SECRET & CONFIDENTIAL       ALL RIGHTS RESERVED
;
;
;                                CD_inits.inc
;
;       This include file contains all of the equates needed for the CDROM
;	functions.
;
;                                REVISION HISTORY
;
; REV.  DATE       BY            DESCRIPTION OF EDIT
; """"  """"       ""            """""""""""""""""""
; 1.00  14 Jun 93  DMS   Original. Equates taken from DATA.213
; 2.00  25 Jun 93  DMS   Add more variables for full DSA Bus testing
; 3.00  19 Jul 93  DMS   Add Butch's registers. move variable mem up.
; 4.00  26 Jul 93  DMS   Add more CD variables
; 5.00  19 Aug 93  DMS   Fix half-buff pointer
; 6.01  17 Sep 93  DMS   Modify for 2 MB Sylvester (Rapier Rev 2 PCB)
; 6.02  20 Sep 93  DMS   For CD.s demos and Programmer's Interface doc
; 6.03  01 Oct 93  DMS   Add CD_retry
;****************************************************************************

				; Butch's hardware registers
BUTCH     equ  $DFFF00		; base of Butch=interrupt control register, R/W
DSCNTRL   equ  BUTCH+4		; DSA control register, R/W
DS_DATA   equ  BUTCH+$A		; DSA TX/RX data, R/W
I2CNTRL   equ  BUTCH+$10	; i2s bus control register, R/W
SBCNTRL   equ  BUTCH+$14	; CD subcode control register, R/W
SUBDATA   equ  BUTCH+$18	; Subcode data register A
SUBDATB   equ  BUTCH+$1C	; Subcode data register B
SB_TIME   equ  BUTCH+$20	; Subcode time and compare enable (D24)
FIFO_DATA equ  BUTCH+$24	; i2s FIFO data
I2SDAT1   equ  BUTCH+$24	; i2s FIFO data
I2SDAT2   equ  BUTCH+$28	; i2s FIFO data

				; Butch's registers shadow in DRAM
