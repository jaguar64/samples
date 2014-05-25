;---------------------------------------------------------------------------------------
;
; ---+ Embrace the plasma +---
;
;  Sample: "Embrace the night" by Day-Mar
;
;  System: Atari Jaguar
;
;
; a little example on how to do audio, screen buffer and DSP stuff on Atari Jaguar
;
;
; released on the 20th of April 2014
;
; (w)2013/2014 Carsten'lsl'Koeckritz - lsl@quantentunnel.de
; http://checkpoint.atari.org
;---------------------------------------------------------------------------------------
; some notes:
; - Simple Jerry sample-replayer for Atari Jaguar (46KHz)
; - standard Plasma routine running on TOM using double-buffering (320x256, 3vbls)
;
; Based on DOWNFALL.S by ReBoot
; _*.S by ReBoot
; Jerry code based on Sinister Development's modplayer
;---------------------------------------------------------------------------------------

#embrace_the_plasma.abs	... Jaguar executable

TEST.S					... main source
SAMPLE_REPLAY_JERRY.S	... Jerry sample replay

The other sources are service routines.


make.bat				... assemble source,link & copy binary to Virtual Jaguar folder
m_jcp.bat				... send binary to Jaguar using jcp.exe

rmac.exe				... 68000/DSP assembler
rln.exe					... linker
jcp.exe					... transfer program

;---------------------------------------------------------------------------------------
enjoy!
