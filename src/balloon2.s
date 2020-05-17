; C64 User's Guide p. 74 - Up, up, and away!
; Version 2 with multiple sprites and sprite expansion
;
; Sets sprite #2 to a balloon shape and moves it diagonally on the screen
; in an endless loop. Exit using RUN/STOP+RESTORE.

; *** labels ***

clrscr          = $e544         ; initializes and clears the screen, puts cursor into home position
hibase          = $0288         ; top page of screen memory
freezp          = $fb           ; free zero page bytes

; sprite registers
sprdbp          = $07f8         ; sprite #0 data block pointer (default after start)
spena           = $d015         ; sprite enable register
sp2col          = $d029         ; sprite 2 color register
sp2x            = $d004         ; sprite 2 x position
sp2y            = $d005         ; sprite 2 y position
sp3x            = $d006         ; sprite 3 x position
sp3y            = $d007         ; sprite 3 y position
sp4x            = $d008         ; sprite 4 x position
sp4y            = $d009         ; sprite 4 y position
msigx           = $d010         ; most significant bits of sprites 0-7 x position
xxpand          = $d01d         ; sprite horizontal expansion register
yxpand          = $d017         ; sprite vertical expansion register

sprb13          = 13*64         ; sprite block #13 = 13*64 = 832 = $0340 ($33C-$3FB TBUFFER)
waitcount       = $0a           ; count for the busy wait loop

; *** macros ***

; name:         busywait
; description:  a small busywait routine using nested loops
;               the length of the outer loop is configurable
;               preserves all register values
; input:        \1 - length of the outer loop
; output:       -
busywait .macro
                ; Save regs on the stack
                pha
                tya
                pha
                txa
                pha

                ; Wait
                ldy #\1
                ldx #$00
a00
                dex
                bne a00
                dey
                bne a00

                ; Restore regs
                pla
                tax
                pla
                tay
                pla
.endm

; *** main ***

                *=$0801
                ; 1 REM UP, UP, AND AWAY!
                ; 2019 SYS2085
; $0801                
                .word $0819     ; address of next basic line
                .word $0001     ; line number
                .byte $8f       ; rem token
                .text " up, up, and away!"
                .byte $00       ; end of basic line
; $0819                
                .word $0823     ; address of next basic line
                .word $07e3     ; line number
                .byte $9e       ; sys token
                .text "2085"    ; address as string
                .byte $00       ; end of basic line
                .word $0000     ; address of next basic line; $0000 -> end of basic program

                *=$0825         ; sys2085
                jsr clrscr

                ; copy sprite data to block #13
                ldx #$00
copydata        lda balloon,x
                sta sprb13,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata

                ; set data pointer to the sprite block
                ldx #$02        ; sprite #2
                lda #$0d        ; sprite block #13
                sta sprdbp,x
                ldx #$03        ; sprite #3
                sta sprdbp,x    ; also block #13
                ldx #$04        ; sprite #4
                sta sprdbp,x    ; also block #13

                ; enable sprites #2 and #3
                lda #%00011100
                sta spena

                ; expand sprites #2 and #3 in x and y direction
                lda #%00001100
                sta xxpand
                sta yxpand

                ; set sprite #4 y coordinate to 100
                lda #100
                sta sp4y
                
                ; move sprite
movesprite                
                ldx #$00
moveloop                
                stx sp2x        ; update sprite #2 x coordinate
                stx sp3x        ; update sprite #3 x coordinate
                stx sp4x        ; update sprite #4 x coordinate
                stx sp2y        ; update sprite #2 y coordinate                
                stx freezp
                lda #190                
                sec
                sbc freezp
                sta sp3y        ; update sprite #3 y coordinate

                #busywait waitcount ; delay a bit
                inx
                cpx #191
                bne moveloop

                jmp movesprite  ; endless loop just like in the BASIC example

; *** data ***

                ; balloon sprite data
balloon         .byte $00, $7f, $00
                .byte $01, $ff, $c0
                .byte $03, $ff, $e0
                .byte $03, $e7, $e0
                .byte $07, $d9, $f0
                .byte $07, $df, $f0
                .byte $07, $d9, $f0
                .byte $03, $e7, $e0
                .byte $03, $ff, $e0
                .byte $03, $ff, $e0
                .byte $02, $ff, $a0
                .byte $01, $7f, $40
                .byte $01, $3e, $40
                .byte $00, $9c, $80
                .byte $00, $9c, $80
                .byte $00, $49, $00
                .byte $00, $49, $00
                .byte $00, $3e, $00
                .byte $00, $3e, $00
                .byte $00, $3e, $00
                .byte $00, $1c, $00
