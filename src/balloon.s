; C64 User's Guide p. 71 - Up, up, and away!
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
msigx           = $d010         ; most significant bits of sprites 0-7 x position

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

                ; copy sprite data to block #13 = 13*64 = 832 = $0340 ($33C-$3FB TBUFFER)
                ldx #$00
copydata        lda balloon,x
                sta $0340,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata

                ; set data pointer to the sprite block
                ldx #$02        ; sprite #2
                lda #$0d        ; sprite block #13
                sta sprdbp,x

                ; enable sprite #2
                lda #%00000100
                sta spena

                ; move sprite
movesprite                
                ldx #$00
moveloop                
                stx sp2x        ; update sprite #2 x coordinate
                stx sp2y        ; update sprite #2 y coordinate
                #busywait waitcount ; delay a bit
                inx
                cpx #201
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
