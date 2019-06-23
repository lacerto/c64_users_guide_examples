; Test how a sprite looks on the screen with it's expanded variant.

; *** labels ***

clrscr          = $e544         ; initializes and clears the screen, puts cursor into home position
getin           = $ffe4         ; get one byte from the input device

; sprite registers
sprdbp          = $07f8         ; sprite #0 data block pointer (default after start)
spena           = $d015         ; sprite enable register
sp0x            = $d000         ; sprite 0 x position
sp0y            = $d001         ; sprite 0 y position
sp1x            = $d002         ; sprite 1 x position
sp1y            = $d003         ; sprite 1 y position
msigx           = $d010         ; most significant bits of sprites 0-7 x position
xxpand          = $d01d         ; sprite horizontal expansion register
yxpand          = $d017         ; sprite vertical expansion register

sprb13          = 13*64         ; sprite block #13 = 13*64 = 832 = $0340 ($33C-$3FB TBUFFER)

; *** main ***

                *=$0801
                ; 1 REM SPRITE TEST PRG
                ; 2019 SYS2085
; $0801                
                .word $0819     ; address of next basic line
                .word $0001     ; line number
                .byte $8f       ; rem token
                .text " sprite test prg  "
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
copydata        lda spritedata,x
                sta sprb13,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata

                ; set data pointer to the sprite block
                ldx #$00        ; sprite #0
                lda #$0d        ; sprite block #13
                sta sprdbp,x
                ldx #$01        ; sprite #1
                sta sprdbp,x    ; also block #13

                ; enable sprites #0 and #1
                lda #%00000011
                sta spena

                ; expand sprite #1 in x and y direction
                lda #%00000010
                sta xxpand
                sta yxpand

                ; set sprite coordinates
                lda #140
                sta sp0y

                lda #129
                sta sp1y

                lda #106
                sta sp0x

                lda #213
                sta sp1x

                ; wait for a key
inputloop       jsr getin
                beq inputloop

                ; disable all sprites
                lda #$00
                sta spena

                rts                

; *** data ***

                ; paste here the sprite data to test (63 bytes)
spritedata      
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $03, $e0, $00
                .byte $07, $f3, $e0
                .byte $0f, $ff, $f0
                .byte $0f, $ff, $f8
                .byte $3e, $f7, $f8
                .byte $7d, $6b, $fc
                .byte $7f, $ff, $fe
                .byte $ff, $ff, $ff
                .byte $ff, $6f, $ff
                .byte $7f, $9f, $fe
                .byte $1f, $ff, $f8
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
