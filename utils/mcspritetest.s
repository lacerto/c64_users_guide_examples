; Test multicolor sprite.

; *** labels ***

clrscr          = $e544         ; initializes and clears the screen, puts cursor into home position
getin           = $ffe4         ; get one byte from the input device

; sprite registers
sprdbp          = $07f8         ; sprite #0 data block pointer (default after start)
spena           = $d015         ; sprite enable register
sp0x            = $d000         ; sprite 0 x position
sp0y            = $d001         ; sprite 0 y position
msigx           = $d010         ; most significant bits of sprites 0-7 x position
yxpand          = $d017         ; sprite vertical expansion register
spmc            = $d01c         ; sprite multicolor register
xxpand          = $d01d         ; sprite horizontal expansion register
extcol          = $d020         ; border color register
bgcol0          = $d021         ; background color 0
spmc0           = $d025         ; sprite multicolor register 0
spmc1           = $d026         ; sprite multicolor register 1
sp0col          = $d027         ; sprite 0 color register

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
                ldx #$00
                stx extcol
                stx bgcol0
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

                ; reset expand
                lda #%00000000
                sta xxpand
                sta yxpand

                ; set sprite 0 multicolor mode
                lda #%00000001
                sta spmc

                ; set colors
                lda #$08
                sta spmc0
                lda #$06
                sta spmc1
                lda #$03
                sta sp0col

                ; set sprite coordinates
                lda #140
                sta sp0y

                lda #106
                sta sp0x

                ; enable sprite #0 
                lda #%00000001
                sta spena

                ; wait for a key
inputloop0      jsr getin
                beq inputloop0

                ; expand sprite #0 in x and y direction
                lda #%00000001
                sta xxpand
                sta yxpand

                ; wait for a key
inputloop1      jsr getin
                beq inputloop1

                ; disable all sprites
                lda #$00
                sta spena

                rts                

; *** data ***

                ; paste here the sprite data to test (63 bytes)
spritedata      
                .byte $00,$0a,$00,$00,$20,$80,$00,$80
                .byte $20,$02,$00,$08,$08,$05,$02,$08
                .byte $10,$42,$08,$10,$42,$08,$10,$42
                .byte $08,$15,$42,$08,$10,$42,$02,$00
                .byte $08,$00,$80,$20,$00,$e0,$80,$03
                .byte $fa,$00,$0f,$f0,$00,$0f,$c0,$00
                .byte $3f,$00,$00,$3f,$00,$00,$fc,$00
                .byte $00,$f0,$00,$00,$f0,$00,$00
