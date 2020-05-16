; View Chars
;
; Switch to VIC Bank #3, copy CHARGEN to RAM and show the
; characters in it.

; *** labels ***
hibase          = $288          ; top page for screen memory
ci1icr          = $dc0d         ; CIA1 interrupt control register
ci1cra          = $dc0e         ; CIA1 control register A
ci2pra          = $dd00         ; CIA2 data port register A
vmcsb           = $d018         ; VIC memory control register
linprt          = $bdcd
clrscr          = $e544
clrlin          = $e9ff 
chrout          = $ffd2
getin           = $ffe4
plot            = $e50a

; *** main ***

                *=$0801
                ; 1 REM VIEW CHARGEN
                .word $0814     ; address of next line
                .word $0001     ; line number (1)
                .byte $8f       ; rem token
                .text " view chargen"
                .byte $00       ; end of line

                ; 2020 SYS2090
                .word $081e     ; address of next line
                .word $07e4     ; line number (2020)
                .byte $9e       ; sys token
                .text "2080"    ; address of ML program ($0820)
                .byte $00       ; end of line

                ; end of BASIC program
                .word $0000

                *=$0820
                jsr switchbank
                jsr clrscr

loop
                lda charidx
                ldx charidx+1
                jsr getchardata

                clc
                ldx #$00
                ldy #$00
                jsr plot
                jsr showchar

                ldx #$08
                jsr clrlin
                
                ldx charidx
                lda charidx+1
                jsr linprt

                jsr getkey
                bmi end
                jmp loop
end
                rts

; *** subroutines ***

getkey
.block
                jsr getin       ; get a char from the keyboard
                cmp #$44        ; D
                beq right
                cmp #$41        ; A
                beq left
                cmp #$57        ; W
                beq up
                cmp #$53        ; S
                beq down
                cmp #$1d        ; right
                beq right
                cmp #$9d        ; left
                beq left
                cmp #$91        ; up
                beq up
                cmp #$11        ; down
                beq down
                cmp #$20        ; space
                beq exit
                cmp #$0d        ; return
                beq exit               
                bne getkey

                ; handle key press events
right                       
up              
                clc
                inc charidx
                bne end
                inc charidx+1
                jmp end                
left            
down
                clc
                lda charidx
                bne a00
                dec charidx+1
a00
                dec charidx
                jmp end
end                
                lda #$00
                rts
exit    
                lda #$ff
                rts
.bend

showchar
.block
                ldy #$00
loop                
                lda chardata,y                
                jsr prtline
                iny
                cpy #$08
                bne loop
                rts
.bend

prtline
.block
                ldx #$08
                clc
a00                
                asl
                bcs a01
                pha
                lda #$92
                bne a03
a01             
                pha
                lda #$12
a03                                
                jsr chrout
                lda #$20
                jsr chrout
                pla
                dex
                bne a00
                lda #$0d
                jsr chrout
                rts
           
.bend

; name:         switchbank
; description:  switch to VIC Bank #3
; input:        -
; output:       -
switchbank
.block
                sei

                ; ACK CIA1 interrupts and stop timer A, so that no
                ; IRQ can occur.
                lda #%01111111  ; $7f - acknowledge CIA1 interrupts
                sta ci1icr
                lda ci1cra
                and #%11111110  ; $fe - bit #0=0 stops CIA1 timer A (system timer)
                sta ci1cra

                ; Enable CHARGEN access.
                lda $01
                and #%11111011  ; select character generator ROM at $d000
                sta $01

                ; Copy CHARGEN to RAM.
                jsr copy
                
                ; Turn off CHARGEN access.
                lda $01
                ora #%00000100  ; select I/O instead of chargen ROM
                sta $01

                ; Restart CIA1 timer A.
                lda ci1cra
                ora #%00000001  ; start timer A
                sta ci1cra
                lda #%10000001  ; enable system timer interrupt
                sta ci1icr

                ; The lowest 2 bits of CIA2 Port A select the VIC bank.
                ; 00 -> VIC Bank #3
                lda ci2pra
                and #%11111100  ; bit 0-1 select VIC bank - select bank #3
                sta ci2pra

                ; Set screen ram and CHARGEN addresses.
                lda #%00111100  ; bit 7-4: 0011 -> screen ram offset = 3*1k = $c00
                sta vmcsb       ; bit 3-1: 110 -> 4*2k = $3000 chargen offset
                                ; as we are in bank #3: screen=$cc00; chargen= $f000

                lda #$cc        ; tell the OS that the screen RAM is at page 204 ($cc)
                sta hibase

                cli
                rts
.bend

; name:         copy
; description:  copy the character generator ROM from $d000 to $f000-$ffff
; input:        -
; output:       -
copy
.block
                ; set start addresses
                ; $fb/$fc -> $d000
                ; $fd/$fe -> $f000
                lda #$00
                sta $fb
                sta $fd
                lda #$d0
                sta $fc
                lda #$f0
                sta $fe
                ldx #$10    ; outer loop: run inner loop 16 times (16*256 = 4kB)
a00
                ldy #$00    ; inner loop: copy 256 bytes
a01
                lda ($fb),y
                sta ($fd),y
                dey
                bne a01
                inc $fc
                inc $fe
                dex
                bne a00
                rts
.bend

getchardata
.block
                sei
                stx $fc
                sta $fb
                asl $fb
                rol $fc
                asl $fb
                rol $fc
                asl $fb
                rol $fc
                lda $fc
                clc
                adc #$f0
                sta $fc

                ; switch out KERNAL
                lda $01
                and #%11111101
                sta $01

                ldy #$00
a00                
                lda ($fb),y
                sta chardata,y
                iny
                cpy #$08
                bne a00

                ; switch on KERNAL
                lda $01
                ora #%00000010
                sta $01
                cli            
                rts
.bend

; *** data ***

charidx         .byte $00, $00
chardata        .repeat 8, $00
