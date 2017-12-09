; C64 User's Guide pp. 65-66 - More bouncing balls
;

; *** labels ***

fscadr          = $e9f0
clrscr          = $e544
xcoord          = $fb
ycoord          = $fc 
dirflags        = $fd 
extcol          = $d020
bgcol0          = $d021                 

; *** macros ***

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

                *=$0334         ; sys820
                lda #$00
                sta dirflags
                sta xcoord
                sta ycoord

                lda #$07
                sta extcol
                lda #$0d
                sta bgcol0
                jsr clrscr

mainloop
                ldx ycoord
                jsr fscadr
                lda char
                ldy xcoord
                sta ($d1),y
                #busywait $20
                lda sp
                sta ($d1),y
                
                bit dirflags
                bmi decxc
                inc xcoord
                lda xcoord
                cmp #$27
                bne checky
                jmp invertx
decxc
                dec xcoord
                beq invertx                

checky
                bit dirflags
                bvs decyc
                inc ycoord
                lda ycoord
                cmp #$18
                bne continue
                jmp inverty
decyc
                dec ycoord
                beq inverty

continue                
                jmp mainloop

invertx         
                lda #%10000000
                eor dirflags
                sta dirflags
                jmp checky

inverty         
                lda #%01000000
                eor dirflags
                sta dirflags
                jmp mainloop                

char            .byte $51
sp              .byte $20