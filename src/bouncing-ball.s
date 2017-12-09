; C64 User's Guide p. 65 - More bouncing balls
;
; Changes the border and background color and
; displays a moving "ball" on the screen that
; bounces back each time it hits the border.
; The ball is animated by directly writing its
; screen code to the screen memory.

; *** labels ***

fscadr          = $e9f0         ; puts the address of the start of screen line x to $d1/$d2 (pnt)
screen          = $e505         ; returns screen size in x and y
clrscr          = $e544         ; initializes and clears the screen, puts cursor into home position
pnt             = $d1           ; $d1/$d2: address of the start of a screen line
xcoord          = $fb           ; x coordinate of the ball
ycoord          = $fc           ; y coordinate of the ball
dirflags        = $fd           ; bit 7 and bit 6 store the direction flags of x and y coords respectively
extcol          = $d020         ; border color vic reg
bgcol0          = $d021         ; background color vic reg

char            = $51           ; the ball's screen code
sp              = $20           ; space
waitcount       = $20           ; count for the busy wait loop
bordercol       = $07           ; border color (7 = yellow)
bgcolor         = $0d           ; background color (13 = light green)         

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

                *=$0334         ; sys820
                lda #$00        ; initialize
                sta dirflags    ;   the direction flags
                sta xcoord      ;   and the coordinates of the ball
                sta ycoord      ;   it starts in the upper left corner (0,0)

                jsr screen      ; get the screen size and store it (returns 40,25 on C64)
                dex             ; decrement the values by 1 as we use a
                stx xsize       ;   0 based coordinate system
                dey
                sty ysize

                lda #bordercol  ; set the border and background colors
                sta extcol
                lda #bgcolor
                sta bgcol0
                jsr clrscr      ; clear the screen

mainloop
                ldx ycoord      ; load the y coordinate of the ball
                jsr fscadr      ; get the start address of line ycoord in pnt
                                ; the low bytes for the start of each screen line
                                ;   are stored in a rom table
                                ; the high bytes are calculated using a ram table
                                ;   and they take the current base address of the screen
                                ;   memory into account
                                ; meaning that this routine should function even
                                ;   if the screen memory is relocated
                lda #char       ; get the character screen code we want to display (ball)
                ldy xcoord      ; load the x coordinate of the ball
                sta (pnt),y     ; as pnt contains the start address of the screen line
                                ;   we only have to use the x coordinate as an index
                #busywait waitcount
                lda #sp         ; delete the ball, overwrite it with a space
                sta (pnt),y
                
                bit dirflags    ; check the direction flag
                                ; the bit command copies bits 6 and 7 to bits
                                ;   6 (overflow flag) and 7 (negative flag) of the
                                ;   status register                                
                bmi decxc       ; if the negative flag is high then decrement the x coordinate
                inc xcoord      ; otherwise increment it
                lda xcoord
                cmp xsize       ; reached the rightmost column?
                bne checky      ;   no  -> go on with checking the y coordinate
invertx         
                lda #%10000000  ;   yes -> invert the x direction flag
                eor dirflags
                sta dirflags
                jmp checky      ; go on with checking the y coordinate                                
decxc
                dec xcoord      ; x--
                beq invertx     ; reached 0? -> invert the direction flag      

checky
                bit dirflags    ; check the direction flag
                bvs decyc       ; if the overflow flag is high then decrement the y coordinate
                inc ycoord      ; otherwise increment it
                lda ycoord
                cmp ysize       ; reached the bottom screen line?
                bne continue    ;   no  -> continue the main loop
inverty         
                lda #%01000000  ;   yes -> invert the y direction flag
                eor dirflags
                sta dirflags
                jmp mainloop    ; continue the main loop                
decyc
                dec ycoord      ; y--
                beq inverty     ; reached 0? -> invert the direction flag

continue                
                jmp mainloop    ; loop forever

; *** data ***

xsize           .byte $00       ; max number of screen columns
ysize           .byte $00       ; max number of screen lines