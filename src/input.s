; C64 User's Guide p. 45 - Input
;
; Gets input from the user and prints it.

; *** labels ***

inlin  = $a560
strout = $ab1e
prtqm  = $ab45 ; print "?"
prtspc = $ab3b ; print space


; *** main ***

                *=$02a8         ; sys 680
                                ; $02a7-$02ff is unused
                                ; these free bytes are a perfect fit for such
                                ; a short routine
                
                jsr prtqm       ; print question mark
                jsr prtspc      ; print space
                    
                jsr inlin       ; get a line into the basic input buffer @ $0200    
                
                lda #<answer    ; print the answer text
                ldy #>answer
                jsr strout
                
                lda #$00        ; buffer addr. low byte
                ldy #$02        ; buffer addr. high byte
                jsr strout      ; print what the user typed
                
                ; clear the buffer so that basic does not
                ; start to interpret it and throw a syntax error
                ldx #$00
                lda #$00        
clearbuf        ldy $0200,x     ; check the current byte
                beq end         ; 0? then we have reached the end
                sta $0200,x     ; as the buffer contained a 0 terminated string
                inx
                jmp clearbuf
end             rts


; *** data ***

answer
    .null "you typed: "
