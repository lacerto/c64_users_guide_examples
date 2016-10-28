; C64 User's Guide p. 45 - Input
;
; Gets input from the user and prints it.

inlin  = $a560
strout = $ab1e
prtqm  = $ab45 ; print "?"
prtspc = $ab3b ; print space
;prtchr = $ab47 ; print char in a

    *=$02a8         ; sys680
    
    jsr prtqm       ; print question mark
    jsr prtspc      ; print space
        
    jsr inlin       ; get a line into the basic input buffer at $0200    
    
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
clearbuf    
    ldy $0200,x     ; check the current byte
    beq end         ; 0? then we have reached the end
    sta $0200,x     ; as the buffer contained a 0 terminated string
    inx
    jmp clearbuf
end
    rts
    
answer
    .null "you typed: "
