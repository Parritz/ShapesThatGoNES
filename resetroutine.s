Reset:

    sei ; Disables all interrupts so this code is not interrupted, my son.
    cld ; disables decimal mode because NES doesn't support decimals, my child.

    ; Disable sound IRQ to make sure values aren't interpreted as sound data and random sounds don't play when we load the program.
    ldx #$40
    stx $4017 ; APU Frame Counter

    ; Initialize the stack register. Shhh, be nice to it. We wouldn't want it to overflow...
    ldx #$FF ; We load FF because the stack register decrements with each addition to stack instead of incrementing like you'd expect
    txs ; Transfer X to Stack Register

    inx ;  #$FF + 1 = #$00

    ; Zero out PPU registers se we don't see random graphics when we load the program
    stx $2000 ; PPU control address
    stx $2001 ; PPU mask address. Stores whether background and sprites are enabled and some other options like that, fam.

    stx $4010 ; This also disables sounds and shiz.
  
    ;We need to check that the console has attempted to draw its first V-Blank. This tells us that it is ready to rumble!
    ; BIT gets the 7th bit from an address and evaluates that value to either branch or continue. The 7th bit signifies whether the value is signed or not
:
    bit $2002
    bpl :- ; branch to previous anonymous label, if positive. if we are in a vblank, $2002 will be signed negative and the 7th bit will be 1. Otherwise, it is 0 and it is positive.

    txa ; transfers X reg to A reg. The value is currently 0

  CLEARMEM: ; This is basically a low level for loop that clears out the NES memory.
    sta $0000, X ; X is the offset, so its value will be added to the address to clear out every column of memory at the address
    sta $0100, X
    sta $0300, X ; Notice how we don't clear $0200 because we usin that for sprite data and shiz
    sta $0400, X
    sta $0500, X
    sta $0600, X
    sta $0700, X

    ;initializing our Sprite data address
    lda #$FF
    sta $0200, X ; stores FF into all memory columns of address $0200
    lda #$00 ; set back to 00 so we can go back to for loopin like nothin ever happened

    inx
    bne CLEARMEM ; we loop back until X equals A, or 0 in this case

  : ; Waiting for a second Vblank to make extra extra sure that the PPU is ready for us to grace it with our fabulous art.
    bit $2002 ; branch to previous anonymous label, if positive.
    bpl :-

    lda #$02 ; The HIbyte of our PPU data address.
    sta $4014 ; OAM DMA Register. Tells the OAM where our sprite data is or some shiz not entirely sure.
    nop ; PPU needs some time, so give it some time with the new No Operation instruction!

    lda #%10010000 ; tells ppu that we want to be interrupted for NMI and that we want to use tileset 2 for background tiles instead of tileset1
    sta $2000 ; store in ppu control reg
    lda #%00011110 ; tells ppu that we want to enable rendering for first leftmost 8px for sprites and background and to enable rendering of sprites and background in general
    sta $2001