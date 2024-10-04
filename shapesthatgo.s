.segment "HEADER"
  .byte "NES"
  .byte $1A
  .byte $02 ; 2 16kb PRG ROM chips
  .byte $01 ; 1 8kb CHR ROM chip
  .byte %00000000 ; mapper and mirroring
  .byte $00
  .byte $00
  .byte $00
  .byte $00
  .byte $00, $00, $00, $00, $00 ; filler bytes

.segment "RODATA"
  coolPalette:
    .byte $12, $05, $17, $1A
    .byte $12, $35, $28, $31
    .byte $12, $47, $23, $34
    .byte $12, $39, $67, $1A
    .byte $12, $05, $17, $1A
    .byte $12, $35, $28, $31
    .byte $12, $47, $23, $34
    .byte $12, $39, $67, $1A

  player:
    .byte $00, $00, $00, $00 ; Y pos, tile index, attributes, X pos
    .byte $00, $01, $00, $08
    .byte $08, $02, $00, $00
    .byte $08, $03, $00, $08

.segment "DATA"
  gravity = 1
  maxYV = 6
  screenHeight = 208
  jumpHeight = 60

.segment "ZEROPAGE"
  playerX: .res 1 ; Stores the player's x position
  playerY: .res 1 ; Stores the player's y position
  playerXV: .res 1 ; Stores the speed at which the player is moving in the x direction
  playerYV: .res 1 ; Stores the speed at which the player is moving in the y direction
  playerYAfterJump: .res 1 ; Stores the player's y position after a jump
  isJumping: .res 1 ; Stores whether the player is currently jumping
  onGround: .res 1 ; Stores whether the player is on the ground

.segment "STARTUP"
  .include "resetroutine.s"

.segment "CODE"

  lda #$3F
  sta $2006
  lda #$00
  sta $2006

  ; Write colors to the palette
  loadPalettes:
    lda coolPalette, X
    sta $2007
    inx
    cpx #32
    bne loadPalettes
  
  ldx #0
  loadSprite:
    lda player, X
    sta $0200, X
    inx
    cpx #16
    bne loadSprite

Forever:
  jmp Forever     ; Infinite loop

NMI:
  pha             ; Push A to the stack
  lda $2002       ; Read PPU status to clear the VBlank flag
  
  ; Start reading controller input
  lda #1
  sta $4016
  lda #0
  sta $4016

  ReadA:
    ; Check if A button is pressed
    lda $4016
    and #1
    beq ReadADone

    ; Check if the player is on the ground
    ldx onGround
    cpx #0
    beq ReadADone

    ; Safety check to make sure the player isn't currently jumping
    ldx isJumping
    cpx #1
    beq ReadADone

    ; Set player velocity for the jump
    ; This will start high then go down 
    ldx #14
    stx playerYV

    ; Store the player's Y position after a jump
    lda playerY
    sec
    sbc #jumpHeight
    sta playerYAfterJump

    ldy playerYAfterJump

    lda #1
    sta isJumping
    jmp ReadADone

  ReadADone:

  ldx #0
  setPlayerPos:
    clc
    lda player+3, x
    adc playerX
    sta $0203, x
    lda player, x
    adc playerY
    sta $0200, x
    inx
    inx
    inx
    inx
    cpx #16
    bne setPlayerPos

  ; Check if the player's Y velocity is at its max, and if it is, don't apply gravity
  ; Instead, continue applying the player's current Y velocity
  lda playerYV
  sec
  cmp #maxYV
  bcc addGrav
  jmp addYV

  addGrav:
    ; Temporary check for not applying gravity when the player is on the ground
    ; This will have to be changed so that when the player has reached their jump height destination,
    ; they will start falling back down and accelerating with gravity again
    lda playerYV
    clc
    adc #gravity
    sta playerYV

  addYV:
    ; Check if the player is on the ground
    lda playerY
    cmp #screenHeight
    bcs setOnGround

    ; Set onGround to false
    ldx #0
    stx onGround

    ; If the player is jumping, skip the gravity logic
    ldx isJumping
    cpx #1
    beq handleJump

    ; Add player velocity to Y position
    clc
    adc playerYV
    sta playerY
    jmp done

  setOnGround:
    ; Set onGround
    lda #1
    sta onGround
    jmp handleJump
    
  handleJump:
    ; Check if player is jumping
    lda isJumping
    cmp #0
    beq done ; Skip jump logic

    jmp jump

  jump:
    ; Increase player y position 
    lda playerY
    sec
    sbc playerYV
    sta playerY

    ; Decrease player y velocity
    lda playerYV
    cmp #2
    bcs @decreaseVelocity

    jmp @dontDecreaseVelocity
    @decreaseVelocity:
      sbc #2
      sta playerYV

    @dontDecreaseVelocity:

    ; Check if player has reached their jump height
    lda playerY
    cmp playerYAfterJump
    bcc doneJumping

    jmp done

  doneJumping:
    lda #0
    sta isJumping
    ; sta playerYV
    jmp done
    
done:
  lda #$00
  sta $2003
  lda #$02
  sta $4014

  pla             ; Pull A back from the stack
  rti             ; Return from interrupt

.segment "VECTORS"
  .word NMI        ; NMI vector
  .word Reset      ; Reset vector
  .word 0          ; IRQ/BRK vector (not used)

.segment "CHARS"
  .incbin "shapes.chr"