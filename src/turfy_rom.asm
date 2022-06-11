FNAME "turfy.rom"

;db 0x0FE ; This segment is the BIN file header, all the way until "start:"
;dw start, endadr, start
;org 0xc000
;org 0xb800


org $4000   ; Somewhere out of the way of small basic programs
;org $c000

db "AB" ;   ROM signature
dw start  ; start address
db 0,0,0,0,0,0,0,0,0,0,0,0



start: NOP ; End BIN file header

; Constants
FORCLR: equ $F3E9
BAKCLR: equ $F3EA
BDRCLR: equ $F3EB
CLIKSW: equ $f3db

CHGCLR: equ $0062
CHGMOD: equ $005F
LINL32: equ $F3AF

WRTVDP: equ $0047
WRTVRM: equ $004D ; Write to VRAM, A=value, HL=address
RDVRM: equ $004A ; Read from VRAM, A <- value, HL = address
LDIRVM: equ $005C ; Block transfer from memory to VRAM; BC = block length, HL = mem start, DE = start VRAM
VDP1_MIRROR: equ $F3E0


GTSTCK: equ $00D5 ; input: a = joystick to test (0 = cursor, 1 = port 1, 2 = port 2); output: a = direction
GTTRIG: equ $00D8 
CALPAT: equ $0084
CALATR: equ $0087
;; ----------------------------------
;; Turfy - a two-player card game
;;
;; TODO:
;;   - use the larger guidance texts
;;   + make a better-looking splash screen
;;   + display the current score
;;   + display character tokens
;;     + enable large sprites
;;     + define sprites
;;     + render them at proper places
;;     + render CORRECT sprits
;;     + randomize decks
;;     + randomize random seed
;;     + render remaining deck size correctly
;;   - gameplay loop
;;     + main loop
;;       + convert cursor position to board position
;;       + choose where to play a card
;;       + point out playable cards
;;       + choose card to play
;;       + properly remove card from hand
;;       + choose the card from the correct hand on player 2
;;       + choose where to move
;;       + check if movement is impossible
;;     + final score computation
;;   + splash screen
;;   - animation?
;;   - elementary sound effects
;;   + basic AI (+?)
;;     + sort the cards in the AI's hands by their value
;;     - increase planning horizon depth dynamically?

	; START OF THE MAIN CODE: INITIALIZE
	LD ix, BAKCLR ; Initialize the screen colours. 
	ld (ix), 1
	LD ix, FORCLR
	ld (ix), 15
	LD ix, BDRCLR
	ld (ix), 12
	call CHGCLR

	call $00CC ; Disable function key display.

	ld a,1      ; Change screen mode
    call CHGMOD
	
	ld bc, $e201 ;; Should allow 16x16 sprites.
    call WRTVDP

	ld a, 0
	ld (CLIKSW), a
	
	;call move_ROMpage2_to_memorypage1
	call copyROMtoRAM
	call Initialize
	call MenuLoop
	ret
	

BoldFont:
	;; Bold font from '!' to 'Z'
	ld hl, 33 * 8
	ld bc, 464
  .loop:
    call RDVRM
	ld d, a
	sra a
	or d
	call WRTVRM
	inc hl
	dec bc
	ld a, c
	cp 0
	jp nz, .loop
	ld a, b
	cp 0
	jp nz, .loop
	ret
    
	
    
	
	


;;; PREPARE TO REMOVE	
;RSLREG: equ $0138
;ENASLT: equ $0024
;move_ROMpage2_to_memorypage1:
;    call RSLREG     ; Reads the primary slot register
;    rrca
;    rrca
;    and $03         ; keep the two bits for page 1
;    ld c,a
;    add a,$C1       
;    ld l,a
;    ld h,$FC        ; HL = EXPTBL + a
;    ld a,(hl)
;    and $80         ; keep just the most significant bit (expanded or not)
;    or c
;    ld c,a          ; c = a || c (a had #80 if slot was expanded, and #00 otherwise)
;    inc l           
;    inc l
;    inc l
;    inc l           ; increment 4, in order to get tot the corresponding SLTTBL
;    ld a,(hl)       
;    and $0C         
;    or c            ; in A the rom slotvar 
;    ld h,$80        ; move page 1 of the ROM to page 2 in main memory
;    call ENASLT       
;    ret

	
	
	
	
	
STR_GAME_TITLE_1: db "** T U R F Y **"
STR_GAME_TITLE_2: db   "(C) 2016   "
STR_OPT_1: db          "P1 VS  P2"
STR_OPT_2: db          "P1 VS CPU1"
STR_OPT_3: db          "P1 VS CPU2"
STR_OPT_4: db          "VIEW RULES"


HideSprites:
	;; Remove the sprites.

	ld a, 10
	call CALATR
	ex de, hl
	ld a, 192
	ld (_REND_HELP + 1), a
	ld (_REND_HELP), a
	ld (_REND_HELP + 2), a
	ld (_REND_HELP + 3), a
	ld hl, _REND_HELP
	ld bc, 4
	call LDIRVM 

	ld a, 11
	call CALATR
	ex de, hl
	ld hl, _REND_HELP
	ld bc, 4
	call LDIRVM

	ld a, 12
	call CALATR
	ex de, hl
	ld hl, _REND_HELP
	ld bc, 4
	call LDIRVM

	ld a, 0
	call CALATR
	ex de, hl
	ld hl, _REND_HELP
	ld bc, 4
	call LDIRVM 
	ret


	
ClearScreen:
	ld a, 32
	ld bc, 3
	ld hl, $1800
  .loop1:
	ld b, 0
  .loop2:
    push bc
	push hl
	ld a, 32
	call WRTVRM
	pop hl
	inc hl
	pop bc
	djnz .loop2
    dec c
	ld a, c
	cp 0
	ret z
	jp .loop1

SplashScreen:
	;; Render the game title etc.
	call ClearScreen
	;ld de, $1800 + 232 ;; VRAM start
	;ld bc, 15 ;; Length
	;ld hl, STR_GAME_TITLE_1 ;; RAM
	;call LDIRVM
	ld de, $1800 + 3 + 32 * 1
	ld bc, 26
	ld hl, TITLE_1
	call LDIRVM

	ld de, $1800 + 3 + 32 * 2
	ld bc, 26
	ld hl, TITLE_2
	call LDIRVM

	ld de, $1800 + 3 + 32 * 3
	ld bc, 26
	ld hl, TITLE_3
	call LDIRVM

	ld de, $1800 + 3 + 32 * 4
	ld bc, 26
	ld hl, TITLE_4
	call LDIRVM

	ld de, $1800 + 3 + 32 * 5
	ld bc, 26
	ld hl, TITLE_5
	call LDIRVM

	ld de, $1800 + 3 + 32 * 6
	ld bc, 26
	ld hl, TITLE_6
	call LDIRVM

	ld de, $1800 + 3 + 32 * 7
	ld bc, 26
	ld hl, TITLE_7
	call LDIRVM

	ld de, $1800 + 3 + 32 * 8
	ld bc, 26
	ld hl, TITLE_8
	call LDIRVM
	


	ld de, $1800 + 746 ;; (C) notice
	ld bc, 11
	ld hl, STR_GAME_TITLE_2
	call LDIRVM
	ld de, $1800 + 330
	ld bc, 9
	ld hl, STR_OPT_1
	call LDIRVM
	ld de, $1800 + 362
	ld bc, 10
	ld hl, STR_OPT_2
	call LDIRVM
	ld de, $1800 + 362 + 32
	ld bc, 10
	ld hl, STR_OPT_3
	call LDIRVM

	ld de, $1800 + 362 + 32 * 2
	ld bc, 10
	ld hl, STR_OPT_4
	call LDIRVM

	
	;; Remove the sprites.
	call HideSprites
	
	;; Let's choose the game mode
	ld a, 160
	ld (CursorX), a
	ld a, MAINMENU_TOP
	ld (CursorY), a
	
  .redrawPointer:
	ld a, 10
	call CALATR
	ex de, hl
	ld a, (CursorY)
    ld (_REND_HELP), a
	ld a, (CursorX)
	ld (_REND_HELP + 1), a
	ld a, (CursorY)
	ld (_REND_HELP), a
	ld a, 0
	ld (_REND_HELP + 2), a
	ld a, 15
	ld (_REND_HELP + 3), a
	ld hl, _REND_HELP
	ld bc, 4
	call LDIRVM 
	call Delay

	
  .waitForControl:
    ld a, (Rng)
	inc a
	ld (Rng), a
    ld a, 0
	call GTSTCK
	cp 0
	jp nz, .gotMove
	ld a, 1
	call GTSTCK
	cp 0
	jp nz, .gotMove
	ld a, 0
	call GTTRIG
	cp 0
	jp nz, .chosen
	ld a, 1
	call GTTRIG
	cp 0
	jp nz, .chosen
	
	jp .waitForControl
	
  .gotMove:
    cp 1
	jp z, .movedUp
	cp 5
	jp z, .movedDown
	jp .waitForControl
  
  .movedUp:
    ld a, (CursorY)
	cp a, MAINMENU_TOP
	jp z, .toBottom
	sub a, 8
	jp .doneMove
  .toBottom:
    ld a, MAINMENU_TOP + 3 * 8
	jp .doneMove
  .movedDown:
    ld a, (CursorY)
	cp a, MAINMENU_TOP + 3 * 8
	jp z, .toTop
	add a, 8
	jp .doneMove
  .toTop:
	ld a, MAINMENU_TOP
	jp .doneMove
  .doneMove:
	ld (CursorY), a
    jp .redrawPointer
	
  .chosen:
	call HideSprites
	call ClearScreen
	call WaitNoFire
	;call SFXPlayCard
	ld a, 0
	ld (PlayerTypes), a
	ld (PlayerTypes + 1), a
	ld a, (CursorY)
	cp MAINMENU_TOP
	ret z
	
	cp MAINMENU_TOP + 3 * 8
	jp z, DisplayHelp
	
	ld a, 1
	ld (PlayerTypes + 1), a
	ld a, (CursorY)
	cp MAINMENU_TOP + 8
	jp z, .easyCPU
	ld a, 0
	ld (EasyAI), a
	ret
  .easyCPU:
    ld a, 1
	ld (EasyAI), a
	ret

DisplayHelp:
	ld bc, 32
	ld hl, HELP_VIEW - 32
	ld de, DISP_START - 32
	push hl
	push de
	ld a, 24
	push af
  .loop:
    pop af
	dec a
	cp 0
	jp z, .waitForClosing
	pop hl
	ld bc, 32
	add hl, bc
	ex de, hl
	pop hl
	ld bc, 32
	add hl, bc
	push hl
	push de
	push af
	call LDIRVM
	jp .loop
  .waitForClosing:
	pop de
	pop hl
	call WaitNoFire
    call WaitFire
	call WaitNoFire
	jp SplashScreen
	
	
	
	
	
MAINMENU_TOP: equ 78
	

Delay:	
	push af
	push bc
	ld bc, 20000
  .delay:
	nop
    dec bc
	ld a, c
	cp 0
	jp nz, .delay
	ld a, b
	cp 0
	jp nz, .delay
	pop bc
	pop af
	ret
   
	
  
  
	
	

	
CardValueToA:
	push ix
	push bc
	ld ix, Values
	ld bc, 0
	ld c, a
	add ix, bc
	ld a, (ix)
	pop bc
	pop ix
	ret
	
	

	
SortHand:
	;; Sorts the cards in the active player's hand
	;; by their value.
	ld b, 0
	
  .outerLoop:
    ld c, b
	inc c
	ld a, b
	call LoadCardValueToA
	ld d, a
	
  .innerLoop:
    ld a, c
	call LoadCardValueToA
	cp d
	jp nc, .nextInner
	jp z, .nextInner

	;; Swap cards at B and C
	ld d, a 
	call SwapHandCards
	
  .nextInner:
    inc c
	ld a, c
	cp HandSize
	jp nz, .innerLoop
	
  .nextOuter:
    inc b
	ld a, b
	cp HandSize - 1
	jp c, .outerLoop
	
  .end:
	ret


SwapHandCards:
	;; Load card indices from B and C
	push bc
	push de
	push ix
	push iy
	push bc

	call CurrentHandPtrToIY

	pop bc
	push iy
	ld e, b
	ld b, 0
	add iy, bc
	ld a, (iy)
	ld d, a
	push iy
	pop ix
	pop iy
	ld c, e
	add iy, bc
	ld a, (iy)
	ld (iy), d
	ld (ix), a
	
	pop iy
	pop ix
	pop de
	pop bc
	ret
	
	
LoadCardValueToA:
	;; A contains the card index in hand.
	push bc
	push iy
	push af
	call CurrentHandPtrToIY
	pop af
	ld c, a
	ld b, 0
	add iy, bc
	ld a, (iy) ;; Now has card index
	cp NO_CARD
	jp z, .for_return
	ld iy, Values
	ld c, a
	add iy, bc
	ld a, (iy)
  .for_return:
	pop iy
	pop bc
	ret
  
	
	
RandomNumber:
	;; Produces a random integer. Doesn't work quite as it should, but hopefully well enough.
	;; Let's try:
	;; 	a, c, m = 132, 63, 131
	;;  x_(i+1) = (a * x_(i) + c ) % m
	
	push af
	push bc
	push de 
	push hl
	push ix
	push iy
	
	ld a, (Rng)
	ld l, a
	ld a, (Rng + 1)
	ld h, a
	
	
	;; DEBUG
	;ld h, 5
	;ld l, 8
	
	;; Multiply by A.
	;; HL has the original value.
	
	;ld h, d
	;ld l, e
	ld a, 9 ;; Factor
	;; DE has the product
	ld de, 0
	
  .loop1:
    sra a
	jp nc, .noAdd
	ld b, a
	ld a, d
	ld a, l
	add a, e
	ld e, a
	ld a, d
	adc a, h
	ld d, a
	ld a, b
  .noAdd:
    add hl, hl
	cp 0
	jp nz, .loop1
	
	;; Move product to IX and add C (63)
    ld ix, 63
	add ix, de
	
	;; Now, modulo 256? Let's not.
	ld a, ixl
	ld (Rng), a
	ld a, ixh
	ld (Rng + 1), a
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af
	ret

RandomModulo:

	push de
	push bc
	push af
	call RandomNumber
	ld a, (Rng) ;; Now get the modulo.
	ld c, a
	pop af
	ld d, a
	ld a, 0 ;; Start from 0
	ld b, 8
	
  .redo:
    rl c
	rla
	cp d
	jp c, .next
	sub d
  .next:
	djnz .redo
	pop bc
	pop de
	ret



	
ShuffleValues:
	;; BC holds the first address to shuffle.
	;; D holds the number of values to shuffle.
   	push af
	push bc
	push de 
	push hl
	push ix
	push iy
	
	ld ix, 0
	add ix, bc
  .start:
	push ix ;; Stack contains the current first slot to cover.
	ld a, d 
	call RandomModulo
	;; Now, swap IX and IX + a

	ld b, 0
	ld c, a
	pop iy
	add iy, bc
	ld h, (iy)
	ld l, (ix)
	ld (ix), h
	ld (iy), l
	inc ix
	dec d
	ld a, d
	cp 1
	jp nz, .start
	
  	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af
	ret

	
	
	

CPUTurn:
	;; The current turn is made by the CPU.
	ld a, 0
	ld (AIStackDepth), a
	ld a, -1
	call SimpleAI
	;; Unpack the results.
	ld e, a
	push de
	push bc
    ;; B = the best paint target
	;; C = the best paint card
	;; D = the best move position	
	;; E = the score
	; Did we play a card?
	ld a, b
	ld (SuggestedBoardPosition), a
	ld a, c
	ld (SuggestedCard), a
	call IsOkPainting
	pop bc
	cp 0
	jp z, .discard
	; Play the card.
	call PlayCard
	jp .move
  .discard:
	call RemoveCard

  .move:
    pop de
	ld b, d
	call BoardIndexToCoords
	ld a, (CurrentTurn)
	cp 0
	jp z, .p1
	ld a, d
	ld (XCoords + 1), a
	ld a, e
	ld (YCoords + 1), a
	ret
  .p1:
	ld a, d
	ld (XCoords), a
	ld a, e
	ld (YCoords), a
	ret


AI_STACK_MAX: EQU 3
	
	

SimpleAI:
    ld iy, 0
	add iy, sp
	ld ix, -16
	add ix, sp
	ld sp, ix
	push iy ;; Store original SP
	

	; ix      : current movement table offset
	; ix + 1  : Current paint target offset
	; ix + 2  : Current painted/discarded card in hand
	; ix + 3  : Current player token position (board index)
	; ix + 4  : Current paint goodness
	; ix + 5  : Best movement tile choice
	; ix + 6  : Best paint tile choice
	; ix + 7  : Best painted card choice
	; ix + 8  : Best score so far
	; ix + 9  : Previous card in the used tile
	; ix + 10 : Painted board slot index
	; ix + 11 : Was any move valid?	
	; ix + 12 : The starting point for card loops.
	
	inc a
	ld (ix + 12), a ; The first card index to use.
	
	
	
	
	
	
	; Go through all four possible cells to paint.
	ld a, 0
	ld (ix), 0 ; Current movement offset.
	ld (ix + 1), 0 ; Current paint target offset
	ld (ix + 2), 0 ; Current painted/discarded card in hand.
	ld (ix + 4), 0
	ld (ix + 9), 0 ; 

	ld (ix + 5), MOVEMENT_OPT_LEN - 1
	ld (ix + 6), 0
	ld (ix + 7), 0
	ld (ix + 8), 0
	ld (ix + 9), 0
	ld (ix + 10), 0
	ld (ix + 11), 0

	ld a, (CurrentTurn)
	cp 0
	jp z, .p1Init
	ld a, (XCoords + 1)
	ld d, a
	ld a, (YCoords + 1)
	ld e, a
	jp .endInit
  .p1Init:
    ld a, (XCoords)
	ld d, a
	ld a, (YCoords)
	ld e, a
	jp .endInit
  .endInit:
	call CoordsToBoardIndex
	ld a, b
	ld (ix + 3), a ; Current player token position

	; Go through all cards in hand.

    ld a, 0
	ld (ix + 1), a
	ld a, (AIStackDepth)
	inc a
	ld (AIStackDepth), a
	cp AI_STACK_MAX
	jp nc, .ending

	ld a, (ix + 12)
	cp HandSize - 1
	jp nc, .ending
	
	
  .paintTargetLoop:
	ld iy, PAINT_OPTS
	ld b, 0
	ld c, (ix + 1)
	add iy, bc ;; Now has the address to the 16-bit address offset.
	ld c, (iy)
	ld a, (ix + 3)
	add a, c
	cp 25
	jp nc, .nextTarget
	
	ld (ix + 10), a
	
	ld a, (ix + 12)
	ld (ix + 2), a


	
	
  .handCardLoop:
	; Is card already played in this branch?
	ld iy, HandCardsPlayed
	ld b, 0
	ld c, (ix + 2)
	add iy, bc
	ld a, (iy)
	cp 0
	jp nz, .nextHandCard ;; NZ -> cannot be played again.
	
	ld a, (EasyAI)
	cp 0
	jp z, .noLimitation
	ld a, (ix + 2)
	and 5
	cp 1
	jp z, .nextHandCard
	jp .noLimitation
	
	
  .noLimitation:
	call CurrentHandPtrToIY ;; Is the card a real card?
	add iy, bc
	ld a, (iy)
	cp NO_CARD
	jp z, .nextHandCard
	
	; Check if the card can be played in the specified spot.
	ld (SuggestedCard), a
	ld iy, PAINT_OPTS
	ld b, 0
	ld c, (ix + 1)
	add iy, bc
	ld b, (iy) ;; Paint offset
	ld a, (ix + 3)
	add a, b ;; A has now the supposed paint target.
	ld (SuggestedBoardPosition), a
	ld iy, Board
	ld b, 0
	ld c, a
	add iy, bc
	ld a, (iy)
	ld (ix + 9), a ;; Save the old contents of the tile
	
	
	call IsOkPainting
	cp 0
	jp z, .wrapUpPainting ;; Register A=0 means the score will be 0.
	;; We can paint. Count the score difference.
	
	;; Update the table
	ld a, (SuggestedBoardPosition)
	ld b, 0
	ld c, a
	ld iy, Board
	add iy, bc
	ld a, (SuggestedCard)
    ld (iy), a
	ld iy, BoardStacks
	add iy, bc
	ld a, (iy)
	;; We don't update the stack size, because it won't matter - we can't paint in it again.
	cp 3
	jp nc, .onlyTwoPoints
	ld c, a
	ld iy, StackPoints
	add iy, bc
	ld a, (iy)
    jp .wrapUpPainting
  .onlyTwoPoints:
    ld a, (StackPoints + 3)
    jp .wrapUpPainting
	
  .wrapUpPainting:
    ;; Add the point value.
	ld (ix + 4), a
	;; Mark the card as used
	ld iy, HandCardsPlayed
	ld c, (ix + 2)
	ld b, 0
	add iy, bc
	ld (iy), 1
	
	;; Prepare for movement.
	ld a, (AIStackDepth)
	cp AI_STACK_MAX
	jp z, .skipToLastMovement
	jp .allMovements

	.skipToLastMovement:
	ld a, MOVEMENT_OPT_LEN - 1
	ld (ix), a
	ld (ix + 11), 1 ;; This move was valid.
	jp .movementLoop
  
	
  .allMovements:
	ld a, 0
	ld (ix), a ;; Reset movement table.
	ld (ix + 11), a ;; No move was valid.
	
  .movementLoop:
    ;; Third loop: try moving.
	
	;; If the stack is at its peak, let's look only at the
	;; no-move option.
	
	
	ld iy, MOVEMENT_OPTS
	ld d, (ix + 3)
	ld bc, 0
	ld c, (ix)
	add iy, bc
	ld a, (iy)
	add a, d ;; A has now the target coordinate. Is it valid?
	cp 25
	jp nc, .notValidMove
	ld (SuggestedBoardPosition), a
	call IsMoveValid
	cp 0
	jp nz, .movementOk
	;; Check if the movement option is the last. If yes and no valid moves have been
	;; seen, accept this.
	ld a, (ix + 11)
	cp 0
	jp nz, .notValidMove
	ld a, (ix)
	cp MOVEMENT_OPT_LEN - 1
	jp z, .movementOk
	jp .notValidMove
	
  .movementOk:
    ;; Apply the movement.
	ld (ix + 11), 1
	ld a, (SuggestedBoardPosition)
	ld b, a
	call BoardIndexToCoords
	ld a, (CurrentTurn)
	cp 0
	jp z, .movementP1
	ld a, d
	ld (XCoords + 1), a
	ld a, e 
	ld (YCoords + 1), a
	jp .recursion
	
  .movementP1:
    ld a, d
	ld (XCoords), a
	ld a, e
	ld (YCoords), a
	jp .recursion
	
  .recursion:
	;; Call the recursion loop.	
	ld a, (ix + 12)
	push ix
    call SimpleAI
	pop ix
	;; A = the score
	;; B = the best paint target
	;; C = the best paint card
	;; D = the best move position
	
	ld e, a
	ld a, (ix + 4)
	add a, e
	ld e, a
	ld a, (ix + 8)
	;; Compare the results to the best results so far. Store the best as appropriate.
	cp e
	jp nc, .nextMovement ; Jump if the new is worse than old.
	jp z, .nextMovement
	
  .newBestFound:
    ld a, e
	ld (ix + 8), a
	ld a, (ix)
	ld (ix + 5), a; Best movement offset
	ld a, (ix + 1)
	ld (ix + 6), a; Best paint tile choice offset
	ld a, (ix + 2)
	ld (ix + 7), a ; Best paint card choice offset
	
	jp .nextMovement
	
  .notValidMove:
    ;; Cannot move, so we can do nothing but go to .nextMovement
	jp .nextMovement
	
  .nextMovement:
	;; Revert the changes made:
	;; return XCoords, YCoords to their former values.
	ld b, (ix + 3)
	call BoardIndexToCoords
	ld a, (CurrentTurn)
	cp 0
	jp nz, .nextMoveP2
	
    ld iy, YCoords
	ld (iy), e
	ld iy, XCoords
	ld (iy), d
	jp .finishMovementLoop
  .nextMoveP2:
    ld iy, YCoords + 1
	ld (iy), e
	ld iy, XCoords + 1
	ld (iy), d
	jp .finishMovementLoop

  .finishMovementLoop:
    ld a, (ix)	;; increase movement pointer
	inc a
	ld (ix), a
	cp MOVEMENT_OPT_LEN
	jp nz, .movementLoop
    
  .revertBoard:
	;; revert the board.
	ld iy, PAINT_OPTS
	ld c, (ix + 1)
	ld b, 0
	add iy, bc
	ld a, (ix + 9)
	ld iy, Board
	ld b, 0
	ld c, (ix + 10)
	add iy, bc
	ld (iy), a ;; Return the old card.

    ;; reset the played card indicator
	ld c, (ix + 2)
	ld b, 0
	ld iy, HandCardsPlayed
	add iy, bc
	ld (iy), 0

  .nextHandCard:
	ld c, (ix + 2)
	;; increase the hand indicator.
	inc c
	ld (ix + 2), c
	ld a, c
	;; Is this loop over?
	
	cp HandSize
	jp nz, .handCardLoop

	ld a, (ix + 12) ;; Reset this loop.
	inc a
	ld (ix + 2), a

  .nextTarget:
    ld a, (ix + 1)
	inc a
	cp PAINT_OPTS_LEN
	jp z, .ending 
	ld (ix + 1), a
	jp .paintTargetLoop

	


  .dispEnding:
	; ix      : current movement table offset
	; ix + 1  : Current paint target offset
	; ix + 2  : Current painted/discarded card in hand
	; ix + 3  : Current player token position (board index)
	; ix + 4  : Current paint goodness
	; ix + 5  : Best movement tile choice index
	; ix + 6  : Best paint tile choice index
	; ix + 7  : Best painted card choice index
	; ix + 8  : Best score so far
	; ix + 9  : Previous card in the used tile
	; ix + 10 : Painted board slot index
	; ix + 11 : Was any move valid?
	
	push ix
	call DisplayFirstGameScreen
	call RedrawSprites
	
	pop ix
	push ix
    ld hl, $1800 + 662
	ld a, (ix + 5) ;; Movement
	add a, 65
	CALL WRTVRM
	pop ix
	push ix
    ld hl, $1800 + 663
	ld a, (ix + 6) ;; Paint target
	add a, 65
	CALL WRTVRM
	pop ix
	push ix
    ld hl, $1800 + 664
	ld a, (ix + 7) ;; Best painted card index
	add a, 65
	CALL WRTVRM
	pop ix
	push ix
    ld hl, $1800 + 665
	ld a, (ix + 8) ;; Best score
	add a, 65
	CALL WRTVRM
	pop ix
	push ix
    ld hl, $1800 + 666
	ld a, (AIStackDepth) ;; Stack depth
	add a, 65
	CALL WRTVRM
	pop ix

  .ending:
    ; Return the best option and the associated score.	
	;; A = the score
	;; B = the best paint target
	;; C = the best paint card
	;; D = the best move position
	
	ld a, (AIStackDepth)
	dec a
	ld (AIStackDepth), a
	
	ld iy, MOVEMENT_OPTS
	ld b, 0
	ld c, (ix + 5)
	add iy, bc
	ld a, (iy)
	ld b, (ix + 3)
	add a, b
	ld d, a ;; Now D has the best movement target.
	
	ld iy, PAINT_OPTS
	ld b, 0
	ld c, (ix + 6) ;; Paint offset
	add iy, bc
	ld a, (ix + 3)
	ld c, (iy)
	add a, c ;; Add to the token position to get paint target.
	ld e, a
	
	call CurrentHandPtrToIY
	ld b, 0
	ld c, (ix + 7)
	add iy, bc
	ld c, (iy)
	ld b, e
	
  .finalTouch:

    ld a, (ix + 8) ;; Best score.
	pop ix
	ld sp, ix

	cp 0 ;; Punish late values.
	ret z
	dec a
    ret 
	
	
	
	
	
	
ResetCursor:
	;; Initialize the cursor at where the player token is.
	push ix
	push iy
	push bc
	push af
	ld a, (CurrentTurn)
	ld ix, XCoords
	ld iy, YCoords
	ld b, 0
	ld c, a
	add ix, bc
	add iy, bc
	ld a, (ix)
	sla a ; Multiply by 2**5=32
	sla a
	sla a
	sla a
	sla a
	add a, 16
	ld (CursorX), a
	
	ld a, (iy) 
	sla a ; Multiply by 2**5=32
	sla a
	sla a
	sla a
	sla a
	add a, 16
	ld (CursorY), a
	pop af
	pop bc
	pop iy
	pop ix
	ret

MenuLoop:
  .loop:
	call SplashScreen
	call HideSprites
	call MainGameLoop
    jp .loop
	
	
MainGameLoop:
    ;; The actual game loop.
	
	call InitializeGame
	;; Start with player 1.
	ld a, 1
	ld (CurrentTurn), a
	ld a, PHASE_WAIT
	ld (CurrentPhase), a
	call DisplayFirstGameScreen
	call DisplayStatus
	call WaitTurnSwitch
	
  .startRound:
    ld a, (CurrentTurn)
	xor 1 ; Swap between 0 and 1.
	
	ld (CurrentTurn), a
	
	ld ix, PlayerTypes
	ld b, 0
	ld c, a
	add ix, bc
	ld a, (ix)
	cp 0
	jp z, .notCPU
	
	;; TODO: Add a new game phase for CPU pondering.
	call DrawCard
	
	ld a, PHASE_CPU
	ld (CurrentPhase), a
	call DisplayStatus
	call SortHand
	
	call DisplayStatus
	call CPUTurn
	
	jp .endTurn
	
	
  .notCPU:
	
	call ResetCursor

	;; Draw a card.
	ld a, PHASE_DRAW
	ld (CurrentPhase), a
	call DrawCard
	ld ix, HandLen
	ld bc, 0
	ld a, (CurrentTurn)
	ld c, a
	add ix, bc
	ld a, (ix)
	cp 0
	jp z, .gameEnded
	
	; Now, paint phase.

  .movePaintCursor:
	ld a, PHASE_PAINT
	ld (CurrentPhase), a
	call DisplayFirstGameScreen
	call DisplayStatus
    call ResetCursor
    call PointerControlLoop
    ld a, PHASE_PAINT_PICK_CARD
	ld (CurrentPhase), a
	call DisplayStatus
	;call UpdateCardOptions
	call WaitNoFire
	call CardSelectionLoop
	cp 0
	jp z, .movePaintCursor ;; Cancelled
	
	call IsOkPainting
	;; If not OK, just discard the card.
	cp 0
	jp z, .discardCard
	call PlayCard
	jp .moveMoveCursor
  .discardCard:
    call RemoveCard
	jp .moveMoveCursor
	

	;; Move phase
  .moveMoveCursor:
	ld a, PHASE_MOVE
	ld (CurrentPhase), a
	call DisplayFirstGameScreen
	call DisplayStatus

	call ResetCursor
	call WaitNoFire
	
	;; TODO: Check if any move is OK. If not, skip movement.

	call IsAnyMoveValid
	cp 0
	jp nz, .doMove
	
	call WaitNoFire
	call WaitFire
	call WaitNoFire
	jp .endTurn
	
	
  .doMove:
    call PointerControlLoop
	
	call WaitNoFire
  .endTurn:
	
	ld a, PHASE_WAIT
	ld (CurrentPhase), a

	call DisplayFirstGameScreen
	call RedrawSprites
	call DisplayStatus
	call SFXPassToNext
	call WaitTurnSwitch
	jp .startRound
	
  .gameEnded:
    ; The game ended.
	; Count scores, declare winner.
	
	call GameEnding
	
	ret

AToNumChars:
	;; Input: A
	;; Output: D = A / 10, E = A % 10
	ld de, $3030 ;; Set to "00"
  .atontenLoop:
    cp 10
	jp c, .atondone
	inc d
	sub 10
	jp .atontenLoop
  .atondone:
    add a, $30
	ld e, a
	ld a, d
	cp $30
	jp nz, .atonreallyDone
	ld d, 32
  .atonreallyDone:
	ret

	
GameEnding:

	;; Display the game results.
	call HideSprites	
	ld hl, ENDSCREEN_EMPTY
	ld bc, 18
	ld de, $1800 + 225
	call LDIRVM
	ld hl, ENDSCREEN_TITLE
	ld bc, 18
	ld de, $1800 + 257
	call LDIRVM
	ld hl, ENDSCREEN_EMPTY
	ld bc, 18
	ld de, $1800 + 289 
	call LDIRVM

	ld hl, ENDSCREEN_EMPTY
	ld bc, 18
	ld de, $1800 + 289 + 32
	call LDIRVM
	
	ld hl, ENDSCREEN_EMPTY
	ld bc, 18
	ld de, $1800 + 289 + 32 + 32
	call LDIRVM
	
  .drumrollDelay:

    call WaitNoFire
	call WaitFire
	call WaitNoFire
  
  ;; TODO: the wait

    call CountScore
	ex de, hl

	call UpdateScore
	
	ld a, h
	cp l
	jp z, .tie
	jp c, .victor2
	ld ix, ENDSCREEN_VICTOR
	ld a, $31
	ld (ix + 16), a
	ld hl, ENDSCREEN_VICTOR
	ld bc, 18
	ld de, $1800 + 289 + 32 
	call LDIRVM
	jp .endPart
  .victor2:
	ld ix, ENDSCREEN_VICTOR
	ld a, $32
	ld (ix + 16), a
	ld hl, ENDSCREEN_VICTOR
	ld bc, 18
	ld de, $1800 + 289 + 32 
	call LDIRVM
	jp .endPart
  .tie:
	ld hl, ENDSCREEN_TIE
	ld bc, 18
	ld de, $1800 + 289 + 32 
	call LDIRVM
	jp .endPart
	
  .endPart:
	ld hl, ENDSCREEN_SCORE
	ld bc, 18
	ld de, $1800 + 289
	call LDIRVM
    call WaitNoFire
	call WaitFire
	call WaitNoFire
	ret

UpdateScore:
;; HL - scores for player 1 and 2.
;; Updates the ENDSCREEN_SCORE string.
	ld a, h
	ld ix, ENDSCREEN_SCORE
	call AToNumChars
	ld a, d
	ld (ix + 5), a
	ld a, e
	ld (ix + 6), a
	ld a, l
	call AToNumChars
	ld a, d
	ld (ix + 10), a
	ld a, e
	ld (ix + 11), a
	ret
	
	

RemoveCard:
	ld ix, HandLen
	ld a, (CurrentTurn)
	cp 0
	jp z, .handLenPtrOK
	inc ix

  .handLenPtrOK:
	ld a, (ix)
	dec a
	ld (ix), a ;; Decrease hand size
	
	call CurrentHandPtrToIY
	
    ld a, iyl ;; Prepare for removing the played card from the hand.
	ld ixl, a
	ld a, iyh
	ld ixh, a
	ld a, (SuggestedCard)
	ld d, a
    ld b, 4
  .shiftLoop:
    ld a, (iy)
	cp d
	jp z, .foundMatch
	;; Not match, copy from (iy) to (ix)
	jp .next
  .foundMatch:
    inc iy
	jp .next
  .next:
    ld a, (iy)
	ld (ix), a
	inc iy
	inc ix
	dec b
	ld a, b
	cp 0
	jp nz, .shiftLoop
	;;
	call CurrentHandPtrToIY
	ld bc, HandSize - 1
	add iy, bc
	ld a, NO_CARD
	ld (iy), a
	
	ret
	
PlayCard:
    ;; Plays a card on the table.
	;; SuggestedBoardPosition: where is played
	;; SuggestedCard: which is played
	ld a, (SuggestedBoardPosition) ;; Update board data
	ld ix, Board
	ld iy, BoardStacks
	ld b, 0
	ld c, a
	add ix, bc
	add iy, bc
	ld a, (iy)
	inc a
	ld (iy), a
	ld a, (SuggestedCard)
	ld (ix), a
	;;
	call CurrentHandPtrToIY  ;; Decrease hand size
	ld a, (CurrentTurn)
	cp 0
	jp z, .removeCard
	inc ix
  .removeCard:
    call RemoveCard
	call SFXPlayCard
	ret


    
	
	
	
	

WaitTurnSwitch:
	push af
	ld a, (PlayerTypes + 1)
	cp 1
	jp z, .end
	call waitNoFire
	call waitFire
	call waitNoFire
  .end:
    pop af
	ret

	
WaitNoFire:
    push af
  .waitDropFire:
	ld a, 0
	call GTTRIG
	cp 0
	jp nz, .waitDropFire
	ld a, 1
	call GTTRIG
	cp 0
	jp nz, .waitDropFire
    pop af
	ret

WaitFire:
    push af
  .waitFire:
	ld a, 0
	call GTTRIG
	cp 0
	jp nz, .done
	ld a, 1
	call GTTRIG
	cp 0
	jp z, .waitFire
  .done:
    
	pop af
	ret

	

	

CardSelectionLoop:
	ld a, 29 * 8
	ld (CursorX), a
	ld a, 38; 40
	ld (CursorY), a
	
	ld bc, 0
	push bc
	
  .renderSprite:
	ld a, 0
	call CALATR
	ex de, hl
	ld a, (CursorY)
    ld (_REND_HELP), a
	ld a, (CursorX)
	ld (_REND_HELP + 1), a
	ld a, 0
	ld (_REND_HELP + 2), a
	ld a, 15
	ld (_REND_HELP + 3), a
	ld hl, _REND_HELP
	ld bc, 4
	call LDIRVM

	pop bc
	push bc
	ld a, 254
  .delayLoop:
    dec a
	ld b, 54
  .innerDelay:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
	djnz .innerDelay
	cp 0
	jp nz, .delayLoop
	
  .readLoop:
	ld a, 0
	call GTSTCK
	cp 1
	jp z, .moveUp
	cp 5
	jp z, .moveDown
	
	ld a, 1
	call GTSTCK
	cp 1
	jp z, .moveUp
	cp 5
	jp z, .moveDown

	ld a, 0
	call GTTRIG
	jp nz, .chosen
	ld a, 1
	call GTTRIG
	jp nz, .chosen
	jp .readLoop
	
  .moveUp:
    pop bc
	dec b
	ld a, b
	cp -1
	jp nz, .updateSelection
	ld b, 5
	jp .updateSelection
  .moveDown:
    pop bc
	inc b
	ld a, b
	cp 6
	jp nz, .updateSelection
	ld b, 0
	jp .updateSelection
  .updateSelection:
    push bc
	ld c, b
	ld d, b
	ld b, 0
	call CurrentHandPtrToIY
	add iy, bc
	ld a, (iy)
	ld (SuggestedCard), a
	ld c, 38 ;40
	ld a, d
	sla a ;; Multiply by 8.
	sla a ;;
	sla a ;; 
	add a, c
	ld (CursorY), a
	jp .renderSprite
	
  .chosen:
    pop bc
	ld a, b
	cp 5 ;; Was this a cancel?
	jp z, .cancel
	call CurrentHandPtrToIY
	ld d, b
	ld c, b
	ld b, 0
	add iy, bc
	ld a, (iy)
	ld (SuggestedCard), a
    call WaitNoFire
	ld a, 1
	ret
  .cancel:
    call WaitNoFire
    ld a, 0
	ret

  
RedrawSprites:
    push af
    push bc
    push de
    push ix
    push hl
    ;; Render the pointer
    ld a, 10
    call CALATR
    ex de, hl
    ld a, (CursorX)
    ld (_REND_HELP + 1), a
    ld a, (CursorY)
    ld (_REND_HELP), a
    ;ld a, 4
    ;ld (_REND_HELP + 2), a
    ld a, 5
    ld (_REND_HELP + 3), a
    ld a, (CurrentPhase)
    cp PHASE_MOVE
    jp z, .checkIfOKMove
    jp .renderPointer
  .checkifOKMove:
    ld a, (FlagIsValidMove)
	cp 1
	jp z, .renderPointer
	ld a, 4 * 4
	ld (_REND_HELP + 2), a
	
	ld a, (_REND_HELP + 1)
	sub a, 8
	ld (_REND_HELP + 1), a
	ld a, (_REND_HELP)
	sub a, 8
	ld (_REND_HELP), a
	
	jp .finishCursorRender
  
  .renderPointer:
    ld a, 4
    ld (_REND_HELP + 2), a  
	jp .finishCursorRender
  
  .finishCursorRender:
    ld hl, _REND_HELP
    ld bc, 4
    call LDIRVM 
  
  
  ;; Render the player 1 token
  ld a, 11
  call CALATR
  ex de, hl
  ld a, (XCoords)
  sla a
  sla a
  sla a
  sla a
  sla a
  add a, 16
  ld (_REND_HELP + 1), a
  ld a, (YCoords)
  sla a
  sla a
  sla a
  sla a
  sla a
  add a, 15
  ld (_REND_HELP), a
  ld a, 8
  ld (_REND_HELP + 2), a
  ld a, 8
  ld (_REND_HELP + 3), a
  ld hl, _REND_HELP
  ld bc, 4
  call LDIRVM

    ;; Render the player 2 token
  ld a, 12
  call CALATR
  ex de, hl
  ld a, (XCoords + 1)
  sla a
  sla a
  sla a
  sla a
  sla a
  add a, 8
  ld (_REND_HELP + 1), a
  ld a, (YCoords + 1)
  sla a
  sla a
  sla a
  sla a
  sla a
  add a, 15
  ld (_REND_HELP), a
  ld a, 15
  ld (_REND_HELP + 2), a
  ld a, 1
  ld (_REND_HELP + 3), a
  ld hl, _REND_HELP
  ld bc, 4
  call LDIRVM
  
  ld a, 0
  call CALATR
  ex de, hl
  ld (_REND_HELP), a
  ld a, 16
  ld (_REND_HELP + 2), a
  ld a, 0
  ld (_REND_HELP + 3), a
  ld hl, _REND_HELP
  ld bc, 4
  call LDIRVM

  
  
  pop hl
  pop ix
  pop de
  pop bc
  pop af
  ret
 

UpdateOkMoveFlag:
	;; Updates FlagIsValidMove based on if the proposed move is OK.
	push ix
	push iy
	push bc
	push de
    ld a, (CursorX)
    ld d, a
    ld a, (CursorY)
    ld e, a
	call ConvertPixelsToCells
	call CoordsToBoardIndex
	
	ld a, b
	ld (SuggestedBoardPosition), a
	call IsMoveValid
	ld (FlagIsValidMove), a
	pop de
	pop bc
	pop iy
	pop ix
	ret
	
 
	

PointerControlLoop:
    ld a, (CursorX)
    ld d, a
    ld a, (CursorY)
    ld e, a
	call ConvertPixelsToCells
    push de
	ld a, (CurrentPhase)
	cp PHASE_PAINT
	call z, UpdateCardOptions
	cp PHASE_MOVE
	call z, UpdateOkMoveFlag
  
  .redrawSprites:
    call RedrawSprites
	ld a, 254
  .delayLoop:
    dec a 
	nop
	nop
	nop
    cp 0
	jp nz, .delayLoop


  .mainLoop:
	ld a, 0
	call GTSTCK
	cp 0
	jp nz, .gotInput
	ld a, 1
	call GTSTCK
	cp 0
	jp nz, .gotInput
	ld a, 0
	call GTTRIG
	cp 0
	jp nz, .gotTrigger
	ld a, 1
	call GTTRIG
	cp 0
	jp nz, .gotTrigger
	jp .mainLoop
	
  .gotInput:
    ; Switch based on the mode.
	nop
	call MoveOnBoard
	ld b, a
	ld a, (CurrentPhase)
	cp PHASE_MOVE
	jp z, .movePhaseMove
	cp PHASE_PAINT
	jp z, .movePhasePaint
	jp .mainLoop
	
  .movePhaseMove:
	;; Check if we need to recompute valid moves.
	pop hl
	call WasChosenCellChanged
	push de
	cp 0
	jp nz, .reevaluateMove
	jp .redrawSprites
	
  .reevaluateMove:
    ;;
	call UpdateOkMoveFlag
	
	jp .mainLoop
  
	
  .movePhasePaint:
    ;; Check if we need to recompute valid cards.
	pop hl
	call WasChosenCellChanged
	push de
	cp 0
	jp nz, .reevaluatePaintCards
	jp .redrawSprites
	
  .reevaluatePaintCards:
    call CoordsToBoardIndex
	ld a, b
	ld (SuggestedBoardPosition), a
	call UpdateCardOptions
	jp .redrawSprites
	
	
  .gotTrigger:
    ld a, (CurrentPhase)
	;; TODO: CONTINUE FROM HERE
	ld a, (CurrentPhase)
	cp PHASE_MOVE
	jp z, .tryMoving
	ld a, (CurrentPhase)
	cp PHASE_PAINT
	jp z, .checkCardAvailability
	jp .mainLoop
	
  .tryMoving:
	;; Check if the selected position is OK for moving.
	;; TODO: The check
	ld a, (CursorX)
	ld d, a
	ld a, (CursorY)
	ld e, a
	
	call ConvertPixelsToCells
	
	call CoordsToBoardIndex
	
	ld a, b
	ld (SuggestedBoardPosition), a
	call IsMoveValid
	cp 0
	jp z, .mainLoop
	;; The move is OK.
	
	ld a, (SuggestedBoardPosition)
	ld b, a
	call BoardIndexToCoords
	ld a, (CurrentTurn)
	cp 1
	jp z, .moveP2
	ld ix, XCoords
	ld iy, YCoords
	jp .actualMove
  .moveP2:
    ld ix, XCoords + 1
	ld iy, YCoords + 1
	jp .actualMove
  .actualMove:
    ld a, d
	ld (ix), a
	ld a, e
	ld (iy), a
	
	
	;; Success: remove the heap top entry
	pop hl
	ret
	
  .checkCardAvailability:
    pop de
    call CoordsToBoardIndex
	ld a, b
	ld (SuggestedBoardPosition), a
	;;call UpdateCardOptions
	;;cp 0
	;;jp z, .redrawSprites
	;;ld a, 1
	ret

IsAnyMoveValid:
	;; Checks five cells to see if moving is an option.
	ld bc, 0
	ld a, (CurrentTurn)
	cp 0
	ld ix, XCoords
	ld iy, YCoords
	jp z, .loadCoords
	inc ix
	inc iy
  .loadCoords:
    ld a, (ix)
	ld d, a
	ld a, (iy)
	ld e, a
	;; Compare D0,D0
	call CoordsToBoardIndex
	push de
	ld a, b
	ld (SuggestedBoardPosition), a
	call IsMoveValid
	cp 1
	jp z, .retTrue
	;; Compare D0, D-1.
	pop de
	push de
	ld a, e
	cp 0
	jp z, .compD0Dp1
	dec e
	call CoordsToBoardIndex
	ld a, b
	ld (SuggestedBoardPosition), a
	call IsMoveValid
	cp 1
	jp z, .retTrue
  .compD0Dp1:
    pop de
	push de
	ld a, e
	cp 4
	jp z, .compDn1D0
	inc e
	call CoordsToBoardIndex
	ld a, b
	ld (SuggestedBoardPosition), a
	call IsMoveValid
	cp 1
	jp z, .retTrue
  .compDn1D0:
    pop de
	push de
	ld a, d
	cp 0
	jp z, .compDp1D0
	dec d
	call CoordsToBoardIndex
	ld a, b
	ld (SuggestedBoardPosition), a
	call IsMoveValid
	cp 1
	jp z, .retTrue
  .compDp1D0:
    pop de
	push de
    ld a, d
	cp 4
	jp z, .retFalse
	dec d
	call CoordsToBoardIndex
	ld a, b
	ld (SuggestedBoardPosition), a
	call IsMoveValid
	cp 1
	jp z, .retTrue
	jp .retFalse
	
  .retTrue:
    ld a, 1
    pop bc
	ret
  .retFalse:
    ld a, 0
	pop bc
	ret
	
	
	

	
IsMoveValid:
	;; In response, A = 0 if the move was not valid and A = 1, if the move was not valid.
	
	;; TODO: Prologue is borked!
	push ix
	push iy
	ld iy, 0
	add iy, sp
	ld ix, -10
	add ix, sp ;; New stack pointer.
	ld sp, ix 
	push iy ;; Save "original" SP
	
	ld a, (SuggestedBoardPosition)
	cp 25
	jp nc, .retFalse
	ld b, a
	
	call BoardIndexToCoords
	ld a, d
	ld (ix + 4), a ;; Target X
	ld a, e
	ld (ix + 5), a ;; Target Y
	ex de, hl
	
	ld a, (CurrentTurn)
	cp 0
	jp z, .step1
	
	ld a, (XCoords + 1)
	ld d, a
	ld a, (YCoords + 1)
	ld e, a
	jp .step10
  .step1:
	ld a, (XCoords)
	ld d, a
	ld a, (YCoords)
	ld e, a
	jp .step10
  .step10:  
	ld a, d
	ld (ix + 2), a ;; Token X
	ld a, e
	ld (ix + 3), a ;; Token Y
	
	;; Now, check the distance. DE = target coords, HL = token coords
	ld a, h
	sub d
	jp nc, .step11
	neg
  .step11:
    ld d, a
	ld a, l
	sub e
	jp nc, .step12
	neg
  .step12:
    add a, d
	cp 3
	jp nc, .retFalse ; No distance above 3 is OK.
	ld h, a ;; Store for later checks.
	
	ld iy, Board
	ld h, a
	ld bc, 0
	ld a, (SuggestedBoardPosition)
	ld c, a
	add iy, bc
	ld a, (iy)
	cp NO_CARD
	jp z, .suitOk
	ld iy, Suits
	ld c, a
	add iy, bc
	ld a, (iy)
	ld c, a
	call IsOwnSuit
	cp 0
    jp z, .retFalse	
	
  .suitOk:
    ;; Now check if we can move that far.
	ld a, h
	cp 2
	jp c, .retTrue ;; Dist 0 - always OK. Dist 1 - always OK, too.

	;; Dist is two, so we need to check that the either x0, y1 or x1, y0 is of own colour.

    ;; Check if either X or Y are the same. In such case, check the middle card.
    ld a, (ix + 2)
	ld d, a
	ld a, (ix + 4)
	sub d
	cp 0
	jp z, .check3
	ld a, (ix + 3)
	ld d, a
	ld a, (ix + 5)
	sub d
	jp z, .check3
	jp .check4
  .check3:
    ;; Add the two coordinates together, then div by 2.
	;; This'll give the mid-point.
    ld a, (ix + 2)
	ld d, a
	ld a, (ix + 4)
	add a, d
	srl a
	ld d, a
    ld a, (ix + 3)
	ld e, a
	ld a, (ix + 5)
	add a, e
	srl a
	ld e, a
	call CoordsToBoardIndex
	ld iy, Board
	ld c, b
	ld b, 0
	add iy, bc
	ld a, (iy) ;; A <- card
	cp NO_CARD
	jp z, .retFalse
	;; Get the card.
	ld iy, Suits
	ld c, a
	add iy, bc
	ld a, (iy) ;; A <- suite
	ld c, a
	call IsOwnSuit
	cp 1
	jp z, .retTrue
	jp .retFalse

  .check4:
	ld a, (ix + 2)
	ld d, a
	ld a, (ix + 5)
	ld e, a
	call CoordsToBoardIndex
	ld iy, Board
	ld c, b
	ld b, 0
	add iy, bc
	ld a, (iy) ;; A <- card
	cp NO_CARD
	;; NO_CARD is not OK.
	jp z, .check2
	;; Get the card.
	ld iy, Suits
	ld c, a
	add iy, bc
	ld a, (iy) ;; A <- suite
	ld c, a
	call IsOwnSuit
	cp 1
	jp z, .retTrue
	
  .check2:
	ld a, (ix + 4)
	ld d, a
	ld a, (ix + 3)
	ld e, a
	call CoordsToBoardIndex
	ld iy, Board
	ld c, b
	ld b, 0
	add iy, bc
	ld a, (iy) ;; A <- card
	cp NO_CARD
	;; NO_CARD is not OK.
	jp z, .retFalse
	;; Get the card.
	ld iy, Suits
	ld c, a
	add iy, bc
	ld a, (iy) ;; A <- suite
	ld c, a
	call IsOwnSuit
	cp 1
	jp z, .retTrue
  	jp .retFalse
	
  .retTrue:
	pop ix ;; Original SP
	ld sp, ix
	pop iy
	ld a, 1
	pop ix
	ret 
	
  .retFalse:
	pop ix ;; Original SP
	ld sp, ix
	pop iy
    ld a, 0
	pop ix
	ret


IsOwnSuit:
	;; A - will have 1 for True.
	;; C - the suit to compare
	ld a, (CurrentTurn)
	cp 0
	jp z, .p1
  .p2:
    ld a, c
	cp 2
	jp nc, .ok
	jp .notOk
  .p1:
    ld a, c
	cp 2
	jp nc, .notOk
	jp .ok
  .ok:
	ld a, 1
	ret
  .notOk:
	ld a, 0
	ret
	

	
UpdateCardOptions:
  .prologue:
	push ix ;; Push the original IX.
	push iy ;; Push the original IY
    ld iy, 0
	add iy, sp
	
	ld ix, -10 ;; 8 bytes for local variables.
	add ix, sp ;;
	ld sp, ix  ;
	push iy ;; Original SP 
	
	;; (ix + 7): valid cards met
	;; (ix + 6): card counter
	;; (ix + 3, ix + 3): VRAM address
	;; (ix + 4, ix + 5): Current card pointer
	call CoordsToBoardIndex
	ld a, b
	ld (SuggestedBoardPosition), a
    call CurrentHandPtrToIY
	
	ld (ix + 7), a
	ld l, 188
	ld (ix + 3), l ; (1800+188) / 256
	ld h, 24
	ld (ix + 2), h ; (1800+188) % 256
	ld a, 0
	ld (ix + 6), a
	ld (ix + 7), a
  .loopHand:
	ld a, (iy)
	ld (SuggestedCard), a
	inc iy
	call IsOkPainting
	cp 0
	jp z, .clear
	;; Mark that we can paint this card.
	ld a, (ix + 7)
	inc a
	ld (ix + 7), a
	ld a, CHR_CHECKMARK
	
	jp .next
  .clear:
    ;; Mark that we cannot paint this card.
	ld a, 32
	jp .next
  .next:
	ld l, (ix + 3)
	ld h, (ix + 2)
    call WRTVRM
	ld a, (ix + 6)
	cp 4
	jp z, .epilogue
	inc a
	ld (ix + 6), a
	ld bc, 32
	add hl, bc
	ld (ix + 3), l
	ld (ix + 2), h
	
	jp .loopHand


  .epilogue:
    ld a, (ix + 7)
    pop iy ;; Load the initial SP to IY.
    ld sp, iy
	pop iy
    pop ix
	ret
	
    
    
	
	
CurrentHandPtrToIY:
	push af
	ld iy, Hands
	ld a, (CurrentTurn)
	cp 0
	jp z, .done
	push bc
	ld bc, 5
	add iy, bc
	pop bc
  .done:
	pop af
	ret

WasChosenCellChanged:
	;; HL - previous X and Y cell coordinates
	;; OUT: A, 0 if no change.
	;; OUT: HL, cell X in H, cell Y in L.
	ld a, (CursorX)
	ld d, a
	ld a, (CursorY)
	ld e, a
	call ConvertPixelsToCells
	ld a, d
	cp h
	jp nz, .reevaluateMove
	ld a, e
	cp l
	jp nz, .reevaluateMove	
	ld a, 0
	ret
  .reevaluateMove:
    ld a, 1
	ret

	
	
	
	
	
	
	
	
	


	

ConvertPixelsToCells:
	;; Converts pixel coordinates to board coordinates.
	;; D - X, E - Y coordinate
	;; In response, D, E are the board coordinates.
	
	push af
	ld a, D
	sub a, 8
	;; Divide by 32 = a**5
	srl a
	srl a
	srl a
	srl a
	srl a
	ld d, a
	ld a, e
	sub a, 8
	srl a
	srl a
	srl a
	srl a
	srl a
	ld e, a
	pop af
	ret
  
	
	
MoveOnBoard:
	;; Moves the cursor on the screen.
	;; A - the joystick direction
    ld c, a
	ld ix, XSpeed
	ld iy, YSpeed
	ld b, 0
	add ix, bc
	add iy, bc
	
	ld a, (ix)
	ld b, a
	ld a, (iy)
	ld c, a
	ld a, (CursorX)
	add a, b
	;; Check that X is within limits
	cp 160
	jp c, .xHalfOk
	ld a, 160
	jp .xOK
  .xHalfOk:
    cp 8
	jp nc, .xOK
	ld a, 8
	jp .xOK
  .xOK:
    ld (CursorX), a
	
	ld a, (CursorY)
	add a, c 
	;; Check that Y is within limits.
	cp 160
	jp c, .yHalfOk
	ld a, 160
	jp .yOK
  .yHalfOk:
    cp 8
	jp nc, .yOK
	ld a, 8
	jp .yOK
  .yOK:
    ld (CursorY), a
	ret	

	
CountScore:
	;; Counts the final score.
	ld ix, 0
	add ix, sp
	ld iy, -6
	add iy, sp
	ld sp, iy
	push ix
	ld bc, 0
	ld a, 0
	ld (iy), 0 ;; Player 1 
	ld (iy+1), 0 ;; Player 2
	ld de, 0
	ld hl, 0
	
	
  .loop:
    ld ix, Board
	add ix, bc
	ld a, (ix)
	ld l, a
	cp NO_CARD
	jp z, .next
	ld ix, BoardStacks
	add ix, bc
	ld a, (ix)
	cp 1
	jp z, .threePoints
	cp 2
	jp z, .twoPoints
	jp .onePoint
	
  .threePoints:
	ld e, 3
	jp .findRecipient
  .twoPoints:
    ld e, 2
	jp .findRecipient
  .onePoint:
    ld e, 1
	jp .findRecipient
  
  .findRecipient:
    ld ix, Suits
	push bc
	ld b, h
	ld c, l
	add ix, bc
	ld a, (ix)
	srl a
	pop bc
	cp 0
	jp z, .player1
	ld a, (iy + 1)
	add a, e
	ld (iy + 1), a
	jp .next
  .player1:
    ld a, (iy)
	add a, e
	ld (iy), a
	jp .next
  .next:
    inc c
	ld a, c
	cp 25
	jp z, .end
	jp .loop
	
  .end:
    ld d, (iy)
	ld e, (iy + 1)
    pop ix
	ld sp, ix
    ret
	
	
	
InitializeGraphics:
	; Loads the characters from RAM to VRAM.
	call BoldFont
	
	ld hl, _CUSTOMCHARS
	ld bc, 448 + 16
	ld de, 96 * 8; Start from one before 'a'. 16 red characters, 16 black, bunch with board bg
	call LDIRVM
	ld bc, 8
	ld de, 8204
	ld hl, _CUSTOMCLRS
	call LDIRVM
	
	ld a, 0
	ld (_REND_HELP), a
	;jp .spritesDone
	
	call CALPAT
	ex de, hl
	ld ixh, d
	ld ixl, e
	push ix
  .nextSprite:  ;; d274
	ld a, (_REND_HELP)
	call CALPAT
	ex de, hl
	; Copy the sprites.
	ld iy, SPRITE_ADDRS
	ld a, (_REND_HELP)
	ld b, 0
	ld c, a ;BC has sprite index now.
	sla a
	ld c, a
	add iy, bc ;  now has the address for the proper sprite.
	ld a, (iy)
	ld l, a
	ld a, (iy + 1)
	ld h, a
	ld bc, 32

	pop ix
	add ix, bc
	push ix
	
	
	CALL LDIRVM
	ld a, (_REND_HELP)
	cp 5
	jp z, .spritesDone
	inc a
	ld (_REND_HELP), a
	jp .nextSprite

  .spritesDone:
    pop ix
		
	;call DisplayFirstGameScreen
	ret

Checkpoint:
	push af
	push bc
	push de
	push hl
	push ix
	push iy
	call WaitTurnSwitch
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af
	ret
	
DisplayStatus:
    ; Displays the status data on the right side of the screen.
	; Hopefully redundant drawing of text here.
	; Let's print the static parts first.

	;call Checkpoint	

	ld hl, STR_PLAYER
	ld de, DISP_START + 54
	ld bc, 6
	call LDIRVM
	
	ld a, (CurrentTurn)
	add a, 49
	ld hl, DISP_START + 61
	call WRTVRM

	;call Checkpoint	
	
	ld hl, STR_HAND
	ld de, DISP_START + 150
	ld bc, 4
	call LDIRVM

	;call Checkpoint	

	ld hl, STR_DECK_LEFT
	ld de, DISP_START + 374 + 32
	ld bc, 9
	call LDIRVM
	

	ld hl, STR_SCORE
	ld bc, 6
	ld de, $1800 + 22 + 17 * 32
	call LDIRVM
	
	
	ld c, 49
	ld b, 5
	ld hl, DISP_START + 182
	ld de, 31

	
  .handLoop: ; Prints the hand numbers, not cards.
    ld a, c
	push af
	push bc
	push de
	push hl
    call WRTVRM
	pop hl
	pop de
	pop bc
	pop af
	inc c
	ld a, 58 ; asc(":")
    inc hl
	call WRTVRM
	add hl, de
	;call Checkpoint	
	djnz .handLoop
	
	
    ld a, (CurrentPhase)
	ld de, DISP_START + 470 + 32
	cp PHASE_PAINT
	jp z, .inPhasePaint
	cp PHASE_PAINT_PICK_CARD
	jp z, .inPhasePaintPickCard
	cp PHASE_MOVE
	jp z, .inPhaseMove
	cp PHASE_WAIT
	jp z, .inPhasePass
	cp PHASE_CPU
	jp z, .inPhaseCPU
	
  .inPhaseCPU:
    ld hl, STR_CPU
	jp .renderPhase
  .inPhasePaint:
    ld hl, STR_PAINT
	jp .renderPhase
  .inPhasePaintPickCard:
    ld hl, STR_PAINT_PICK_CARD
	jp .renderPhase
  .inPhaseMove:
    ld hl, STR_MOVE
	jp .renderPhase
  .inPhasePass:
    ld hl, STR_PASS_ON
	jp .renderPhase
  
  .renderPhase:
	;call Checkpoint	
    ld bc, 10
	call LDIRVM
    ld a, (CurrentPhase)
	
	;; If the phase is PHASE_PAINT_PICK_CARD, don't clear.
	cp PHASE_PAINT_PICK_CARD
	jp z, .postHandRender
	
	; Clear the displayed hand.
	ld bc, 5
	ld de, DISP_START + 184
	ld hl, STR_BLANK
	call LDIRVM
    ld bc, 5
	ld de, DISP_START + 216
	ld hl, STR_BLANK
	call LDIRVM
    ld bc, 5
	ld de, DISP_START + 248
	ld hl, STR_BLANK
	call LDIRVM
    ld bc, 5
	ld de, DISP_START + 280
	ld hl, STR_BLANK
	call LDIRVM
    ld bc, 5
	ld de, DISP_START + 312
	ld hl, STR_BLANK
	call LDIRVM
    ld bc, 6
	ld de, DISP_START + 342
	ld hl, STR_BLANK
	call LDIRVM
    ; If phase is PASS ON, do not draw the hand.
    ; If active player is CPU, do not draw the hand.
    ld a, (CurrentPhase)
    cp PHASE_WAIT
    jp z, .postHandRender
    ld a, (CurrentTurn)
	ld b, 0
	ld c, a
    ld ix, PlayerTypes
	add ix, bc
	ld a, (ix)
	cp 1
	jp z, .postHandRender
	
	; Render the hand.
	
	ld de, DISP_START + 342
	ld hl, STR_CANCEL
	ld bc, 6
	call LDIRVM
	
	ld a, (CurrentTurn)
	ld bc, 0
	ld ix, Hands
	cp 0
	jp z, .beginHandLoop
	ld c, 5 ; Hand size
	add ix, bc
	
  .beginHandLoop: ; IX: hand start
    ld a, 0
	ld (_REND_HELP), a ; Card in hand to draw
	ld iy, DISP_START + 184 - 32; Start for first card.
	ld a, 0
	push ix ; Pointer to current card in hand.
	push iy ; Display position

  .continueHandLoop:
    ; Get the card suite.
	ld a, (_REND_HELP)
	inc a
	ld (_REND_HELP), a
	
	pop hl ; Update display position
	pop ix
	ld bc, 32
	ld a, (ix)
	ld (_REND_HELP + 1), a ; Card index
	inc ix
	add hl, bc
	push ix ; Updated card index.
	push hl
	
	cp NO_CARD ; No card to draw, quit.
	jp z, .renderDeckSize
	
	ld b,0
	ld ix, Suits
	ld iy, Values
	ld c, a
	add ix, bc
	add iy, bc
	ld a, (ix)
	ld (_REND_HELP + 2), a ; Card suit
	ld a, (iy)
	ld (_REND_HELP + 3), a ; Card value


	
	ld a, (_REND_HELP + 2) ; Card suit.
	add a, CHR_SUIT_START
	inc hl ; Add 2 to get the VRAM address for the suit
	inc hl
	call WRTVRM ; Draw the suit.
	
	ld a, CHR_EMPTY_CARD ; Reset the background.
	ld (TGFX_LINE1), a
	ld (TGFX_LINE1 + 1), a
	
	; The first character.

	ld bc, 0
	ld d, CUSTOMCHAR_START
	ld a, (_REND_HELP + 2)
	cp 2
	jp c, .printChar1
	ld d, CUSTOMCHAR_START + 18 ; D contains the offset
	
  .printChar1:
    ld hl, CardNames
	ld a, (_REND_HELP + 3)
	sla a
	ld b, 0
	ld c, a
	add hl, bc
	ld a, (hl)
	cp 48
	jp z, .printChar2
	add a, d
	ld (TGFX_LINE1), a
	jp .printChar2
  .printChar2:
    inc hl
	ld a, (hl)
	cp 48
	jp z, .renderCardName
	add a, d
	ld (TGFX_LINE1 + 1), a
	jp .renderCardName
  .renderCardName:
	pop de
	push de
	ld hl, TGFX_LINE1
	ld bc, 2
	call LDIRVM
	
	ld a, (_REND_HELP)
	cp 5
	jp nz, .continueHandLoop
	
	jp .renderDeckSize

  .renderDeckSize:
    pop de ; Remove the unnecessary entries.
	pop de 
    ld bc, DeckLen
	ld a, (CurrentTurn)
	call AddABC
	ld a, (bc) ; Deck size
	
	; Deck size is 0 to <52. Divide B by 10.
	ld d, a
	ld b, 0
	ld c, 0

  .loopDeckLeftCounter:
    cp 10
	jp c, .loopDeckRightCounter 
	sub a, 10
	inc b
	jp .loopDeckLeftCounter
  .loopDeckRightCounter:
    ld c, a
	
	ld a, 48
	add a, b
	ld (TGFX_LINE1), a
	ld a, 48
	add a, c
	ld (TGFX_LINE1 + 1), a
	ld hl, TGFX_LINE1
	ld bc, 2
	ld de, DISP_START + 409 + 32
	call LDIRVM
	ld hl, STR_DECK_LEFT
	ld bc, 9
	ld de, DISP_START + 22 + 11*32 + 32
	call LDIRVM
	
	
  .postHandRender:
	;; Update the score display.
    call CountScore
	ex de, hl

	call UpdateScore
	ld hl, ENDSCREEN_SCORE + 5
	ld bc, 7
	ld de, $1800 + 23 + 18 * 32
	call LDIRVM


	ret
	jp .postHandRender

AddABC: ; Adds A to BC; A is lost.
    add a, c
	ld c, a
	ret nc
	inc b
	ret
	

IsOkPainting: ; UNTESTED.
    ; Checks CurrentTurn, SuggestedBoardPosition, SuggestedCard
	; Ends: A<- 0 if invalid combination, but if OK, then A<- 1
	push af
	push bc
	push de
	push hl
	push ix
	push iy
	ld ix, XCoords
	ld iy, YCoords
	
	ld a, (CurrentTurn)
	cp 0
	jp z, .compDist
	inc ix
	inc iy
	
  .compDist:
    ld a, (ix)
	ld d, a
	ld a, (iy)
	ld e, a
	ld h, d ;; H = token X
	
	call CoordsToBoardIndex
	ld a, (SuggestedBoardPosition)
	sub b
	cp 0
	jp z, .posOk
	cp 1
	jp z, .diff1
	cp 5
	jp z, .posOk
	cp -5
	jp z, .posOk
	cp -1
	jp z, .diff1
	jp .notOk
	
  .notOk:
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af
    ld a, 0
	ret
	
  .diff1: ; Check if the X-coordinates match. If yes, then everything's good.
    ld a, (SuggestedBoardPosition)
	ld b, a
	
	call BoardIndexToCoords
	ld a, d
	sub h
	cp 0
	jp z, .posOk
	jp .notOk
	
  .posOk: ; Compare the cards.
    ld b, 0
	ld a, (SuggestedCard)
	cp NO_CARD
	jp z, .notOk ; Do not allow empty card to be placed.
	ld a, (SuggestedBoardPosition)
	ld c, a
	ld h, a
	ld ix, Board
	add ix, bc
	ld a, (ix) ; Load the card index.
	ld l, a
	cp NO_CARD
	jp z, .allOk ; Can place any card in an empty cell.
	; Check if the new value is better.
	ld c, a
	ld ix, Values
	add ix, bc
	ld a, (ix) ; Load the value of the card on the table.
	ld h, a ; and store it in H.
	ld ix, Values
	ld a, (SuggestedCard)
	ld c, a
	add ix, bc
	ld a, (ix) ; Load the value of the suggested card.
	sub h
	jp c, .notOk ; Lesser
	jp z, .notOk ; and equal do not suffice.
	
	ld ix, Suits
	ld c, l
	add ix, bc
	ld a, (ix)
	srl a
	ld b, a ; B<- 0 (for red cards) or 1 (for black cards)
	ld a, (CurrentTurn)
	cp b
	jp z, .notOk ; Cannot repaint own cards.
	
  .allOk:
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af

  
    ld a, 1
	ret
	

Initialize: ; Initialize the overall game, incl. gfx
	call InitializeGame
	call InitializeGraphics
	call DisplayStatus
	ret
	

; ***************************************************
; Finished routines
; ***************************************************


DisplayFirstGameScreen:
	call DisplayBoardStatics

  ; Add fake cards.
	ld ix, Board
	ld iy, BoardStacks

    ; Loop drawing
	ld a, 24
  .drawBoardLoop:
	ld (SuggestedBoardPosition), a
	call DisplayCardOnBoard
	ld a, (SuggestedBoardPosition)
	dec a
	ld (SuggestedBoardPosition), a
	cp -1
	jp nz, .drawBoardLoop
	ret
	

	
DisplayBoardStatics:
    ; Draw the unchanging characters on the table.
	ld ixh, 19
    ld hl, $1800 + 32
	ld de, 32
  .horizBorderLoop:
	ld a, CHR_STACK1_MID
	CALL WRTVRM
	add hl, de
	dec ixh
	ld a, ixh
	cp 0
	jp nz, .horizBorderLoop
	
	
	ld hl, TGFX_TOPROW ; First the topmost row.
	ld bc, 21
	ld de, $1800
	CALL LDIRVM
	ld hl, TGFX_MIDROW
	ld de, $1800 + 128
	ld bc, 21
	CALL LDIRVM
	ld de, $1800 + 256
	ld hl, TGFX_MIDROW
	ld bc, 21
	CALL LDIRVM
	ld de, $1800 + 384
	ld hl, TGFX_MIDROW
	ld bc, 21
	CALL LDIRVM
	ld de, $1800 + 512
	ld hl, TGFX_MIDROW
	ld bc, 21
	CALL LDIRVM
	ld de, $1800 + 640
	ld hl, TGFX_BTMROW
	ld bc, 21
	CALL LDIRVM
	ret
	
DisplayCardOnBoard:
	; SuggestedBoardPosition - which slot to draw.
	
	; Check the card we're drawing
	ld ix, Board
	ld a, (SuggestedBoardPosition)
	ld c, a
	ld b, 0
	add ix, bc
	ld a, (ix)
	cp a, NO_CARD
	jp z, .emptySlot
	
	; Check stack size, draw the card backs.
    ld ix, BoardStacks
	add ix, bc
	ld a, (ix)
	ld iy, TGFX_LINE1
	cp 1
	jp z, .stackLenOne
	cp 2
	jp z, .stackLenTwo
	jp .stackLenThreePlus
	
	; Stack len 3 or more
  .stackLenThreePlus:
    ld a, CHR_STACK3_TOP
	ld (iy + 3), a
    ld a, CHR_STACK3_MID
	ld (iy + 7), a
    ld a, CHR_STACK3_BTM
	ld (iy + 11), a
	jp .paintBorders
  .stackLenOne:
    ld a, CHR_STACK1_TOP
	ld (iy + 3), a
    ld a, CHR_STACK1_MID
	ld (iy + 7), a
    ld a, CHR_STACK1_BTM
	ld (iy + 11), a
	jp .paintBorders
  .stackLenTwo:
    ld a, CHR_STACK2_TOP
	ld (iy + 3), a
    ld a, CHR_STACK2_MID
	ld (iy + 7), a
    ld a, CHR_STACK2_BTM
	ld (iy + 11), a
	jp .paintBorders
  
  .paintBorders:
  ; Now, draw the insides for the card.
    ld a, CHR_EMPTY_CARD ; Fill the background with card white
	ld (iy + 2), a
	ld (iy + 4), a
	ld (iy + 6), a
	ld (iy + 8), a
	ld (iy + 9), a
	ld (iy + 10), a
    ld ix, Board
	ld b, 0
	ld a, (SuggestedBoardPosition)
	ld c, a
	add ix, bc
	ld a, (ix)
	ld b, 0
	ld c, a
	ld hl, Board
	add hl, bc
	ld a, (hl) ; Load the card#.
	ld (_REND_HELP), a ; Card index
	
	
	ld hl, Values
	add hl, bc
	ld a, (hl)
	ld (_REND_HELP + 2), a ; Card value
	
	ld hl, Suits
	add hl, bc
	ld a, (hl) ; Load the card suite
	ld (_REND_HELP + 1), a ; Suite
	
	add a, CHR_SUIT_START ; Point to correct suite.
	ld ix, TGFX_LINE2
	ld (ix + 1), a

	; Then the two-letter card value.
	ld a, (_REND_HELP + 2) ; Load card value.
	sla a ; Multiply by two to get offset in the name table
	ld b, 0
	ld c, a
	ld iy, CardNames
	add iy, bc
	ld bc, 0
	ld a, (_REND_HELP + 1)
	ld h, 0
	cp 2 
	jp c, .noMoreShifts ; No need to add.
	ld h, 18 ; Shift the offset position this much.
  .noMoreShifts:
    ld a, h
	add a, CUSTOMCHAR_START
	ld h, a ; Now points to the suite-specific start.
	
    ; Now, render the characters. 48 = no drawing.
	ld ix, TGFX_LINE1
	ld a, CHR_EMPTY_CARD
	ld (ix), a
	ld (ix + 1), a
	ld a, (iy)
	cp 48
	jp z, .char2
	add a, h
	ld (ix), a
  .char2:
    ld a, (iy + 1)
	cp 48
	jp z, .renderCard
	add a, h
	ld (ix + 1), a
	jp .renderCard

  .mulDE2: ; Multiply DE by 2. 
    sla d
	sla e
	ret nc
	inc d
	ret
	

  .renderCard: ; Determine the VRAM addresses to copy the memory template.
    ld a, (SuggestedBoardPosition)
	ld b, a
	call BoardIndexToCoords; D, E = table X, Y
	; Y * 32 + X * 4
	; (4 * Y + 1) * 32 + (1 + X * 4)
	ld b, 0
	ld c, d ;; X 
	sla c ; Multiply by two
	sla c ; Make that four
    inc c ; Apply offset	
	;ld e, d
	ld d, 0
	sla e ; Multiply Y by 4
	sla e
	inc e ; Apply offset +1
	; Bits 0-5 are now active.
	call .mulDE2 ; Multiply by 32 = 2**5
	call .mulDE2
	call .mulDE2
	call .mulDE2
	call .mulDE2
	
    ld hl, $1800 ; Base for characters
	add hl, de
	add hl, bc
	; Copy the three rows to VRAM.
	ld bc, 4
	ex de, hl
	push de
	ld hl, TGFX_LINE1
	call LDIRVM
	
	; add 32 to HL.
	ld bc, 32
	pop hl
	add hl, bc
	push hl
	ex de, hl
	ld bc, 4
	ld hl, TGFX_LINE2
	call LDIRVM

	; add 32 to HL.
	ld bc, 32
	pop hl
	add hl, bc
	ex de, hl
	ld bc, 4
	ld hl, TGFX_LINE3
	call LDIRVM
	
	ret
	
  .emptySlot: ; Prepare an empty slot for drawing.
    ld a, CHR_EMPTY_BOARD
    ld ix, TGFX_LINE1
	ld (ix), a
	ld (ix+1), a
	ld (ix+2), a

	ld (ix+4), a
	ld (ix+5), a
	ld (ix+6), a

    ld (ix+8), a
	ld (ix+9), a
	ld (ix+10), a
	ld a, CHR_STACK1_TOP
	ld (ix+3), a
	ld a, CHR_STACK1_MID
	ld (ix+7), a
    ld a, CHR_EMPTY_BOARD
	ld a, CHR_STACK1_BTM
	ld (ix+11), a
	jp .renderCard
	
	


BoardIndexToCoords:
	; b = index
	; In response, D = x, E = y
	; index = y * 5 + x
	ld de, 0
	ld a, b
  .xLoop:
    cp 5
	jp c, .yLoop
	sub a, 5
	inc d
	jp .xLoop
  .yLoop:
    ld e, a
	ret

CoordsToBoardIndex:
    ; D, E = x, y
	; In response, b = index
	; Resets register c.
	ld b, e
	ld a, d
	
  .xLoop:
    cp 0
	ret z
	dec a
	inc b ; Do this better.
	inc b
	inc b
	inc b
	inc b	
	jp .xLoop
	

DrawCard: ; Draws a card if possible from the deck.
	ld a, (CurrentTurn)
	cp 0
	jp z, .wasP1
	
  .wasP2:
	ld a, (DeckLen + 1)
	cp 0
	ret z
	
	ld b, 0
	ld c, a
	push bc ; Contains the deck length
	ld de, Decks + DeckSize
	push de ; Deck 0
	ld ix, Hands + HandSize
	push ix ; Hand 0
	ld a, (HandLen + 1)
	; A = hand size.
	jp .findHandCard
	
  .wasP1:
	ld a, (DeckLen)
	cp 0
	ret z
	ld b, 0
	ld c, a
	push bc ; Contains the deck length
	ld hl, Decks
	push hl ; Deck 0
	ld de, Hands
	push de ; Hand 0
	ld a, (HandLen)
	jp .findHandCard
  
  .findHandCard:
	ld b, a
	pop de ; Get hand 0
	cp 0
	jp z, .pastLoopHand
  .loopHand:
    inc de
	djnz .loopHand

  .pastLoopHand:
	pop hl ; Get deck 0
    pop bc ; Get deck length.
	ld a, c
	dec a ; if len=1, no moving forward.
	cp 0
	jp z, .copyCard
	ld b, a
	
  .loopDeck:
    inc hl
	djnz .loopDeck
  
  .copyCard:
    ldi
	ld hl, DeckLen
	ld ix, HandLen
	ld b, 0
	ld a, (CurrentTurn)
	ld c, a
	add hl, bc
	add ix, bc
	
	ld a, (hl) ; Decrease deck length
	dec a
	ld (hl), a
	ld a, (ix) ; Increase hand length
	inc a
	ld (ix), a
	ret
	
InitializeGame:
	; Initialize the game variables for a new game.
	ld a, 0
	ld (CurrentTurn), a
	ld ix, HandLen ; Reset the hand sizes.
	ld (ix), 0 
	ld (ix + 1), 0
	
	ld ix, DeckLen
	ld (ix), 26
	ld (ix + 1), 26
	
	ld b, 25 ; 5*5 table, reset all of them.
	ld ix, Board
	ld a, NO_CARD

  .boardLoop: ; Clear the table.
    ld (ix), a
	inc ix
	djnz .boardLoop

	ld ix, BoardStacks
	ld a, 0
	ld b, 25
  .stackLoop:
    ld (ix), 0
	inc ix
	djnz .stackLoop
	
	
	ld a, 0 ; Reset the player tokens.
	ld (XCoords), a
	ld (YCoords + 1), a
	ld a, 4
	ld (Xcoords + 1), a
	ld (YCoords), a


	;; Shuffle the cards
	ld bc, Decks
	ld d, 26
	call ShuffleValues
	ld bc, Decks + 26
	ld d, 26
	call ShuffleValues
	
	
	
    ld a, 1
    ld (CurrentTurn), a
    call DrawCard
    call DrawCard
    call DrawCard
    call DrawCard
  
    ld a, 0
    ld (CurrentTurn), a
    call DrawCard
    call DrawCard
    call DrawCard
    call DrawCard
	
	ld a, 1
	ld (CurrentPhase), a
	  
	ret	


WRTPSG: equ $0093
SFXPlayCard:
	;; Sound for playing a card on the table.
	;; 8->16
	;; 7->&b10110111
	;; 12->3
	;; 6->4
	;; 13 -> &b0111
	ld a, 8
	ld e, 16
	call WRTPSG
	ld a, 7
	ld e, %10110111
	call WRTPSG
	ld a, 12
	ld e, 3
	call WRTPSG
	ld a, 6
	ld e, 4
	call WRTPSG
	ld a, 13
	ld e, %100
	call WRTPSG
	ret
	
SFXPassToNext:
	;; Sound to tell to pass to another player.
	;; 8->16
	;; 7->&b10111110
	;; 12->25
	;; 0, 104
	;; 1, 0
	;; 13, &b0011
	ld a, 8
	ld e, 16
	call WRTPSG
	ld a, 7
	ld e, %10111110
	call WRTPSG
	ld a, 12
	ld e, 25
	call WRTPSG
	ld a, 0
	ld e, 104
	call WRTPSG
	ld a, 1
	ld e, 0
	call WRTPSG
	ld a, 13
	ld e, 3
	call WRTPSG
	ret
	
	
	

	

;; More constants
DeckSize: EQU 26
NO_CARD: EQU 99
HandSize: EQU 5
B_WIDTH: EQU 5
B_HEIGHT: EQU 5
CHR_EMPTY_BOARD: equ 142
CHR_CHECKMARK: equ 145

CHR_SUIT_START: EQU 110
CHR_EMPTY_CARD: EQU 144

CHR_STACK1_TOP: EQU 129
CHR_STACK1_MID: EQU 129
CHR_STACK1_BTM: EQU 129

CHR_STACK2_TOP: EQU 131
CHR_STACK2_MID: EQU 132
CHR_STACK2_BTM: EQU 133

CHR_STACK3_TOP: EQU 134
CHR_STACK3_MID: EQU 135
CHR_STACK3_BTM: EQU 136


DISP_START: EQU $1800	
	
CardNames: DB 10, 48, 1, 48, 2, 48, 3, 48, 4, 48, 5, 48, 6, 48, 7, 48, 8, 48, 0, 9, 11,48, 12,48, 13, 48

XSpeed: DB 0, 0, 1, 1, 1, 0, -1, -1, -1
YSpeed: DB 0, -1, -1, 0, 1, 1, 1, 0, -1
Suits: DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
Values: DB 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
PAINT_OPTS: db 0, -1, 1, 5, -5
MOVEMENT_OPTS: db -1, 1, 5, -5, -10, 10, 6, -6, 4, -4, -2, 2, 0
MOVEMENT_OPT_LEN: equ 13
PAINT_OPTS_LEN: equ 5
StackPoints: db 3, 5, 3, 2
CUSTOMCHAR_START: equ 96
_CUSTOMCLRS: db $6f, $6f, $1f, $1f, $1b, $1b, $1f, $1f ;

_SPRITE0: INCBIN "torppo_sprites.txt.0.bin"

_SPRITE1: INCBIN "torppo_sprites.txt.1.bin"

_SPRITE2: INCBIN "torppo_sprites.txt.2.bin"

_SPRITE3: INCBIN "torppo_sprites.txt.3.bin"

_SPRITE4: INCBIN "torppo_sprites.txt.4.bin"


SPRITE_ADDRS: DW _SPRITE0, _SPRITE1, _SPRITE2, _SPRITE3, _SPRITE4

TGFX_BTMROW: DB 138, 137, 137, 137, 142, 137, 137, 137, 142, 137, 137, 137, 142, 137, 137, 137, 142, 137, 137, 137, 139
TGFX_TOPROW: DB 140, 128, 128, 128, 137, 128, 128, 128, 137, 128, 128, 128, 137, 128, 128, 128, 137, 128, 128, 128, 141
TGFX_MIDROW: DB 138, 128, 128, 128, 142, 128, 128, 128, 142, 128, 128, 128, 142, 128, 128, 128, 142, 128, 128, 128, 139

_CUSTOMCHARS: INCBIN "torppo_chars.txt.bin"

STR_DECK_LEFT: db       "DECK LEFT"
STR_PAINT: db           "PICK TILE "
STR_PAINT_PICK_CARD: db "PICK CARD "
STR_MOVE: db            "MOVE      "
STR_PASS_ON: db         "PASS ON   "
STR_HAND: db "HAND"
STR_PLAYER: db "PLAYER"
STR_BLANK: db           "          "
STR_CPU: db             "CPU: HMM.."
STR_CANCEL: db          "CANCEL"
STR_SCORE: db           "SCORE "


TITLE_1: DB 110, 110, 110, 110, 110, 110, 147,  32,  32,  32,  32,  32,  32,  32,  32,  32, 146, 113, 113, 113, 113, 147,  32,  32,  32,  32
TITLE_2: DB  32,  32, 110, 110,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32, 113, 113,  32,  32,  32,  32,  32,  32,  32,  32
TITLE_3: DB  32,  32, 110, 110,  32, 146, 112,  32,  32, 112,  32, 111, 146, 111, 111, 147, 113, 113, 113, 147, 146, 110,  32,  32, 146, 112
TITLE_4: DB  32,  32, 110, 110,  32, 112, 112,  32,  32, 112,  32, 111, 111, 111, 147,  32, 113, 113, 147,  32, 110, 110,  32,  32, 112, 112
TITLE_5: DB  32,  32, 110, 110,  32, 112, 112,  32, 152, 112, 150, 111, 111,  32,  32, 152, 113, 113,  32,  32, 110, 110,  32, 146, 112, 112
TITLE_6: DB  32,  32, 110, 110,  32, 112, 112,  32, 153, 112, 151, 148, 111, 150,  32, 153, 113, 147,  32,  32, 148, 110, 110, 110, 112, 147
TITLE_7: DB  32,  32, 110, 110,  32, 112, 112, 146, 112, 112, 112,  32, 111, 151,  32, 113, 113,  32,  32,  32,  32,  32, 146, 112, 112,  32
TITLE_8: DB  32,  32, 110, 110,  32, 148, 112, 112, 112, 147, 112,  32, 148, 111,  32, 113, 113,  32,  32, 148, 112, 112, 112, 112, 147,  32


HELP_VIEW: 
DB " PHASE 1, PAINTING:             "
DB "   PLAY ANY CARD ON EMPTY       " 
DB "   TILES.                       "
DB "   PLAY A HIGHER CARD OVER      "
DB "   OPPONENT'S CARDS, ACES LOW.  "
DB "   OR, DISCARD A CARD.          "
DB "                                "  
DB " PHASE 2, MOVING:               "
DB "   NEIGHBORS ONLY, NOT ONTO     "
DB "   OPPONENT CARDS.              "  
DB "   TWO-STEP JUMPS OK WHEN       "
DB "   MOVING FIRST OVER OWN CARD.  "
DB "                                "
DB " SCORING:                       "
DB "   THE CARD ON TOP DECIDES WHO  "
DB "   GETS THE POINTS FROM EACH    "
DB "   TILE.                        "
DB "                                "
DB "   STACK OF 1 CARD:     3 PTS   "
DB "   STACKS OF 2:         2 PTS   "
DB "   STACKS OF 3 OR MORE: 1 PT    "
DB "                                "
DB "   PRESS SPACE/FIRE TO RETURN   "
	


;GUIDE_PAINT_TILE: db      " CHOOSE TILE TO PLACE CARD ON "
;GUIDE_PAINT_PICK_CARD: db "     CHOOSE CARD TO PLAY      "
;GUIDE_MOVE:            db "     CHOOSE WHERE TO MOVE     "
;GUIDE_NO_MOVE:         db "  NO MOVE IS POSSIBLE - PASS  "
;GUIDE_PASS:            db "       SWITCH PLAYER          "
	
ENDSCREEN_EMPTY:  db "                  "
ENDSCREEN_TITLE:  db "   GAME FINISHED  "
ENDSCREEN_TIE:    db "       TIED!      "

PHASE_WAIT: EQU 1
PHASE_MOVE: EQU 2
PHASE_PAINT: EQU 3
PHASE_PAINT_PICK_CARD: EQU 4
PHASE_DRAW: EQU 5
PHASE_CPU: EQU 6


copyROMtoRAM:
	ld bc, end_ROM_copy - start_ROM_copy
	ld de, RAMtop
	ld hl, start_ROM_copy
    ldir
	ret
	
start_ROM_copy:
	ROMDecks: DB 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51
	ROMHands: DB 99, 99, 99, 99, 99, 99, 99, 99, 99, 99 ; Hand, like Decks; 5 + 5 cards
	ROMHandCardsPlayed: db 0, 0, 0, 0, 0
	ROMSuggestedCard: DB 0
	ROMSuggestedBoardPosition: DB 0
	ROMPlayerTypes: DB 0, 0
	ROMENDSCREEN_VICTOR: db " VICTOR, PLAYER  !"
	ROMENDSCREEN_SCORE:  db "     00 - 00      "
end_ROM_copy: nop


endadr: ds $8000 - $


	
	; Variable definitions

	; TODO: Use RB/RW here.
org $c800 ;; These are the variables' positions in RAM.
	RAMtop:
	Decks: RB 52
	Hands: RB 10 ; Hand, like Decks; 5 + 5 cards
	HandCardsPlayed: RB 5

	SuggestedCard: RB 1
	SuggestedBoardPosition: RB 1
	PlayerTypes: RB 2
	ENDSCREEN_VICTOR: RB 18
	ENDSCREEN_SCORE:  RB 18

	
	_REND_HELP: RB 4

	XCoords: RB 2
	YCoords: RB 2
	CursorX: RB 1
	CursorY: RB 1

	CurrentTurn: RB 1
	CurrentPhase: RB 1
	DeckLen: RB 2; Initialize to Deck, Deck
	AIStackDepth: RB 2
	HandLen: RB 2 ; Hand size (0-5)
	Rng: RB 2		
	TGFX_LINE1: RB 4 ; Use this to draw cards.
	TGFX_LINE2: RB 4
	TGFX_LINE3: RB 4
	Board: RB 5*5
	BoardStacks: RB 5*5
	EasyAI: RB 1
	FlagIsValidMove: RB 1


	