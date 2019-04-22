/*
 * makeSnake.asm
 *
 *  Created: 2015-08-02 13:44:52
 *   Author: Niklas
 *	 Author: Jimmy
 *	 Author: Viktor
 */ 
 .MACRO lsr2
	lsr @0
	lsr @0
.ENDMACRO
.MACRO setY
	ldi @0, 0x0F
	and @0, @1

	push @1

	ldi @1, 0x01
	ldi @2, 0x01

	cp @1, @0
	BREQ _YSet

	ldi @1, 0x02
	ldi @2, 0x02

	cp @1, @0
	BREQ _YSet

	ldi @1, 0x03
	ldi @2, 0x04

	cp @1, @0
	BREQ _YSet

	ldi @1, 0x04
	ldi @2, 0x08

	cp @1, @0
	BREQ _YSet

	ldi @1, 0x05
	ldi @2, 0x10

	cp @1, @0
	BREQ _YSet

	ldi @1, 0x06
	ldi @2, 0x20

	cp @1, @0
	BREQ _YSet

	ldi @1, 0x07
	ldi @2, 0x40

	cp @1, @0
	BREQ _YSet

	ldi @1, 0x08
	ldi @2, 0x80

	cp @1, @0
	BREQ _YSet

	_YSet:
	mov @0, @1
	pop @1


.ENDMACRO
.MACRO setX

	ldi @0, 0xF0
	and @0, @1

	push @1

	ldi @1, 0x10

	cp @1, @0
	BREQ _XSet

	ld @1, X+
	ldi @1, 0x20
	
	cp @1, @0
	BREQ _XSet

	ld @1, X+
	ldi @1, 0x30
	
	cp @1, @0
	BREQ _XSet

	ld @1, X+
	ldi @1, 0x40
	
	cp @1, @0
	BREQ _XSet

	ld @1, X+
	ldi @1, 0x50
	
	cp @1, @0
	BREQ _XSet
		
	ld @1, X+
	ldi @1, 0x60
	
	cp @1, @0
	BREQ _XSet

	ld @1, X+
	ldi @1, 0x70
	
	cp @1, @0
	BREQ _XSet

	ld @1, X+

	_XSet:
	mov @0, @1
	pop @1

.ENDMACRO
 .DSEG
	ScreenMatrix:	.byte 8	//The Screen matrix, This is the virutal matrix representing the screen
	snake:			.byte 64	//This is snake, eatch segment of snake is made out of 1 byte so we can make him as long as we want to :D
	ApplePos:		.byte 16	//instead of randomizing the position i will store 32 random positions and then move them around and swap x and y with one another

.CSEG
	.ORG 0x0000
		jmp _Init	// Upon startup go to init, to begin initialization.
		nop			//This nope is here if just incase.
	.ORG 0x0020
		jmp	_ClckOverFlow //When An overflow happens with the clock TIMSK0 we jump to overflow segment.
		nop // Just incase.

.ORG INT_VECTORS_SIZE



_Init:
	.DEF Speed = r10 // This saves Defult value of ADMUX
	.DEF AppleCount = r11 // This counts the amount of untill the player collets the apple
	.DEF MoveDir = r12	// This holds the player movment speed in X and Y dirs
	.DEF StickX = r13	// Stick X
	.DEF StickY = r14	// Stick Y 
	.DEF Apple = r15	// Apple Position, 
	.DEF rTemp = r16	// registry 16 is our temp register.
	.DEF rTemp2 = r17	// register 17 is also a temp register.
	.DEF PiD = r18	// register 18 will primarely be used to stor data that will be painted to the screen. to PORTD.
	.DEF PiB = r19	// register 19 will primarely be used to stor data that will be painted to the screen. to PORTB.
	.DEF PiC = r20	// register 20 will primarely be used to stor data that will be painted to the screen. to PORTC.
	.DEF rColums = r21 //the clums
	.DEF rLong = r22	// This is the Long count register It is just used for a delay 
	.DEF snakeReg = r23	// This is the snake register, it will hold all the snake parts
	.DEF countSnake = r24 // This will be used for random iterations

	ldi rTemp, 0x00
	mov AppleCount, rTemp

	ldi rTemp, 0xFF
	mov Speed, rTemp

	ldi rTemp, 0x10
	mov MoveDir, rTemp
	ldi rTemp, 0x81
	mov Apple, rTemp

	//Set base values and enable muxer.
	ldi rTemp, 0X60
	sts ADMUX, rTemp

	ldi rTemp, 0x87
	sts ADCSRA, rTemp


	ldi countSnake, 0x00
		// This segment sets the stackpointer
	ldi rTemp, HIGH(RAMEND)
	out SPH, rTemp
	ldi rTemp, LOW(RAMEND)
	out SPL, rTemp
		// We set the stackpointer at the end of our memmory, so it can grow towards bigger lower addresses.
	
		// This segment sets the timer clock mode for TIMSK0
	ldi rTemp, 0x05 // we want the clock mode.
	out TCCR0B, rTemp // The clock will now count to 1024 then activate an overflow and start at zero.

	sei	// Activate global interups, i.e. we can now react when something happens.

	ldi rTemp, (1<<TOIE0)	//TOIE0 in TIMSK0 Should be one so we set in on load.
	lds rTemp2, TIMSK0		// load timsk0 
	or rTemp, rTemp2		// Or in the changed bit so we don't disturb any other bit.
	sts TIMSK0, rTemp		// Now we store the new data that tells timsk0 to activate and react to interupts.
	// The clock TIMSK0 

	ldi rTemp, 0x3F	// Here we load 0011 1111 into rTemp 
	out DDRB, rTemp // We store this data in DDRB all the ones will be outputs and all the zeros will be input.

	ldi rTemp, 0x0F // here we load 0000 1111 into rTemp
	out DDRC, rTemp // we Store this data in DDRC to tell it what pins are outputs and inputs.

	ldi rTemp, 0xFC	// Here we load 1111 1100 into rTemp
	out DDRD, rTemp // We stor this data in DDRD to tell it what pins are outputs and inputs

	ldi rLong, 0x00
_Set32AppelPos:
	ldi XH, HIGH(ApplePos)	// Here we load in the high and low 
	ldi XL, LOW(ApplePos)	// address of ScreenMatrix into X

	//mov XHigh, XH
	//mov Xlow, XL

	ldi rTemp, 0x19
	st X+, rTemp
	ldi rTemp, 0x81
	st X+, rTemp
	ldi rTemp, 0x27
	st X+, rTemp
	ldi rTemp, 0x62
	st X+, rTemp
	ldi rTemp, 0x35
	st X+, rTemp
	ldi rTemp, 0x34
	st X+, rTemp
	ldi rTemp, 0x91
	st X+, rTemp
	ldi rTemp, 0x29
	st X+, rTemp

	ldi rTemp, 0x38
	st X+, rTemp
	ldi rTemp, 0x84
	st X+, rTemp
	ldi rTemp, 0x57
	st X+, rTemp
	ldi rTemp, 0x76
	st X+, rTemp
	ldi rTemp, 0x66
	st X+, rTemp
	ldi rTemp, 0x67
	st X+, rTemp
	ldi rTemp, 0x55
	st X+, rTemp
	ldi rTemp, 0x81
	st X+, rTemp

_ResetSneak:
	ldi XH, HIGH(snake)	// Here we load in the high and low 
	ldi XL, LOW(snake)	// address of ScreenMatrix into X

	ldi rTemp, 0x04	// Snakes lenght
	st X+, rTemp
	ldi rTemp, 0x41	// Snakes head position
	st X+, rTemp
	ldi rTemp, 0x31	// Snakes more snake parts
	st X+, rTemp
	ldi rTemp, 0x21
	st X+, rTemp
	ldi rTemp, 0x11
	st X+, rTemp
	st X+, rTemp
	st X+, rTemp
	st X+, rTemp
	st X+, rTemp

/*_ClearScreenMatrix:
	ldi XH, HIGH(ScreenMatrix)	// Here we load in the high and low 
	ldi XL, LOW(ScreenMatrix)	// address of ScreenMatrix into X

	clr rTemp				// we clear rTemp to zero
	st X+, rTemp			// then we stor zero on all
	st X+, rTemp			// 8 bytes so we don't have any data left for last game.
	st X+, rTemp
	st X+, rTemp

	st X+, rTemp			//... you know you have been using 
	st X+, rTemp				//your server to much when you try
	st X+, rTemp					//Try Closing Vim and saving by typing
	st X+, rTemp						// :wq*/


_Test:
	ldi XH, HIGH(ScreenMatrix)	// Here we load in the high and low 
	ldi XL, LOW(ScreenMatrix)	// address of ScreenMatrix into X

	ldi rTemp,0x3C
	st X+,rTemp
	ldi rTemp,0x42
	st X+,rTemp
	ldi rTemp,0x52
	st X+,rTemp
	ldi rTemp,0x76
	st X+,rTemp
	ldi rTemp,0x3C
	st X+,rTemp
	ldi rTemp,0x42
	st X+,rTemp
	ldi rTemp,0x42
	st X+,rTemp
	ldi rTemp,0x3C
	st X+,rTemp

	// This is test data.
	// This will just look funky.
	// The Test data is now Start data.
	// It prints GO in the screen 4 seconds befor start
	// GO can be Game Over and GO! YAY
_StartDraw:
	ldi XH, HIGH(ScreenMatrix)
	ldi XL, LOW(ScreenMatrix)	
	ldi rTemp2, 0x01 // we will use this to count the number of rows down 
_MainLoop:
	// PiD, PiC, PiB
	clr PiD	//clear PiD
	clr PiC	//clear PiC
	clr PiB	//clear PiB

	ld rColums, X+	// load in the column from Location X in memory, after that increment X

	ldi PiC, 0x0F	
	and PiC, rColums		// The data from rColums that will be used for painting a colum

	swap rColums	
	lsl rColums
	lsl rColums
	
	ldi PiD, 0x3C			
	and PiD, rColums		// Lod The important part of rColums into PiD


	ldi rTemp, 0x03			// 0000 0011
	and rTemp, rTemp2		// if time is on the two first bits
	lsl rTemp				// 0000 0XX0
	lsl rTemp				// 0000 XX00 Bitshift left
	swap rTemp				// XX00 0000 swap nibbles
	
	or PiD, rTemp			// or it in in PiD

	ldi rTemp, 0xFC
	and rTemp, rTemp2
	lsr rTemp
	lsr rTemp
	or PiB, rTemp


	out PORTD, PiD
	out PORTC, PiC
	out PORTB, PiB

	lsl rTemp2		// I'm a little confused since this works.... now i don't want to mess with it
	BREQ _StartDraw			

	
	nop
	jmp _MainLoop
	nop

 _ClckOverFlow:
	push rTemp
	push rTemp2
	mov rTemp, Speed // move Speed can be adjusted with speed
	subi rLong, -1

	cp rTemp, rLong
	BREQ _OverFlow
	//ldi rTemp, 0xFB // This dosn't do anything yet. 
 _DoneWithThing:

 	jmp _MoveMux
	_muxDone:

	pop rTemp2
	pop rTemp
	reti

	_OverFlow:
   jmp _LongOverFlow

 _MoveMux:
   push countSnake

    lds rTemp, ADMUX
    ldi rTemp2, 0b11111000	
    and rTemp, rTemp2		
    ldi rTemp2, 0b00000100
    or rTemp, rTemp2
    sts ADMUX, rTemp	// Prep muxer for use
   
    lds rTemp, ADCSRA
    ldi rTemp2, 0b01000000
    or rTemp, rTemp2
    sts ADCSRA, rTemp	// set bit 6 in ADCSRA to one

wait_for_Xconv_finished:
	lds rTemp2, ADCSRA
	sbrc rTemp2, 6  //bit 6 goes low after conversion done         
	rjmp wait_for_Xconv_finished

   lds   rTemp, ADCL
   lds   rTemp, ADCH	// When the Muxing process is done we can load high and low bytes into rTemp

   //mov StickY, rTemp	// I will use 

   cpi rTemp, 200 // If the Stick value is over 200 we can be sure the stick is tilted.
   BRSH _stickYUpp


   cpi rTemp,15	// If the stick value is under 15 we can be sure the stick is tilted the other way.
   BRLO _stickYDown
   _YStickDone:

	lds rTemp, ADMUX
    ldi rTemp2, 0b11111000
    and rTemp, rTemp2
    ldi rTemp2, 0b00000101
    or rTemp, rTemp2
    sts ADMUX, rTemp		// We prep the muxer once again this time for the X axis

	lds rTemp, ADCSRA
    ldi rTemp2, 0b01000000
    or rTemp, rTemp2
    sts ADCSRA, rTemp

wait_for_Yconv_finished:
	lds rTemp2, ADCSRA
	sbrc rTemp2, 6  //bit 6 goes low after conversion done         
	rjmp wait_for_Yconv_finished

   lds   rTemp, ADCL
   lds   rTemp, ADCH

   //mov StickX, rTemp

	cpi rTemp, 200
	BRSH _stickXUpp


   cpi rTemp, 15
   BRLO _stickXDown
   _StickDone:
	
	//Fixa fel med styrnigen är för trött nu :C
	//

   pop countSnake
   jmp _muxDone

   

   _stickYUpp:
	ldi countSnake, 0x0F
	SBRS MoveDir, 0			// if bit 0 is set then we will ignore next instruktion, this prevents 180 degree turns
	mov MoveDir, countSnake	// If we aren't moving in the opposite direction then we change direktion
	jmp _YStickDone

  _stickYDown:
	ldi countSnake, 0x01
	SBRS MoveDir, 0
	mov MoveDir, countSnake
	jmp _YStickDone

	_stickXUpp:
	ldi countSnake, 0xF0
	SBRS MoveDir, 4			// if bit 0 is set then we will ignore next instruktion, this prevents 180 degree turns
	mov MoveDir, countSnake	// If we aren't moving in the opposite direction then we change direktion
	jmp _StickDone

  _stickXDown:
	ldi countSnake, 0x10
	SBRS MoveDir, 4
	mov MoveDir, countSnake
	jmp _StickDone

	// Do X 
		// Then collision  Then apple then Done


 _LongOverFlow:
	ldi rLong, 0x3F		// Change speed so snake moves faster
	mov Speed, rLong
	ldi rLong, 0x00

	push XH
	push XL

	ldi XH, HIGH(ScreenMatrix)
	ldi XL, LOW(ScreenMatrix)

	clr rTemp				// we clear rTemp to zero
	st X+, rTemp			// then we stor zero on all
	st X+, rTemp			// 8 bytes so we don't have any data left for last game.
	st X+, rTemp
	st X+, rTemp

	st X+, rTemp			//... you know you have been using 
	st X+, rTemp				//your server to much when you try
	st X+, rTemp					//Try Closing Vim and saving by typing
	st X+, rTemp
	
	jmp _MoveSnake			// move snake rutine
	_MoveDone:
	jmp _PintApple			// paint apple rutine
	_ApplePainted:
	jmp _Collide			// Collision check
	_CollisonDone:

	ldi YH, HIGH(snake)		
	ldi YL, LOW(snake)
	
	ld rTemp2, Y+	// load snake length 
	_SnakeArea:

	ld snakeReg, Y+  // Load the snakes head.

	push rTemp2

	setY rTemp, snakeReg, rTemp2		// decode Y cordinate.

	mov rTemp, rTemp2
	pop rTemp2
	push rTemp

	jmp _Skipp

	_Climb:
	jmp _SnakeArea

	_Skipp:

	ldi XH, HIGH(ScreenMatrix)
	ldi XL, LOW(ScreenMatrix)
	setX rTemp, snakeReg		// Decode X cordinate.

	pop rTemp
	push rTemp2
	ld rTemp2, X

	or rTemp, rTemp2			// We cannot disturb the rest of snake so we or in the new data.
	st X, rTemp					// store Y cordinate in X cordinate.

	pop rTemp2
	
	inc countSnake

	cp rTemp2, countSnake		// Denna ska bli till Branch if lessthen or equals
	BRNE _Climb

	ldi countSnake, 0x00

	pop XL
	pop XH

	jmp _DoneWithThing
	nop

_MoveSnake:
	push YH
	push YL

	ldi YH, HIGH(snake)
	ldi YL, LOW(snake)
	
	ld rTemp2, Y+		// loade snake length
	push countSnake
	mov countSnake, MoveDir	
		
	ld snakeReg, Y
	mov rTemp, snakeReg		// load snake head

	push PiC
	push PiD
	ldi PiD, 0xF0			
	ldi PiC, 0xF0
	and PiC, snakeReg
	and PiD, countSnake		// Prep snake X for math

	add PiC, PiD			// add the two parts i.e. snake X and the moveDir X component
	ldi PiD, 0x0F
	and PiD, snakeReg
	or PiC, PiD
	mov snakeReg, PiC		// move to snakeReg

	ldi PiD, 0x0F
	ldi PiC, 0x0F
	and PiC, snakeReg
	and PiD, countSnake

	add PiC, PiD
	ldi PiD, 0x0F
	and PiD, PiC
	mov PiC, PiD		// prep Y cordinate and preform math on Y cordinate.

	ldi PiD, 0xF0
	and PiD, snakeReg
	or PiC, PiD
	mov snakeReg, PiC

	pop PiD
	pop PiC

	jmp _AdjustX	// if X is out of bounds we fix it
	_Back:
	jmp _AdjustY	// if Y is out of bounds we fix it
	_Done:

	st Y+, snakeReg		// we then store this head value on the heads location

	ldi countSnake, 0x00
	_Round:
	push countSnake

	ld snakeReg, Y		// after all that math we can just send the old positions down snake 
	mov countSnake, snakeReg
	mov snakeReg, rTemp
	mov rTemp, countSnake

	st Y+, snakeReg		// onse the old snake pos is given to the part behind it we stor this new data on that bodypart
	pop countSnake
	inc countSnake

	cp rTemp2, countSnake		// Denna ska bli till Branch if lessthen or equals
	BRNE _Round

	
	pop countSnake

	pop YL
	pop YH

	jmp _MoveDone

_AdjustX:  //Wrap X
	push rTemp
	ldi rTemp, 0xF0
	and rTemp, snakeReg
	
	ldi countSnake, 0x90
	
	cp rTemp, countSnake
	BREQ _XOver

	ldi countSnake, 0x00

	cp rTemp, countSnake
	BREQ _XUnder
	pop rTemp
	jmp _Back

	_XOver:
	pop rTemp
	ldi snakeReg, 0x0F
	and snakeReg, rTemp
	ori snakeReg, 0x10	// if we are out of bounds upwards we go to the boton

	jmp _Back

	_XUnder:
	pop rTemp
	ldi snakeReg, 0x0F // if we go out of bounds downwards we go to the top
	and snakeReg, rTemp
	ori snakeReg, 0x80
	
	jmp _Back

	_AdjustY: //Wrap Y
	push rTemp
	ldi rTemp, 0x0F
	and rTemp, snakeReg
	
	ldi countSnake, 0x09
	
	cp rTemp, countSnake
	BREQ _YOver

	ldi countSnake, 0x00

	cp rTemp, countSnake
	BREQ _YUnder
	pop rTemp
	jmp _Done

	_YOver:
	pop rTemp
	ldi snakeReg, 0xF0
	and snakeReg, rTemp
	ori snakeReg, 0x01		// If Y component is out of bounds we wrap it around to the lowest location
		
	jmp _Done

	_YUnder:
	pop rTemp
	ldi snakeReg, 0xF0
	and snakeReg, rTemp
	ori snakeReg, 0x08	// If Y component is out of bounds we wrap it around to the highest location
	
	jmp _Done

_PintApple:
	push snakeReg
	push rTemp2

	mov snakeReg, Apple
	setY rTemp, snakeReg, rTemp2	// here we paint the apple, seince the apple is just one dot we have a seperet draw call for it.

	mov rTemp, rTemp2
	push rTemp

	ldi XH, HIGH(ScreenMatrix)
	ldi XL, LOW(ScreenMatrix)
	setX rTemp, snakeReg

	pop rTemp
	st X, rTemp	// we store the apple first so we don't need to think about snake 
	pop snakeReg
	pop rTemp2

	jmp _ApplePainted

_Collide:
	push rTemp
	push rTemp2
	push snakeReg
	push countSnake
	
	ldi YH, HIGH(snake)
	ldi YL, LOW(snake)

	ld rTemp2, Y+
	ld snakeReg, Y+	// load the snake head

	cp snakeReg, Apple // compare the snakes head to the apple if they have the same position we can 
	BREQ _HitApple		// exicute hit the apple code.
	
	_SelfHitTest:
	ld rTemp, Y+

	cp rTemp, snakeReg
	BREQ _SnakeFail

	inc countSnake
	cp rTemp2, countSnake
	BRNE _SelfHitTest		// here we do a self hit test

	inc AppleCount
	_HitDone:		// since you can't hit yourself and eat the apple at the same time we just say jump here when a hit has been found

	pop countSnake
	pop snakeReg
	pop rTemp2
	pop rTemp
	jmp _CollisonDone

	_SnakeFail:
		jmp _Init	// if we hit oursekfe we die so RESTART :C
	
	_HitApple:		// WE hit the apple YAY! 
	push YH
	push YL
	push XH
	push XL
	

	ldi YH, HIGH(snake)
	ldi YL, LOW(snake)

	ld rTemp, Y
	inc rTemp
	st Y+, rTemp	// now we increase snakes lenght and go forward

	push rTemp2
/*	push rTemp

	mov XH, XHigh
	mov XL, Xlow

	ld rTemp, X
	ld rTemp2, Y+

	swap Apple
	st X+, Apple

	mov Apple, rTemp

	cp Apple, rTemp2
	BRNE */

	// New random code.


	ldi rTemp, 0x0F
	and rTemp, StickX
	swap rTemp

	ldi rTemp2, 0x0F
	and rTemp2, StickY
	or rTemp, rTemp2

	EOR rTemp, Apple

	mov rTemp2, rTemp
	push rTemp2

	ldi rTemp, 0xF0
	and rTemp, StickX

	ldi rTemp2, 0xF0
	and rTemp2, StickY
	eor rTemp, rTemp2	

	pop rTemp2
	swap rTemp
	or rTemp, rTemp2
	or rTemp, AppleCount

	ldi rTemp2, 0xF0
	and rTemp2, rTemp 
	
	cpi rTemp2, 0x90
	brsh _fixAppleXHight
	
	cpi rTemp2, 0x00
	breq _fixAppleXLow

	_AppleXFixed:

	ldi rTemp2, 0x0F
	and rTemp2, rTemp 
	
	cpi rTemp2, 0x09
	brsh _fixAppleYHight
	
	cpi rTemp2, 0x00
	breq _fixAppleYLow

	_AppleYFixed:
	

	 pop rTemp2
	

	mov Apple, rTemp

	ldi rTemp, 0x00
	mov AppleCount, rTemp


	pop XL
	pop XH
	pop YL
	pop YH
	jmp _HitDone

_fixAppleXHight:		// if we generate a position outside the screen then we fix the apple
	ldi rTemp2, 0x0F	// just like with snake
	and rTemp, rTemp2
	ldi rTemp2, 0x80
	or rTemp, rTemp2
	jmp _AppleXFixed

_fixAppleXLow:
	ldi rTemp2, 0x0F
	and rTemp, rTemp2
	ldi rTemp2, 0x00
	or rTemp, rTemp2
	jmp _AppleXFixed

_fixAppleYHight:
	ldi rTemp2, 0xF0
	and rTemp, rTemp2
	ldi rTemp2, 0x08
	or rTemp, rTemp2
	jmp _AppleYFixed

_fixAppleYLow:
	ldi rTemp2, 0xF0
	and rTemp, rTemp2
	ldi rTemp2, 0x00
	or rTemp, rTemp2
	jmp _AppleYFixed
