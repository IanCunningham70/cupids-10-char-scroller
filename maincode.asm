                        //------------------------------------------------------------------------------
						//
						// 18/10/2020 : got the EBC mode selector working, speed and pause should also
						// work. 
						//
						// next ... let the padua crew decide.
						//
						// a nice logo
						// differant music
						// rasters ?
                        //------------------------------------------------------------------------------

                        //------------------------------------------------------------------------------
						// memory map
						//
						// $0800 - $0fff - spare
						// $0c00 - $0cff - cover sprites
						// $1000 - $27ff - music
						// $2800 - $2fff - big charset
						// $3000 - $37ff - charset gfx for end part
						// $4000 - $4800 - main code + lokup tables
						// $5000 - $5fff - greetings scroll text
						// $8000 - $9600 - 16x10 lookup tables
						//
                        //------------------------------------------------------------------------------
BasicUpstart2(start)
                        //------------------------------------------------------------------------------
						// import music.
                        //------------------------------------------------------------------------------
						.var music = LoadSid("Cheers_and_Tributes.sid")				// double speed tune
						*=music.location "Music"
        				.fill music.size, music.getData(i)
                        //------------------------------------------------------------------------------

                        //------------------------------------------------------------------------------
						// import standard library definitions
                        //------------------------------------------------------------------------------
						#import "standardLibrary.asm"			// 
                        //------------------------------------------------------------------------------
						.var scrollerline = $0400 + ( 40 * 14 )

						.var charLine01 = $8000
						.var charLine02 = charLine01 + $200
						.var charLine03 = charLine02 + $200
						.var charLine04 = charLine03 + $200
						.var charLine05 = charLine04 + $200
						.var charLine06 = charLine05 + $200
						.var charLine07 = charLine06 + $200
						.var charLine08 = charLine07 + $200
						.var charLine09 = charLine08 + $200
						.var charLine10 = charLine09 + $200
						.var charLine11 = charLine10 + $200
						
						.var apage = $ca
						//------------------------------------------------------------------------------

						//------------------------------------------------------------------------------
						*=$4000 "main code"
start:		
						sei
						lda #$35
						sta $01

						lda #$00
						tay
						tax
						jsr music.init

						// initialise the scroller pointers

						jsr scrollerinit

						ldx #$00
				!:
						lda #$00
						sta $0400,x 
						sta $0500,x 
						sta $0600,x 
						sta $0700,x 
						sta $d800,x 
						sta $d900,x 
						sta $da00,x 
						sta $db00,x 
						inx
						bne !-

						// setsprite pointers to mask off both sides of the screen
						jsr setup_sprites


						lda #$7f
						sta $dc0d
						sta $dd0d

						lda #$01
						sta irqflag
						sta irqenable						

						ldx #<irq1
						ldy #>irq1
						stx $fffe
						sty $ffff

						cli

						jmp *
                        //------------------------------------------------------------------------------

                        //------------------------------------------------------------------------------
nmi:					rti
                        //------------------------------------------------------------------------------

                        //------------------------------------------------------------------------------
irq1:  		
						pha
						txa
						pha
						tya
						pha








						ldx #(13*8)+$32						// calculate raster line 
						cpx raster
						bne *-3

						bit $c45e
						bit $c45e
						bit $c45e
						bit $c45e

						lda #28
						sta charset

				        lda #$1b        					// Enable EBC Flashing Mode.
				        ora #64
				        sta screenmode

					    lda #BLACK
					    sta border
						lda #LIGHT_GRAY
					    sta screen
				        lda col22
				        sta backcol0
				        lda col23
				        sta backcol1
				        lda col24
				        sta backcol2

						lda smoothpos
						and #$c0
						ora scrollXpos
						sta smoothpos

						jsr $1003

						ldx #(25*8)+$32						// calculate raster line 
						cpx raster
						bne *-3

						ldx #$09
					!:	dex
						bne !-

						lda #$1b				// kill EBC mode
						sta screenmode

						lda #BROWN
						sta border
						jsr scroller
						jsr ebc_flash
						lda #BLACK
						sta border

						jsr $1006


						lda #$01
						sta $d019	
						pla
						tay
						pla
						tax
						pla
						rti
                        //------------------------------------------------------------------------------
						// setup sprites that cover the scroller's edges
                        //------------------------------------------------------------------------------
setup_sprites:			lda #%11111111
						sta spriteset

                        lda #%00000000
                        sta spriteexpy

                        lda #%11111111
                        sta spriteexpx

            	        lda #%00000000
			            sta spritepr

                        lda #%11110000
                        sta spritermsb


                        lda #BLACK
                        sta spritecolors
                        sta spritecolors+1
                        sta spritecolors+2
                        sta spritecolors+3
                        sta spritecolors+4
                        sta spritecolors+5
                        sta spritecolors+6
                        sta spritecolors+7
						
                        lda #(sprite_left / 64)
						sta 2040
						sta 2041
						sta 2042
						sta 2043

                        lda #(sprite_right / 64)
						sta 2044
						sta 2045
						sta 2046
                        sta 2047
                        				
						// define x position of sprites
                        
						lda #30
						sta sprite0x
						sta sprite1x
						sta sprite2x
						sta sprite3x

						lda #40
						sta sprite4x
						sta sprite5x
						sta sprite6x
						sta sprite7x

						// define y position of 1st sprite, then add sprite height to
						// get next position.
						
                        lda #160
                        sta sprite0y
						sta sprite4y
						adc #20
                        sta sprite1y
						sta sprite5y
						adc #20
                        sta sprite2y
                        sta sprite6y
						adc #20
                        sta sprite3y
                        sta sprite7y
						rts
	                    //------------------------------------------------------------------------------
						// cycle through the ebc color tables to give the effect of flashing
	                    //------------------------------------------------------------------------------
ebc_flash:     			lda ebc_delay
	        			sec
	        			sbc #$02
	        			and #$07
	        			sta ebc_delay
	        			bcc ebc_flasha
	        			rts
ebc_flasha:				ldy ebc_number
	        			cpy #$0f
	        			beq ebc_flashb
ebc_flashc:				lda ebc_color1,y
        				sta col22
        				lda ebc_color2,y
        				sta col23
        				lda ebc_color3,y
        				sta col24
        				inc ebc_number
        				rts
ebc_flashb:				ldy #$00
        				sty ebc_number
        				jmp	ebc_flashc
	                    //------------------------------------------------------------------------------
						// main scroller routine
                        //------------------------------------------------------------------------------
scroller:				lda scrollDelay
			        	beq scrollerNext
			        	dec scrollDelay
			        	rts
scrollerNext:			lda scrollXpos
			        	sec
			    	    sbc scrollSpeed
				        and #$07
				        sta scrollXpos
				        bcc scrollerMove
				        rts						

				        // move scroller 1 physical character left

scrollerMove:    		ldx #$00
						lda scrollerline+(40*0)+1,x
						sta scrollerline+(40*0),x
						lda scrollerline+(40*1)+1,x
						sta scrollerline+(40*1),x
						lda scrollerline+(40*2)+1,x
						sta scrollerline+(40*2),x
						lda scrollerline+(40*3)+1,x
						sta scrollerline+(40*3),x
						lda scrollerline+(40*4)+1,x
						sta scrollerline+(40*4),x
						lda scrollerline+(40*5)+1,x
						sta scrollerline+(40*5),x
						lda scrollerline+(40*6)+1,x
						sta scrollerline+(40*6),x
						lda scrollerline+(40*7)+1,x
						sta scrollerline+(40*7),x
						lda scrollerline+(40*8)+1,x
						sta scrollerline+(40*8),x
						lda scrollerline+(40*9)+1,x
						sta scrollerline+(40*9),x
						lda scrollerline+(40*10)+1,x
						sta scrollerline+(40*10),x
						inx
						cpx #39
						bne scrollerMove+2
						
						ldx charWidth			// check current width count
						cpx widthCheck			// against what it should be
						bne plotChar01			// not complete, then plot next section.

						jsr nextCharacter

						// plot the next horizontal line of the character

         				ldx #$00
plotChar01:    			lda charLine01,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 1 ) -1
plotChar02:				lda charLine02,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 2 ) -1
plotChar03:				lda charLine03,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 3 ) -1					 
plotChar04:         	lda charLine04,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 4 ) -1
plotChar05:    			lda charLine05,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 5 ) -1
plotChar06:				lda charLine06,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 6 ) -1
plotChar07:				lda charLine07,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 7 ) -1					 
plotChar08:         	lda charLine08,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 8 ) -1

plotChar09:         	lda charLine09,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 9 ) -1

plotChar10:         	lda charLine10,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 10 ) -1

plotChar11:         	lda charLine11,x
						clc
						adc ebc_code
         				sta scrollerline + (40 * 11 ) -1

						inc charWidth		// increment width counter
						rts
						
						//------------------------------------------------------------------------------

nextCharacter:			ldy #$00
						sty charWidth
						lda (apage),y
						cmp #$ff 						// check for end of scroller character
						bne nextChar

						// reset scroller to default pointers

						jsr scrollerinit				
						jmp nextCharacter				// loop back to load next character in scroller

nextChar:				sta tempCharacter				// store the character just loaded

						//----------------------------------------------------------
         				// change speed ?
						//----------------------------------------------------------

checkSpeed:     		ldy #$05
			        	cmp speedtable,y
			        	beq checkSpeed1
			        	dey
			        	bpl checkSpeed+2
			        	jmp checkPause
checkSpeed1:    		sec
			        	sbc #$8f
			        	sta scrollSpeed
			        	lda #$20
			        	sta tempCharacter
			        	jmp plotCharacter

						//----------------------------------------------------------
         				// pause text ?
						//----------------------------------------------------------

checkPause:     		ldy #$04
			        	cmp pausetable,y
			        	beq checkPause1
			        	dey
			        	bpl checkPause+2
			    	    jmp checkEcm
checkPause1:			lda pauseTable,y
				        sta scrollDelay
				        lda #$20
				        sta tempCharacter
				        jmp plotCharacter

						//----------------------------------------------------------
         				// change ECM mode
						//----------------------------------------------------------

checkEcm:				ldy #$03
			        	cmp ebctable,y
			        	beq checkEcm1
			       	 	dey
			        	bpl checkEcm+2
			        	jmp plotCharacter
checkEcm1: 				lda ebc_set,y
						sta ebc_code
						lda #$20
			        	sta tempCharacter
			        	// jmp plotCharacter

plotCharacter:			
                        //------------------------------------------------------------------------------
						// determine what character has been read into memory and select its width and 
						// memory start position from the appropiate tables
                        //------------------------------------------------------------------------------

						lda tempCharacter
						and #$7f
						tay
						
						// set next characters width
						lda charWidths,y
						sta widthCheck

						// load hi byte pointer and store, then add $02 for the next hi pointer

						lda character_hi,y
						sta plotChar01+2
						clc
						adc #$02
						sta plotChar02+2
                        clc 
                        adc #$02
						sta plotChar03+2
                        clc 
                        adc #$02
						sta plotChar04+2
                        clc 
                        adc #$02
						sta plotChar05+2
                        clc 
                        adc #$02
						sta plotChar06+2
                        clc 
                        adc #$02
						sta plotChar07+2
						clc 
                        adc #$02
						sta plotChar08+2
                        clc 
                        adc #$02
						sta plotChar09+2
                        clc 
                        adc #$02
						sta plotChar10+2
                        clc 
                        adc #$02
						sta plotChar11+2

						// load lo byte pointer and store in plot routine

						lda character_lo,y
						sta plotChar01+1
						sta plotChar02+1
						sta plotChar03+1
						sta plotChar04+1
						sta plotChar05+1
						sta plotChar06+1
						sta plotChar07+1
						sta plotChar08+1
						sta plotChar09+1
						sta plotChar10+1
						sta plotChar11+1

						// move the text pointer onto the next position

						inc apage
						bne plotCharacterExit
						inc apage+1
plotCharacterExit:  	rts
                        //------------------------------------------------------------------------------
						//
                        //------------------------------------------------------------------------------
scrollerinit:    		ldx #<scrolltext
						ldy #>scrolltext
						stx apage
						sty apage+1
						lda #$02
						sta charWidth
						sta widthCheck
						rts
                        //------------------------------------------------------------------------------

                        //------------------------------------------------------------------------------
						// the following are the data bytes used for scrolling, plotting each character
						// and checking its width.
                        //------------------------------------------------------------------------------
tempbyte: 				.byte $00
scrollXpos:    			.byte $00
scrollDelay:   			.byte $00
scrollSpeed:   			.byte $01 								// scroll speed.
tempCharacter:    		.byte $00,$00							// current character.
charWidth:  			.byte $00,$00
widthCheck:  			.byte $00,$00

pauseTable:    			.byte 100,125,150,175,200


						//    off  01  02  03
ebctable:    			.byte $81,$82,$83,$84					// ebc mocde codes

speedtable:	    		.byte $90,$91,$92,$93,$95,$96			// speed codes

pausetable:    			.byte $a0,$a1,$a2,$a3,$a4				// pause scroller

charColor:				.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15

ebc_set:				.byte $00,64,128,192

ebc_code:				.byte $00								// byte to add when plotting character

characterColor:  		.byte $0f								// current 'boring' character color
screenColor:  			.byte $00
ebc_delay:	        	.byte $00								// extended background colour delay
ebc_number:				.byte $00								//
col22:   				.byte $00								//
col23:   				.byte $00								//
col24:   				.byte $00								//

						// extended background colour table 01
ebc_color1:				.byte $00,$06,$02,$04,$0a,$0f,$07,$01,$07,$0f,$0a,$04,$02,$06,$00

						// extended background colour table 02
ebc_color2:				.byte $01,$07,$0f,$0a,$08,$02,$09,$00,$00,$09,$02,$08,$0a,$0f,$07

						// extended background color table 03
ebc_color3:				.byte $04,$02,$06,$00,$00,$06,$02,$04,$0a,$0f,$07,$01,$07,$0f,$0a		

						*= $0c00 "cover sprite left"
sprite_left:			.byte $cc,$00,$00,$f0,$80,$00,$cc,$00,$00,$f3,$00,$00,$cc,$80,$00,$f0
						.byte $80,$00,$cc,$00,$00,$f3,$00,$00,$cc,$00,$00,$f0,$80,$00,$cc,$00
						.byte $00,$f3,$00,$00,$cc,$00,$00,$f0,$80,$00,$cc,$00,$00,$f3,$00,$00
						.byte $cc,$00,$00,$f0,$80,$00,$cc,$00,$00,$f3,$00,$00,$cc,$00,$00,$01

sprite_right:			.byte $00,$00,$33,$00,$01,$0f,$00,$00,$33,$00,$00,$cf,$00,$01,$33,$00
						.byte $01,$0f,$00,$00,$33,$00,$00,$cf,$00,$00,$33,$00,$01,$0f,$00,$00
						.byte $33,$00,$00,$cf,$00,$00,$33,$00,$01,$0f,$00,$00,$33,$00,$00,$cf
						.byte $00,$00,$33,$00,$01,$0f,$00,$00,$33,$00,$00,$cf,$00,$00,$33,$01


						*=$6000 "greetings scroll text"
scrolltext:				
						.byte $90
						.text " PADUA "
						.byte $a1
						
						.byte $93
						.text "would like to buy drinks for...       "   

                        .byte $82
						.text "Abyss Connection , Accession, Active, Alpha FLight, Arsenic, Arise, Artline Designs, Atlantis, "
                        .byte $83
						.byte $95
						.text "Bonzai, Booze Designs, "
                        .byte $84
						.byte $96
						.text "C64 Club Berlin, Camelot, Censor Design, Chorus, Crest, CRT, CSDb staff, "
                        .byte $83
						.byte $92
						.text "Delysid, dual crew, "
						.byte $81

						.text "Excess, EXON, "
						
						.text "F4CG, Fairlight, Finnish Gold, Focus, Fossil, "
						
						.text "Genesis Project, Gideon, Glance, "
						
						.text "Hack'n'Trade, Hoaxers, Hokuto Force, Hitmen, Horizon, "
						
						.text "Insane, Inversion, "
						
						.text "JCH, "
						
						.text "K2, "
						
						.text "Laxity, Lethargy, Level64, Lepsi, LFT, "
						
						.text "Marcin Skoczylas, Mahoney, Maniacs of Noise, Mayday!, "
						
						.text "Nah-Kolor, Nuance, "
						
						.text "Offence, OMG, Onslaught, Oxyron, "
						
						.text "Plush, Poo-Brain, Pretzel Logic, Protovision, Proxima, "
						
						.text "Rabenauge, Radwar, RBBS, Rebels, Resource, Role, "
						
						.text "Samar, SCS+TRC, Siesta, Silicon Ltd, Singular, Squoquo, "
						
						.text "Tempest, The Performers, The Solution, The Weekenders Demo Group, TIA, TOM, TRSI, Triad, "
						
						.text "Unreal, "
						
						.text "Vantage, Vibrants, VICE team, "
						
						.text "Welle:Erdball, "
						
						.text "Xenon, "
						
						.text "Zoolon "

						.text " ... and anyone else we mau have forgotten.           "
						.byte $ff

						//------------------------------------------------------------------------------
						// define varible pointers to each character, starting at $5000 and moving along
						// until all characters have a 'start' pointer that can be used to ge the top   
						// left pointers from a lookup table.
						//------------------------------------------------------------------------------
.var character_at	= $8000
.var character_32   = character_at        // SPACE
.var character_a    = character_at+6
.var character_b	= character_a+6
.var character_c	= character_b+6
.var character_d	= character_c+6
.var character_e	= character_d+6
.var character_f	= character_e+6
.var character_g	= character_f+4
.var character_h	= character_g+6
.var character_i	= character_h+6
.var character_j	= character_i+2
.var character_k	= character_j+4
.var character_l	= character_k+6
.var character_m	= character_l+4
.var character_n	= character_m + 10
.var character_33   = character_n + 6		// !
.var character_o	= character_33+2
.var character_p	= character_o+6
.var character_q	= character_p+6
.var character_r	= character_q+6
.var character_s	= character_r+6
.var character_t	= character_s+6
.var character_u	= character_t+4
.var character_v	= character_u+6
.var character_w	= character_v+6
.var character_x	= character_w+10
.var character_y	= character_x+6
.var character_z	= character_y+6
.var character_46   = character_z + 6    	// .
.var character_58   = character_46 + 2		// :
.var character_39   = character_58 + 2    	// '
.var character_48   = character_39 + 2  	// 0
.var character_49   = character_48 + 6  	// 1
.var character_50   = character_49 + 4   	// 2
.var character_51   = character_50 + 6    	// 3
.var character_52   = character_51 + 6    	// 4
.var character_53   = character_52 + 6    	// 5
.var character_54   = character_53 + 6    	// 6
.var character_55   = character_54 + 6    	// 7
.var character_56   = character_55 + 6    	// 8
.var character_57   = character_56 + 6    	// 9
.var character_63   = character_57 + 6    	// ?
.var character_34   = character_63 + 6    	// "
.var character_47   = character_34 + 6    	// /
.var character_44   = character_47 + 7    	// ,
.var character_A    = character_44 + 3
.var character_B    = character_A+6
.var character_C    = character_B+6
.var character_D    = character_C+6
.var character_E    = character_D+6
.var character_F    = character_E+6
.var character_G    = character_F+6
.var character_H    = character_G+6
.var character_I    = character_H+6
.var character_J    = character_I+6
.var character_K    = character_J+6
.var character_L    = character_K+6
.var character_M    = character_L+6
.var character_N    = character_M+6
.var character_O    = character_N+6
.var character_P    = character_O+6
.var character_Q    = character_P+6
.var character_R    = character_Q+6
.var character_S    = character_R+6
.var character_T    = character_S+6
.var character_U    = character_T+6
.var character_V    = character_U+6
.var character_W    = character_V+6
.var character_X    = character_W+6
.var character_Y    = character_X+6
.var character_Z    = character_Y+6
                        //------------------------------------------------------------------------------
                        // following characters have no definition, so point towards a blank one.
                        //------------------------------------------------------------------------------
.var character_30   = character_32    // ^
.var character_27   = character_32    // [
.var character_28   = character_32    // £
.var character_29   = character_32    // ]
.var character_31   = character_32    // _
.var character_35   = character_32    // #
.var character_36   = character_32    // $
.var character_38   = character_32    // &
.var character_42   = character_32    // *
.var character_62   = character_32    // >
.var character_60   = character_32    // <
.var character_59   = character_32    // ;
.var character_37   = character_32    // %
.var character_61   = character_32    // =
.var character_40   = character_32    // (
.var character_41   = character_32    // )
.var character_43   = character_32    // +
.var character_45   = character_32    // -
                        //------------------------------------------------------------------------------
						*=$5000 "character lo table"
character_lo:   		.byte <character_at,<character_a,<character_b,<character_c,<character_d,<character_e,<character_f,<character_g,<character_h
						.byte <character_i,<character_j,<character_k,<character_l,<character_m,<character_n,<character_o,<character_p,<character_q
						.byte <character_r,<character_s,<character_t,<character_u,<character_v,<character_w,<character_x,<character_y,<character_z
						//        [               £              ]       Up Arrow      Left Arrow
						.byte <character_27,<character_28,<character_29,<character_30,<character_31
						//       space           !             "             #              $             % 
						.byte <character_32,<character_33,<character_34,<character_35,<character_36,<character_37
						//        &              '             (             )              *             +
						.byte <character_38,<character_39,<character_40,<character_41,<character_42,<character_43
						//        ,              -             .             /
						.byte <character_44,<character_45,<character_46,<character_47
						//        0              1             2             3             4             5
						.byte <character_48,<character_49,<character_50,<character_51,<character_52,<character_53
						//        6              7             8             9
						.byte <character_54,<character_55,<character_56,<character_57
						//        :              ;             <             =             >             ?
						.byte <character_58,<character_59,<character_60,<character_61,<character_62,<character_63
						// define pointers for the upper-case character lo-bits
						.byte <character_32,<character_A,<character_B,<character_C,<character_D,<character_E,<character_F,<character_G,<character_H
						.byte <character_I,<character_J,<character_K,<character_L,<character_M,<character_N,<character_O,<character_P,<character_Q
						.byte <character_R,<character_S,<character_T,<character_U,<character_V,<character_W,<character_X,<character_Y,<character_Z

						*=$5100 "character hi table"
character_hi:           .byte >character_at,>character_a,>character_b,>character_c,>character_d,>character_e,>character_f,>character_g,>character_h
						.byte >character_i,>character_j,>character_k,>character_l,>character_m,>character_n,>character_o,>character_p,>character_q
						.byte >character_r,>character_s,>character_t,>character_u,>character_v,>character_w,>character_x,>character_y,>character_z
						//        [               £              ]       Up Arrow      Left Arrow
						.byte >character_27,<character_28,<character_29,<character_30,<character_31
						//       space           !             "             #              $             % 
						.byte >character_32,>character_33,>character_34,>character_35,>character_36,>character_37
						//        &              '             (             )              *             +
						.byte >character_38,>character_39,>character_40,>character_41,>character_42,>character_43
						//        ,              -             .             /
						.byte >character_44,>character_45,>character_46,>character_47
						//        0              1             2             3             4             5
						.byte >character_48,>character_49,>character_50,>character_51,>character_52,>character_53
						//        6              7             8             9
						.byte >character_54,>character_55,>character_56,>character_57
						//        :              ;             <             =             >             ?
						.byte >character_58,>character_59,>character_60,>character_61,>character_62,>character_63
						// define pointers for the upper-case character lo-bits
						.byte >character_32,>character_A,>character_B,>character_C,>character_D,>character_E,>character_F,>character_G,>character_H
						.byte >character_I,>character_J,>character_K,>character_L,>character_M,>character_N,>character_O,>character_P,>character_Q
						.byte >character_R,>character_S,>character_T,>character_U,>character_V,>character_W,>character_X,>character_Y,>character_Z

                        //------------------------------------------------------------------------------
						*=$5200 "character widths table"
                        //------------------------------------------------------------------------------
charWidths:             //    @ a b c d e f g h i j k l  m n o p q r s t u v  w x y z  
						.byte 2,6,6,6,6,6,4,6,6,2,4,6,4,10,6,6,6,6,6,6,4,6,6,10,6,6,6
						//    [  £  ] ^ SP ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 8 7 8 9
						.byte 2,2,2,2,2,04,2,6,2,2,2,2,2,2,2,2,2,3,2,2,7,6,4,6,6,6,6,6,6,6,6
						//    : ; < = > ?
						.byte 2,2,2,2,2,6
                        //    @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 
						.byte 2,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
                        //------------------------------------------------------------------------------

						* = $8000 "line 1"
						#import "line01.asm"

						* = $8200 "line 2"
						#import "line02.asm"

						* = $8400 "line 3"
						#import "line03.asm"

						* = $8600 "line 4"
						#import "line04.asm"

						* = $8800 "line 5"
						#import "line05.asm"

						* = $8a00 "line 6"
						#import "line06.asm"

						* = $8c00 "line 7"
						#import "line07.asm"

						* = $8e00 "line 8"
						#import "line08.asm"

						* = $9000 "line 9"
						#import "line09.asm"

						* = $9200 "line 10"
						#import "line10.asm"

						* = $9400 "line 11"
						#import "line11.asm"

						* = $9600 "line 12"
						#import "line12.asm"

						*= $3000 "charset_data"
						#import "cupid_font.asm"
                        //------------------------------------------------------------------------------
						