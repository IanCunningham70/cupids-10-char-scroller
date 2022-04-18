

; CHARSET IMAGE DATA...
; 30 images, 8 bytes per image, total size is 240 ($F0) bytes.

charset_data

.byte $00,$00,$00,$00,$00,$00,$00,$00,$FE,$FE,$FE,$FE,$FE,$FE,$FE,$FE
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
.byte $0F,$3F,$7F,$7F,$FF,$FF,$FF,$FF,$07,$1F,$3F,$3F,$7F,$7F,$7F,$7F
.byte $E0,$F8,$FC,$FC,$FE,$FE,$FE,$FE,$F0,$FC,$FE,$FE,$FF,$FF,$FF,$FF
.byte $E0,$F8,$FC,$FC,$FE,$FC,$FE,$FE,$80,$C0,$E0,$F0,$F8,$FC,$FE,$FE
.byte $80,$C0,$E0,$F0,$F8,$FC,$FE,$FF,$01,$03,$07,$0F,$1F,$3F,$7F,$7F
.byte $01,$03,$07,$0F,$1F,$3F,$7F,$FF,$F0,$C0,$80,$80,$00,$00,$00,$00
.byte $FF,$FF,$FF,$FF,$7F,$7F,$3F,$0F,$7F,$7F,$7F,$7F,$3F,$3F,$1F,$07
.byte $FF,$FF,$FF,$FF,$FE,$FE,$FC,$F0,$FE,$FE,$FE,$FE,$FC,$FC,$F8,$E0
.byte $FF,$FF,$FF,$FF,$FE,$FE,$FC,$F0,$0F,$03,$01,$01,$00,$00,$00,$00
.byte $00,$00,$00,$00,$01,$01,$03,$0F,$FE,$FE,$FC,$F8,$F0,$E0,$C0,$80
.byte $FF,$FE,$FC,$F8,$F0,$E0,$C0,$80,$7F,$7F,$3F,$1F,$0F,$07,$03,$01
.byte $FE,$7E,$3E,$1E,$0E,$06,$02,$00,$FF,$7F,$3F,$1F,$0F,$07,$03,$01
.byte $FE,$FC,$F0,$C0,$C0,$F0,$FC,$FE,$00,$00,$00,$00,$80,$80,$C0,$F0
.byte $7F,$3F,$0F,$03,$03,$0F,$3F,$7F,$FF,$FF,$7F,$FF,$7F,$7F,$3F,$0F


; CHARSET IMAGE ATTRIBUTE DATA...
; 30 attributes, 1 attribute per image, 8 bits per attribute, total size is 30 ($1E) bytes.
; nb. Upper nybbles = material, lower nybbles = colour.

charset_attrib_data

.byte $05,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B
.byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B
