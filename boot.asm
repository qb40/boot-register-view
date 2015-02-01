;fasm prog for booting
ORG 7C00h


	xor	ax, ax
	mov	ds, ax
	mov	si, HelloWorld
	mov	ah, 0Eh		;Teletype character
	mov	bh, 0		;Page number
	mov	bl, 7		;Text attribute
nextchar:
	lodsb
	or	al, al
	jz	displayover
	int	10h		;BIOS video interrupt
	jmp	nextchar
displayover:
	jmp	displayover	;infinite loop

HelloWorld	db	'Hello World!', 0Dh, 0Ah, 0	;last carriage return, new line
times	(510-($-$$))	db	0
dw	0AA55h		;last two bytes must be 55 AA (boot loader signature)
