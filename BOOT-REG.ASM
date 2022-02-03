;fasm prog for booting
[BITS 16]
[ORG 7C00h]

jmp	bootstart

;------------Variables---------------;
dataseg		dw	0
stackseg	dw	0
drive		db	0                  ; boot drive number  [usually 0]
;----------End Variables-------------;

;-------------Messages---------------;
message1	db	'Hello, this is me. I have started to run.', 0Ah, 0Dh, 0h
message3	db	'CS: 0000h   DS: 0000h   ES: 0000h   SS: 0000h   FS: 0000h   GS: 0000h', 0Ah, 0Dh, 0h
message6	db	'The current drive number is 0000h.', 0Ah, 0Dh, 0h
halterror	db	'Reading disk.... .CPU has now to be reset. Press any key ...', 0Ah, 0Dh, 0h
;-----------End Messages-------------;

;------------Functions---------------;

;  SUB SHOWSTRING
; Description:
; This function uses BIOS interrupt 10h to display an
; ASCIZ string.
; Input:
; ds:si	= String address
; bl	= Text attribute
; Destroys = AX, BH
;
showstring:
	mov	ah, 0Eh		;Teletype character
	mov	bh, 0		;Page number
nextchar:
	lodsb
	or	al, al
	jz	displayover
	int	10h		;BIOS video interrupt
	jmp	nextchar
displayover:
	ret

;  SUB WRITEHEX
; Description:
; This function uses showstring function to display a
; hexadecimal number stored in AX.
; Input:
; ax = hex number
; ds:si	= String address
; Destroys = BH, CL
;
writehex:
	mov	cl, 4
	mov	bh, ah
	shr	bh, cl
	call	hex0
	mov	[si], bh
	mov	bh, ah
	and	bh, 0Fh
	call	hex0
	mov	[si+1], bh
	mov	bh, al
	shr	bh, cl
	call	hex0
	mov	[si+2], bh
	mov	bh, al
	and	bh, 0Fh
	call	hex0
	mov	[si+3], bh
	ret
hex0:
	cmp	bh, 9h
	ja	hex1
	add	bh, 30h
	jmp hex2
hex1:
	add	bh, 37h
hex2:
	ret

;  SUB WAITKBDINPUTEMPTY
; Description:
; Waits for the keyboard controller input buffer
; to become empty.
; Destroys = AL
;
waitkbdinputempty:
	xor	al, al
	in	al, 64h			; get kbd status
	test	al, 2			; is bit 1 clear?
	jnz	waitkbdinputempty	; if not wait some more
	ret

;  SUB WAITKBDOUTPUTFULL
; Description:
; Waits for the keyboard controller output buffer
; to become full.
; Destroys = AL
;
;wkbdoutputfull:
;	in	al, 64h			; get kbd status
;	test	al, 1			; is bit 0 clear?
;	jz	wkbdoutputfull		; if not wait some more
;	ret

;  SUB HALTONERROR
; Description:
; Halts the CPU.
; Destroys = Well, does that really matter?
;
haltonerror:
	xor	ax, ax
	mov	ds, ax
	mov	si, [halterror]
	mov	bl, 7
	call	showstring
	xor	ah, ah
	int	16h
	call	waitkbdinputempty
	mov	al, 0FEh		;reset
	out	64h, al
	cli
	hlt

;----------End Functions-------------;

;---------------------End Data---------------------;
bootstart:
	push	ss		;save original address of SS
	push	ds		;save original address of DS
	xor	ax, ax
	mov	ds, ax		;make DS to look to our data
	pop	ax
	mov	[dataseg], ax	;store original DS
	pop	ax
	mov	[stackseg], ax	;store original SS
	mov	ax, 1D00h
	mov	ss, ax		;make a new stack
	mov	sp, 200h	;of 512 bytes large

	mov [drive], dl		;save boot drive number(0x00=floppy 0x80=hard drive)

	mov	si, [message1]
	mov	bl, 7
	call	showstring
;	mov	si, [message2]
;	mov	bl, 6
;	call	showstring
	mov	ax, cs
	lea	si, [message3+4]
	call	writehex
	mov	ax, [dataseg]
	add	si, 12
	call	writehex
	mov	ax, es
	add	si, 12
	call	writehex
	mov	ax, [stackseg]
	add	si, 12
	call	writehex
	mov	ax, fs
	add	si, 12
	call	writehex
	mov	ax, gs
	add	si, 12
	call	writehex
	mov	si, [message3]
	mov	bl, 5
	call	showstring
	mov	al, [drive]
	xor	ah, ah
	lea	si, [message6+28]
	call	writehex
	mov	si, [message6]
	mov	bl, 7
	call	showstring


discread0:
	xor	ax, ax				; Floppy Reset BIOS Function
	mov	dl, [drive]			; Select floppy that was booted from
	int	13h
	jc	discread0


discread1:
	mov	ax, 0FFFFh
	mov	es, ax
	mov	bx, 10h
	mov	ah, 2				; Function to read disk
	mov	al, 17				; Total sectors to read
	mov	ch, 0				; Track
	mov	cl, 2				; Sector
	mov	dh, 0				; Head | Drive is already loaded
	int	13h				; Call BIOS read disk function
	jc	discread1			; motor error, try again

discread2:
	xor	ax, ax				; Floppy Reset BIOS Function
	mov	dl, [drive]			; Select floppy that was booted from
	int	13h
	jc	discread2

	call	haltonerror

	
TIMES 510-($-$$) DB 0
SIGNATURE DW 0xAA55
