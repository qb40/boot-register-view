;fasm prog for booting
[BITS 16]
[ORG 7C00h]

	jmp	bootstart

;------------Variables---------------;
dataseg		dw	0
stackseg	dw	0
;start_s	db	0                  ; starting sector    [0x1-0x12]
;total		db	0                  ; number of sector   [max 2880]
;track		db	0                  ; track number       [max 160]
;head		db	0                  ; head number        [max 2]
drive		db	0                  ; boot drive number  [usually 0]
;bseg		db	0                  ; memory address segment
;boff		dw	0                  ; and offset to load into
;----------End Variables-------------;

;-------------Messages---------------;
message1	db	'Press a key to reset disk ...', 0Ah, 0Dh, 0h
resetfailed	db	'Disk reset failed.  ', 0h
resetsuccess	db	'Disk reset was successful.', 0Ah, 0Dh, 0h
readfailed	db	'Disk read failed.   ', 0h
readsuccess	db	'Disk read success.', 0Ah, 0Dh, 0h
halterror	db	'Reading disk complete. CPU has now to be reset. Press any key ...', 0Ah, 0Dh, 0h
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
	xor	ax, ax
	mov	ds, ax		;make DS to look to our data
	mov	ax, 1D00h
	mov	ss, ax		;make a new stack
	mov	sp, 200h	;of 512 bytes large

	mov [drive], dl		;save boot drive number(0x00=floppy 0x80=hard drive)

	mov	si, [message1]
	mov	bl, 7
	call	showstring

discread0:
	xor	ax, ax				; Floppy Reset BIOS Function
	mov	dl, [drive]			; Select floppy that was booted from
	int	13h
	jnc	discread1
	mov	si, [resetfailed]
	mov	bl, 6
	call	showstring
	jmp	discread0

discread1:
	mov	si, [resetsuccess]
	mov	bl, 5
	call	showstring
	mov	ax, 0FFFFh
	mov	es, ax
	mov	bx, 10h

	mov	ah, 2				; Function to read disk
	mov	al, 17				; Total sectors to read
	mov	ch, 0				; Track
	mov	cl, 2				; Sector
	mov	dh, 0				; Head | Drive is already loaded
	int	13h				; Call BIOS read disk function
	jnc	discread2			; motor error, try again
	mov	si, [readfailed]
	mov	bl, 6
	call	showstring
	jmp	discread1


discread2:
	mov	si, [readsuccess]
	mov	bl, 5
	call	showstring
	xor	ax, ax				; Floppy Reset BIOS Function
	mov	dl, [drive]			; Select floppy that was booted from
	int	13h
	jnc	discread1
	mov	si, [resetfailed]
	mov	bl, 6
	call	showstring
	jmp	discread0
	mov	si, [resetsuccess]
	mov	bl, 5
	call	showstring

	call	haltonerror


TIMES 510-($-$$) DB 0
SIGNATURE DW 0xAA55
