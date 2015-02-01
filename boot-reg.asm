;fasm prog for booting
[BITS 16]
[ORG 7C00h]

	jmp	bootstart

;------------GDT Table---------------;
;GDTR:
;GDTsize DW GDT_END-GDT-1
;GDTbase DD 0x500

;GDT:
;NULL_SEL         EQU $-GDT  ; null descriptor is required (64bit per entry)
;	DD 0x0
;	DD 0x0
;CODESEL          EQU $-GDT  ; 4GB Flat Code at 0x0 with max 0xFFFFF limit
;	DW     0xFFFF           ; Limit(2):0xFFFF
;	DW     0x0              ; Base(3)
;	DB     0x0              ; Base(2)
;	DB     0x9A             ; Type: present,ring0,code,exec/read/accessed (10011000)
;	DB     0xCF             ; Limit(1):0xF | Flags:4Kb inc,32bit (11001111)
;	DB     0x0              ; Base(1)
;DATASEL          EQU $-GDT  ; 4GB Flat Data at 0x0 with max 0xFFFFF limit
;	DW     0xFFFF           ; Limit(2):0xFFFF
;	DW     0x0              ; Base(3)
;	DB     0x0              ; Base(2)
;	DB     0x92             ; Type: present,ring0,data/stack,read/write (10010010)
;	DB     0xCF             ; Limit(1):0xF | Flags:4Kb inc,32bit (11001111)
;	DB     0x0              ; Base(1)
;GDT_END:
;----------End GDT Table-------------;

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
message1	db	'Hello, this is me. I have started to run.', 0Ah, 0Dh, 0h
;message2	db	'Lets check out some values.', 0Ah, 0Dh, 0h
;                            4           16          28          40          52
message3	db	'CS: 0000h   DS: 0000h   ES: 0000h   SS: 0000h   FS: 0000h   GS: 0000h', 0Ah, 0Dh, 0h
;message4	db	'Their current values.', 0Ah, 0Dh, 0h
;message5	db	'CS: 0000h   DS: 0000h   ES: 0000h   SS: 0000h   FS: 0000h   GS: 0000h', 0Ah, 0Dh, 0h
message6	db	'The current drive number is 0000h.', 0Ah, 0Dh, 0h
;message7	db	'Trying to enable A20 gate ...', 0Ah, 0Dh, 0h
;a20enabled	db	'A20 was successfully enabled. Now creating GDT ...', 0Ah, 0Dh, 0h
;a20notenabled	db	'A20 couldnt be enabled. Fatal error!', 0Ah, 0Dh, 0h
;gdtcreated	db	'GDT has been created. Now, switching to Protected Mode ...', 0Ah, 0Dh, 0h
;pmodedone	db	'Muhahaha, we are now in protected mode. Now halting CPU.', 0Ah, 0Dh, 0h
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

;mov ax,0xb800              ; setup video segment
;mov gs,ax



;enable a20
;cli
;
;call waitkbdinputempty                   ; wait for kbd buffer to clear
;mov al,0xd1                ; tell it we want to write to output port
;out 0x64,al
;call waitkbdinputempty                   ; wait again for kbd to clear
;mov al,0xdf                ; set desired settings (A20 gate)
;out 0x60,al                ; send value to data reg
;call waitkbdinputempty                   ; wait for kbd to clear
;
;mov cx,0x10
;kbdwait:
;xor ax,ax                  ; do anything
;out 0xe0,ax                ; some mor nonsense
;loop kbdwait               ; loop to waste time
;
;check if a20 was enabled
;mov al,0xd0
;out 0x64,al                 ; tell kbdc we want to read output port
;call wkbdoutputfull                    ; wait for data to get in it
;in al,0x60                  ; get it
;test al,2                   ; test if A20 is on
;jnz a20_on                  ; if it is clear, then it is off
;mov al,'A'                  ; Error: A20 gate not enabled, halt pc
;call haltonerror
;a20_on:


;sti



;load kernel.bin from disk to 0x100000
;read:
;xor ax,ax                     ; Floppy Reset BIOS Function
;mov dl,[drive]                ; Select floppy that was booted from
;int 0x13
;jc read

;mov ax,0xffff
;mov es,ax                     ; Data buffer for file
;mov bx,0x10                   ; Start of segment
;mov ah,2        	      ; Function to read disk
;mov al,17                     ; Total sectors to read
;mov ch,0                      ; Track
;mov cl,2                      ; Sector
;mov dh,0                      ; Head | Drive is already loaded
;int 0x13                      ; Call BIOS read disk function
;jc read 	              ; motor error, try again

;mov ax,0xffff
;mov es,ax
;mov bx, 0x2210
;mov ah,2        	      ; Function to read disk
;mov al,18                     ; Total sectors to read
;mov ch,0                      ; Track
;mov cl,1                      ; Sector
;mov dh,1                      ; Head | Drive is already loaded
;int 0x13                      ; Call BIOS read disk function

;mov dx,0x3F2                  ; stop the motor
;mov al,0x0C                   ; from spinning
;out dx,al





; move GDT to 0x500
;xor ax,ax
;mov ds,ax
;mov es,ax
;mov si,GDT                    ; Move From [DS:SI]
;mov di,[GDTbase]              ; Move to [ES:DI]
;mov cx,[GDTsize]              ; size of GDT
;cld                           ; Clear the Direction Flag
;rep movsb                     ; Move it



;cli
;enter pmode
;mov eax,cr0
;or al,1
;mov cr0,eax


;load gdt
;lgdt[GDTR]


;clear cs/ip/eip
;jmp CODESEL:FLUSH        ; set cs to CODESEL

;[bits 32]
;FLUSH:

;refresh all segment registers
;mov eax,DATASEL
;mov ds,eax
;mov es,eax
;mov fs,eax
;mov gs,eax
;mov ss,eax
;mov esp,0xffff

;jump to k_init.asm
;jmp CODESEL:0x100000

;hlt



TIMES 510-($-$$) DB 0
SIGNATURE DW 0xAA55
