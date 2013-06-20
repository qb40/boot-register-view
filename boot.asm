; boot.asm
; OS Bootloader
; Author: Xsism
; Date:   2/8/03


; Memory Layout:
; 0x0000-0x03FF	   IVT                
; 0x0400-0x04FF    BDA      
; 0x500-0xCFF      GDT
; 0xD00-0x1CFF     IDT
; 0x1D00-0x1F00    System Stack           
; 0xF0000-0xFFFFF  System code/data 
          
; 0xA0000-0xB7FFF VGA Buffer        
; 0xB8000-0xB8F9F Text Buffer	      

; Floppy setup: (C/H/S) 80C, 2H/C, 18S/H, 512b/S 2880S
; 00-01	Boot Sector code
; 01-33	System code/data
; 33-96	System Application code/data

; *  set these accordingly before you call 'read'  *
; ah=BIOS function    al=number of sectors to read into memory
; es:bx= seg:off memory location
; ch=track number     cl=starting sector
; dh=head number      dl=drive number
; calls interrupt 0x13 and checks CF for error(s)
;   ah=(on error)sectors that were read in

[bits 16]
[org 0x7c00]

jmp boot                      ; jump over the data to our code

;-----------------------Data-----------------------;
;------------GDT Table---------------;
GDTR:
GDTsize DW GDT_END-GDT-1
GDTbase DD 0x500

GDT:
NULL_SEL         EQU $-GDT  ; null descriptor is required (64bit per entry)
      DD 0x0
      DD 0x0
CODESEL          EQU $-GDT  ; 4GB Flat Code at 0x0 with max 0xFFFFF limit
      DW     0xFFFF           ; Limit(2):0xFFFF
      DW     0x0              ; Base(3)
      DB     0x0              ; Base(2)
      DB     0x9A             ; Type: present,ring0,code,exec/read/accessed (10011000)
      DB     0xCF             ; Limit(1):0xF | Flags:4Kb inc,32bit (11001111)
      DB     0x0              ; Base(1)
DATASEL          EQU $-GDT  ; 4GB Flat Data at 0x0 with max 0xFFFFF limit
      DW     0xFFFF           ; Limit(2):0xFFFF
      DW     0x0              ; Base(3)
      DB     0x0              ; Base(2)
      DB     0x92             ; Type: present,ring0,data/stack,read/write (10010010)
      DB     0xCF             ; Limit(1):0xF | Flags:4Kb inc,32bit (11001111)
      DB     0x0              ; Base(1)
GDT_END:
;----------End GDT Table-------------;

;------------Variables---------------;
start_s DB 0                  ; starting sector    [0x1-0x12]
total   DB 0                  ; number of sector   [max 2880]
track   DB 0                  ; track number       [max 160]
head    DB 0                  ; head number        [max 2]
drive   DB 0                  ; boot drive number  [usually 0]
bseg    DB 0                  ; memory address segment
boff    DW 0                  ; and offset to load into
;----------End Variables-------------;

;------------Functions---------------;
;; 'wait keyboard to clear' function ;;
wkc:
xor al,al
in al, 0x64                   ; get kbd status
test al, 2                    ; is bit 1 clear?
jnz wkc                       ; if not wait some more
ret


;; 'wait keyboard to be full' function ;;
wkf:
xor cx,cx
in al, 0x64                   ; get kbd status
test al, 1                    ; is bit 0 clear?
jz wkf                        ; if not wait some more
ret

;; 'halt on error' function ;;
halt:
mov byte [gs:0],al
mov byte [gs:1],0x04
cli
hlt
;----------End Functions-------------;

;---------------------End Data---------------------;
boot:
mov [drive],dl             ; save boot drive number(0x00=floppy 0x80=hard drive)

mov ax,cs                  ; setup ds segment
mov ds,ax
mov es,ax
mov fs,ax
mov ax,0x1D0               ; stack is at 0x1D00
mov ss,ax                  ; align stack also
mov sp,0x200               ; 512 byte stack

mov ax,0xb800              ; setup video segment
mov gs,ax

jmp init                   ; Some BIOSes jump to 0x7c0:0x0 rather than 0x0:0x7c0


init:


;enable a20
cli

call wkc                   ; wait for kbd buffer to clear
mov al,0xd1                ; tell it we want to write to output port
out 0x64,al
call wkc                   ; wait again for kbd to clear
mov al,0xdf                ; set desired settings (A20 gate)
out 0x60,al                ; send value to data reg
call wkc                   ; wait for kbd to clear

mov cx,0x10
kbdwait:
xor ax,ax                  ; do anything
out 0xe0,ax                ; some mor nonsense
loop kbdwait               ; loop to waste time

;check if a20 was enabled
mov al,0xd0
out 0x64,al                 ; tell kbdc we want to read output port
call wkf                    ; wait for data to get in it
in al,0x60                  ; get it
test al,2                   ; test if A20 is on
jnz a20_on                  ; if it is clear, then it is off
mov al,'A'                  ; Error: A20 gate not enabled, halt pc
call halt
a20_on:


sti



;load kernel.bin from disk to 0x100000
read:
xor ax,ax                     ; Floppy Reset BIOS Function
mov dl,[drive]                ; Select floppy that was booted from
int 0x13 
jc read

mov ax,0xffff 
mov es,ax                     ; Data buffer for file
mov bx,0x10                   ; Start of segment
mov ah,2        	      ; Function to read disk
mov al,17                     ; Total sectors to read
mov ch,0                      ; Track
mov cl,2                      ; Sector
mov dh,0                      ; Head | Drive is already loaded
int 0x13                      ; Call BIOS read disk function
jc read 	              ; motor error, try again

mov ax,0xffff 
mov es,ax
mov bx, 0x2210
mov ah,2        	      ; Function to read disk
mov al,18                     ; Total sectors to read
mov ch,0                      ; Track
mov cl,1                      ; Sector
mov dh,1                      ; Head | Drive is already loaded
int 0x13                      ; Call BIOS read disk function

mov dx,0x3F2                  ; stop the motor
mov al,0x0C                   ; from spinning
out dx,al 





; move GDT to 0x500
xor ax,ax
mov ds,ax
mov es,ax
mov si,GDT                    ; Move From [DS:SI]
mov di,[GDTbase]              ; Move to [ES:DI]
mov cx,[GDTsize]              ; size of GDT
cld                           ; Clear the Direction Flag
rep movsb                     ; Move it



cli
;enter pmode
mov eax,cr0
or al,1
mov cr0,eax


;load gdt
lgdt[GDTR]


;clear cs/ip/eip
jmp CODESEL:FLUSH        ; set cs to CODESEL

[bits 32]
FLUSH:

;refresh all segment registers
mov eax,DATASEL
mov ds,eax
mov es,eax
mov fs,eax
mov gs,eax
mov ss,eax
mov esp,0xffff

;jump to k_init.asm
jmp CODESEL:0x100000

hlt



TIMES 510-($-$$) DB 0

SIGNATURE DW 0xAA55
