# -*- coding: utf-8 -*-
strr = """; =============================================================================
; BareMetal -- a 64-bit OS written in Assembly for x86-64 systems
; Copyright (C) 2008-2014 Return Infinity -- see LICENSE.TXT
;
; INIT_64
; =============================================================================

align 16
db 'DEBUG: INIT_64  '
align 16


init_64:
	; Make sure that memory range 0x110000 - 0x200000 is cleared
	mov rdi, os_SystemVariables
	mov rcx, 122880            ; Clear 960 KiB
	xor rax, rax
	rep stosq                  ; Store rax to [rdi], rcx - 1, rdi + 8, if rcx > 0 then do it again

	mov word [os_Screen_Rows], 25
	mov word [os_Screen_Cols], 80
	mov rsi, 0x5080
	lodsd
	cmp eax, 0
	je nographics
	call init_screen
nographics:

	mov word [os_Screen_Cursor_Row], 0
	mov word [os_Screen_Cursor_Col], 0
	call os_screen_clear		; Clear screen and display cursor

	; Display CPU information
	mov ax, [os_Screen_Rows]
	sub ax, 5
	mov word [os_Screen_Cursor_Row], ax
	mov word [os_Screen_Cursor_Col], 0
	mov rsi, cpumsg
	call os_output
	xor eax, eax
	mov rsi, 0x5012
	lodsw
	mov rdi, os_temp_string
	mov rsi, rdi
	call os_int_to_string
	call os_output
	mov rsi, coresmsg
	call os_output
	mov rsi, 0x5010
	lodsw
	mov rdi, os_temp_string
	mov rsi, rdi
	call os_int_to_string
	call os_output
	mov rsi, mhzmsg
	call os_output

	xor rdi, rdi 			; Create the 64-bit IDT (at linear address 0x0000000000000000) as defined by Pure64

	; Create exception gate stubs (Pure64 has already set the correct gate markers)
	mov rcx, 32
	mov rax, exception_gate
make_exception_gate_stubs:
	call create_gate
	add rdi, 1
	sub rcx, 1
	jnz make_exception_gate_stubs

	; Create interrupt gate stubs (Pure64 has already set the correct gate markers)
	mov rcx, 256-32
	mov rax, interrupt_gate
make_interrupt_gate_stubs:
	call create_gate
	add rdi, 1
	sub rcx, 1
	jnz make_interrupt_gate_stubs

	; Set up the exception gates for all of the CPU exceptions
	mov rcx, 20
	xor rdi, rdi
	mov rax, exception_gate_00
make_exception_gates:
	call create_gate
	add rdi, 1
	add rax, 16			; The exception gates are aligned at 16 bytes
	sub rcx, 1
	jnz make_exception_gates

	; Set up the IRQ handlers (Network IRQ handler is configured in init_net)
	mov rdi, 0x21
	mov rax, keyboard
	call create_gate
	mov rdi, 0x22
	mov rax, cascade
	call create_gate
	mov rdi, 0x28
	mov rax, rtc
	call create_gate
	mov rdi, 0x80
	mov rax, ap_wakeup
	call create_gate
	mov rdi, 0x81
	mov rax, ap_reset
	call create_gate

	; Set up RTC
	; Rate defines how often the RTC interrupt is triggered
	; Rate is a 4-bit value from 1 to 15. 1 = 32768Hz, 6 = 1024Hz, 15 = 2Hz
	; RTC value must stay at 32.768KHz or the computer will not keep the correct time
	; http://wiki.osdev.org/RTC
rtc_poll:
	mov al, 0x0A			; Status Register A
	out 0x70, al
	in al, 0x71
	test al, 0x80			; Is there an update in process?
	jne rtc_poll			; If so then keep polling
	mov al, 0x0A			; Status Register A
	out 0x70, al
	mov al, 00101101b		; RTC@32.768KHz (0010), Rate@8Hz (1101)
	out 0x71, al
	mov al, 0x0B			; Status Register B
	out 0x70, al			; Select the address
	in al, 0x71			; Read the current settings
	push rax
	mov al, 0x0B			; Status Register B
	out 0x70, al			; Select the address
	pop rax
	bts ax, 6			; Set Periodic(6)
	out 0x71, al			; Write the new settings
	mov al, 0x0C			; Acknowledge the RTC
	out 0x70, al
	in al, 0x71

	; Set color palette
	xor eax, eax
	mov dx, 0x03C8			; DAC Address Write Mode Register
	out dx, al
	mov dx, 0x03C9			; DAC Data Register
	mov rbx, 16			; 16 lines
nextline:
	mov rcx, 16			; 16 colors
	mov rsi, palette
nexttritone:
	lodsb
	out dx, al
	lodsb
	out dx, al
	lodsb
	out dx, al
	dec rcx
	cmp rcx, 0
	jne nexttritone
	dec rbx
	cmp rbx, 0
	jne nextline			; Set the next 16 colors to the same
	mov eax, 0x14			; Fix for color 6
	mov dx, 0x03c8			; DAC Address Write Mode Register
	out dx, al
	mov dx, 0x03c9			; DAC Data Register
	mov rsi, palette
	add rsi, 18
	lodsb
	out dx, al
	lodsb
	out dx, al
	lodsb
	out dx, al

	xor eax, eax
	xor ebx, ebx
	xor ecx, ecx
	; Grab data from Pure64's infomap
	mov rsi, 0x5008
	lodsd				; Load the BSP ID
	mov ebx, eax			; Save it to EBX
	mov rsi, 0x5012
	lodsw				; Load the number of activated cores
	mov cx, ax			; Save it to CX
	mov rsi, 0x5060
	lodsq
	mov [os_LocalAPICAddress], rax
	lodsq
	mov [os_IOAPICAddress], rax

	mov rsi, 0x5012
	lodsw
	mov [os_NumCores], ax

	mov rsi, 0x5020
	lodsd
	mov [os_MemAmount], eax		; In MiB's

	mov rsi, 0x5040
	lodsq
	mov [os_HPETAddress], rax

	; Build the OS memory table
	call init_memory_map

	; Initialize all AP's to run our reset code. Skip the BSP
	xor rax, rax
	mov rsi, 0x0000000000005100	; Location in memory of the Pure64 CPU data
next_ap:
	cmp cx, 0
	je no_more_aps
	lodsb				; Load the CPU APIC ID
	cmp al, bl
	je skip_ap
	call os_smp_reset		; Reset the CPU
skip_ap:
	sub cx, 1
	jmp next_ap

no_more_aps:

	; Display memory information
	mov rsi, memmsg
	call os_output
	mov eax, [os_MemAmount]		; In MiB's
	mov rdi, os_temp_string
	mov rsi, rdi
	call os_int_to_string
	call os_output
	mov rsi, mibmsg
	call os_output

	; Enable specific interrupts
	mov al, 0x01			; Keyboard IRQ
	call os_pic_mask_clear
	mov al, 0x02			; Cascade IRQ
	call os_pic_mask_clear
	mov al, 0x08			; RTC IRQ
	call os_pic_mask_clear

	ret

; create_gate
; rax = address of handler
; rdi = gate # to configure
create_gate:
	push rdi
	push rax

	shl rdi, 4			; quickly multiply rdi by 16
	stosw				; store the low word (15..0)
	shr rax, 16
	add rdi, 4			; skip the gate marker
	stosw				; store the high word (31..16)
	shr rax, 16
	stosd				; store the high dword (63..32)

	pop rax
	pop rdi
	ret


init_memory_map:			; Build the OS memory table
	push rax
	push rcx
	push rdi

	; Build a fresh memory map for the system
	mov rdi, os_MemoryMap
	push rdi
	xor rcx, rcx
	mov cx, [os_MemAmount]
	shr cx, 1			; Divide actual memory by 2
	mov al, 1
	rep stosb
	pop rdi
	mov al, 2
	stosb				; Mark the first 2 MiB as in use (by Kernel and system buffers)
;	stosb				; As well as the second 2 MiB (by loaded application)
	; The CLI should take care of the Application memory

	; Allocate memory for CPU stacks (2 MiB's for each core)
	xor rcx, rcx
	mov cx, [os_NumCores]		; Get the amount of cores in the system
	call os_mem_allocate		; Allocate a page for each core
	cmp rcx, 0			; os_mem_allocate returns 0 on failure
	je system_failure
	add rax, 2097152
	mov [os_StackBase], rax		; Store the Stack base address

	pop rdi
	pop rcx
	pop rax
	ret


system_failure:
	mov rsi, memory_message
	call os_output
system_failure_hang:
	hlt
	jmp system_failure_hang
	ret


; -----------------------------------------------------------------------------
init_screen:
	mov rsi, 0x5080
	xor eax, eax
	lodsd				; VIDEO_BASE
	mov [os_VideoBase], rax
	xor eax, eax
	xor ecx, ecx

	lodsw				; VIDEO_X
	mov [os_VideoX], ax		; ex: 1024

	xor edx, edx
	mov cl, [font_width]
	div cx
	mov [os_Screen_Cols], ax

	lodsw				; VIDEO_Y
	mov [os_VideoY], ax		; ex: 768

	xor edx, edx
	mov cl, [font_height]
	div cx
	mov [os_Screen_Rows], ax

	lodsb				; VIDEO_DEPTH
	mov [os_VideoDepth], al

	xor eax, eax
	xor ecx, ecx
	mov ax, [os_VideoX]
	mov cx, [os_VideoY]
	mul ecx
	mov [os_Screen_Pixels], eax
	xor ecx, ecx
	mov cl, [os_VideoDepth]
	shr cl, 3
	mul ecx
	mov [os_Screen_Bytes], eax

	xor eax, eax
	xor ecx, ecx
	mov ax, [os_VideoX]
	mov cl, [font_height]
	mul cx
	mov cl, [os_VideoDepth]
	shr cl, 3
	mul ecx
	mov dword [os_Screen_Row_2], eax

	mov eax, 0x00FFFFFF
	mov [os_Font_Color], eax

	mov al, 1
	mov [os_VideoEnabled], al

	ret
; -----------------------------------------------------------------------------


; =============================================================================
; EOHEF
; =============================================================================
; BareMetal -- a 64-bit OS written in Assembly for x86-64 systems
; Copyright (C) 2008-2014 Return Infinity -- see LICENSE.TXT
;
; INIT_NET
; =============================================================================

align 16
db 'DEBUG: INIT_NET '
align 16


init_net:
	mov rsi, networkmsg
	call os_output

	; Search for a supported NIC
	xor ebx, ebx			; Clear the Bus number
	xor ecx, ecx			; Clear the Device/Slot number
	mov edx, 2			; Register 2 for Class code/Subclass

init_net_probe_next:
	call os_pci_read_reg
	shr eax, 16			; Move the Class/Subclass code to AX
	cmp ax, 0x0200			; Network Controller (02) / Ethernet (00)
	je init_net_probe_find_driver	; Found a Network Controller... now search for a driver
	add ecx, 1
	cmp ecx, 256			; Maximum 256 devices/functions per bus
	je init_net_probe_next_bus
	jmp init_net_probe_next

init_net_probe_next_bus:
	xor ecx, ecx
	add ebx, 1
	cmp ebx, 256			; Maximum 256 buses
	je init_net_probe_not_found
	jmp init_net_probe_next

init_net_probe_find_driver:
	xor edx, edx				; Register 0 for Device/Vendor ID
	call os_pci_read_reg			; Read the Device/Vendor ID from the PCI device
	mov r8d, eax				; Save the Device/Vendor ID in R8D
	mov rsi, NIC_DeviceVendor_ID
	lodsd					; Load a driver ID - Low half must be 0xFFFF
init_net_probe_find_next_driver:
	mov rdx, rax				; Save the driver ID
init_net_probe_find_next_device:
	lodsd					; Load a device and vendor ID from our list of supported NICs
	cmp eax, 0x00000000			; 0x00000000 means we have reached the end of the list
	je init_net_probe_not_found		; No supported NIC found
	cmp ax, 0xFFFF				; New driver ID?
	je init_net_probe_find_next_driver	; We found the next driver type
	cmp eax, r8d
	je init_net_probe_found			; If Carry is clear then we found a supported NIC
	jmp init_net_probe_find_next_device	; Check the next device

init_net_probe_found:
	cmp edx, 0x8169FFFF
	je init_net_probe_found_rtl8169
	cmp edx, 0x8254FFFF
	je init_net_probe_found_i8254x
	jmp init_net_probe_not_found

init_net_probe_found_rtl8169:
	call os_net_rtl8169_init
	mov rdi, os_net_transmit
	mov rax, os_net_rtl8169_transmit
	stosq
	mov rax, os_net_rtl8169_poll
	stosq
	mov rax, os_net_rtl8169_ack_int
	stosq
	jmp init_net_probe_found_finish

init_net_probe_found_i8254x:
	call os_net_i8254x_init
	mov rdi, os_net_transmit
	mov rax, os_net_i8254x_transmit
	stosq
	mov rax, os_net_i8254x_poll
	stosq
	mov rax, os_net_i8254x_ack_int
	stosq
	jmp init_net_probe_found_finish

init_net_probe_found_finish:
	xor eax, eax
	mov al, [os_NetIRQ]

	add al, 0x20
	mov rdi, rax
	mov rax, network
	call create_gate

	; Enable the Network IRQ
	mov al, [os_NetIRQ]
	call os_pic_mask_clear

	mov byte [os_NetEnabled], 1	; A supported NIC was found. Signal to the OS that networking is enabled
	call os_ethernet_ack_int	; Call the driver function to acknowledge the interrupt internally

	mov cl, 6
	mov rsi, os_NetMAC
nextbyte:
	lodsb
	call os_debug_dump_al
	sub cl, 1
	cmp cl, 0
	jne nextbyte
	mov rsi, closebracketmsg
	call os_output
	ret
	
init_net_probe_not_found:
	mov rsi, namsg
	call os_output
	mov rsi, closebracketmsg
	call os_output
	ret


; =============================================================================
; EOHEF
; =============================================================================
; BareMetal -- a 64-bit OS written in Assembly for x86-64 systems
; Copyright (C) 2008-2014 Return Infinity -- see LICENSE.TXT
;
; BMFS Functions
; =============================================================================

align 16
db 'DEBUG: BMFS     '
align 16


; -----------------------------------------------------------------------------
; init_bmfs -- Initialize the BMFS driver
init_bmfs:
	push rdi
	push rdx
	push rcx
	push rax

	mov byte [bmfs_directory], 0

	cmp byte [os_DiskEnabled], 0x01
	jne init_bmfs_nodisk

	; Read directory to memory
	mov rax, 8			; Start to read from 4K in
	mov rcx, 8			; Read 8 sectors (4KiB)
	xor edx, edx			; Read from drive 0
	mov rdi, bmfs_directory
	call readsectors

	; Get total blocks
	mov eax, [hd1_size]		; in mebibytes (MiB)
	shr rax, 1
	mov [bmfs_TotalBlocks], rax

init_bmfs_nodisk:

	pop rax
	pop rcx
	pop rdx
	pop rdi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_file_open -- Open a file on disk
; IN:	RSI = File name (zero-terminated string)
; OUT:	RAX = File I/O handler, 0 on error
;	All other registers preserved
os_bmfs_file_open:
	push rsi
	push rdx
	push rcx
	push rbx

	; Query the existence
	call os_bmfs_file_internal_query
	jc os_bmfs_file_open_error
	mov rax, rbx			; Slot #
	add rax, 10			; Files start at 10

	; Is it already open? If not, mark as open
	mov rsi, os_filehandlers
	add rsi, rbx
	cmp byte [rsi], 0		; 0 is closed
	jne os_bmfs_file_open_error
	mov byte [rsi], 1		; Set to open

	; Reset the seek
	mov rsi, os_filehandlers_seek
	shl rbx, 3			; Quick multiply by 8
	add rsi, rbx
	xor ebx, ebx			; SEEK_START
	mov qword [rsi], rbx

	jmp os_bmfs_file_open_done

os_bmfs_file_open_error:
	xor eax, eax

os_bmfs_file_open_done:
	pop rbx
	pop rcx
	pop rdx
	pop rsi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_file_close -- Close an open file
; IN:	RAX = File I/O handler
; OUT:	All registers preserved
os_bmfs_file_close:
	push rsi
	push rax

	; Is it in the valid file handler range?
	sub rax, 10			; Subtract the handler offset
	cmp rax, 64			; BMFS has up to 64 files
	jg os_bmfs_file_close_error

	; Mark as closed
	mov rsi, os_filehandlers
	add rsi, rax
	mov byte [rsi], 0		; Set to closed

os_bmfs_file_close_error:

os_bmfs_file_close_done:
	pop rax
	pop rsi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_file_read -- Read a number of bytes from a file
; IN:	RAX = File I/O handler
;	RCX = Number of bytes to read (automatically rounded up to next 2MiB)
;	RDI = Destination memory address
; OUT:	RCX = Number of bytes read
;	All other registers preserved
os_bmfs_file_read:
	push rdi
	push rsi
	push rdx
	push rcx
	push rbx
	push rax

	; Is it a valid read?
	cmp rcx, 0
	je os_bmfs_file_read_error

	; Is it in the valid file handler range?
	sub rax, 10			; Subtract the handler offset
	mov rbx, rax			; Keep the file ID
	cmp rax, 64			; BMFS has up to 64 files
	jg os_bmfs_file_read_error

	; Is this an open file?
	mov rsi, os_filehandlers
	add rsi, rax
	cmp byte [rsi], 0
	je os_bmfs_file_read_error

	; Get the starting block
	mov rsi, bmfs_directory		; Beginning of directory structure
	shl rax, 6			; Quickly multiply by 64 (size of BMFS record)
	add rsi, rax
	add rsi, 32			; Offset to starting block
	lodsq				; Load starting block in RAX

	; Add the current offset
	; Currently always starting from start

	; Round up 'bytes to read' to the next 2MiB block
	add rcx, 2097151		; 2MiB - 1 byte
	shr rcx, 21			; Quick divide by 2097152

	; Read the block(s)
	xor edx, edx			; Drive 0
	call os_bmfs_block_read
	jmp os_bmfs_file_read_done

os_bmfs_file_read_error:
	xor ecx, ecx

os_bmfs_file_read_done:

	; Increment the offset

	pop rax
	pop rbx
	pop rcx
	pop rdx
	pop rsi
	pop rdi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_file_write -- Write a number of bytes to a file
; IN:	RAX = File I/O handler
;	RCX = Number of bytes to write
;	RSI = Source memory address
; OUT:	RCX = Number of bytes written
;	All other registers preserved
os_bmfs_file_write:
	push rdi
	push rsi
	push rdx
	push rcx
	push rbx
	push rax

	; Is it a valid write?
	cmp rcx, 0
	je os_bmfs_file_write_error

	; Is it in the valid file handler range?
	sub rax, 10			; Subtract the handler offset
	mov rbx, rax			; Keep the file ID
	cmp rax, 64			; BMFS has up to 64 files
	jg os_bmfs_file_write_error

	; Is this an open file?
	mov rdi, os_filehandlers
	add rdi, rax
	cmp byte [rdi], 0
	je os_bmfs_file_write_error

	; Flush directory to disk

os_bmfs_file_write_error:
	xor ecx, ecx

os_bmfs_file_write_done:

	pop rax
	pop rbx
	pop rcx
	pop rdx
	pop rsi
	pop rdi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_file_seek -- Seek to position in a file
; IN:	RAX = File I/O handler
;	RCX = Number of bytes to offset from origin
;	RDX = Origin
; OUT:	All registers preserved
os_bmfs_file_seek:
	; Is this an open file?

	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_file_internal_query -- Search for a file name and return information
; IN:	RSI = Pointer to file name
; OUT:	RAX = Staring block number
;	RBX = Offset to entry
;	RCX = File size in bytes
;	RDX = Reserved blocks
;	Carry set if not found. If carry is set then ignore returned values
os_bmfs_file_internal_query:
	push rdi

	clc				; Clear carry
	mov rdi, bmfs_directory		; Beginning of directory structure

os_bmfs_file_internal_query_next:
	call os_string_compare
	jc os_bmfs_file_internal_query_found
	add rdi, 64			; Next record
	cmp rdi, bmfs_directory + 0x1000	; End of directory
	jne os_bmfs_file_internal_query_next
	stc				; Set flag for file not found
	pop rdi
	ret

os_bmfs_file_internal_query_found:
	clc				; Clear flag for file found
	mov rbx, rdi
	sub rbx, bmfs_directory
	shr rbx, 6				; Quick divide by 64 for offset (entry) number
	mov rdx, [rdi + BMFS_DirEnt.reserved]	; Reserved blocks
	mov rcx, [rdi + BMFS_DirEnt.size]	; Size in bytes
	mov rax, [rdi + BMFS_DirEnt.start]	; Starting block number

	pop rdi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_file_query -- Search for a file name and return information
; IN:	RSI = Pointer to file name
; OUT:	RCX = File size in bytes
;	Carry set if not found. If carry is set then ignore returned values
os_bmfs_file_query:
	push rdi

	clc				; Clear carry
	mov rdi, bmfs_directory		; Beginning of directory structure

os_bmfs_file_query_next:
	call os_string_compare
	jc os_bmfs_file_query_found
	add rdi, 64			; Next record
	cmp rdi, bmfs_directory + 0x1000	; End of directory
	jne os_bmfs_file_query_next
	stc				; Set flag for file not found
	pop rdi
	ret

os_bmfs_file_query_found:
	clc				; Clear flag for file found
	mov rcx, [rdi + BMFS_DirEnt.size]	; Size in bytes

	pop rdi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_file_create -- Create a file on the hard disk
; IN:	RSI = Pointer to file name, must be <= 32 characters
;	RCX = File size to reserve (rounded up to the nearest 2MiB)
; OUT:	Carry clear on success, set on failure
; Note:	This function pre-allocates all blocks required for the file
os_bmfs_file_create:

	; Flush directory to disk

	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_file_delete -- Delete a file from the hard disk
; IN:	RSI = File name to delete
; OUT:	Carry clear on success, set on failure
os_bmfs_file_delete:
	push rdx
	push rcx
	push rbx
	push rax

	call os_bmfs_file_internal_query
	jc os_bmfs_file_delete_notfound

	mov byte [rbx + BMFS_DirEnt.filename], 0x01 ; Add deleted marker to file name

	; Flush directory to disk

os_bmfs_file_delete_notfound:
	pop rax
	pop rbx
	pop rcx
	pop rdx
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_block_read -- Read a number of blocks into memory
; IN:	RAX = Starting block #
;	RCX = Number of blocks to read
;	RDI = Memory location to store blocks
; OUT:	
os_bmfs_block_read:
	cmp rcx, 0
	je os_bmfs_block_read_done	; Bail out if instructed to read nothing

	; Calculate the starting sector
	shl rax, 12			; Multiply block start count by 4096 to get sector start count

	; Calculate sectors to read
	shl rcx, 12			; Multiply block count by 4096 to get number of sectors to read
	mov rbx, rcx
	
os_bmfs_block_read_loop:
	mov rcx, 4096			; Read 2MiB at a time (4096 512-byte sectors = 2MiB)
	call readsectors
	sub rbx, 4096
	jnz os_bmfs_block_read_loop

os_bmfs_block_read_done:
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_bmfs_block_write -- Write a number of blocks to disk
; IN:	RAX = Starting block #
;	RCX = Number of blocks to write
;	RSI = Memory location of blocks to store
; OUT:	
os_bmfs_block_write:
	cmp rcx, 0
	je os_bmfs_block_write_done	; Bail out if instructed to write nothing	

	; Calculate the starting sector
	shl rax, 12			; Multiply block start count by 4096 to get sector start count

	; Calculate sectors to write
	shl rcx, 12			; Multiply block count by 4096 to get number of sectors to write
	mov rbx, rcx
	
os_bmfs_block_write_loop:
	mov rcx, 4096			; Write 2MiB at a time (4096 512-byte sectors = 2MiB)
	call writesectors
	sub rbx, 4096
	jnz os_bmfs_block_write_loop

os_bmfs_block_write_done:
	ret
; -----------------------------------------------------------------------------


; =============================================================================
; EOHEF
; =============================================================================
; BareMetal -- a 64-bit OS written in Assembly for x86-64 systems
; Copyright (C) 2008-2014 Return Infinity -- see LICENSE.TXT
;
; Intel i8254x NIC.
; =============================================================================

align 16
db 'DEBUG: I8254X   '
align 16


; -----------------------------------------------------------------------------
; Initialize an Intel 8254x NIC
;  IN:	BL  = Bus number of the Intel device
;	CL  = Device/Slot number of the Intel device
os_net_i8254x_init:
	push rsi
	push rdx
	push rcx
	push rax

	; Read BAR4, If BAR4 is all 0'z then we are using 32-bit addresses

	; Grab the Base I/O Address of the device
	mov dl, 0x04				; BAR0
	call os_pci_read_reg
	and eax, 0xFFFFFFF0			; EAX now holds the Base Memory IO Address (clear the low 4 bits)
	mov dword [os_NetIOBaseMem], eax

	; Grab the IRQ of the device
	mov dl, 0x0F				; Get device's IRQ number from PCI Register 15 (IRQ is bits 7-0)
	call os_pci_read_reg
	mov [os_NetIRQ], al			; AL holds the IRQ

	; Enable PCI Bus Mastering
	mov dl, 0x01				; Get Status/Command
	call os_pci_read_reg
	bts eax, 2
	call os_pci_write_reg

	; Grab the MAC address
	mov rsi, [os_NetIOBaseMem]
	mov eax, [rsi+0x5400]				; RAL
	cmp eax, 0x00000000
	je os_net_i8254x_init_get_MAC_via_EPROM
	mov [os_NetMAC], al
	shr eax, 8
	mov [os_NetMAC+1], al
	shr eax, 8
	mov [os_NetMAC+2], al
	shr eax, 8
	mov [os_NetMAC+3], al
	mov eax, [rsi+0x5404]				; RAH
	mov [os_NetMAC+4], al
	shr eax, 8
	mov [os_NetMAC+5], al
	jmp os_net_i8254x_init_done_MAC

os_net_i8254x_init_get_MAC_via_EPROM:
	mov rsi, [os_NetIOBaseMem]
	mov eax, 0x00000001
	mov [rsi+0x14], eax
	mov eax, [rsi+0x14]
	shr eax, 16
	mov [os_NetMAC], al
	shr eax, 8
	mov [os_NetMAC+1], al
	mov eax, 0x00000101
	mov [rsi+0x14], eax
	mov eax, [rsi+0x14]
	shr eax, 16
	mov [os_NetMAC+2], al
	shr eax, 8
	mov [os_NetMAC+3], al
	mov eax, 0x00000201
	mov [rsi+0x14], eax
	mov eax, [rsi+0x14]
	shr eax, 16
	mov [os_NetMAC+4], al
	shr eax, 8
	mov [os_NetMAC+5], al
os_net_i8254x_init_done_MAC:

	; Reset the device
	call os_net_i8254x_reset

	pop rax
	pop rcx
	pop rdx
	pop rsi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_net_i8254x_reset - Reset an Intel 8254x NIC
;  IN:	Nothing
; OUT:	Nothing, all registers preserved
os_net_i8254x_reset:
	mov rsi, [os_NetIOBaseMem]
	mov rdi, rsi

	mov eax, 0xFFFFFFFF
	mov [rsi+I8254X_REG_IMC], eax		; Disable all interrupt causes
	mov eax, [rsi+I8254X_REG_ICR]		; Clear any pending interrupts
	xor eax, eax
	mov [rsi+I8254X_REG_ITR], eax		; Disable interrupt throttling logic

	mov eax, 0x00000030
	mov [rsi+I8254X_REG_PBA], eax		; PBA: set the RX buffer size to 48KB (TX buffer is calculated as 64-RX buffer)

	mov eax, 0x80008060
	mov [rsi+I8254X_REG_TXCW], eax		; TXCW: set ANE, TxConfigWord (Half/Full duplex, Next Page Request)

	mov eax, [rsi+I8254X_REG_CTRL]
	btr eax, 3
	bts eax, 6
	bts eax, 5
	btr eax, 31
	btr eax, 30
	btr eax, 7
	mov [rsi+I8254X_REG_CTRL], eax		; CTRL: clear LRST, set SLU and ASDE, clear RSTPHY, VME, and ILOS

	push rdi
	add rdi, 0x5200				; MTA: reset
	mov eax, 0xFFFFFFFF
	stosd
	stosd
	stosd
	stosd
	pop rdi

	mov rax, os_eth_rx_buffer
	mov [rsi+I8254X_REG_RDBAL], eax		; Receive Descriptor Base Address Low
	shr rax, 32
	mov [rsi+I8254X_REG_RDBAH], eax		; Receive Descriptor Base Address High
	mov eax, (32 * 16)
	mov [rsi+I8254X_REG_RDLEN], eax		; Receive Descriptor Length
	xor eax, eax
	mov [rsi+I8254X_REG_RDH], eax		; Receive Descriptor Head
	mov eax, 1
	mov [rsi+I8254X_REG_RDT], eax		; Receive Descriptor Tail
	mov eax, 0x04008006			; Receiver Enable, Store Bad Packets, Broadcast Accept Mode, Strip Ethernet CRC from incoming packet
	mov [rsi+I8254X_REG_RCTL], eax		; Receive Control Register

	push rdi
	mov rdi, os_eth_rx_buffer
	mov rax, 0x1c9000
	stosd
	pop rdi

	mov rax, os_eth_tx_buffer
	mov [rsi+I8254X_REG_TDBAL], eax		; Transmit Descriptor Base Address Low
	shr rax, 32
	mov [rsi+I8254X_REG_TDBAH], eax		; Transmit Descriptor Base Address High
	mov eax, (32 * 16)
	mov [rsi+I8254X_REG_TDLEN], eax		; Transmit Descriptor Length
	xor eax, eax
	mov [rsi+I8254X_REG_TDH], eax		; Transmit Descriptor Head
	mov [rsi+I8254X_REG_TDT], eax		; Transmit Descriptor Tail
	mov eax, 0x010400FA			; Enabled, Pad Short Packets, 15 retries, 64-byte COLD, Re-transmit on Late Collision
	mov [rsi+I8254X_REG_TCTL], eax		; Transmit Control Register
	mov eax, 0x0060200A			; IPGT 10, IPGR1 8, IPGR2 6
	mov [rsi+I8254X_REG_TIPG], eax		; Transmit IPG Register

	xor eax, eax
	mov [rsi+I8254X_REG_RDTR], eax		; Clear the Receive Delay Timer Register
	mov [rsi+I8254X_REG_RADV], eax		; Clear the Receive Interrupt Absolute Delay Timer
	mov [rsi+I8254X_REG_RSRPD], eax		; Clear the Receive Small Packet Detect Interrupt
	bts eax, 0				; TXDW
	bts eax, 7				; RXT0
	mov eax, 0x1FFFF			; Temp enable all interrupt types
	mov [rsi+I8254X_REG_IMS], eax		; Enable interrupt types

	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_net_i8254x_transmit - Transmit a packet via an Intel 8254x NIC
;  IN:	RSI = Location of packet
;	RCX = Length of packet
; OUT:	Nothing
;	Uses RAX, RCX, RSI, RDI
os_net_i8254x_transmit:
	mov rdi, os_eth_tx_buffer		; Transmit Descriptor Base Address
	mov rax, rsi
	stosq					; Store the data location
	mov rax, rcx				; The packet size is in CX
	bts rax, 24				; EOP
	bts rax, 25				; IFCS
	bts rax, 27				; RS
	stosq
	mov rdi, [os_NetIOBaseMem]
	xor eax, eax
	mov [rdi+I8254X_REG_TDH], eax		; TDH - Transmit Descriptor Head
	add eax, 1
	mov [rdi+I8254X_REG_TDT], eax		; TDL - Transmit Descriptor Tail
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_net_i8254x_poll - Polls the Intel 8254x NIC for a received packet
;  IN:	RDI = Location to store packet
; OUT:	RCX = Length of packet
;	Uses RAX, RCX, RDX, RSI, RDI
os_net_i8254x_poll:
	xor ecx, ecx

	mov cx, [os_eth_rx_buffer+8]		; Get the packet length
	mov rsi, 0x1c9000
	push rcx
	rep movsb
	pop rcx
	mov rsi, [os_NetIOBaseMem]
	xor eax, eax
	mov [rsi+I8254X_REG_RDH], eax		; Receive Descriptor Head
	mov eax, 1
	mov [rsi+I8254X_REG_RDT], eax		; Receive Descriptor Tail

	push rdi
	mov rdi, os_eth_rx_buffer
	mov rax, 0x1c9000
	stosd
	pop rdi

	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_net_i8254x_ack_int - Acknowledge an internal interrupt of the Intel 8254x NIC
;  IN:	Nothing
; OUT:	RAX = Ethernet status
;	Uses RDI
os_net_i8254x_ack_int:
	push rdi
	xor eax, eax
	mov rdi, [os_NetIOBaseMem]
	mov eax, [rdi+I8254X_REG_ICR]
	pop rdi
	ret
; -----------------------------------------------------------------------------


; Maximum packet size
I8254X_MAX_PKT_SIZE	equ 16384

; Register list
I8254X_REG_CTRL		equ 0x0000 ; Control Register
I8254X_REG_STATUS	equ 0x0008 ; Device Status Register
I8254X_REG_CTRLEXT	equ 0x0018 ; Extended Control Register
I8254X_REG_MDIC		equ 0x0020 ; MDI Control Register
I8254X_REG_FCAL		equ 0x0028 ; Flow Control Address Low
I8254X_REG_FCAH		equ 0x002C ; Flow Control Address High
I8254X_REG_FCT		equ 0x0030 ; Flow Control Type
I8254X_REG_VET		equ 0x0038 ; VLAN Ether Type
I8254X_REG_ICR		equ 0x00C0 ; Interrupt Cause Read
I8254X_REG_ITR		equ 0x00C4 ; Interrupt Throttling Register
I8254X_REG_ICS		equ 0x00C8 ; Interrupt Cause Set Register
I8254X_REG_IMS		equ 0x00D0 ; Interrupt Mask Set/Read Register
I8254X_REG_IMC		equ 0x00D8 ; Interrupt Mask Clear Register
I8254X_REG_RCTL		equ 0x0100 ; Receive Control Register
I8254X_REG_FCTTV	equ 0x0170 ; Flow Control Transmit Timer Value
I8254X_REG_TXCW		equ 0x0178 ; Transmit Configuration Word
I8254X_REG_RXCW		equ 0x0180 ; Receive Configuration Word
I8254X_REG_TCTL		equ 0x0400 ; Transmit Control Register
I8254X_REG_TIPG		equ 0x0410 ; Transmit Inter Packet Gap

I8254X_REG_LEDCTL	equ 0x0E00 ; LED Control
I8254X_REG_PBA		equ 0x1000 ; Packet Buffer Allocation

I8254X_REG_RDBAL	equ 0x2800 ; RX Descriptor Base Address Low
I8254X_REG_RDBAH	equ 0x2804 ; RX Descriptor Base Address High
I8254X_REG_RDLEN	equ 0x2808 ; RX Descriptor Length
I8254X_REG_RDH		equ 0x2810 ; RX Descriptor Head
I8254X_REG_RDT		equ 0x2818 ; RX Descriptor Tail
I8254X_REG_RDTR		equ 0x2820 ; RX Delay Timer Register
I8254X_REG_RXDCTL	equ 0x3828 ; RX Descriptor Control
I8254X_REG_RADV		equ 0x282C ; RX Int. Absolute Delay Timer
I8254X_REG_RSRPD	equ 0x2C00 ; RX Small Packet Detect Interrupt

I8254X_REG_TXDMAC	equ 0x3000 ; TX DMA Control
I8254X_REG_TDBAL	equ 0x3800 ; TX Descriptor Base Address Low
I8254X_REG_TDBAH	equ 0x3804 ; TX Descriptor Base Address High
I8254X_REG_TDLEN	equ 0x3808 ; TX Descriptor Length
I8254X_REG_TDH		equ 0x3810 ; TX Descriptor Head
I8254X_REG_TDT		equ 0x3818 ; TX Descriptor Tail
I8254X_REG_TIDV		equ 0x3820 ; TX Interrupt Delay Value
I8254X_REG_TXDCTL	equ 0x3828 ; TX Descriptor Control
I8254X_REG_TADV		equ 0x382C ; TX Absolute Interrupt Delay Value
I8254X_REG_TSPMT	equ 0x3830 ; TCP Segmentation Pad & Min Threshold

I8254X_REG_RXCSUM	equ 0x5000 ; RX Checksum Control

; Register list for i8254x
I82542_REG_RDTR		equ 0x0108 ; RX Delay Timer Register
I82542_REG_RDBAL	equ 0x0110 ; RX Descriptor Base Address Low
I82542_REG_RDBAH	equ 0x0114 ; RX Descriptor Base Address High
I82542_REG_RDLEN	equ 0x0118 ; RX Descriptor Length
I82542_REG_RDH		equ 0x0120 ; RDH for i82542
I82542_REG_RDT		equ 0x0128 ; RDT for i82542
I82542_REG_TDBAL	equ 0x0420 ; TX Descriptor Base Address Low
I82542_REG_TDBAH	equ 0x0424 ; TX Descriptor Base Address Low
I82542_REG_TDLEN	equ 0x0428 ; TX Descriptor Length
I82542_REG_TDH		equ 0x0430 ; TDH for i82542
I82542_REG_TDT		equ 0x0438 ; TDT for i82542

; CTRL - Control Register (0x0000)
I8254X_CTRL_FD		equ 0x00000001 ; Full Duplex
I8254X_CTRL_LRST	equ 0x00000008 ; Link Reset
I8254X_CTRL_ASDE	equ 0x00000020 ; Auto-speed detection
I8254X_CTRL_SLU		equ 0x00000040 ; Set Link Up
I8254X_CTRL_ILOS	equ 0x00000080 ; Invert Loss of Signal
I8254X_CTRL_SPEED_MASK	equ 0x00000300 ; Speed selection
I8254X_CTRL_SPEED_SHIFT	equ 8
I8254X_CTRL_FRCSPD	equ 0x00000800 ; Force Speed
I8254X_CTRL_FRCDPLX	equ 0x00001000 ; Force Duplex
I8254X_CTRL_SDP0_DATA	equ 0x00040000 ; SDP0 data
I8254X_CTRL_SDP1_DATA	equ 0x00080000 ; SDP1 data
I8254X_CTRL_SDP0_IODIR	equ 0x00400000 ; SDP0 direction
I8254X_CTRL_SDP1_IODIR	equ 0x00800000 ; SDP1 direction
I8254X_CTRL_RST		equ 0x04000000 ; Device Reset
I8254X_CTRL_RFCE	equ 0x08000000 ; RX Flow Ctrl Enable
I8254X_CTRL_TFCE	equ 0x10000000 ; TX Flow Ctrl Enable
I8254X_CTRL_VME		equ 0x40000000 ; VLAN Mode Enable
I8254X_CTRL_PHY_RST	equ 0x80000000 ; PHY reset

; STATUS - Device Status Register (0x0008)
I8254X_STATUS_FD		equ 0x00000001 ; Full Duplex
I8254X_STATUS_LU		equ 0x00000002 ; Link Up
I8254X_STATUS_TXOFF		equ 0x00000010 ; Transmit paused
I8254X_STATUS_TBIMODE		equ 0x00000020 ; TBI Mode
I8254X_STATUS_SPEED_MASK	equ 0x000000C0 ; Link Speed setting
I8254X_STATUS_SPEED_SHIFT	equ 6
I8254X_STATUS_ASDV_MASK		equ 0x00000300 ; Auto Speed Detection
I8254X_STATUS_ASDV_SHIFT	equ 8
I8254X_STATUS_PCI66		equ 0x00000800 ; PCI bus speed
I8254X_STATUS_BUS64		equ 0x00001000 ; PCI bus width
I8254X_STATUS_PCIX_MODE		equ 0x00002000 ; PCI-X mode
I8254X_STATUS_PCIXSPD_MASK	equ 0x0000C000 ; PCI-X speed
I8254X_STATUS_PCIXSPD_SHIFT	equ 14

; CTRL_EXT - Extended Device Control Register (0x0018)
I8254X_CTRLEXT_PHY_INT		equ 0x00000020 ; PHY interrupt
I8254X_CTRLEXT_SDP6_DATA	equ 0x00000040 ; SDP6 data
I8254X_CTRLEXT_SDP7_DATA	equ 0x00000080 ; SDP7 data
I8254X_CTRLEXT_SDP6_IODIR	equ 0x00000400 ; SDP6 direction
I8254X_CTRLEXT_SDP7_IODIR	equ 0x00000800 ; SDP7 direction
I8254X_CTRLEXT_ASDCHK		equ 0x00001000 ; Auto-Speed Detect Chk
I8254X_CTRLEXT_EE_RST		equ 0x00002000 ; EEPROM reset
I8254X_CTRLEXT_SPD_BYPS		equ 0x00008000 ; Speed Select Bypass
I8254X_CTRLEXT_RO_DIS		equ 0x00020000 ; Relaxed Ordering Dis.
I8254X_CTRLEXT_LNKMOD_MASK	equ 0x00C00000 ; Link Mode
I8254X_CTRLEXT_LNKMOD_SHIFT	equ 22

; MDIC - MDI Control Register (0x0020)
I8254X_MDIC_DATA_MASK	equ 0x0000FFFF ; Data
I8254X_MDIC_REG_MASK	equ 0x001F0000 ; PHY Register
I8254X_MDIC_REG_SHIFT	equ 16
I8254X_MDIC_PHY_MASK	equ 0x03E00000 ; PHY Address
I8254X_MDIC_PHY_SHIFT	equ 21
I8254X_MDIC_OP_MASK	equ 0x0C000000 ; Opcode
I8254X_MDIC_OP_SHIFT	equ 26
I8254X_MDIC_R		equ 0x10000000 ; Ready
I8254X_MDIC_I		equ 0x20000000 ; Interrupt Enable
I8254X_MDIC_E		equ 0x40000000 ; Error

; ICR - Interrupt Cause Read (0x00c0)
I8254X_ICR_TXDW		equ 0x00000001 ; TX Desc Written back
I8254X_ICR_TXQE		equ 0x00000002 ; TX Queue Empty
I8254X_ICR_LSC		equ 0x00000004 ; Link Status Change
I8254X_ICR_RXSEQ	equ 0x00000008 ; RX Sequence Error
I8254X_ICR_RXDMT0	equ 0x00000010 ; RX Desc min threshold reached
I8254X_ICR_RXO		equ 0x00000040 ; RX Overrun
I8254X_ICR_RXT0		equ 0x00000080 ; RX Timer Interrupt
I8254X_ICR_MDAC		equ 0x00000200 ; MDIO Access Complete
I8254X_ICR_RXCFG	equ 0x00000400
I8254X_ICR_PHY_INT	equ 0x00001000 ; PHY Interrupt
I8254X_ICR_GPI_SDP6	equ 0x00002000 ; GPI on SDP6
I8254X_ICR_GPI_SDP7	equ 0x00004000 ; GPI on SDP7
I8254X_ICR_TXD_LOW	equ 0x00008000 ; TX Desc low threshold hit
I8254X_ICR_SRPD		equ 0x00010000 ; Small RX packet detected

; RCTL - Receive Control Register (0x0100)
I8254X_RCTL_EN		equ 0x00000002 ; Receiver Enable
I8254X_RCTL_SBP		equ 0x00000004 ; Store Bad Packets
I8254X_RCTL_UPE		equ 0x00000008 ; Unicast Promiscuous Enabled
I8254X_RCTL_MPE		equ 0x00000010 ; Xcast Promiscuous Enabled
I8254X_RCTL_LPE		equ 0x00000020 ; Long Packet Reception Enable
I8254X_RCTL_LBM_MASK	equ 0x000000C0 ; Loopback Mode
I8254X_RCTL_LBM_SHIFT	equ 6
I8254X_RCTL_RDMTS_MASK	equ 0x00000300 ; RX Desc Min Threshold Size
I8254X_RCTL_RDMTS_SHIFT	equ 8
I8254X_RCTL_MO_MASK	equ 0x00003000 ; Multicast Offset
I8254X_RCTL_MO_SHIFT	equ 12
I8254X_RCTL_BAM		equ 0x00008000 ; Broadcast Accept Mode
I8254X_RCTL_BSIZE_MASK	equ 0x00030000 ; RX Buffer Size
I8254X_RCTL_BSIZE_SHIFT	equ 16
I8254X_RCTL_VFE		equ 0x00040000 ; VLAN Filter Enable
I8254X_RCTL_CFIEN	equ 0x00080000 ; CFI Enable
I8254X_RCTL_CFI		equ 0x00100000 ; Canonical Form Indicator Bit
I8254X_RCTL_DPF		equ 0x00400000 ; Discard Pause Frames
I8254X_RCTL_PMCF	equ 0x00800000 ; Pass MAC Control Frames
I8254X_RCTL_BSEX	equ 0x02000000 ; Buffer Size Extension
I8254X_RCTL_SECRC	equ 0x04000000 ; Strip Ethernet CRC

; TCTL - Transmit Control Register (0x0400)
I8254X_TCTL_EN		equ 0x00000002 ; Transmit Enable
I8254X_TCTL_PSP		equ 0x00000008 ; Pad short packets
I8254X_TCTL_SWXOFF	equ 0x00400000 ; Software XOFF Transmission

; PBA - Packet Buffer Allocation (0x1000)
I8254X_PBA_RXA_MASK	equ 0x0000FFFF ; RX Packet Buffer
I8254X_PBA_RXA_SHIFT	equ 0
I8254X_PBA_TXA_MASK	equ 0xFFFF0000 ; TX Packet Buffer
I8254X_PBA_TXA_SHIFT	equ 16

; Flow Control Type
I8254X_FCT_TYPE_DEFAULT	equ 0x8808

; === TX Descriptor fields ===

; TX Packet Length (word 2)
I8254X_TXDESC_LEN_MASK	equ 0x0000ffff

; TX Descriptor CMD field (word 2)
I8254X_TXDESC_IDE	equ 0x80000000 ; Interrupt Delay Enable
I8254X_TXDESC_VLE	equ 0x40000000 ; VLAN Packet Enable
I8254X_TXDESC_DEXT	equ 0x20000000 ; Extension
I8254X_TXDESC_RPS	equ 0x10000000 ; Report Packet Sent
I8254X_TXDESC_RS	equ 0x08000000 ; Report Status
I8254X_TXDESC_IC	equ 0x04000000 ; Insert Checksum
I8254X_TXDESC_IFCS	equ 0x02000000 ; Insert FCS
I8254X_TXDESC_EOP	equ 0x01000000 ; End Of Packet

; TX Descriptor STA field (word 3)
I8254X_TXDESC_TU	equ 0x00000008 ; Transmit Underrun
I8254X_TXDESC_LC	equ 0x00000004 ; Late Collision
I8254X_TXDESC_EC	equ 0x00000002 ; Excess Collisions
I8254X_TXDESC_DD	equ 0x00000001 ; Descriptor Done

; === RX Descriptor fields ===

; RX Packet Length (word 2)
I8254X_RXDESC_LEN_MASK	equ 0x0000ffff

; RX Descriptor STA field (word 3)
I8254X_RXDESC_PIF	equ 0x00000080 ; Passed In-exact Filter
I8254X_RXDESC_IPCS	equ 0x00000040 ; IP cksum calculated
I8254X_RXDESC_TCPCS	equ 0x00000020 ; TCP cksum calculated
I8254X_RXDESC_VP	equ 0x00000008 ; Packet is 802.1Q
I8254X_RXDESC_IXSM	equ 0x00000004 ; Ignore cksum indication
I8254X_RXDESC_EOP	equ 0x00000002 ; End Of Packet
I8254X_RXDESC_DD	equ 0x00000001 ; Descriptor Done

; =============================================================================
; EOHEF
; =============================================================================
; BareMetal -- a 64-bit OS written in Assembly for x86-64 systems
; Copyright (C) 2008-2014 Return Infinity -- see LICENSE.TXT
;
; Realtek 8169 NIC. http://wiki.osdev.org/RTL8169
; =============================================================================

align 16
db 'DEBUG: RTL8169  '
align 16


; -----------------------------------------------------------------------------
; Initialize a Realtek 8169 NIC
;  IN:	BL  = Bus number of the Realtek device
;	CL  = Device/Slot number of the Realtek device
os_net_rtl8169_init:
	push rsi
	push rdx
	push rcx
	push rax

	; Grab the Base I/O Address of the device
	mov dl, 0x04				; BAR0
	call os_pci_read_reg
	and eax, 0xFFFFFFFC			; EAX now holds the Base IO Address (clear the low 2 bits)
	mov word [os_NetIOAddress], ax

	; Grab the IRQ of the device
	mov dl, 0x0F				; Get device's IRQ number from PCI Register 15 (IRQ is bits 7-0)
	call os_pci_read_reg
	mov [os_NetIRQ], al			; AL holds the IRQ

	; Grab the MAC address
	mov dx, word [os_NetIOAddress]
	in al, dx
	mov [os_NetMAC], al
	add dx, 1
	in al, dx
	mov [os_NetMAC+1], al
	add dx, 1
	in al, dx
	mov [os_NetMAC+2], al
	add dx, 1
	in al, dx
	mov [os_NetMAC+3], al
	add dx, 1
	in al, dx
	mov [os_NetMAC+4], al
	add dx, 1
	in al, dx
	mov [os_NetMAC+5], al

	; Reset the device
	call os_net_rtl8169_reset

	pop rax
	pop rcx
	pop rdx
	pop rsi
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_net_rtl8136_reset - Reset a Realtek 8169 NIC
;  IN:	Nothing
; OUT:	Nothing, all registers preserved
os_net_rtl8169_reset:
	push rdx
	push rcx
	push rax

	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_COMMAND
	mov al, 0x10				; Bit 4 set for Reset
	out dx, al
	mov cx, 1000				; Wait no longer for the reset to complete
wait_for_8169_reset:
	in al, dx
	test al, 0x10
	jz reset_8169_completed			; RST remains 1 during reset, Reset complete when 0
	dec cx
	jns wait_for_8169_reset
reset_8169_completed:

	; Unlock config registers
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_9346CR
	mov al, 0xC0				; Unlock
	out dx, al

	; Set the C+ Command
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_CCR
	in ax, dx
	bts ax, 3				; Enable PCI Multiple Read/Write
	btc ax, 9				; Little-endian mode
	out dx, ax

	; Power management?

	; Recieve configuration
	mov dx, word [os_NetIOAddress]
	add edx, RTL8169_REG_RCR
	mov eax, 0x0000E70A			; Set bits 1 (APM), 3 (AB), 8-10 (Unlimited), 13-15 (No limit)
	out dx, eax

	; Set up TCR
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_TCR
	mov eax, 0x03000700
	out dx, eax

	; Setup max RX size
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_MAXRX
	mov ax, 0x3FFF				; 16384 - 1
	out dx, ax

	; Setup max TX size
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_MAXTX
	mov al, 0x3B
	out dx, al

	; Set the Transmit Normal Priority Descriptor Start Address
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_TNPDS
	mov rax, os_eth_tx_buffer
	out dx, eax				; Write the low bits
	shr rax, 32
	add dx, 4
	out dx, eax				; Write the high bits
	mov eax, 0x70000000			; Set bit 30 (End of Descriptor Ring), 29 (FS), and 28 (LS)
	mov [os_eth_tx_buffer], eax

	; Set the Receive Descriptor Start Address
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_RDSAR
	mov rax, os_eth_rx_buffer
	out dx, eax				; Write the low bits
	shr rax, 32
	add dx, 4
	out dx, eax				; Write the high bits
	mov eax, 0x80001FF8			; Set bits 31 (Ownership), also buffer size (Max 0x1FF8)
	mov [os_eth_rx_buffer], eax
	mov rax, os_ethernet_rx_buffer
	mov [os_eth_rx_buffer+8], rax
	mov eax, 0xC0001FF8			; Set bits 31 (Ownership) and 30 (End of Descriptor Ring), also buffer size (Max 0x1FF8)
	mov [os_eth_rx_buffer+16], eax
	mov rax, os_ethernet_rx_buffer
	mov [os_eth_rx_buffer+24], rax

	; Initialize multicast registers (no filtering)
	mov eax, 0xFFFFFFFF
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_MAR0
	out dx, eax
	add dx, 4				; MAR4
	out dx, eax

	; Enable Rx/Tx in the Command register
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_COMMAND
	mov al, (1 << RTL8169_BIT_RE) | (1 << RTL8169_BIT_TE) ;0x0C				; Set bits 2 (TE) and 3 (RE)
	out dx, al

	; Enable Receive and Transmit interrupts
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_IMR
	mov ax, 0x0005				; Set bits 0 (RX OK) and 2 (TX OK)
	out dx, ax

	; Lock config register
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_9346CR
	mov al, 0x00				; Lock
	out dx, al

	pop rax
	pop rcx
	pop rdx
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_net_rtl8169_transmit - Transmit a packet via a Realtek 8169 NIC
;  IN:	RSI = Location of packet
;	RCX = Length of packet
; OUT:	Nothing
;	Uses RAX, RCX, RDX, RSI, RDI
; ToDo:	Check for proper timeout
os_net_rtl8169_transmit:
	mov rdi, os_eth_tx_buffer
	mov rax, rcx
	stosw					; Store the frame length
	add rdi, 6				; Should the other data be cleared here?
	mov rax, rsi
	stosq					; Store the packet location
	or dword [os_eth_tx_buffer], 0xF0000000	; Set bit 31 (OWN), 30 (EOR), 29 (FS), and 28 (LS)
	mov dx, word [os_NetIOAddress]
	add dx, RTL8169_REG_TPPOLL
	mov al, 0x40
	out dx, al				; Set up TX Polling
os_net_rtl8169_transmit_sendloop:
	mov eax, [os_eth_tx_buffer]
	and eax, 0x80000000			; Check the ownership bit (BT command instead?)
	cmp eax, 0x80000000			; If the ownership bit is clear then the NIC sent the packet
	je os_net_rtl8169_transmit_sendloop
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_net_rtl8169_poll - Polls the Realtek 8169 NIC for a received packet
;  IN:	RDI = Location to store packet
; OUT:	RCX = Length of packet
;	Uses RAX, RCX, RDX, RSI, RDI
os_net_rtl8169_poll:
	xor ecx, ecx
	mov cx, [os_eth_rx_buffer]
	and cx, 0x3FFF				; Clear the two high bits as length is bits 13-0
	cmp cx, 0x1FF8
	jne os_net_rtl8169_poll_first_descriptor
	mov cx, [os_eth_rx_buffer+16]
	and cx, 0x3FFF				; Clear the two high bits as length is bits 13-0
os_net_rtl8169_poll_first_descriptor:
	mov rsi, os_ethernet_rx_buffer
	push rcx
	rep movsb				; Copy the packet to the lacation stored in RDI
	pop rcx
	mov eax, 0x80001FF8			; Set bits 31 (Ownership), also buffer size (Max 0x1FF8)
	mov [os_eth_rx_buffer], eax
	mov rax, os_ethernet_rx_buffer
	mov [os_eth_rx_buffer+8], rax
	mov eax, 0xC0001FF8			; Set bits 31 (Ownership) and 30 (End of Descriptor Ring), also buffer size (Max 0x1FF8)
	mov [os_eth_rx_buffer+16], eax
	mov rax, os_ethernet_rx_buffer
	mov [os_eth_rx_buffer+24], rax
	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_net_rtl8169_ack_int - Acknowledge an internal interrupt of the Realtek 8169 NIC
;  IN:	Nothing
; OUT:	RAX = Ethernet status
;	Uses RDI
os_net_rtl8169_ack_int:
	push rdx
	mov dx, word [os_NetIOAddress]		; Clear active interrupt sources
	add dx, RTL8169_REG_ISR
	in ax, dx
	out dx, ax
	shr eax, 2
	pop rdx
	ret
; -----------------------------------------------------------------------------


; Register Descriptors
	RTL8169_REG_IDR0	equ 0x00	; ID Register 0
	RTL8169_REG_IDR1	equ 0x01	; ID Register 1
	RTL8169_REG_IDR2	equ 0x02	; ID Register 2
	RTL8169_REG_IDR3	equ 0x03	; ID Register 3
	RTL8169_REG_IDR4	equ 0x04	; ID Register 4
	RTL8169_REG_IDR5	equ 0x05	; ID Register 5
	RTL8169_REG_MAR0	equ 0x08	; Multicast Register 0
	RTL8169_REG_MAR1	equ 0x09	; Multicast Register 1
	RTL8169_REG_MAR2	equ 0x0A	; Multicast Register 2
	RTL8169_REG_MAR3	equ 0x0B	; Multicast Register 3
	RTL8169_REG_MAR4	equ 0x0C	; Multicast Register 4
	RTL8169_REG_MAR5	equ 0x0D	; Multicast Register 5
	RTL8169_REG_MAR6	equ 0x0E	; Multicast Register 6
	RTL8169_REG_MAR7	equ 0x0F	; Multicast Register 7
	RTL8169_REG_TNPDS	equ 0x20	; Transmit Normal Priority Descriptors: Start address (64-bit). (256-byte alignment) 
	RTL8169_REG_COMMAND	equ 0x37	; Command Register
	RTL8169_REG_TPPOLL	equ 0x38	; Transmit Priority Polling Register
	RTL8169_REG_IMR		equ 0x3C	; Interrupt Mask Register
	RTL8169_REG_ISR		equ 0x3E	; Interrupt Status Register
	RTL8169_REG_TCR		equ 0x40	; Transmit (Tx) Configuration Register
	RTL8169_REG_RCR		equ 0x44	; Receive (Rx) Configuration Register
	RTL8169_REG_9346CR	equ 0x50	; 93C46 (93C56) Command Register
	RTL8169_REG_CONFIG0	equ 0x51	; Configuration Register 0
	RTL8169_REG_CONFIG1	equ 0x52	; Configuration Register 1
	RTL8169_REG_CONFIG2	equ 0x53	; Configuration Register 2
	RTL8169_REG_CONFIG3	equ 0x54	; Configuration Register 3
	RTL8169_REG_CONFIG4	equ 0x55	; Configuration Register 4
	RTL8169_REG_CONFIG5	equ 0x56	; Configuration Register 5
	RTL8169_REG_PHYAR	equ 0x60	; PHY Access Register 
	RTL8169_REG_PHYStatus	equ 0x6C	; PHY(GMII, MII, or TBI) Status Register 
	RTL8169_REG_MAXRX	equ 0xDA	; Mac Receive Packet Size Register
	RTL8169_REG_CCR		equ 0xE0	; C+ Command Register
	RTL8169_REG_RDSAR	equ 0xE4	; Receive Descriptor Start Address Register (256-byte alignment)
	RTL8169_REG_MAXTX	equ 0xEC	; Max Transmit Packet Size Register

; Command Register (Offset 0037h, R/W)	
	RTL8169_BIT_RST		equ 4		; Reset
	RTL8169_BIT_RE		equ 3		; Receiver Enable
	RTL8169_BIT_TE		equ 2		; Transmitter Enable

; Receive Configuration (Offset 0044h-0047h, R/W)
	RTL8169_BIT_AER		equ 5		; Accept Error
	RTL8169_BIT_AR		equ 4		; Accept Runt
	RTL8169_BIT_AB		equ 3		; Accept Broadcast Packets
	RTL8169_BIT_AM		equ 2		; Accept Multicast Packets
	RTL8169_BIT_APM		equ 1		; Accept Physical Match Packets
	RTL8169_BIT_AAP		equ 0		; Accept All Packets with Destination Address

; PHY Register Table
; BMCR (address 0x00) 
	RTL8169_BIT_ANE		equ 12		; Auto-Negotiation Enable


; =============================================================================
; EOHEF
; =============================================================================
; BareMetal -- a 64-bit OS written in Assembly for x86-64 systems
; Copyright (C) 2008-2014 Return Infinity -- see LICENSE.TXT
;
; AHCI Driver
; =============================================================================

align 16
db 'DEBUG: AHCI     '
align 16


; -----------------------------------------------------------------------------
init_ahci:
	mov rsi, diskmsg
	call os_output

; Probe for an AHCI hard drive controller
	xor ebx, ebx			; Clear the Bus number
	xor ecx, ecx			; Clear the Device/Slot number
	mov edx, 2			; Register 2 for Class code/Subclass

init_ahci_probe_next:
	call os_pci_read_reg
	shr eax, 16			; Move the Class/Subclass code to AX
	cmp ax, 0x0106			; Mass Storage Controller (01) / SATA Controller (06)
	je init_ahci_found		; Found a SATA Controller
	add ecx, 1
	cmp ecx, 256			; Maximum 256 devices/functions per bus
	je init_ahci_probe_next_bus
	jmp init_ahci_probe_next

init_ahci_probe_next_bus:
	xor ecx, ecx
	add ebx, 1
	cmp ebx, 256			; Maximum 256 buses
	je init_ahci_err_noahci
	jmp init_ahci_probe_next

init_ahci_found:
	mov dl, 9
	xor eax, eax
	call os_pci_read_reg		; BAR5 (AHCI Base Address Register)
	mov [ahci_base], rax

; Basic config of the controller, port 0
	mov rsi, rax			; RSI holds the ABAR
	mov rdi, rsi

; Search the implemented ports for a drive
	mov eax, [rsi+0x0C]		; PI – Ports Implemented
	mov edx, eax
	xor ecx, ecx
	mov ebx, 0x128			; Offset to Port 0 Serial ATA Status
nextport:
	bt edx, 0			; Valid port?
	jnc nodrive
	mov eax, [rsi+rbx]
	cmp eax, 0
	je nodrive
	jmp founddrive

nodrive:
	add ecx, 1
	shr edx, 1
	add ebx, 0x80			; Each port has a 128 byte memory space
	cmp ecx, 32
	je hdd_setup_err_nodisk
	jmp nextport

; Configure the first port found with a drive attached
founddrive:
	mov [ahci_port], ecx
	mov rdi, rsi
	add rdi, 0x100			; Offset to port 0
	push rcx				; Save port number
	shl rcx, 7			; Quick multiply by 0x80
	add rdi, rcx
	pop rcx				; Restore port number
	mov rax, ahci_cmdlist		; 1024 bytes per port
	stosd				; Offset 00h: PxCLB – Port x Command List Base Address
	xor eax, eax
	stosd				; Offset 04h: PxCLBU – Port x Command List Base Address Upper 32-bits
	mov rax, ahci_cmdlist + 0x1000	; 256 or 4096 bytes per port
	stosd				; Offset 08h: PxFB – Port x FIS Base Address
	xor eax, eax
	stosd				; Offset 0Ch: PxFBU – Port x FIS Base Address Upper 32-bits
	stosd				; Offset 10h: PxIS – Port x Interrupt Status
	stosd				; Offset 14h: PxIE – Port x Interrupt Enable

	; Query drive
	mov rdi, 0x200000
	call iddrive
	mov rsi, 0x200000
	mov eax, [rsi+200]		; Max LBA Extended
	shr rax, 11			; rax = rax * 512 / 1048576	MiB
;	shr rax, 21			; rax = rax * 512 / 1073741824	GiB
	mov [hd1_size], eax		; in mebibytes (MiB)
	mov rdi, os_temp_string
	mov rsi, rdi
	call os_int_to_string
	call os_output
	mov rsi, mibmsg
	call os_output

	; Found a bootable drive
	mov byte [os_DiskEnabled], 0x01

	ret

init_ahci_err_noahci:
hdd_setup_err_nodisk:
	mov rsi, namsg
	call os_output

	ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; iddrive -- Identify a SATA drive
; IN:	RCX = Port # to query
;	RDI = memory location to store details (512 bytes)
; OUT:	Nothing, all registers preserved
iddrive:
	push rdi
	push rsi
	push rcx
	push rax

	shl rcx, 7			; Quick multiply by 0x80
	add rcx, 0x100			; Offset to port 0

	push rdi				; Save the destination memory address

	mov rsi, [ahci_base]

	mov rdi, ahci_cmdlist		; command list (1K with 32 entries, 32 bytes each)
	xor eax, eax
	mov eax, 0x00010005 ;4		; 1 PRDTL Entry, Command FIS Length = 16 bytes
	stosd				; DW 0 - Description Information
	xor eax, eax
	stosd				; DW 1 - Command Status
	mov eax, ahci_cmdtable
	stosd				; DW 2 - Command Table Base Address
	xor eax, eax
	stosd				; DW 3 - Command Table Base Address Upper
	stosd
	stosd
	stosd
	stosd
	; DW 4 - 7 are reserved

	; command table
	mov rdi, ahci_cmdtable		; Build a command table for Port 0
	mov eax, 0x00EC8027		; EC identify, bit 15 set, fis 27 H2D
	stosd				; feature 7:0, command, c, fis
	xor eax, eax
	stosd				; device, lba 23:16, lba 15:8, lba 7:0
	stosd				; feature 15:8, lba 47:40, lba 39:32, lba 31:24
	stosd				; control, ICC, count 15:8, count 7:0
;	stosd				; reserved
	mov rdi, ahci_cmdtable + 0x80
	pop rax				; Restore the destination memory address
	stosd				; Data Base Address
	shr rax, 32
	stosd				; Data Base Address Upper
	xor eax, eax
	stosd				; Reserved
	mov eax, 0x000001FF		; 512 - 1
	stosd				; Description Information

	add rsi, rcx

	mov rdi, rsi
	add rdi, 0x10			; Port x Interrupt Status
	xor eax, eax
	stosd

	mov rdi, rsi
	add rdi, 0x18			; Offset to port 0 Command and Status
	mov eax, [rdi]
	bts eax, 4			; FRE
	bts eax, 0			; ST
	stosd

	mov rdi, rsi
	add rdi, 0x38			; Command Issue
	mov eax, 0x00000001		; Execute Command Slot 0
	stosd

iddrive_poll:
	mov eax, [rsi+0x38]
	cmp eax, 0
	jne iddrive_poll

	mov rdi, rsi
	add rdi, 0x18			; Offset to port 0
	mov eax, [rdi]
	btc eax, 4			; FRE
	btc eax, 0			; ST
	stosd

	pop rax
	pop rcx
	pop rsi
	pop rdi
ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; readsectors -- Read data from a SATA hard drive
; IN:	RAX = starting sector # to read
;	RCX = number of sectors to read (up to 8192 = 4MiB)
;	RDX = disk #
;	RDI = memory location to store sectors
; OUT:	RAX = RAX + number of sectors that were read
;	RCX = number of sectors that were read (0 on error)
;	RDI = RDI + (number of sectors read * 512)
;	All other registers preserved
readsectors:
	push rdx
	push rbx
	push rdi
	push rsi
	push rcx
	push rax

	push rcx				; Save the sector count
	push rdi				; Save the destination memory address
	push rax				; Save the block number
	push rax

	shl rdx, 7			; Quick multiply by 0x80
	add rdx, 0x100			; Offset to port 0

	mov rsi, [ahci_base]

	; Command list setup
	mov rdi, ahci_cmdlist		; command list (1K with 32 entries, 32 bytes each)
	xor eax, eax
	mov eax, 0x00010005		; 1 PRDTL Entry, Command FIS Length = 20 bytes
	stosd				; DW 0 - Description Information
	xor eax, eax
	stosd				; DW 1 - Command Status
	mov eax, ahci_cmdtable
	stosd				; DW 2 - Command Table Base Address
	xor eax, eax
	stosd				; DW 3 - Command Table Base Address Upper
	stosd
	stosd
	stosd
	stosd
	; DW 4 - 7 are reserved

	; Command FIS setup
	mov rdi, ahci_cmdtable		; Build a command table for Port 0
	mov eax, 0x00258027		; 25 READ DMA EXT, bit 15 set, fis 27 H2D
	stosd				; feature 7:0, command, c, fis
	pop rax				; Restore the start sector number
	shl rax, 36
	shr rax, 36			; Upper 36 bits cleared
	bts rax, 30			; bit 30 set for LBA
	stosd				; device, lba 23:16, lba 15:8, lba 7:0
	pop rax				; Restore the start sector number
	shr rax, 24
	stosd				; feature 15:8, lba 47:40, lba 39:32, lba 31:24
	mov rax, rcx			; Read the number of sectors given in rcx
	stosd				; control, ICC, count 15:8, count 7:0
	mov rax, 0x00000000
	stosd				; reserved

	; PRDT setup
	mov rdi, ahci_cmdtable + 0x80
	pop rax				; Restore the destination memory address
	stosd				; Data Base Address
	shr rax, 32
	stosd				; Data Base Address Upper
	stosd				; Reserved
	pop rax				; Restore the sector count
	shl rax, 9			; multiply by 512 for bytes
	sub rax, 1			; subtract 1 (4.2.3.3, DBC is number of bytes - 1)
	stosd				; Description Information

	add rsi, rdx

	mov rdi, rsi
	add rdi, 0x10			; Port x Interrupt Status
	xor eax, eax
	stosd

	mov rdi, rsi
	add rdi, 0x18			; Offset to port 0
	mov eax, [rdi]
	bts eax, 4			; FRE
	bts eax, 0			; ST
	stosd

	mov rdi, rsi
	add rdi, 0x38			; Command Issue
	mov eax, 0x00000001		; Execute Command Slot 0
	stosd

readsectors_poll:
	mov eax, [rsi+0x38]
	cmp eax, 0
	jne readsectors_poll

	mov rdi, rsi
	add rdi, 0x18			; Offset to port 0
	mov eax, [rdi]
	btc eax, 4			; FRE
	btc eax, 0			; ST
	stosd

	pop rax				; rax = start
	pop rcx				; rcx = number of sectors read
	add rax, rcx			; rax = start + number of sectors read
	pop rsi
	pop rdi
	mov rbx, rcx			; rdi = dest addr + number of bytes read
	shl rbx, 9
	add rdi, rbx
	pop rbx
	pop rdx
ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; writesectors -- Write data to a SATA hard drive
; IN:	RAX = starting sector # to write
;	RCX = number of sectors to write (up to 8192 = 4MiB)
;	RDX = disk #
;	RSI = memory location of sectors
; OUT:	RAX = RAX + number of sectors that were written
;	RCX = number of sectors that were written (0 on error)
;	RSI = RSI + (number of sectors written * 512)
;	All other registers preserved
writesectors:
	push rdx
	push rbx
	push rdi
	push rsi
	push rcx
	push rax

	push rcx				; Save the sector count
	push rsi				; Save the source memory address
	push rax				; Save the block number
	push rax

	shl rdx, 7			; Quick multiply by 0x80
	add rdx, 0x100			; Offset to port 0

	mov rsi, [ahci_base]

	; Command list setup
	mov rdi, ahci_cmdlist		; command list (1K with 32 entries, 32 bytes each)
	xor eax, eax
	mov eax, 0x00010045		; 1 PRDTL Entry, write flag, Command FIS Length = 20 bytes
	stosd				; DW 0 - Description Information
	xor eax, eax
	stosd				; DW 1 - Command Status
	mov eax, ahci_cmdtable
	stosd				; DW 2 - Command Table Base Address
	xor eax, eax
	stosd				; DW 3 - Command Table Base Address Upper
	stosd
	stosd
	stosd
	stosd
	; DW 4 - 7 are reserved

	; Command FIS setup
	mov rdi, ahci_cmdtable		; Build a command table for Port 0
	mov eax, 0x00358027		; 35 WRITE DMA EXT, bit 15 set, fis 27 H2D
	stosd				; feature 7:0, command, c, fis
	pop rax				; Restore the start sector number
	shl rax, 36
	shr rax, 36			; Upper 36 bits cleared
	bts rax, 30			; bit 30 set for LBA
	stosd				; device, lba 23:16, lba 15:8, lba 7:0
	pop rax				; Restore the start sector number
	shr rax, 24
	stosd				; feature 15:8, lba 47:40, lba 39:32, lba 31:24
	mov rax, rcx			; Read the number of sectors given in rcx
	stosd				; control, ICC, count 15:8, count 7:0
	mov rax, 0x00000000
	stosd				; reserved

	; PRDT setup
	mov rdi, ahci_cmdtable + 0x80
	pop rax				; Restore the source memory address

	stosd				; Data Base Address
	shr rax, 32
	stosd				; Data Base Address Upper
	stosd				; Reserved
	pop rax				; Restore the sector count
	shl rax, 9			; multiply by 512 for bytes
	add rax, -1			; subtract 1 (4.2.3.3, DBC is number of bytes - 1)
	stosd				; Description Information

	add rsi, rdx

	mov rdi, rsi
	add rdi, 0x10			; Port x Interrupt Status
	xor eax, eax
	stosd

	mov rdi, rsi
	add rdi, 0x18			; Offset to port 0
	mov eax, [rdi]
	bts eax, 4			; FRE
	bts eax, 0			; ST
	stosd

	mov rdi, rsi
	add rdi, 0x38			; Command Issue
	mov eax, 0x00000001		; Execute Command Slot 0
	stosd

writesectors_poll:
	mov eax, [rsi+0x38]
	cmp eax, 0
	jne writesectors_poll

	mov rdi, rsi
	add rdi, 0x18			; Offset to port 0
	mov eax, [rdi]
	btc eax, 4			; FRE
	btc eax, 0			; ST
	stosd

	pop rax				; rax = start
	pop rcx				; rcx = number of sectors read
	add rax, rcx			; rax = start + number of sectors written
	pop rsi
	pop rdi
	mov rbx, rcx			; rdi = dest addr + number of bytes written
	shl rbx, 9
	add rdi, rbx
	pop rbx
	pop rdx
ret
; -----------------------------------------------------------------------------


; =============================================================================
; EOHEF
; =============================================================================
; BareMetal -- a 64-bit OS written in Assembly for x86-64 systems
; Copyright (C) 2008-2014 Return Infinity -- see LICENSE.TXT
;
; PCI Functions. http://wiki.osdev.org/PCI
; =============================================================================

align 16
db 'DEBUG: PCI      '
align 16


; -----------------------------------------------------------------------------
; os_pci_read_reg -- Read from a register on a PCI device
;  IN:	BL  = Bus number
;	CL  = Device/Slot/Function number
;	DL  = Register number (0-15)
; OUT:	EAX = Register information
;	All other registers preserved
os_pci_read_reg:
	push rdx
	push rcx
	push rbx

	shl ebx, 16			; Move Bus number to bits 23 - 16
	shl ecx, 8			; Move Device/Slot/Fuction number to bits 15 - 8
	mov bx, cx
	shl edx, 2
	mov bl, dl
	and ebx, 0x00ffffff		; Clear bits 31 - 24
	or ebx, 0x80000000		; Set bit 31
	mov eax, ebx
	mov dx, PCI_CONFIG_ADDRESS
	out dx, eax
	mov dx, PCI_CONFIG_DATA
	in eax, dx

	pop rbx
	pop rcx
	pop rdx
ret
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; os_pci_write_reg -- Write to a register on a PCI device
;  IN:	BL  = Bus number
;	CL  = Device/Slot/Function number
;	DL  = Register number (0-15)
; OUT:	EAX = Register information
;	All other registers preserved
os_pci_write_reg:
	push rdx
	push rcx
	push rbx
	push rax

	shl ebx, 16			; Move Bus number to bits 23 - 16
	shl ecx, 8			; Move Device/Slot/Fuction number to bits 15 - 8
	mov bx, cx
	shl edx, 2
	mov bl, dl
	and ebx, 0x00ffffff		; Clear bits 31 - 24
	or ebx, 0x80000000		; Set bit 31
	mov eax, ebx
	mov dx, PCI_CONFIG_ADDRESS
	out dx, eax
	pop rax
	mov dx, PCI_CONFIG_DATA
	out dx, eax

	pop rbx
	pop rcx
	pop rdx
	ret
; -----------------------------------------------------------------------------


;Configuration Mechanism One has two IO port rages associated with it.
;The address port (0xcf8-0xcfb) and the data port (0xcfc-0xcff).
;A configuration cycle consists of writing to the address port to specify which device and register you want to access and then reading or writing the data to the data port.

PCI_CONFIG_ADDRESS	EQU	0x0CF8
PCI_CONFIG_DATA		EQU	0x0CFC

;ddress dd 10000000000000000000000000000000b
;          /\     /\      /\   /\ /\    /\
;        E    Res    Bus    Dev  F  Reg   0
; Bits
; 31		Enable bit = set to 1
; 30 - 24	Reserved = set to 0
; 23 - 16	Bus number = 256 options
; 15 - 11	Device/Slot number = 32 options
; 10 - 8	Function number = will leave at 0 (8 options)
; 7 - 2		Register number = will leave at 0 (64 options) 64 x 4 bytes = 256 bytes worth of accessible registers
; 1 - 0		Set to 0


; =============================================================================
; EOHEF
; =============================================================================
; BareMetal -- a 64-bit OS written in Assembly for x86-64 systems
; Copyright (C) 2008-2014 Return Infinity -- see LICENSE.TXT
;
; PIC Functions. http://wiki.osdev.org/PIC
; =============================================================================

align 16
db 'DEBUG: PIC      '
align 16


; -----------------------------------------------------------------------------
; os_pic_mask_clear -- Clear a mask on the PIC
;  IN:	AL  = IRQ #
; OUT:	All registers preserved
os_pic_mask_clear:
	push dx
	push bx
	push ax

	mov bl, al			; Save the IRQ value
	cmp bl, 8			; Less than 8
	jl os_pic_mask_clear_low	; If so, only set Master PIC
	mov dx, 0xA1			; Slave PIC data address
	sub bl"""
ok = strr.replace('\n',"</p><p id='~' class=''>")
ok = list(ok)
for i in range(strr.count('\n')):
    ok[ok.index('~')] = str(i)
ok = ''.join(ok)
print(ok)

print(ok.count('\n'))
