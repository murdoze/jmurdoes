.intel_syntax noprefix



.code16
	mov	si, offset msg
	mov	ah, 0x0e

	/* Print "Hello world" */
loop:
	lodsb
	or	al, al
	jz	load
	int	0x10
	jmp	loop
msg:
	.asciz "hello world"

load:
	mov	ah, 2		/* Read sectors into memory */	
	mov	al, 1		/* Number of sectors to read. */                             
	mov	al, 0x80	/* Drive number. Starts at 0x80, second one is 0x81. TODO why not from 0? */
	mov	ch, 0		/* cylinder number */
	mov	dh, 0		/* Head number */
	mov	cl, 2           /* Starting sector number. 2 because 1 was already loaded. */
	mov	bx, offset stage2
				/*
				Where to load to.
				Must coincide with our stage2 for the linking to work.
				The address is calculated as:
				    16 * ES + BX
				*/
	int	0x13

	jmp stage2

/*
Our linker script will put this section on the right place in memory:
just after the magic bytes.
*/
.section .stage2

stage2:
	call	1f
1:	
	pop	ax
	jmp	stage2a

/* Emit character in AL */
emit:
	push	eax

	mov	ah, 0x0e
	int	0x10

	pop	eax
	ret

space:	
	mov	al, ' '
	call	emit
	ret	

/* Hex print value in EAX */
hexprint:
	push	eax
	push	ebx
	push	ecx
	push	edx

	mov	ch, 8		/* Digital places to print */
	mov	bh, ch

	xor	edx, edx	/* Printable result */
	
1:
	mov	cl, al
	and	cl, 0x0f
	add	dl, cl
	shr	eax, 4
	dec	ch
	jz	2f
	shl	edx, 4
	jmp	1b
	
2:	
	xor	ecx, ecx
3:
	mov	cl, dl
	and	cl, 0x0f
	mov	al, [digits + ecx]

	call	emit

	shr	edx, 4
	dec	bh
	jnz	3b

	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	ret
digits:
	.ascii	"0123456789ABCDEF"



stage2a:
	call	hexprint
	
	mov	si, offset msg2
	mov	ah, 0x0e
loop2:
	lodsb
	or	al, al
	jz	stage3
	int	0x10
	jmp	loop2
msg2:
	.asciz "\n\r>Sector 2<"

stage3:
	call	space
	mov	eax, offset stage2
	call	hexprint

	call	space
	mov	eax, 0x12345678
	call	hexprint
	call	space
	mov	eax, 0x90ABCDEF
	call	hexprint

halt:
	hlt
	jmp	halt


/*
 ********************************************************************************** 
 */

.equ	W,	edi
.equ	IP,	esi
.equ	RSP,	ebp

	mov	W, IP
