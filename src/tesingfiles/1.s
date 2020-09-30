	.file	"1.c"
	.text
	.globl	add
	.type	add, @function
add:
.LFB0:
	.cfi_startproc
	leaq	(%rdi,%rsi), %eax
	ret
	.cfi_endproc
.LFE0:
	.size	add, .-add
	.ident	"GCC: (Debian 6.3.0-18+deb9u1) 6.3.0 20170516"
	.section	.note.GNU-stack,"",@progbits
