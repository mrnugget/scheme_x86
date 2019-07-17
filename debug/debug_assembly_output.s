	.file	"debug_assembly_output.c"
	.text
	.p2align 4,,15
	.globl	scheme_entry
	.type	scheme_entry, @function
scheme_entry:
.LFB0:
	.cfi_startproc
	movl	$42, %eax
	ret
	.cfi_endproc
.LFE0:
	.size	scheme_entry, .-scheme_entry
	.ident	"GCC: (Ubuntu 7.4.0-1ubuntu1~18.04.1) 7.4.0"
	.section	.note.GNU-stack,"",@progbits
