    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$16, %rsp
    movq 	$3, -8(%rbp)
    negq 	-8(%rbp)
    movq 	-8(%rbp), %rax
    movq 	%rax, -16(%rbp)
    addq 	$10, -16(%rbp)
    movq 	-16(%rbp), %rax
    leaveq
    retq
