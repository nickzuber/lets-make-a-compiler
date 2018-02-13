    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$24, %rsp
    movq 	$1, -8(%rbp)
    negq 	-8(%rbp)
    movq 	$4, -16(%rbp)
    addq 	$2, -16(%rbp)
    movq 	-8(%rbp), %rax
    movq 	%rax, -24(%rbp)
    movq 	-16(%rbp), %rax
    addq 	%rax, -24(%rbp)
    movq 	-24(%rbp), %rax
    leaveq
    retq
