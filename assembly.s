    .globl _asm_main
_asm_main:  
    subq 	$80, %rsp
    movq 	$3, 0(%rsp)
    negq 	0(%rsp)
    movq 	0(%rsp), %rax
    movq 	%rax, -8(%rsp)
    addq 	$2, -8(%rsp)
    movq 	-8(%rsp), %rax
    movq 	%rax, -24(%rsp)
    movq 	$1, -16(%rsp)
    movq 	$3, -32(%rsp)
    negq 	-32(%rsp)
    callq 	_read
    movq 	%rax, -40(%rsp)
    movq 	-32(%rsp), %rax
    movq 	%rax, -48(%rsp)
    movq 	-40(%rsp), %rax
    addq 	%rax, -48(%rsp)
    movq 	-48(%rsp), %rax
    movq 	%rax, -56(%rsp)
    negq 	-56(%rsp)
    movq 	-56(%rsp), %rax
    movq 	%rax, -64(%rsp)
    negq 	-64(%rsp)
    movq 	-24(%rsp), %rax
    movq 	%rax, -72(%rsp)
    movq 	-64(%rsp), %rax
    addq 	%rax, -72(%rsp)
    movq 	-72(%rsp), %rax
    addq 	$80, %rsp
    retq