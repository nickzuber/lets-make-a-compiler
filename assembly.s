    .globl _asm_main
_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$64, %rsp
    movq 	$1, %rcx
    addq 	$1, %rcx
    movq 	$1, %rdx
    addq 	$1, %rdx
    movq 	%rcx, %rsi
    addq 	%rdx, %rsi
    movq 	$1, %rdi
    addq 	$1, %rdi
    movq 	$1, %r8
    addq 	$1, %r8
    movq 	%rdi, %r9
    addq 	%r8, %r9
    movq 	%rsi, %r10
    addq 	%r9, %r10
    movq 	$1, %r11
    addq 	$1, %r11
    movq 	$1, -16(%rbp)
    addq 	$1, -16(%rbp)
    movq 	%r11, -24(%rbp)
    movq 	-16(%rbp), %rax
    addq 	%rax, -24(%rbp)
    movq 	$1, -32(%rbp)
    addq 	$1, -32(%rbp)
    movq 	$1, -40(%rbp)
    addq 	$1, -40(%rbp)
    movq 	-32(%rbp), %rax
    movq 	%rax, -48(%rbp)
    movq 	-40(%rbp), %rax
    addq 	%rax, -48(%rbp)
    movq 	-24(%rbp), %rax
    movq 	%rax, -56(%rbp)
    movq 	-48(%rbp), %rax
    addq 	%rax, -56(%rbp)
    movq 	%r10, -64(%rbp)
    movq 	-56(%rbp), %rax
    addq 	%rax, -64(%rbp)
    movq 	-64(%rbp), %rax
    leaveq
    retq
