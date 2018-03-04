.globl _asm_main

_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    pushq 	%rcx
    pushq 	%rdx
    pushq 	%r8
    pushq 	%r9
    pushq 	%r10
    pushq 	%r11
    callq 	_read_int
    popq 	%r11
    popq 	%r10
    popq 	%r9
    popq 	%r8
    popq 	%rdx
    popq 	%rcx
    movq 	%rax, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    sete 	%al
    movzbq 	%al, %rcx
    cmpq 	$1, %rcx
    je	 	IF_THEN_0
    movq 	$9000, %rcx
    jmp	 	IF_END_0
IF_THEN_0:
    movq 	$9, %rax
    cmpq 	$10, %rax
    setl 	%al
    movzbq 	%al, %rcx
    cmpq 	$1, %rcx
    je	 	IF_THEN_1
    movq 	$22, %rcx
    jmp	 	IF_END_1
IF_THEN_1:
    movq 	$11, %rcx
IF_END_1:
    movq 	%rcx, %rcx
IF_END_0:
    movq 	%rcx, %rax
    leaveq
    retq
