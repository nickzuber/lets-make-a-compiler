.globl _asm_main

_asm_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$1, %rcx
    xorq 	$1, %rcx
    cmpq 	$1, %rcx
    je	 	IF_THEN_0
    movq 	$0, %rcx
    jmp	 	IF_END_0
IF_THEN_0:
    movq 	$1, %rcx
IF_END_0:
    movq 	%rcx, %rax
    leaveq
    retq
