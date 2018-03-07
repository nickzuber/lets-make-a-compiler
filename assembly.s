.globl _main

_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$1, %rax
    cmpq 	$1, %rax
    je	 	then16
    movq 	$0, %rcx
    jmp	 	if_end16
then16:
    movq 	$21, %rcx
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then17
    movq 	%rcx, %rcx
    addq 	$1, %rcx
    movq 	%rcx, %rcx
    jmp	 	if_end17
then17:
    movq 	$0, %rcx
if_end17:
    movq 	%rcx, %rcx
if_end16:
    movq 	%rcx, %rax
    movq 	%rax, %rdi
    callq 	_print_int
    movq 	$0, %rax
    leaveq
    retq
