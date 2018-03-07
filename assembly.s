.globl _main

_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$1, %rax
    cmpq 	$1, %rax
    je	 	then11
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then14
    movq 	$5, %rcx
    jmp	 	if_end14
then14:
    movq 	$4, %rcx
if_end14:
    movq 	%rcx, %rcx
    jmp	 	if_end11
then11:
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then12
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then13
    movq 	$3, %rcx
    jmp	 	if_end13
then13:
    movq 	$2, %rcx
if_end13:
    movq 	%rcx, %rcx
    jmp	 	if_end12
then12:
    movq 	$1, %rcx
if_end12:
    movq 	%rcx, %rcx
if_end11:
    movq 	%rcx, %rax
    movq 	%rax, %rdi
    callq 	_print_int
    movq 	$0, %rax
    leaveq
    retq
