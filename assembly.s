.globl _main

_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    pushq 	%r14
    pushq 	%r13
    pushq 	%r12
    pushq 	%rbx
    subq 	$0, %rsp
    movq 	$1024, %rdi
    movq 	$1024, %rsi
    callq 	_initialize
    movq 	_rootstack_begin(%rip), %r15
    movq 	$1, %rax
    cmpq 	$1, %rax
    je	 	then13
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then16
    movq 	$5, %rcx
    jmp	 	if_end16
then16:
    movq 	$4, %rcx
if_end16:
    jmp	 	if_end13
then13:
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then14
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then15
    movq 	$3, %rcx
    jmp	 	if_end15
then15:
    movq 	$2, %rcx
if_end15:
    jmp	 	if_end14
then14:
    movq 	$1, %rcx
if_end14:
if_end13:
    movq 	%rcx, %rax
    leaq 	_ty_int(%rip), %rdi
    movq 	%rax, %rsi
    callq 	_print_result
    movq 	$0, %rax
    addq 	$0, %rsp
    popq 	%rbx
    popq 	%r12
    popq 	%r13
    popq 	%r14
    leaveq
    popq 	%rbp
    retq
