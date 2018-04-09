.globl _main

_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    subq 	$0, %rsp
    movq 	$1, %rax
    cmpq 	$1, %rax
    je	 	then0
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then3
    movq 	$5, %rcx
    jmp	 	if_end3
then3:
    movq 	$4, %rcx
if_end3:
    movq 	%rcx, %rcx
    jmp	 	if_end0
then0:
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then1
    movq 	$1, %rax
    cmpq 	$0, %rax
    je	 	then2
    movq 	$3, %rcx
    jmp	 	if_end2
then2:
    movq 	$2, %rcx
if_end2:
    movq 	%rcx, %rcx
    jmp	 	if_end1
then1:
    movq 	$1, %rcx
if_end1:
    movq 	%rcx, %rcx
if_end0:
    movq 	%rcx, %rax
    movq 	%rax, %rdi
    callq 	_print_int
    movq 	$0, %rax
    leaveq
    retq
