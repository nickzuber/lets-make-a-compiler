.data
	#;vectors
.text
	#;functions and closures
	.globl add_nums
	add_nums:      
        pushq 	%rbp
        movq 	%rsp, %rbp
        pushq 	%r15
        pushq 	%r14
        pushq 	%r13
        pushq 	%r12
        pushq 	%rbx
        subq 	$0, %rsp
        movq 	%rsi, %r9
        addq 	%rdx, %r9
        movq 	%r9, %rax
        addq 	$0, %rsp
        popq 	%rbx
        popq 	%r12
        popq 	%r13
        popq 	%r14
        popq 	%r15
        popq 	%rbp
        retq

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
    leaq 	add_nums(%rip), %r8
    movq 	%r15, %rdi
    movq 	$2, %rsi
    movq 	$5, %rdx
    pushq 	%rdi
    pushq 	%rsi
    pushq 	%rdx
    pushq 	%rcx
    pushq 	%r8
    pushq 	%r9
    callq 	*%r8
    pushq 	%r9
    pushq 	%r8
    pushq 	%rcx
    pushq 	%rdx
    pushq 	%rsi
    pushq 	%rdi
    movq 	%rax, %r8
    addq 	$1, %r8
    movq 	%r8, %rax
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
