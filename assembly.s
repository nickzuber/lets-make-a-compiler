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
        movq %rdi, %rbx
movq %rsi, %rbx
movq %rdx, %rcx
addq %rcx, %rbx
movq %rbx, %rax
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
    leaq 	add_nums(%rip), %rcx
    movq 	%r15, %rdi
    movq 	$1, %rsi
    movq 	$2, %rdx
    pushq 	%rdi
    pushq 	%rsi
    pushq 	%rdx
    pushq 	%rcx
    pushq 	%r8
    pushq 	%r9
    callq 	*%rcx
    pushq 	%r9
    pushq 	%r8
    pushq 	%rcx
    pushq 	%rdx
    pushq 	%rsi
    pushq 	%rdi
    movq 	%rax, %rcx
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
