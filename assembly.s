.data
	#;vectors
.text
	#;functions and closures
	.globl max
	max:      
        pushq 	%rbp
        movq 	%rsp, %rbp
        pushq 	%r15
        pushq 	%r14
        pushq 	%r13
        pushq 	%r12
        pushq 	%rbx
        subq 	$0, %rsp
        cmpq 	%rdx, %rsi
        setg 	%al
        movzbq 	%al, %r8
        movq 	$1, %rax
        cmpq 	%r8, %rax
        je	 	then9
        movq 	%rdx, %r8
        jmp	 	if_end9
then9:
        movq 	%rsi, %r8
if_end9:
        movq 	%r8, %rax
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
    leaq 	max(%rip), %r8
    movq 	%r15, %rdi
    movq 	$12, %rsi
    movq 	$2, %rdx
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
