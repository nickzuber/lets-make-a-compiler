.data
	tag_vector9:
		.quad 3
		.quad 2
		.quad tag_int8
		.quad tag_void6
	tag_void6:
		.quad 0
		.quad 0
	tag_int8:
		.quad 2
		.quad 1
.text
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
    movq 	$1, %rcx
    movq 	$0, %rcx
    movq 	_free_ptr(%rip), %rcx
    movq 	%rcx, %r8
    addq 	$2, %r8
    movq 	_fromspace_end(%rip), %rcx
    cmpq 	%rcx, %r8
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then15
    movq 	$0, %rcx
    jmp	 	if_end15
then15:
    movq 	%r15, %rdi
    pushq 	%rcx
    pushq 	%rdx
    pushq 	%r8
    pushq 	%r9
    pushq 	%r10
    callq 	_collect
    popq 	%r10
    popq 	%r9
    popq 	%r8
    popq 	%rdx
    popq 	%rcx
    movq 	%rdx, %rcx
if_end15:
    movq 	_free_ptr(%rip), %rcx
    addq 	$32, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	tag_vector9(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$2, 8(%r11)
    movq 	%rcx, %rax
    leaq 	_ty_vector(%rip), %rdi
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
