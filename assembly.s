.data
	tag_bool25:
		.quad 1
		.quad 1
	tag_vector13:
		.quad 3
		.quad 6
		.quad tag_int20
		.quad tag_bool21
		.quad tag_int22
		.quad tag_int23
		.quad tag_void24
		.quad tag_bool25
	tag_void24:
		.quad 0
		.quad 0
	tag_bool21:
		.quad 1
		.quad 0
	tag_int22:
		.quad 2
		.quad 222
	tag_int23:
		.quad 2
		.quad 333
	tag_int20:
		.quad 2
		.quad 111
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
    movq 	$111, %rcx
    movq 	$0, %rcx
    movq 	$222, %rcx
    movq 	$333, %rcx
    movq 	$0, %rcx
    movq 	$1, %rcx
    movq 	_free_ptr(%rip), %rcx
    movq 	%rcx, %r8
    addq 	$6, %r8
    movq 	_fromspace_end(%rip), %rcx
    cmpq 	%rcx, %r8
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then35
    movq 	$0, %rcx
    jmp	 	if_end35
then35:
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
if_end35:
    movq 	_free_ptr(%rip), %rcx
    addq 	$64, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	tag_vector13(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$6, 8(%r11)
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
