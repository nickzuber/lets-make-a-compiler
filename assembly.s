.data
	ty_int14:
		.quad 2
		.quad 111
	ty_bool15:
		.quad 1
		.quad 0
	ty_int16:
		.quad 2
		.quad 222
	ty_vector9:
		.quad 3
		.quad 4
		.quad ty_int14
		.quad ty_bool15
		.quad ty_int16
		.quad ty_bool17
	ty_bool17:
		.quad 1
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
    movq 	$111, %rcx
    movq 	$0, %rcx
    movq 	$222, %rcx
    movq 	$1, %rcx
    movq 	_free_ptr(%rip), %rcx
    movq 	%rcx, %r8
    addq 	$4, %r8
    movq 	_fromspace_end(%rip), %rcx
    cmpq 	%rcx, %r8
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then25
    movq 	$0, %rcx
    jmp	 	if_end25
then25:
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
if_end25:
    movq 	_free_ptr(%rip), %rcx
    addq 	$48, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	ty_vector9(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$4, 8(%r11)
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
