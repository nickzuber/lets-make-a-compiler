.data
	tag_vector28:
		.quad 3
		.quad 3
		.quad tag_int27
		.quad tag_vector21
		.quad tag_void10
	tag_bool16:
		.quad 1
		.quad 0
	tag_void10:
		.quad 0
		.quad 0
	tag_vector17:
		.quad 3
		.quad 1
		.quad tag_bool16
	tag_vector21:
		.quad 3
		.quad 2
		.quad tag_vector17
		.quad tag_bool13
	tag_int27:
		.quad 2
		.quad 1
	tag_bool13:
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
    movq 	$1, %rcx
    movq 	$0, %rcx
    movq 	_free_ptr(%rip), %rcx
    movq 	%rcx, %r8
    addq 	$1, %r8
    movq 	_fromspace_end(%rip), %rcx
    cmpq 	%rcx, %r8
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then39
    movq 	$0, %rcx
    jmp	 	if_end39
then39:
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
if_end39:
    movq 	_free_ptr(%rip), %rcx
    addq 	$24, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	tag_vector17(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$1, 8(%r11)
    movq 	$1, %rcx
    movq 	_free_ptr(%rip), %rcx
    addq 	$2, %rcx
    movq 	_fromspace_end(%rip), %r8
    cmpq 	%r8, %rcx
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then38
    movq 	$0, %rcx
    jmp	 	if_end38
then38:
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
if_end38:
    movq 	_free_ptr(%rip), %rcx
    addq 	$32, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	tag_vector21(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$2, 8(%r11)
    movq 	$0, %rcx
    movq 	_free_ptr(%rip), %rcx
    addq 	$3, %rcx
    movq 	_fromspace_end(%rip), %r8
    cmpq 	%r8, %rcx
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then37
    movq 	$0, %rcx
    jmp	 	if_end37
then37:
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
if_end37:
    movq 	_free_ptr(%rip), %rcx
    addq 	$40, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	tag_vector28(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$3, 8(%r11)
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
