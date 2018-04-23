.data
	tag_vector13:
		.quad 3
		.quad 2
		.quad tag_int12
		.quad tag_bool10
	tag_int12:
		.quad 2
		.quad 2
	tag_void7:
		.quad 0
		.quad 0
	tag_vector20:
		.quad 3
		.quad 3
		.quad tag_int19
		.quad tag_vector13
		.quad tag_void7
	tag_bool10:
		.quad 1
		.quad 0
	tag_int19:
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
    movq 	$2, %rcx
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
    je	 	then29
    movq 	$0, %rcx
    jmp	 	if_end29
then29:
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
if_end29:
    movq 	_free_ptr(%rip), %rcx
    addq 	$32, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	tag_vector13(%rip), %rcx
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
    je	 	then28
    movq 	$0, %rcx
    jmp	 	if_end28
then28:
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
if_end28:
    movq 	_free_ptr(%rip), %rcx
    addq 	$40, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	tag_vector20(%rip), %rcx
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
