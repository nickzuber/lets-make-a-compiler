.data
	tag_bool77:
		.quad 1
		.quad 0
	tag_int63:
		.quad 2
		.quad 123
	tag_int79:
		.quad 2
		.quad 321
	tag_vector44:
		.quad 3
		.quad 3
		.quad tag_int43
		.quad tag_bool41
		.quad tag_int39
	tag_int31:
		.quad 2
		.quad 321
	tag_vector80:
		.quad 3
		.quad 3
		.quad tag_int79
		.quad tag_bool77
		.quad tag_int75
	tag_int75:
		.quad 2
		.quad 123
	tag_bool65:
		.quad 1
		.quad 0
	tag_int67:
		.quad 2
		.quad 321
	tag_vector68:
		.quad 3
		.quad 3
		.quad tag_int67
		.quad tag_bool65
		.quad tag_int63
	tag_int27:
		.quad 2
		.quad 123
	tag_vector56:
		.quad 3
		.quad 3
		.quad tag_int55
		.quad tag_bool53
		.quad tag_int51
	tag_int55:
		.quad 2
		.quad 321
	tag_int51:
		.quad 2
		.quad 123
	tag_int39:
		.quad 2
		.quad 123
	tag_bool53:
		.quad 1
		.quad 0
	tag_bool41:
		.quad 1
		.quad 0
	tag_bool29:
		.quad 1
		.quad 0
	tag_int43:
		.quad 2
		.quad 321
	tag_vector32:
		.quad 3
		.quad 3
		.quad tag_int31
		.quad tag_bool29
		.quad tag_int27
.text
	.globl _main
_main:  
    pushq 	%rbp
    movq 	%rsp, %rbp
    pushq 	%rdi
    pushq 	%rsi
    pushq 	%rdx
    pushq 	%rcx
    pushq 	%r8
    pushq 	%r9
    subq 	$0, %rsp
    movq 	$1024, %rdi
    movq 	$1024, %rsi
    callq 	_initialize
    movq 	_rootstack_begin(%rip), %r15
    movq 	$321, %rdx
    movq 	$0, %rdx
    movq 	$123, %rdx
    movq 	_free_ptr(%rip), %rdx
    movq 	%rdx, %r8
    addq 	$3, %r8
    movq 	_fromspace_end(%rip), %rdx
    cmpq 	%rdx, %r8
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then95
    movq 	$0, %rdx
    jmp	 	if_end95
then95:
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
    movq 	%rcx, %rdx
if_end95:
    movq 	_free_ptr(%rip), %rdx
    addq 	$40, _free_ptr(%rip)
    movq 	%rdx, %r11
    leaq 	tag_vector32(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$3, 8(%r11)
    movq 	$321, %rdx
    movq 	$0, %rdx
    movq 	$123, %rdx
    movq 	_free_ptr(%rip), %rdx
    addq 	$3, %rdx
    movq 	_fromspace_end(%rip), %r8
    cmpq 	%r8, %rdx
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then94
    movq 	$0, %rdx
    jmp	 	if_end94
then94:
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
    movq 	%rcx, %rdx
if_end94:
    movq 	_free_ptr(%rip), %rdx
    addq 	$40, _free_ptr(%rip)
    movq 	%rdx, %r11
    leaq 	tag_vector44(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$3, 8(%r11)
    movq 	$321, %rdx
    movq 	$0, %rdx
    movq 	$123, %rdx
    movq 	_free_ptr(%rip), %rdx
    addq 	$3, %rdx
    movq 	_fromspace_end(%rip), %r8
    cmpq 	%r8, %rdx
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then93
    movq 	$0, %rdx
    jmp	 	if_end93
then93:
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
    movq 	%rcx, %rdx
if_end93:
    movq 	_free_ptr(%rip), %rdx
    addq 	$40, _free_ptr(%rip)
    movq 	%rdx, %r11
    leaq 	tag_vector56(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$3, 8(%r11)
    movq 	$321, %rdx
    movq 	$0, %rdx
    movq 	$123, %rdx
    movq 	_free_ptr(%rip), %rdx
    addq 	$3, %rdx
    movq 	_fromspace_end(%rip), %r8
    cmpq 	%r8, %rdx
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then92
    movq 	$0, %rdx
    jmp	 	if_end92
then92:
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
    movq 	%rcx, %rdx
if_end92:
    movq 	_free_ptr(%rip), %rdx
    addq 	$40, _free_ptr(%rip)
    movq 	%rdx, %r11
    leaq 	tag_vector68(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$3, 8(%r11)
    movq 	$321, %rdx
    movq 	$0, %rdx
    movq 	$123, %rdx
    movq 	_free_ptr(%rip), %rdx
    addq 	$3, %rdx
    movq 	_fromspace_end(%rip), %r8
    cmpq 	%r8, %rdx
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then91
    movq 	$0, %rcx
    jmp	 	if_end91
then91:
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
if_end91:
    movq 	_free_ptr(%rip), %rcx
    addq 	$40, _free_ptr(%rip)
    movq 	%rcx, %r11
    leaq 	tag_vector80(%rip), %rcx
    movq 	%rcx, 0(%r11)
    movq 	$3, 8(%r11)
    movq 	%rcx, %rax
    leaq 	_ty_vector(%rip), %rdi
    movq 	%rax, %rsi
    callq 	_print_result
    movq 	$0, %rax
    addq 	$0, %rsp
    pushq 	%r9
    pushq 	%r8
    pushq 	%rcx
    pushq 	%rdx
    pushq 	%rsi
    pushq 	%rdi
    leaveq
    popq 	%rbp
    retq
