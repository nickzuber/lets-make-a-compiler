.data
    n123:
        .quad 3
        .quad 2
        .quad n1
        .quad v2
    v2:
        .quad 3
        .quad 1
        .quad b4
    n1:
        .quad 2
        .quad 111
    n3:
        .quad 2
        .quad 333
    b4:
        .quad 1
        .quad 0
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
    movq 	$21, %rdx
    movq 	$0, %r8
    movq 	_free_ptr(%rip), %rcx
    movq 	%rcx, %r10
    addq 	$2, %r10
    movq 	_fromspace_end(%rip), %rcx
    cmpq 	%rcx, %r10
    setg 	%al
    movzbq 	%al, %rcx
    movq 	$1, %rax
    cmpq 	%rcx, %rax
    je	 	then14
    movq 	$0, %rcx
    jmp	 	if_end14
then14:
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
    movq 	%r9, %rcx
if_end14:
    movq 	_free_ptr(%rip), %rcx
    pushq 	%rcx
    pushq 	%rdx
    pushq 	%r8
    pushq 	%r9
    pushq 	%r10
    callq 	_show_freeptr
    popq 	%r10
    popq 	%r9
    popq 	%r8
    popq 	%rdx
    popq 	%rcx
    addq 	$32, _free_ptr(%rip)
    movq 	%rcx, %r11
    movq 	$3, 0(%r11)
    movq 	$2, 8(%r11)
    movq 	%rcx, %r11
    movq 	%rdx, 16(%r11)
    movq 	$0, %rdx
    movq 	%rcx, %r11
    movq 	%r8, 24(%r11)
    movq 	$0, %rcx
    leaq    n123(%rip), %rax
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
