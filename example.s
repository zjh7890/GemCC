.intel_syntax noprefix
.bss
.data
.text
.global add
add:
  push rbp
  push rbx
  push r12
  push r13
  push r14
  push r15
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 16], edi
  mov [rbp - 8], esi
  lea rax, [rbp - 16]
  mov eax, [rax]
  mov ebx, eax
  lea rax, [rbp - 8]
  mov eax, [rax]
  add eax, ebx
  jmp .L2
.L2:
  mov rsp, rbp
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  ret