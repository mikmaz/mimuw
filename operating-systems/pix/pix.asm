; Author:       Mikołaj Mazurczyk
; Index number: 426819

extern pixtime

; What this macro does can be written as:
;         %1 := (rdx:rax) mod %2
; Value at 'rdx:rax' has to be set outside the macro.
%macro modulo 2
  div %2
  mov %1, rdx
%endmacro

; Stack pointer has to be set properly outside macro.
%macro run_pixtime 0
  rdtsc                               ; result stored in edx:eax
  mov    rdi, rdx                     ; rdi := '000:edx'
  shl    rax, 32                      ; rax := 'eax:000'
  shld   rdi, rax, 32                 ; rdi = edx:eax
  call   pixtime
%endmacro

global pix

section .text

; Procedure calculating expression '16^n mod m' using 
; "Modulo fast exponentiation" algorithm.
; Arguments of procedure:
;   * r10 - m (value left unchanged)
;   * r11 - n (value is changed during procedure's runtime)
; Other registers used in procedure:
;   * r9  - result
;   * r12 - least significant bit sign of n
;   * r13 - consecutive powers of 16 modulo 'm' (referred later in comments 
;           as 'x')
fast_expo_mod:
  xor rdx, rdx
  mov rax, 1
  modulo r9, r10   ; result := 1 mod m
  cmp r11, 0       ; If 'n == 0' return '1 mod m'.
  je ret_expo
  mov r9, 1        ; result := 1
  xor rdx, rdx
  mov rax, 16
  modulo r13, r10  ; x := 16 % m
.loop:
  mov r12, r11 
  and r12, 1       ; r12 := least significant bit sign of r11
  cmp r12, 0       ; If 'r12 == 0' skip result multiplication.
  je .skip_res_mult
  mov rax, r9
  mul r13          ; rdx:rax := result * x
  modulo r9, r10   ; result := (result * x) mod m
.skip_res_mult:
  mov rax, r13
  mul r13          ; rdx:rax := x * x
  modulo r13, r10  ; x := (x * x) mod m
  shr r11, 1       ; r11 >> 1
  cmp r11, 0       ; If 'n == 0' return 'result'.
  jne .loop
ret_expo:
  ret


; Procedure calculating:
;   {{∑[k=0][n] 16^(n - k) mod (8k + j)) / (8k + j)} +
;    ∑[k=n+1][∞] 16^(n - k) / (8k + j)} 
; on 64 bit registers. Of course result won't be exact value of formula written 
; above, but I assume that first 32 bits will be the same as in the formula for 
; 'n' values given in test. Consecutive values of second sum are calculated as 
; long as they are not smaller than 2^(-64).
; Arguments of procedure:
;   * rsi - n (value left unchanged)
;   * r8 - j (value left unchanged)
; Other registers used in procedure:
;   * rdx/rax - used for calculations
;   * rcx - k
;   * r9 - stores values of consecutive numerators from first sum of formula
;   * r10 - stores values of consecutive denominators from both sums of formula
;   * r14 - result
;   * r15 - (n - k)
calculate_Sj:
  xor r14, r14       ; result := 0
  xor rcx, rcx       ; k := 0
  mov r10, r8        ; denominator := j
  mov r15, rsi       ; r11 := n
.sum1:
  mov r11, r15       ; r11 := n - k (argument for 'fast_expo_mod' procedure)
  call fast_expo_mod ; r9 := 16^(r11) mod r10
  mov rdx, r9        
  xor rax, rax
  div r10            ; rdx:rax := (2^64 * 16^(r11) mod r10) / (8k + j)
  add r14, rax       ; result += {numerator / denominator}
  inc rcx            ; k += 1
  dec r15            ; r15 := n - k
  add r10, 8         ; r10 := 8k + j
  cmp rcx, rsi       ; If 'k <= n', calculate next element of sum.
  jbe .sum1
  sub r10, 8         ; denominator := 8(k - 1) + j
  xor rcx, rcx
.sum2:
  add r10, 8         ; r10 := 8k + j
  add cl, 4          ; cl := k - n
  xor rax, rax
  mov rdx, 1
  div r10            ; 2^64 / (8k + j)
  shr rax, cl        ; rax := (1 / 8k + j) / 16^(k - n)
  add r14, rax       ; result += 1 / ((8k + j) * 16^(k - n))
  cmp rax, 0         ; If current element of sum is smaller than 2^(-64), end
                     ; calculations.
  jne .sum2
  ret

; Implementation of C function:
;          'void pix(uint32_t *ppi, uint64_t *pidx, uint64_t max)'
; Arguments:
;   * rdi - ppi
;   * rsi - pidx (referred later in comments as 'm')
;   * rdx - max
pix:
  push rdi                   ; Store 'pix' arguments before calling 'pixtime'.
  push rsi
  push rdx

  run_pixtime

  pop rdx                    ; Restore 'pix' arguments.
  pop rsi
  pop rdi

  push rbx                   ; Store values of preserved registers.
  push rbp
  push r12
  push r13
  push r14
  push r15                   ; stack := r15|r14|r13|r12|rbp|rbx|...

  mov rbx, rsi               ; rbx := pidx
  mov rbp, rdx               ; rbp := max
.pix_loop:
  mov eax, 1
  lock \
  xadd qword [rbx], rax      ; rax := [rbx] ; [rbx] += 1
  mov rsi, rax               ; rsi := m
  cmp rsi, rbp               ; If 'm >= max', return.
  jae .pix_ret
  shl rsi, 3                 ; rsi := 8m

  mov r8, 6                  ; j := 6
  call calculate_Sj          ; r14 := S6
  push r14

  mov r8, 5                  ; j := 5
  call calculate_Sj          ; r14 := S5
  push r14

  mov r8, 4                  ; j := 4
  call calculate_Sj          ; r14 := S4
  push r14

  mov r8, 1                  ; j := 6
  call calculate_Sj          ; r14 := S1

  mov rax, r14               ; rax := S1
  shl rax, 2                 ; rax := 4 * S1

  pop rdx                    ; rdx := S4
  shl rdx, 1                 ; rdx := 2 * S4
  sub rax, rdx               ; rax := 4 * S1 - 2 * S4

  pop rdx                    ; rdx := S5
  sub rax, rdx               ; rax := 4 * S1 - 2 * S4 - S5

  pop rdx                    ; rdx := S6
  sub rax, rdx               ; rax := 4 * S1 - 2 * S4 - S5 - S6

  shr rax, 32
  mov rcx, rsi               ; rcx := 8m
  shr rcx, 1                 ; rcx := 4m
  mov dword [rdi + rcx], eax ; ppi[m] := 4 * S1 - 2 * S4 - S5 - S6
  jmp .pix_loop
.pix_ret:
  pop r15                    ; stack := r14|r13|r12|rbp|rbx|... 
                             ; Sets stack pointer for pixtime.
  run_pixtime
  pop r14
  pop r13
  pop r12
  pop rbp
  pop rbx
  ret
