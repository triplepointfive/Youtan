; ModuleID = 'my cool jit'

declare i32 @putchar(i32)

declare i32 @getchar()

define i32 @main() {
entry:
  %0 = alloca i32
  store i32 0, i32* %0
  
  %1 = alloca [ 1 x i32 ]

  store i32 0, i32* %1

  ; %t = tail call i32 @getchar()

  ; store i32 %t, i32* %0

  %2 = load i32* %0
  ; tail call i32 @putchar(i32 %1)

  ret i32 0
}
