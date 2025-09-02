@.str.reg19 = private constant [7 x i8] c"Hello!\00", align 1
declare ccc i32 @InitWindow(i32 %width, i32 %height, i64 %title)
declare ccc i32 @BeginDrawing()
declare ccc i32 @GetMouseX()
declare ccc i32 @GetMouseY()
declare ccc i32 @DrawRectangle(i32 %x, i32 %y, i32 %width, i32 %height, i32 %color)
declare ccc i32 @DrawCircle(i32 %x, i32 %y, float %radius, i32 %color)
declare ccc i32 @GetMouseWheelMove()
declare ccc i32 @EndDrawing()
declare ccc i32 @WindowShouldClose()
declare ccc i32 @exit(i32 %code)
define ccc i32 @reg0() {
extern:
    %reg4 = add i64 0, 69
    %reg6 = add i64 0, 21
    %reg7 = add i64 0, 2
    %reg5 = mul i64 %reg6, %reg7
    %reg3 = add i64 %reg4, %reg5
    %reg2 = trunc i64 %reg3 to i32
    call ccc void @exit(i32 %reg2)
    ret i32 0
}
define ccc i32 @reg8() {
extern:
    %reg11 = add i64 0, 0
    %reg10 = trunc i64 %reg11 to i32
    call ccc void @exit(i32 %reg10)
    ret i32 0
}
define ccc i32 @main() {
extern:
    %reg14 = add i64 0, 800
    %reg13 = trunc i64 %reg14 to i32
    %reg16 = add i64 0, 600
    %reg15 = trunc i64 %reg16 to i32
    %reg18 = getelementptr i64, ptr @.str.reg19
    %reg17 = ptrtoint ptr %reg18 to i64
    call ccc void @InitWindow(i32 %reg13, i32 %reg15, i64 %reg17)
    br label %reg20
reg20:
    %reg24 = call ccc i1 @WindowShouldClose()
    %reg26 = add i64 0, 0
    %reg25 = trunc i64 %reg26 to i1
    %reg23 = icmp eq i1 %reg24, %reg25
    br i1 %reg23, label %reg21, label %reg22
reg21:
    call ccc void @BeginDrawing()
    %reg29 = call ccc i32 @GetMouseX()
    %reg30 = call ccc i32 @GetMouseY()
    %reg32 = call ccc float @GetMouseWheelMove()
    %reg33 = fadd float 0.0, 5.0000000000
    %reg31 = fmul float %reg32, %reg33
    %reg35 = add i64 0, 4278190335
    %reg34 = trunc i64 %reg35 to i32
    call ccc void @DrawCircle(i32 %reg29, i32 %reg30, float %reg31, i32 %reg34)
    call ccc void @EndDrawing()
    br label %reg20
reg22:
    %reg37 = call ccc i32 @reg8()
    ret i32 0
}

