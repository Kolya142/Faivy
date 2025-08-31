typedef unsigned long long u64;typedef unsigned int u32;typedef long long i64;typedef int i32;typedef char *cstring;typedef float float32;

extern u64 write(i32 fd, void* data, u64 size);extern u64 strlen(cstring str);typedef struct Slice {void* arr; u64 size; } Slice;typedef struct SString {cstring str; u64 size; } SString;static SString sstring_from_cstring(cstring str){return ((SString){str,strlen(str)
}
);;}static void print_str(SString value){write(1,(value . str),(value . size))
;}static void hello(SString name){print_str(sstring_from_cstring("Hello, ")
)
;print_str(name)
;print_str(sstring_from_cstring("!\n")
)
;}i32 main(){SString name=sstring_from_cstring("Faivy")
;;if (1;){hello(name)
;}}