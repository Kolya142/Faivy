#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#if !defined(__x86_64__) && !defined(_M_X64)
#error "Allowed only x86_64. Sorry."
#endif
uint64_t regs[8192];
uint64_t stack[8192];
size_t sp;
#define push(w) do {stack[sp++] = (uint64_t)(w);} while(0)
#define pop (stack[--sp])
uint8_t static_data[] = {0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x2C, 0x20, 0x00, 0x21, 0x0A, 0x00, 0x50, 0x72, 0x6F, 0x67, 0x72, 0x61, 0x6D, 0x6D, 0x69, 0x6E, 0x67, 0x2C, 0x20, 0x63, 0x75, 0x70, 0x20, 0x6F, 0x66, 0x20, 0x74, 0x65, 0x61, 0x20, 0x4E, 0x6F, 0x2E, 0x00, 0x48, 0x69, 0x2C, 0x20, 0x66, 0x72, 0x6F, 0x6D, 0x20, 0x63, 0x6F, 0x6D, 0x70, 0x74, 0x69, 0x6D, 0x65, 0x21, 0x0A, 0x00, 0x21, 0x0A, 0x00};
int __gen_fn001() {
	push(regs[8001]);
	regs[8001] = sp;
	push(regs[0]);
	push(static_data+0);
	regs[0] = pop;
	printf("%s", (const char *)regs[0]);
	push(35);
	push(34);
	regs[514] = pop;
	regs[513] = pop;
	regs[512] = regs[514]+regs[513];
	push(regs[512]);
	push(42);
	regs[514] = pop;
	regs[513] = pop;
	regs[512] = regs[514]+regs[513];
	push(regs[512]);
	regs[0] = pop;
	printf("%zu", regs[0]);
	push(static_data+8);
	regs[0] = pop;
	printf("%s", (const char *)regs[0]);
	sp = regs[8001];
	regs[8001] = pop;
	return 0;
}
int __gen_fn002() {
	push(regs[8001]);
	regs[8001] = sp;
	push(static_data+11);
	regs[0] = pop;
	printf("%s", (const char *)regs[0]);
	push(69-42);

	push(10);
	regs[514] = pop;
	regs[513] = pop;
	regs[512] = regs[514]+regs[513];
	push(regs[512]);
	regs[0] = pop;
	printf("%zu", regs[0]);
	push(static_data+59);
	regs[0] = pop;
	printf("%s", (const char *)regs[0]);
	sp = regs[8001];
	regs[8001] = pop;
	return 0;
}
int __gen_main() {
	push(regs[8001]);
	regs[8001] = sp;
	push(69);
	regs[0] = pop;
	__gen_fn001(regs[0]);
	__gen_fn002();
	sp = regs[8001];
	regs[8001] = pop;
	return 0;
}
int main() {
	__gen_main();
}
