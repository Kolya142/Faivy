#include <stdint.h>
#include <assert.h>
#include <stdio.h>
uint8_t stack[8192];
size_t sp;
size_t istack_pop64() {
	if (sp < 8) {
		return 0;
	}
	sp -= 8;
	return *(size_t*)&stack[sp];
}
void istack_push64(size_t v) {
	if (sp > 8192-8) {
		return;
	}
	*(size_t*)&stack[sp] = v;
	sp += 8;
}
uint8_t static_data[] = {0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x2C, 0x20, 0x00, 0x21, 0x0A, 0x00, 0x50, 0x72, 0x6F, 0x67, 0x72, 0x61, 0x6D, 0x6D, 0x69, 0x6E, 0x67, 0x2C, 0x20, 0x00, 0x2D, 0x6E, 0x64, 0x20, 0x77, 0x6F, 0x72, 0x6C, 0x64, 0x21, 0x0A, 0x00};
int __gen_fn001() {
	istack_push64((size_t)static_data+0);
	printf("%s", (const char *)istack_pop64());
	istack_push64(35);
	istack_push64(34);
	{
		size_t b = istack_pop64();
		size_t a = istack_pop64();
		istack_push64(b+a);
	}
	istack_push64(42);
	{
		size_t b = istack_pop64();
		size_t a = istack_pop64();
		istack_push64(b+a);
	}
	printf("%zu", istack_pop64());
	istack_push64((size_t)static_data+8);
	printf("%s", (const char *)istack_pop64());
	istack_push64(0);
	return istack_pop64();
}
int __gen_fn002() {
	istack_push64((size_t)static_data+11);
	printf("%s", (const char *)istack_pop64());
	istack_push64(1);
	istack_push64(2);
	{
		size_t b = istack_pop64();
		size_t a = istack_pop64();
		istack_push64(b+a);
	}
	istack_push64(4);
	{
		size_t b = istack_pop64();
		size_t a = istack_pop64();
		istack_push64(b+a);
	}
	istack_push64(8);
	{
		size_t b = istack_pop64();
		size_t a = istack_pop64();
		istack_push64(b+a);
	}
	istack_push64(16);
	{
		size_t b = istack_pop64();
		size_t a = istack_pop64();
		istack_push64(b+a);
	}
	printf("%zu", istack_pop64());
	istack_push64((size_t)static_data+25);
	printf("%s", (const char *)istack_pop64());
	istack_push64(0);
	return istack_pop64();
}
int __gen_main() {
	__gen_fn001();
	__gen_fn002();
	istack_push64(0);
	return istack_pop64();
}
int main() {
	__gen_main();
}
