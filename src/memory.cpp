#include <faivy.hpp>

Alloc::Context::Context(size_t cap) {
    this->cap = cap;
    this->used = 0;
    this->start = malloc(cap);
}
void *Alloc::Context::alloc(size_t size) {
    if (this->used+size>this->cap) return NULL;
    void *ptr = (void *)((size_t)this->start+this->used);
    this->used += size;
    return ptr;
}
size_t Alloc::Context::save() {
    return this->used;
}
void Alloc::Context::restore(size_t v) {
    this->used = v;
}
char *Alloc::Context::tprintf(const char *fmt, ...) {
    va_list args;
    va_list args0;
    va_start(args, fmt);
    va_copy(args0, args);
    size_t s = vsnprintf(NULL, 0, fmt, args0);
    char *buf = (char *)alloc(s+1);
    vsnprintf(buf, s+1, fmt, args);
    va_end(args);
    return buf;
}
Alloc::Context::~Context() {
    free(this->start);
}
