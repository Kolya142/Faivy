#pragma once
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#ifndef __STR
#define __STR0(v) #v
#define __STR(v) __STR0(v)
#endif

#define mp_da_append(a, m, x)						\
    do {								\
        if ((a).count >= (a).cap) {					\
	    if (!(a).cap) (a).cap = 2;					\
            else (a).cap <<= 1;						\
            (a).m = realloc((a).m, (a).cap*sizeof(*((a).m)));		\
        }								\
        (a).m[(a).count++] = (x);					\
    } while (0)

#define mp_todo() printf("TODO: "__FILE__ ":" __STR(__LINE__) "\n"); exit(1);

typedef struct mp_string_view {
    const char *cstr;
    size_t size;
} mp_string_view_t;

typedef struct mp_string_builder {
    char *arr;
    size_t count, cap;
} mp_string_builder_t;

void mp_string_view_destroy(mp_string_view_t *sv);
mp_string_view_t mp_string_view_from_cstr(const char *cstr);
mp_string_view_t mp_string_view_from_lstr(const char *cstr, size_t size);
char *mp_cstr_from_string_view(mp_string_view_t sv);

void mp_string_builder_destroy(mp_string_builder_t *sb);
void mp_string_builder_clear(mp_string_builder_t *sb);
void mp_string_builder_push(mp_string_builder_t *sb, char c);
void mp_string_builder_printf(mp_string_builder_t *sb, const char *fmt, ...);

mp_string_view_t mp_string_view_from_printf(const char *fmt, ...);

mp_string_view_t mp_read_all(const char *fname);
void mp_write_all(const char *fname, mp_string_view_t sv);
void mp_write_all_sb(const char *fname, mp_string_builder_t sb);

int mp_systemf(const char *fmt, ...);

// The following function must be provided by the programmer.
extern int mp_user_rebuild(int argc, char **argv);
extern int mp_user_main(int argc, char **argv);

#ifdef MP_IMPLEMENTATION

#include <stdarg.h>


void mp_string_view_destroy(mp_string_view_t *sv) {
    if (sv->cstr) {
	free((void *)sv->cstr);
	sv->cstr = NULL;
    }
}

mp_string_view_t mp_string_view_from_cstr(const char *cstr) {
    return (mp_string_view_t) {
	strdup(cstr),
	strlen(cstr)
    };
}

mp_string_view_t mp_string_view_from_lstr(const char *cstr, size_t size) {
    return (mp_string_view_t) {
	strdup(cstr),
        size
    };
}

char *mp_cstr_from_string_view(mp_string_view_t sv) {
    return strndup(sv.cstr, sv.size);
}

mp_string_view_t mp_string_view_from_printf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    va_list args0;
    va_copy(args0, args);
    size_t sz = vsnprintf(NULL, 0, fmt, args0);
    const char *out = malloc(sz+1);
    vsnprintf((char *)out, sz+1, fmt, args);
    va_end(args);
    return (mp_string_view_t) {
	out,
	sz
    };
}

void mp_string_builder_destroy(mp_string_builder_t *sb) {
    if (sb->arr) {
	free(sb->arr);
	sb->arr = NULL;
    }
}

void mp_string_builder_clear(mp_string_builder_t *sb) {
    sb->count = 0;
}

void mp_string_builder_push(mp_string_builder_t *sb, char c) {
    mp_da_append(*sb, arr, c);
}

void mp_string_builder_printf(mp_string_builder_t *sb, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    va_list args0;
    va_copy(args0, args);
    size_t sz = vsnprintf(NULL, 0, fmt, args0);
    char *out = malloc(sz+1);
    vsnprintf(out, sz+1, fmt, args);
    va_end(args);
    char *it = out;
    while (*it) {
	mp_string_builder_push(sb, *it);
	++it;
    }
    free(out);
}

mp_string_view_t mp_read_all(const char *fname) {
    FILE *fptr = fopen(fname, "rb");
    fseek(fptr, 0, SEEK_END);
    size_t sz = ftell(fptr);
    fseek(fptr, 0, SEEK_SET);
    const char *buf = malloc(sz);
    fread((char *)buf, 1, sz, fptr);
    fclose(fptr);
    return (mp_string_view_t) {
	buf,
	sz
    };
}

void mp_write_all(const char *fname, mp_string_view_t sv) {
    FILE *fptr = fopen(fname, "wb");
    fwrite(sv.cstr, 1, sv.size, fptr);
    fclose(fptr);
}

void mp_write_all_sb(const char *fname, mp_string_builder_t sb) {
    FILE *fptr = fopen(fname, "wb");
    fwrite(sb.arr, 1, sb.count, fptr);
    fclose(fptr);
}

int mp_systemf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    va_list args0;
    va_copy(args0, args);
    size_t sz = vsnprintf(NULL, 0, fmt, args0);
    char *out = malloc(sz+1);
    vsnprintf(out, sz+1, fmt, args);
    va_end(args);
    printf("mp_systemf: running `%s`\n", out);
    int res = system(out);
    free(out);
    return res;
}

int main(int argc, char **argv) {
    bool need_to_build = false;
    bool need_to_run = true;
    if (argc == 2) {
	if (!strcmp(argv[1], "--mp")) {
	    fprintf(stderr, "Usage of metaprogram: %s --mp [help|rebuild]\n", argv[0]);
	    return 1;
	}
    }
    if (argc >= 3) {
	if (!strcmp(argv[1], "--mp")) {
	    if (!strcmp(argv[2], "help")) {
		fprintf(stderr, "Usage of metaprogram: %s --mp [help|rebuild]\n", argv[0]);
		return 0;
	    }
	    if (!strcmp(argv[2], "rebuild")) {
		need_to_build = true;
		need_to_run = false;
	    }
	}
    }
    
    if (need_to_build) {
	return mp_user_rebuild(argc, argv);
    }
    if (need_to_run) {
	return mp_user_main(argc, argv);
    }
}

#endif // MP_IMPLEMENTATION
