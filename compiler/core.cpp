#include <faivy.hpp>

namespace Faivy {
    size_t istack_pop64(Slice<uint8_t> *s, size_t *sp) {
        if (*sp < 8) {
            return 0;
        }
        *sp -= 8;
        return *(size_t*)&s->start[*sp];
    }
    void istack_push64(Slice<uint8_t> *s, size_t *sp, size_t v) {
        if (*sp > s->size-8) {
            return;
        }
        *(size_t*)&s->start[*sp] = v;
        *sp += 8;
    }
    std::string ssprintf(const char *fmt, ...) {
        va_list args;
        va_list args0;
        va_start(args, fmt);
        va_copy(args0, args);
        size_t s = vsnprintf(NULL, 0, fmt, args0);
        char *buf = (char *)malloc(s+1);
        vsnprintf(buf, s+1, fmt, args);
        std::string str(buf);
        free(buf);
        va_end(args);
        return str;
    }
    /*
    void interpret(Slice<ByteCodeInst> code, Slice<uint8_t> static_data) {
        size_t ip = 0;
        Slice<uint8_t> stack((uint8_t *)malloc(8192), 8192);
        size_t sp = 0;
        std::unordered_map<std::string, size_t> locals;
        while (ip < code.size) {
            ByteCodeInst *inst = &code.start[ip];
            switch (inst->kind) {
            case B_PUSH: {
                istack_push64(&stack, &sp, inst->n0);
            } break;
            case B_ADD: {
                size_t b = istack_pop64(&stack, &sp);
                size_t a = istack_pop64(&stack, &sp);
                istack_push64(&stack, &sp, b+a);
            } break;
            case B_STR: {
                istack_push64(&stack, &sp, (size_t)static_data.start+inst->n0);
            } break;
            case B_CALL: {
                if (inst->s0 == "print") {
                    printf("%s", (const char *)istack_pop64(&stack, &sp));
                }
                if (inst->s0 == "print_int") {
                    printf("%zu", istack_pop64(&stack, &sp));
                }
            } break;
            case B_ALLOCA: {
                if (sp < inst->n0) {
                    break;
                }
                sp -= inst->n0;
                locals[inst->s0] = (size_t)&stack.start[sp];
            } break;
            case B_SAVE_STACK: {
                locals[inst->s0] = sp;
            } break;
            case B_RES_STACK: {
                sp = locals[inst->s0];
            } break;
            }
            ++ip;
        }
    }
    */
    std::string compile(Slice<ByteCodeInst> code, Slice<uint8_t> static_data, std::unordered_map<std::string, size_t> *procs) {
        std::string output;
        output += "#include <stdint.h>\n";
        output += "#include <assert.h>\n";
        output += "#include <stdio.h>\n";
        output += "uint8_t stack[8192];\n";
        output += "size_t sp;\n";
        output += "size_t istack_pop64() {\n";
        output += "\tif (sp < 8) {\n";
        output += "\t\treturn 0;\n";
        output += "\t}\n";
        output += "\tsp -= 8;\n";
        output += "\treturn *(size_t*)&stack[sp];\n";
        output += "}\n";
        output += "void istack_push64(size_t v) {\n";
        output += "\tif (sp > 8192-8) {\n";
        output += "\t\treturn;\n";
        output += "\t}\n";
        output += "\t*(size_t*)&stack[sp] = v;\n";
        output += "\tsp += 8;\n";
        output += "}\n";
        output += "uint8_t static_data[] = {";
        for (size_t i = 0; i < static_data.size; ++i) {
            output += ssprintf("0x%02X", static_data.start[i]);
            if (i < static_data.size - 1) {
                output += ", ";
            }
        }
        output += "};\n";
        size_t cs = code.size;
        for (size_t i = 0; i < cs; ++i) {
            ByteCodeInst *inst = &code.start[i];
            switch (inst->kind) {
            case B_PUSH: {
                output += ssprintf("\tistack_push64(%zu);\n", inst->n0);
            } break;
            case B_ADD: {
                output += "\t{\n";
                output += "\t\tsize_t b = istack_pop64();\n";
                output += "\t\tsize_t a = istack_pop64();\n";
                output += "\t\tistack_push64(b+a);\n";
                output += "\t}\n";
            } break;
            case B_STR: {
                output += ssprintf("\tistack_push64((size_t)static_data+%zu);\n", inst->n0);
            } break;
            case B_CALL: {
                if (inst->s0 == "print") {
                    if (inst->n0 != 1) {
                        fprintf(stderr, "Error: print() function requires 1 arguments.");
                        exit(1);
                    }
                    output += "\tprintf(\"%s\", (const char *)istack_pop64());\n";
                }
                else if (inst->s0 == "print_int") {
                    if (inst->n0 != 1) {
                        fprintf(stderr, "Error: print_int() function requires 1 arguments.");
                        exit(1);
                    }
                    output += "\tprintf(\"%zu\", istack_pop64());\n";
                }
                else {
                    output += ssprintf("\t__gen_%s();\n", inst->s0.c_str());
                }
            } break;
            case B_ALLOCA: {
                output += ssprintf("\tif (sp < %zu) {\n", inst->n0);
                output += "\t\tbreak;\n";
                output += "\t}\n";
                output += ssprintf("\tsp -= %zu;\n", inst->n0);
                output += ssprintf("\tsize_t %s = (size_t)&stack[sp];\n", inst->s0.c_str());
            } break;
            case B_SAVE_STACK: {
                output += ssprintf("\tsize_t %s = sp;\n", inst->s0.c_str());
            } break;
            case B_RES_STACK: {
                output += ssprintf("\tsp = %s;\n", inst->s0.c_str());
            } break;
            case B_CPP: {
                output += ssprintf("\t%s\n", inst->s0.c_str());
            } break;
            case B_RET: {
                output += "\treturn istack_pop64();\n}\n";
            } break;
            case B_OPEN_PROC: {
                output += ssprintf("int __gen_%s() {\n", inst->s0.c_str());
            } break;
            default: {
                fprintf(stderr, "Unknown opcode: %d!\n", inst->kind);
                exit(1);
            } break;
            }
        }
        output += "int main() {\n";
        output += "\t__gen_main();\n";
        output += "}\n";
        return output;
    }
}
