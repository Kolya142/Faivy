#include <faivy.hpp>
#include <assert.h>

namespace Faivy {
    const char *bc_names[] = {
        "MOVI",
        "CALL",
        "CALLR",
        "CALLA",
        "CALLRA",
        "JUC",
        "JCT",
        "JUCI",
        "JCTI",
        "JUCIA",
        "JCTIA",
        "JUCA",
        "JCTA",        
        "RET",        
        "CHG",
        "CHNG",
        "CHL",
        "CHNL",
        "CHE",
        "CHNE",        
        "PUSH",
        "POP",
        "PUSHx",
        "POPx",
        "PUSHI",
        "CPP",
        "PUSHDS",
        "PUSHDSO",
        "ADD",
        "SUB",
        "MUL",
        "DIV",
        "MOD",
        "XOR",
        "AND",
        "OR",
        "NOT",
        "PROC_START",
        "PROC_END",
        "DROP",
        "GSP",
        "SSP",
        "GET_SYM_PTR",
        "PEEK64",
        "POKE64",
    };
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
    void interpret(Slice<ByteCodeInst> code, Slice<uint8_t> static_data, std::unordered_map<std::string, size_t> *procs, size_t ip) {
        uint64_t *stack =(uint64_t *)malloc(8192*8);
        uint64_t *regs = (uint64_t *)malloc(8192*8);
        size_t sp = 0;
        #define push(a) {stack[sp] = a;++sp;}
        #define pop(a) (--sp,stack[sp])
        while (ip < code.size) {
            ByteCodeInst *inst = &code.start[ip];
            switch (inst->kind) {
            case B_MOVI: {
                regs[inst->xs[0]] = inst->xs[1];
            } break;
            case B_CALL: {
                if (inst->s == "print") {
                    if (inst->xs.size() != 1) {
                        fprintf(stderr, "Error: print() function requires 1 arguments.");
                        exit(1);
                    }
                    printf("%s", (const char *)regs[inst->xs[0]]);
                }
                else if (inst->s == "print_int") {
                    if (inst->xs.size() != 1) {
                        fprintf(stderr, "Error: print_int() function requires 1 arguments.");
                        exit(1);
                    }
                    printf("%zu", regs[inst->xs[0]]);
                }
                else if (procs->count(inst->s)) {
                    interpret(code, static_data, procs, (*procs)[inst->s]);
                }
                else {
                    assert(0 && "TODO: external function calls in interpret mode");
                }
            } break;
            case B_CALLR: {
                assert(0 && "TODO: CALL(R)");
            } break;
            case B_CALLA: {
                assert(0 && "TODO: CALL(ABS)");
            } break;
            case B_CALLRA: {
                assert(0 && "TODO: CALL(R,ABS)");
            } break;
            case B_RET: {
                return;
            } break;
            case B_PUSHDSO: {
                push((size_t)static_data.start+inst->xs[0]);
            } break;
            case B_PUSH: {
                push(regs[inst->xs[0]]);
            } break;
            case B_PUSHI: {
                push(inst->xs[0]);
            } break;
            case B_ADD: {
                regs[inst->xs[0]] = regs[inst->xs[1]] + regs[inst->xs[2]];
            } break;
            case B_POP: {
                regs[inst->xs[0]] = pop();
            } break;
            case B_PROC_START: {
            } break;
            case B_PROC_END: {
                return;
            } break;
            case B_CPP: {
                fprintf(stderr, "You cannot use #cpp in comptime mode\n");
                exit(1);
            } break;
            default: {
                fprintf(stderr, "Unknown opcode: %s!\n", bc_names[inst->kind]);
                exit(1);
            } break;
            }
            ++ip;
        }
        free((void *)stack);
        free((void *)regs);
    }
    std::string compile(Slice<ByteCodeInst> code, Slice<uint8_t> static_data, std::unordered_map<std::string, size_t> *procs) {
        std::string output;
        output += "#include <stdio.h>\n";
        output += "#include <stdlib.h>\n";
        output += "#include <stdint.h>\n";
        output += "#include <assert.h>\n";
        output += "#if !defined(__x86_64__) && !defined(_M_X64)\n";
        output += "#error \"Allowed only x86_64. Sorry.\"\n";
        output += "#endif\n";
        output += "uint64_t regs[8192];\n";
        output += "uint64_t stack[8192];\n";
        output += "size_t sp;\n";
        output += "#define push(w) do {stack[sp++] = (uint64_t)(w);} while(0)\n";
        output += "#define pop (stack[--sp])\n";
        output += "uint8_t static_data[] = {";
        for (size_t i = 0; i < static_data.size; ++i) {
            output += ssprintf("0x%02X", static_data.start[i]);
            if (i < static_data.size - 1) {
                output += ", ";
            }
        }
        output += "};\n";
        size_t cs = code.size;
        size_t prev_row = -1;
        for (size_t i = 0; i < cs; ++i) {
            ByteCodeInst *inst = &code.start[i];
            if (prev_row != inst->row) {
                // output += ssprintf(".line %zu \"first.faivy\"\n", inst->row);
                prev_row = inst->row;
            }
            switch (inst->kind) {
            case B_MOVI: {
                output += ssprintf("\tregs[%zu] = %zu;\n", inst->xs[0], inst->xs[1]);
            } break;
            case B_CALL: {
                if (inst->s == "print") {
                    if (inst->xs.size() != 1) {
                        fprintf(stderr, "Error: print() function requires 1 arguments.");
                        exit(1);
                    }
                    output += ssprintf("\tprintf(\"%%s\", (const char *)regs[%zu]);\n", inst->xs[0]);
                }
                else if (inst->s == "print_int") {
                    if (inst->xs.size() != 1) {
                        fprintf(stderr, "Error: print_int() function requires 1 arguments.");
                        exit(1);
                    }
                    output += ssprintf("\tprintf(\"%%zu\", regs[%zu]);\n", inst->xs[0]);
                }
                else {
                    output += ssprintf("\t__gen_%s(", inst->s.c_str());
                    for (size_t i = 0; i < inst->xs.size(); ++i) {
                        output += ssprintf("regs[%zu]", i);
                        if (i != inst->xs.size() - 1) {
                            output += ", ";
                        }
                    }
                    output += ");\n";
                }
            } break;
            case B_CALLR: {
                assert(0 && "TODO: CALL(R)");
            } break;
            case B_CALLA: {
                assert(0 && "TODO: CALL(ABS)");
            } break;
            case B_CALLRA: {
                assert(0 && "TODO: CALL(R,ABS)");
            } break;
            case B_RET: {
                output += "\treturn 0;\n";
            } break;
            case B_PUSHDSO: {
                output += ssprintf("\tpush(static_data+%zu);\n", inst->xs[0]);
            } break;
            case B_PUSH: {
                output += ssprintf("\tpush(regs[%zu]);\n", inst->xs[0]);
            } break;
            case B_PUSHI: {
                output += ssprintf("\tpush(%zu);\n", inst->xs[0]);
            } break;
            case B_GSP: {
                output += ssprintf("\tregs[%zu] = sp;\n", inst->xs[0]);
            } break;
            case B_SSP: {
                output += ssprintf("\tsp = regs[%zu];\n", inst->xs[0]);
            } break;
            case B_ADD: {
                output += ssprintf("\tregs[%zu] = regs[%zu]+regs[%zu];\n", inst->xs[0], inst->xs[1], inst->xs[2]);
            } break;
            case B_POP: {
                output += ssprintf("\tregs[%zu] = pop;\n", inst->xs[0]);
            } break;
            case B_PROC_START: {
                output += ssprintf("int __gen_%s() {\n", inst->s.c_str());
            } break;
            case B_PROC_END: {
                output += "}\n";
            } break;
            case B_CPP: {
                output += ssprintf("\t%s\n", inst->s.c_str());
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
