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
        "LOAD_SYM",
        "SAVE_SYM",
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
    size_t interpret(Slice<ByteCodeInst> code, Slice<uint8_t> static_data, std::unordered_map<std::string, size_t> *procs, size_t ip) {
        uint64_t *stack =(uint64_t *)malloc(8192*8);
        uint64_t *regs = (uint64_t *)malloc(4*8);
        size_t sp = 0;
        #define push(a) {stack[sp++] = a;}
        #define pop stack[--sp]
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
                    printf("%s", (const char *)pop);
                }
                else if (inst->s == "print_int") {
                    if (inst->xs.size() != 1) {
                        fprintf(stderr, "Error: print_int() function requires 1 arguments.");
                        exit(1);
                    }
                    printf("%zu", pop);
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
                return 0;
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
            case B_SUB: {
                regs[inst->xs[0]] = regs[inst->xs[1]] - regs[inst->xs[2]];
            } break;
            case B_MUL: {
                regs[inst->xs[0]] = regs[inst->xs[1]] * regs[inst->xs[2]];
            } break;
            case B_DIV: {
                regs[inst->xs[0]] = regs[inst->xs[1]] / regs[inst->xs[2]];
            } break;
            case B_MOD: {
                regs[inst->xs[0]] = regs[inst->xs[1]] % regs[inst->xs[2]];
            } break;
            case B_POP: {
                regs[inst->xs[0]] = pop;
            } break;
            case B_PROC_START: {
            } break;
            case B_PROC_END: {
                fprintf(stderr, "You cannot currently exit from a function in compile time execution\n");
                exit(1);
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
        size_t r = regs[0];
        free((void *)stack);
        free((void *)regs);
        return r;
    }
    std::string compile(Slice<ByteCodeInst> code, Slice<uint8_t> static_data, std::unordered_map<std::string, size_t> *procs) {
        std::string output;
        output += "#include <stdio.h>\n";
        output += "#include <stdlib.h>\n";
        output += "#include <stdint.h>\n";
        output += "#include <assert.h>\n";
        output += "typedef int8_t s8;\n";
        output += "typedef int16_t s16;\n";
        output += "typedef int32_t s32;\n";
        output += "typedef int64_t s64;\n";
        output += "typedef uint8_t u8;\n";
        output += "typedef uint16_t u16;\n";
        output += "typedef uint32_t u32;\n";
        output += "typedef uint64_t u64;\n";
        output += "typedef _Bool bool;\n";
        output += "#define true ((bool)1);\n";
        output += "#define false ((bool)0);\n";
        output += "#if !defined(__x86_64__) && !defined(_M_X64)\n";
        output += "#error \"Allowed only x86_64. Sorry.\"\n";
        output += "#endif\n";
        output += "uint64_t regs[4];\n";
        output += "uint64_t stack[8192];\n";
        output += "size_t sp;\n";
        output += "#define push(w) do {stack[sp++] = (uint64_t)(w);} while(0)\n";
        output += "#define mtemp(x) {regs[0] = (uint64_t)(x);}\n";
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
        bool in_proc = false;
        for (size_t i = 0; i < cs; ++i) {
            if (in_proc)
                output += ssprintf("\tmarkov%zu:;\n", i);
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
                    output += ssprintf("\tmtemp(printf(\"%%s\", (const char *)regs[%zu]));\n", inst->xs[0]);
                }
                else if (inst->s == "print_int") {
                    if (inst->xs.size() != 1) {
                        fprintf(stderr, "Error: print_int() function requires 1 arguments.");
                        exit(1);
                    }
                    output += ssprintf("\tmtemp(printf(\"%%zu\", regs[%zu]));\n", inst->xs[0]);
                }
                else {
                    output += "\t{\n";
                    if (inst->xs[0]) {
                        output += "\t\tsize_t ";
                        for (size_t i = 0; i < inst->xs[0]; ++i) {
                            output += ssprintf("a%zu = pop", i);
                            if (i != inst->xs[0] - 1) {
                                output += ", ";
                            }
                        }
                        output += ";\n";
                    }
                    output += ssprintf("\t\tmtemp(%s(", inst->s.c_str());
                    for (size_t i = 0; i < inst->xs[0]; ++i) {
                        output += ssprintf("a%zu", i);
                        if (i != inst->xs[0] - 1) {
                            output += ", ";
                        }
                    }
                    output += "));\n";
                    output += "\t}\n";
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
                output += ssprintf("\treturn regs[%zu];\n", inst->xs[0]);
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
            case B_SUB: {
                output += ssprintf("\tregs[%zu] = regs[%zu]-regs[%zu];\n", inst->xs[0], inst->xs[1], inst->xs[2]);
            } break;
            case B_MUL: {
                output += ssprintf("\tregs[%zu] = regs[%zu]*regs[%zu];\n", inst->xs[0], inst->xs[1], inst->xs[2]);
            } break;
            case B_DIV: {
                output += ssprintf("\tregs[%zu] = regs[%zu]/regs[%zu];\n", inst->xs[0], inst->xs[1], inst->xs[2]);
            } break;
            case B_MOD: {
                output += ssprintf("\tregs[%zu] = regs[%zu]%regs[%zu];\n", inst->xs[0], inst->xs[1], inst->xs[2]);
            } break;
            case B_POP: {
                output += ssprintf("\tregs[%zu] = pop;\n", inst->xs[0]);
            } break;
            case B_PROC_START: {
                if (inst->s == "main") {
                    output += "int __generated_main(";
                }
                else {
                    output += ssprintf("size_t %s(", inst->s.c_str());
                }
                if (inst->ses.empty()) {
                    output += "void";
                }
                else
                    for (size_t i = 0; i < inst->ses.size(); i += 2) {
                        output += ssprintf("%s %s", inst->ses[i+1].c_str(), inst->ses[i+0].c_str());
                        if (i != inst->ses.size() - 2) output += ", ";
                    }
                output += ") {\n";
                in_proc = true;
            } break;
            case B_PROC_END: {
                output += "}\n";
                in_proc = false;
            } break;
            case B_LOAD_SYM: {
                output += ssprintf("\tregs[%zu] = %s;\n", inst->xs[0], inst->s.c_str());
            } break;
            case B_CHE: {
                output += ssprintf("\tregs[%zu] = regs[%zu] == regs[%zu];\n", inst->xs[0], inst->xs[1], inst->xs[2]);
            } break;
            case B_JCTI: {
                output += ssprintf("\tif (regs[%zu]) goto markov%zu;\n", inst->xs[0], i+inst->xs[1]);
            } break;
            case B_JUCI: {
                output += ssprintf("\tgoto markov%zu;\n", i+inst->xs[0]);
            } break;
            case B_SAVE_SYM: {
                output += ssprintf("\t%s = regs[%zu];\n", inst->s.c_str(), inst->xs[0]);
            } break;
            case B_CPP: {
                output += ssprintf("\t%s\n", inst->s.c_str());
            } break;
            default: {
                fprintf(stderr, "Unknown opcode: %s!\n", bc_names[inst->kind]);
                exit(1);
            } break;
            }
        }
        output += "int main() {\n";
        output += "\t__generated_main();\n";
        output += "}\n";
        return output;
    }
}
