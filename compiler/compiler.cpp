#include <faivy.hpp>

namespace Compiler {
    void compile(std::vector<Faivy::ByteCodeInst> *prog, Parser::Ast ast, std::unordered_map<std::string, size_t> *procs, std::vector<uint8_t> *static_data) {
        switch (ast.kind) {
        case Parser::AK_NUM: {
            prog->push_back(Faivy::ByteCodeInst{.kind = Faivy::B_PUSH, .n0 = ast.n});
        } break;
        case Parser::AK_STR: {
            const char *s = ast.s.c_str();
            size_t i = static_data->size();
            while (*s) {
                static_data->push_back(*s);
                ++s;
            }
            static_data->push_back(0);
            prog->push_back(Faivy::ByteCodeInst{.kind = Faivy::B_STR, .n0 = i});
        } break;
        case Parser::AK_SUM: {
            compile(prog, ast.inner[0], procs, static_data);
            compile(prog, ast.inner[1], procs, static_data);
            prog->push_back(Faivy::ByteCodeInst{.kind = Faivy::B_ADD});
        } break;
        case Parser::AK_CALL: {
            for (size_t i = ast.inner.size(); i > 0; --i) {
                compile(prog, ast.inner[i-1], procs, static_data);
            }
            prog->push_back(Faivy::ByteCodeInst{.kind = Faivy::B_CALL, .s0 = ast.s, .n0 = ast.inner.size()});
        } break;
        case Parser::AK_SEQ: {
            for (size_t i = 0; i < ast.inner.size(); ++i) {
                compile(prog, ast.inner[i], procs, static_data);
            }
        } break;
        case Parser::AK_PROC: {
            (*procs)[ast.s] = prog->size();
            prog->push_back(Faivy::ByteCodeInst{.kind = Faivy::B_OPEN_PROC, .s0 = ast.s});
            
            for (size_t i = 0; i < ast.inner[1].inner.size(); ++i) {
                compile(prog, ast.inner[1].inner[i], procs, static_data);
            }
            prog->push_back(Faivy::ByteCodeInst{.kind = Faivy::B_PUSH, .n0 = 0});
            prog->push_back(Faivy::ByteCodeInst{.kind = Faivy::B_RET});
        } break;
        default: {
            fprintf(stderr, "TODO!\n");
            exit(1);
        }
        }
    }
}
