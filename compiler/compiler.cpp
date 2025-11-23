#include <faivy.hpp>

/*
  0-511 - args
  512-512 - temp
  513-8000 - calculations
  8001-8192 - function-private data(callee-saved)

   pushi 4
   pushi 5
   pop 514
   pop 513
   add 512 513 514
   push 512
   pushi 512 1
   pop 514
   pop 513
   add 512 513 514
*/

namespace Compiler {
    void compile(std::vector<Faivy::ByteCodeInst> *prog, Parser::Ast ast, std::unordered_map<std::string, size_t> *procs, std::vector<uint8_t> *static_data, std::string cfunc) {
#ifdef VERY_VEBOSE
        std::cout << "[Current Function] `" << cfunc << "` [Inst] `" << ast.to_string() << "` \n";
#endif // VERY_VEBOSE
        switch (ast.kind) {
        case Parser::AK_NUM: {
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSHI, ast.row, ast.col, {ast.n}});
        } break;
        case Parser::AK_STR: {
            const char *s = ast.s.c_str();
            size_t i = static_data->size();
            while (*s) {
                static_data->push_back(*s);
                ++s;
            }
            static_data->push_back(0);
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSHDSO, ast.row, ast.col, {i}});
        } break;
        case Parser::AK_SUM: {
            compile(prog, ast.inner[0], procs, static_data, cfunc);
            compile(prog, ast.inner[1], procs, static_data, cfunc);
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {514}});
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {513}});
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_ADD, ast.row, ast.col, {512, 514, 513}});
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {512}});
        } break;
        case Parser::AK_CALL: {
            std::vector<size_t> args;
            for (size_t _i = ast.inner.size(); _i > 0; --_i) {
                size_t i = _i - 1;
                compile(prog, ast.inner[i], procs, static_data, cfunc);
            }
            for (size_t i = 0; i < ast.inner.size(); ++i) {
                prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {i}});
                args.push_back(i);
            }
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_CALL, ast.row, ast.col, args, ast.s});
        } break;
        case Parser::AK_CPP: {
            if (cfunc != "<top>")
                prog->push_back({Faivy::B_CPP, ast.row, ast.col, {}, Faivy::ssprintf("push(%s);\n", ast.s.c_str())});
            else
                prog->push_back({Faivy::B_CPP, ast.row, ast.col, {}, ast.s});
        } break;
        case Parser::AK_RUN: {
            std::vector<Faivy::ByteCodeInst> sprog = *prog; // VERY FAST CODE!
            compile(prog, ast.inner[0], procs, static_data, cfunc);
            Faivy::interpret(Faivy::Slice<Faivy::ByteCodeInst>(prog->data(), prog->size()), Faivy::Slice<uint8_t>(static_data->data(), static_data->size()), procs, sprog.size());
            *prog = sprog;
        } break;
        case Parser::AK_SEQ: {
            for (size_t i = 0; i < ast.inner.size(); ++i) {
                compile(prog, ast.inner[i], procs, static_data, cfunc);
            }
        } break;
        case Parser::AK_PROC: {
            (*procs)[ast.s] = prog->size();
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_PROC_START, ast.row, ast.col, {ast.inner[0].inner.size()}, ast.s});
            
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {8001}});
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_GSP, ast.row, ast.col, {8001}});
            
            for (size_t i = 0; i < ast.inner[0].inner.size(); ++i) {
                prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {i}});
            }
            
            for (size_t i = 0; i < ast.inner[1].inner.size(); ++i) {
                compile(prog, ast.inner[1].inner[i], procs, static_data, ast.s);
            }
            
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_SSP, ast.row, ast.col, {8001}});
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {8001}});
            
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_RET, ast.row, ast.col});
            prog->push_back(Faivy::ByteCodeInst{Faivy::B_PROC_END, ast.row, ast.col});
        } break;
        default: {
            fprintf(stderr, "TODO!\n");
            exit(1);
        }
        }
    }
}
