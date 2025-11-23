#include <faivy.hpp>

/*
  0 - temp
  1,2,3 - calculations

  i need only 4 (one temp, zero arguments(since i can use stack for that), 3 for calculations)
*/

namespace Compiler {
    void compile(Parser::Ast ast, CompileContext cc) {
#ifdef VERY_VEBOSE
        std::cout << "[Current Function] `" << cfunc << "` [Inst] `" << ast.to_string() << "` \n";
#endif // VERY_VEBOSE
        switch (ast.kind) {
        case Parser::AK_NUM: {
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_MOVI, ast.row, ast.col, {0, ast.n}});
        } break;
        case Parser::AK_STR: {
            const char *s = ast.s.c_str();
            size_t i = cc.static_data->size();
            while (*s) {
                cc.static_data->push_back(*s);
                ++s;
            }
            cc.static_data->push_back(0);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSHDSO, ast.row, ast.col, {i}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {0}});
        } break;
        case Parser::AK_SUM: {
            compile(ast.inner[0], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            compile(ast.inner[1], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {2}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {1}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_ADD, ast.row, ast.col, {0, 2, 1}});
        } break;
        case Parser::AK_SUB: {
            compile(ast.inner[0], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            compile(ast.inner[1], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {2}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {1}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_SUB, ast.row, ast.col, {0, 2, 1}});
        } break;
        case Parser::AK_MUL: {
            compile(ast.inner[0], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            compile(ast.inner[1], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {2}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {1}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_MUL, ast.row, ast.col, {0, 2, 1}});
        } break;
        case Parser::AK_DIV: {
            compile(ast.inner[0], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            compile(ast.inner[1], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {2}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {1}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_DIV, ast.row, ast.col, {0, 2, 1}});
        } break;
        case Parser::AK_MODDIV: {
            compile(ast.inner[0], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            compile(ast.inner[1], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {2}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_POP, ast.row, ast.col, {1}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_MOD, ast.row, ast.col, {0, 2, 1}});
        } break;
        case Parser::AK_CALL: {
            for (size_t _i = ast.inner.size(); _i > 0; --_i) {
                size_t i = _i - 1;
                compile(ast.inner[i], cc);
                cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PUSH, ast.row, ast.col, {0}});
            }
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_CALL, ast.row, ast.col, {ast.inner.size()}, ast.s});
        } break;
        case Parser::AK_CPP: {
            cc.prog->push_back({Faivy::B_CPP, ast.row, ast.col, {}, ast.s});
        } break;
        case Parser::AK_RUN: {
            std::vector<Faivy::ByteCodeInst> sprog = *cc.prog; // TODO: optimize
            compile(ast.inner[0], cc);
            size_t r = Faivy::interpret(Faivy::Slice<Faivy::ByteCodeInst>(cc.prog->data(), cc.prog->size()), Faivy::Slice<uint8_t>(cc.static_data->data(), cc.static_data->size()), cc.procs, sprog.size());
            *cc.prog = sprog;
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_MOVI, ast.row, ast.col, {0, r}});
        } break;
        case Parser::AK_SEQ: {
            for (size_t i = 0; i < ast.inner.size(); ++i) {
                compile(ast.inner[i], cc);
            }
        } break;
        case Parser::AK_RID: {
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_LOAD_SYM, ast.row, ast.col, {0}, ast.s});
        } break;
        case Parser::AK_IF: {
            compile(ast.inner[0], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_MOVI, ast.row, ast.col, {1, 0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_CHE, ast.row, ast.col, {0, 0, 1}});
            size_t patch_id = cc.prog->size();
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_JCTI, ast.row, ast.col, {0, 0xDEADBEEF}});
            compile(ast.inner[1], cc);
            if (ast.inner.size() != 3) {
                (*cc.prog)[patch_id].xs[1] = cc.prog->size()-patch_id;
            }
            else {
                size_t patch1_id = cc.prog->size();
                cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_JUCI, ast.row, ast.col, {0xDEADBEEF}});
                (*cc.prog)[patch_id].xs[1] = cc.prog->size()-patch_id;
                compile(ast.inner[2], cc);
                (*cc.prog)[patch1_id].xs[0] = cc.prog->size()-patch1_id;
            }
        } break;
        case Parser::AK_WHILE: {
            size_t start = cc.prog->size();
            compile(ast.inner[0], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_MOVI, ast.row, ast.col, {1, 0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_CHE, ast.row, ast.col, {0, 0, 1}});
            size_t patch_id = cc.prog->size();
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_JCTI, ast.row, ast.col, {0, 0xDEADBEEF}});
            compile(ast.inner[1], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_JUCI, ast.row, ast.col, {start-cc.prog->size()}});
            (*cc.prog)[patch_id].xs[1] = cc.prog->size()-patch_id;
        } break;
        case Parser::AK_PROC: {
            (*cc.procs)[ast.s] = cc.prog->size();

            std::vector<std::string> ses;

            for (size_t i = 0; i < ast.inner[0].inner.size(); ++i) {
                ses.push_back(ast.inner[0].inner[i].inner[0].s);
                ses.push_back(ast.inner[0].inner[i].inner[1].s);
            }
            
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PROC_START, ast.row, ast.col, {}, ast.s, ses});
            
            for (size_t i = 0; i < ast.inner[1].inner.size(); ++i) {
                auto ncc = cc;
                ncc.cfunc = ast.s;
                compile(ast.inner[1].inner[i], ncc);
            }
            
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_MOVI, ast.row, ast.col, {0, 0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_RET, ast.row, ast.col, {0}});
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_PROC_END, ast.row, ast.col});
        } break;
        case Parser::AK_RETURN: {
            compile(ast.inner[0], cc);
            cc.prog->push_back(Faivy::ByteCodeInst{Faivy::B_RET, ast.row, ast.col, {0}});
        } break;
        default: {
            fprintf(stderr, "TODO: %s!\n", Parser::akn[ast.kind]);
            exit(1);
        }
        }
    }
}
