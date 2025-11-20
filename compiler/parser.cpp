#include <faivy.hpp>


const static char *tkn[] = {
    "TK_ID",
    "TK_STR",
    "TK_NAT",
    "TK_PLUS",
    "TK_SEMICOLON",
    "TK_2COLON",
    "TK_COLON",
    "TK_LPAR",
    "TK_RPAR",
    "TK_LCB",
    "TK_RCB",
    "TK_COMMA"
};

const static char *akn[] = {
    "AK_ID",
    "AK_STR",
    "AK_NUM",
    "AK_SUM",
    "AK_CALL",
    "AK_PROC",
    "AK_SEQ",
    "AK_FIELD"
};

namespace Parser {
    std::string Token::to_string() {
        return Faivy::ssprintf("Token<%s>(\"%s\", %d:%d)", tkn[kind], s.c_str(), row, col);
    }
    std::string Ast::to_string() {
        switch (kind) {
        case AK_ID: case AK_STR:
            return Faivy::ssprintf("Ast<%s>(%s)", akn[kind], s.c_str());
        case AK_NUM:
            return Faivy::ssprintf("Ast<%s>(%d)", akn[kind], n);
        case AK_SUM:
            return Faivy::ssprintf("%s + %s", inner[0].to_string().c_str(), inner[1].to_string().c_str());
        default:
            return "Ast<TODO>()";
        }
        return "Ast<Invalid>()";
    }
    std::vector<Token> lexer(const char *code) {
        std::vector<Token> toks;
        size_t row = 1, col = 0;
        while (*code) {
            switch (*code) {
            case ' ': case '\r': case '\t': case '\v':
                ++col;
                ++code;
                break;
            case '\n':
                ++row;
                col = 0;
                ++code;
                break;
            case '0' ... '9': {
                std::string t;
                while (*code) {
                    if (*code < '0' || *code > '9') break;
                    ++col;
                    t += *code;
                    ++code;
                }
                toks.push_back(Token{TK_NAT, t, row, col});
            } break;
            case '"': {
                std::string t;
                ++col;
                ++code;
                while (*code) {
                    if (*code == '"') break;
                    if (*code == '\\') {
                        ++col;
                        ++code;
                        switch (*code) {
                        case 'n':
                            t += '\n';
                        break;
                        case 'r':
                            t += '\r';
                        break;
                        case '"':
                            t += '"';
                        break;
                        case '\\':
                            goto str_base_char;
                        default: t += *code; goto str_base_char;
                        }
                        ++col;
                        ++code;
                    }
                    else {
                    str_base_char:
                        t += *code;
                        ++col;
                        ++code;
                    }
                }
                if (*code == '"') {
                    ++col;
                    ++code;
                }
                toks.push_back(Token{TK_STR, t, row, col});
            } break;
            case 'a' ... 'z': case 'A' ... 'Z': case '_': {
                std::string t;
                while (*code) {
                    switch (*code) {
                    case 'a' ... 'z': case 'A' ... 'Z': case '_': case '0' ... '9':
                        t += *code;
                        ++col;
                        ++code;
                        break;
                    default: goto num_end;
                    }
                }
                num_end:
                toks.push_back(Token{TK_ID, t, row, col});
            } break;
            case '+': {
                toks.push_back(Token{TK_PLUS, "", row, col});
                ++col;
                ++code;
            } break;
            case ';': {
                toks.push_back(Token{TK_SEMICOLON, "", row, col});
                ++col;
                ++code;
            } break;
            case ',': {
                toks.push_back(Token{TK_COMMA, "", row, col});
                ++col;
                ++code;
            } break;
            case '(': {
                toks.push_back(Token{TK_LPAR, "", row, col});
                ++col;
                ++code;
            } break;
            case ')': {
                toks.push_back(Token{TK_RPAR, "", row, col});
                ++col;
                ++code;
            } break;
            case '{': {
                toks.push_back(Token{TK_LCB, "", row, col});
                ++col;
                ++code;
            } break;
            case '}': {
                toks.push_back(Token{TK_RCB, "", row, col});
                ++col;
                ++code;
            } break;
            case ':': {
                ++col;
                ++code;
                if (*code == ':') {
                    ++col;
                    ++code;
                    toks.push_back(Token{TK_2COLON, "", row, col});
                }
                else toks.push_back(Token{TK_COLON, "", row, col});
            } break;
            default:
                fprintf(stderr, "Unexpected `%c`\n", *code);
                exit(1);
            }
        }
        return toks;
    }
    PR parse_primary(Faivy::Slice<Token> toks) {
        if (toks.size < 1) {
            return (PR) {.is_the=false};
        }
        if (toks.start[0].kind == TK_NAT) {
            auto t = toks.start[0];
            return (PR) {toks.skip(), (Ast) {.kind = AK_NUM, .n = (size_t)std::atoi(t.s.c_str())}, true};
        }
        if (toks.start[0].kind == TK_STR) {
            auto t = toks.start[0];
            return (PR) {toks.skip(), (Ast) {.kind = AK_STR, .s = t.s}, true};
        }
        if (toks.size >= 2 && toks.start[0].kind == TK_ID && toks.start[1].kind == TK_LPAR) {
            auto id = toks.start[0];
            toks = toks.skip(2);
            PR pr{{}, {.kind = AK_CALL, .inner = {} , .s=id.s}, true};
            while (toks.size >= 2) {
                if (toks.start[0].kind == TK_RPAR) {
                    toks = skip(toks, TK_RPAR);
                    break;
                }
                auto arg = parse_rexpr(toks);
                pr.ast.inner.push_back(arg.ast);
                toks = arg.rest;
                if (toks.start[0].kind == TK_COMMA) {
                    toks = skip(toks, TK_COMMA);
                    break;
                }
                else if (toks.start[0].kind == TK_RPAR) {
                    toks = skip(toks, TK_RPAR);
                    break;
                }
                else {
                    fprintf(stderr, "Excepted ')' or ',' but got %s", toks.start[0].to_string().c_str());
                    exit(1);
                }
            }
            pr.rest = toks;
            return pr;
        }
        return (PR) {.is_the=false};
    }
    Faivy::Slice<Token> skip(Faivy::Slice<Token> toks, TokenKind tk) {
        if (toks.size < 1 || toks.start[0].kind != tk) {
            fprintf(stderr, "Excepted %s but got %s", tkn[tk], toks.start[0].to_string().c_str());
        }
        return toks.skip();
    }
    PR parse_rexpr(Faivy::Slice<Token> toks) {
        PR pr_prim = parse_primary(toks);
        if (!pr_prim.is_the) return pr_prim;
        toks = pr_prim.rest;
        while (toks.size >= 2 && toks.start[0].kind == TK_PLUS) {
            toks = toks.skip();
            PR rhs = parse_primary(toks);
            if (!rhs.is_the) return rhs;
            toks = rhs.rest;
            pr_prim = PR{rhs.rest, Ast{AK_SUM, {pr_prim.ast, rhs.ast}}, true};
        }
        return pr_prim;
    }
    PR parse_proc(Faivy::Slice<Token> toks) {
        if (toks.size < 5 /* id₀ ::₁ (₂ )₃ {₄ }₅ */ || toks.start[0].kind != TK_ID || toks.start[1].kind != TK_2COLON || toks.start[2].kind != TK_LPAR) return PR{.is_the=false};
        Ast args{AK_SEQ, {}};
        std::string name = toks.start[0].s;
        toks = toks.skip(3);
        for (;;) {
            PR pr_rexpr = parse_rexpr(toks);
            if (!pr_rexpr.is_the) break;
            toks = pr_rexpr.rest;
            std::string type = toks.start[0].s;
            toks = toks.skip();
            args.inner.push_back({AK_FIELD, {pr_rexpr.ast, {AK_ID, {}, type}}});
        }
        toks = skip(toks, TK_RPAR);
        PR pr_stmt = parse_stmt(toks);
        return PR{pr_stmt.rest, {AK_PROC, {args, pr_stmt.ast}, name}, true};
    }
    PR parse_stmt(Faivy::Slice<Token> toks) {
        if (toks.size >= 2 && toks.start[0].kind == TK_LCB) {
            toks = toks.skip();
            PR pr = parse_block(toks);
            pr.rest = pr.rest.skip();
            return pr;
        }
        PR pr_proc = parse_proc(toks);
        if (pr_proc.is_the) return pr_proc;
        PR pr_rexpr = parse_rexpr(toks);
        if (pr_rexpr.is_the) {
            pr_rexpr.rest = skip(pr_rexpr.rest, TK_SEMICOLON);
            return pr_rexpr;
        }
        return PR{.is_the=false};
    }
    PR parse_block(Faivy::Slice<Token> toks) {
        Ast ast{AK_SEQ, {}};
        for (;;) {
            PR pr_stmt = parse_stmt(toks);
            if (!pr_stmt.is_the) break;
            ast.inner.push_back(pr_stmt.ast);
            toks = pr_stmt.rest;
        }
        return PR{toks, ast, true};
    }
}
