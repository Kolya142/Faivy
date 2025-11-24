#include <faivy.hpp>
#include <assert.h>

std::string read_all(const char *fn);

namespace Parser {
    const char *tkn[] = {
        "TK_ID",
        "TK_STR",
        "TK_NAT",
        "TK_PLUS",
        "TK_MINUS",
        "TK_ASTERISK",
        "TK_RSLASH",
        "TK_PERCENT",
        "TK_SEMICOLON",
        "TK_2COLON",
        "TK_COLON",
        "TK_LPAR",
        "TK_RPAR",
        "TK_LCB",
        "TK_RCB",
        "TK_COMMA",
        "TK_HASH",
        "TK_EQ",
    };

    const char *akn[] = {
        "AK_RID",
        "AK_LID",
        "AK_STR",
        "AK_NUM",
        "AK_SUM",
        "AK_SUB",
        "AK_MUL",
        "AK_DIV",
        "AK_MODDIV",
        "AK_CPP",
        "AK_CALL",
        "AK_PROC",
        "AK_SEQ",
        "AK_FIELD",
        "AK_BC",
        "AK_RUN",
        "AK_IF",
        "AK_WHILE",
        "AK_RETURN"
    };
    
    std::string Token::to_string() {
        return Faivy::ssprintf("Token<%s>(\"%s\", %d:%d)", tkn[kind], s.c_str(), row, col);
    }
    std::string Ast::to_string() {
        switch (kind) {
        case AK_RID: case AK_STR:
            return Faivy::ssprintf("Ast<%s>(%s)", akn[kind], s.c_str());
        case AK_NUM:
            return Faivy::ssprintf("Ast<%s>(%d)", akn[kind], n);
        case AK_SUM:
            return Faivy::ssprintf("%s + %s", inner[0].to_string().c_str(), inner[1].to_string().c_str());
        default:
            return Faivy::ssprintf("Ast<%s>(\?\?\?)", akn[kind]);
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
            case '=': {
                toks.push_back(Token{TK_EQ, "", row, col});
                ++col;
                ++code;
            } break;
            case '+': {
                toks.push_back(Token{TK_PLUS, "", row, col});
                ++col;
                ++code;
            } break;
            case '-': {
                toks.push_back(Token{TK_MINUS, "", row, col});
                ++col;
                ++code;
            } break;
            case '*': {
                toks.push_back(Token{TK_ASTERISK, "", row, col});
                ++col;
                ++code;
            } break;
            case '/': {
                if (code[1] == '/') {
                    while (*code != '\n') ++code;
                    ++row;
                    col = 0;
                    break;
                }
                toks.push_back(Token{TK_RSLASH, "", row, col});
                ++col;
                ++code;
            } break;
            case '%': {
                toks.push_back(Token{TK_PERCENT, "", row, col});
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
            case '#': {
                toks.push_back(Token{TK_HASH, "", row, col});
                ++col;
                ++code;
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
            return (PR) {toks.skip(), (Ast) {.kind = AK_NUM, .row=toks.start[0].row, .col=toks.start[0].col, .n = (size_t)std::atoi(t.s.c_str())}, true};
        }
        if (toks.start[0].kind == TK_STR) {
            auto t = toks.start[0];
            return (PR) {toks.skip(), (Ast) {.kind = AK_STR, .row=toks.start[0].row, .col=toks.start[0].col, .s = t.s}, true};
        }
        if (toks.size >= 2 && toks.start[0].kind == TK_HASH && toks.start[1].kind == TK_ID) {
            if (toks.start[1].s == "cpp") {
                if (toks.size < 3 || toks.start[2].kind != TK_STR)
                    return {.is_the=false};
                return {toks.skip(3), {.kind = AK_CPP, /*I think pointing to '#' is more intuitive for user*/.row=toks.start[0].row, .col=toks.start[0].col, .s = toks.start[2].s}, true};
            }
            if (toks.start[1].s == "run") {
                if (toks.size < 3)
                    return {.is_the=false};
                toks = toks.skip(2);
                PR pr = parse_rexpr(toks);
                if (!pr.is_the) return pr;
                return {pr.rest, {AK_RUN, pr.ast.row, pr.ast.col, {pr.ast}}, true};
            }
            return {.is_the=false};
        }
        if (toks.size >= 2 && toks.start[0].kind == TK_ID && toks.start[1].kind == TK_LPAR) {
            auto id = toks.start[0];
            toks = toks.skip(2);
            PR pr{{}, {.kind = AK_CALL, .row=toks.start[0].row, .col=toks.start[0].col, .inner = {}, .s=id.s}, true};
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
        if (toks.start[0].kind == TK_ID) {
            auto t = toks.start[0];
            return (PR) {toks.skip(), (Ast) {.kind = AK_RID, .row=toks.start[0].row, .col=toks.start[0].col, .s = t.s}, true};
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
        while (toks.size >= 2 && toks.start[0].kind >= TK_PLUS && toks.start[0].kind <= TK_PERCENT) { // TODO: upd it
            auto op = toks.start[0].kind;
            toks = toks.skip();
            PR rhs = parse_primary(toks);
            if (!rhs.is_the) return rhs;
            toks = rhs.rest;
            switch (op) {
            case TK_PLUS:
                pr_prim = PR{rhs.rest, Ast{AK_SUM, pr_prim.ast.row, pr_prim.ast.col, {pr_prim.ast, rhs.ast}}, true};
                break;
            case TK_MINUS:
                pr_prim = PR{rhs.rest, Ast{AK_SUB, pr_prim.ast.row, pr_prim.ast.col, {pr_prim.ast, rhs.ast}}, true};
                break;
            case TK_ASTERISK:
                pr_prim = PR{rhs.rest, Ast{AK_MUL, pr_prim.ast.row, pr_prim.ast.col, {pr_prim.ast, rhs.ast}}, true};
                break;
            case TK_RSLASH:
                pr_prim = PR{rhs.rest, Ast{AK_DIV, pr_prim.ast.row, pr_prim.ast.col, {pr_prim.ast, rhs.ast}}, true};
                break;
            case TK_PERCENT:
                pr_prim = PR{rhs.rest, Ast{AK_MODDIV, pr_prim.ast.row, pr_prim.ast.col, {pr_prim.ast, rhs.ast}}, true};
                break;
            default: assert(0 && "WTF?");
            }
        }
        return pr_prim;
    }
    PR parse_lexpr(Faivy::Slice<Token> toks) {
        if (toks.start[0].kind == TK_ID) {
            auto t = toks.start[0];
            return (PR) {toks.skip(), (Ast) {.kind = AK_LID, .row=toks.start[0].row, .col=toks.start[0].col, .s = t.s}, true};
        }
        return {.is_the=false};
    }
    PR parse_proc(Faivy::Slice<Token> toks) {
        if (toks.size < 5 /* id₀ ::₁ (₂ )₃ {₄ }₅ */ || toks.start[0].kind != TK_ID || toks.start[1].kind != TK_2COLON || toks.start[2].kind != TK_LPAR) return PR{.is_the=false};
        Ast args{AK_SEQ, {}};
        std::string name = toks.start[0].s;
        size_t row = toks.start[0].row;
        size_t col = toks.start[0].col;
        toks = toks.skip(3);
        while (toks.size >= 3 && toks.start[0].kind != TK_RPAR) {
            Token name = toks.start[0];
            toks = toks.skip();
            Token type = toks.start[0];
            toks = toks.skip();
            args.inner.push_back(Ast{AK_FIELD, name.row, name.col, {{AK_RID, name.row, name.col, {}, name.s}, {AK_RID, type.row, type.col, {}, type.s}}}); // TODO: there must be LID.
            if (toks.start[0].kind == TK_RPAR) break;
            toks = skip(toks, TK_COMMA);
        }
        toks = skip(toks, TK_RPAR);
        PR pr_stmt = parse_stmt(toks);
        return PR{pr_stmt.rest, {AK_PROC, row, col, {args, pr_stmt.ast}, name}, true};
    }
    PR parse_stmt(Faivy::Slice<Token> toks) {
        /*
        if (toks.size >= 2 && toks.start[0].kind == TK_HASH && toks.start[1].kind == TK_ID) {
            if (toks.start[1].s == "bytecode") {
                if (toks.size < 3)
                    return {.is_the=false};
                std::vector<Faivy::ByteCodeInst> bc;
                toks = toks.skip(2);
                PR pr = parse_rexpr(toks);
                if (!pr.is_the) return pr;
                return {pr.rest, {.kind=AK_BC, .row=pr.ast.row, .col=pr.ast.col, .bcin=bc}, true};
            }
            return {.is_the=false};
        }
        */
        if (toks.size >= 2 && toks.start[0].kind == TK_LCB) {
            toks = toks.skip();
            PR pr = parse_block(toks);
            pr.rest = pr.rest.skip();
            return pr;
        }
        if (toks.size >= 2 && toks.start[0].kind == TK_ID && toks.start[0].s == "if") {
            size_t row = toks.start[0].row;
            size_t col = toks.start[0].col;
            PR cond = parse_rexpr(toks.skip());
            PR block = parse_stmt(cond.rest);
            toks = block.rest;
            if (toks.size >= 1 && toks.start[0].kind == TK_ID && toks.start[0].s == "else") {
                toks = toks.skip();
                PR block2 = parse_stmt(toks);
                return PR{block2.rest, Ast{AK_IF, row, col, {cond.ast, block.ast, block2.ast}}, true};
            }
            return PR{toks, Ast{AK_IF, row, col, {cond.ast, block.ast}}, true};
        }
        if (toks.size >= 2 && toks.start[0].kind == TK_ID && toks.start[0].s == "while") {
            PR cond = parse_rexpr(toks.skip());
            PR block = parse_stmt(cond.rest);
            return PR{block.rest, Ast{AK_WHILE, toks.start[0].row, toks.start[0].col, {cond.ast, block.ast}}, true};
        }
        if (toks.size >= 2 && toks.start[0].kind == TK_HASH && toks.start[1].kind == TK_ID && toks.start[1].s == "include" && toks.start[2].kind == TK_STR) {
            toks = toks.skip(2);
            auto itoks = Parser::lexer(read_all(toks.start[0].s.c_str()).c_str());
#ifdef VERY_VEBOSE
            for (auto tok : itoks) {
                std::cout << tok.to_string() << "\n";
            }
#endif // VERY_VEBOSE
            auto stoks = Faivy::Slice<Parser::Token>(itoks.data(), itoks.size());
            auto ast = Parser::parse_block(stoks).ast;
            return {toks.skip(), ast, true};
        }
        if (toks.size >= 2 && toks.start[0].kind == TK_ID && toks.start[0].s == "return") {
            PR pr_rexpr = parse_rexpr(toks.skip());
            if (pr_rexpr.is_the) {
                pr_rexpr.rest = skip(pr_rexpr.rest, TK_SEMICOLON);
                return PR{pr_rexpr.rest, {AK_RETURN, toks.start[0].row, toks.start[0].col, {pr_rexpr.ast}}, true};
            }
            return pr_rexpr;
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
            // std::cout << "[First token after a stmt] " << toks.start[0].to_string() << "\n";
        }
        return PR{toks, ast, true};
    }
}
