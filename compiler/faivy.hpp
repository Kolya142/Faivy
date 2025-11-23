#pragma once
#include <unordered_map>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <stdint.h>
#include <stdarg.h>
#include <vector>

#define CC "cc"
#define CFLAGS "-O3"
#define CLIBS ""

namespace Faivy {
    enum ByteCodeInstKind {
        B_MOVI,
        B_CALL,
        B_CALLR,
        B_CALLA,
        B_CALLRA,
        B_JUC,
        B_JCT,
        B_JUCI,
        B_JCTI,
        B_JUCIA,
        B_JCTIA,
        B_JUCA,
        B_JCTA,        
        B_RET,        
        B_CHG,
        B_CHNG,
        B_CHL,
        B_CHNL,
        B_CHE,
        B_CHNE,        
        B_PUSH,
        B_POP,
        B_PUSHx,
        B_POPx,
        B_PUSHI,
        B_CPP,
        B_PUSHDS,
        B_PUSHDSO,
        B_ADD,
        B_SUB,
        B_MUL,
        B_DIV,
        B_MOD,
        B_XOR,
        B_AND,
        B_OR,
        B_NOT,
        B_PROC_START,
        B_PROC_END,
        B_DROP,
        B_GSP,
        B_SSP,
        B_GET_SYM_PTR,
        B_LOAD_SYM,
        B_SAVE_SYM,
        B_PEEK64,
        B_POKE64,
    };
    extern const char *bc_names[];
    struct ByteCodeInst { // Legacy code!
        ByteCodeInstKind kind;
        size_t row, col;
        std::vector<size_t> xs;
        std::string s;
        std::vector<std::string> ses;
    };
    template<typename T>
    struct Slice {
        T *start;
        size_t size;
        Slice() {
        }
        Slice(T *start, size_t size) {
            this->start = start;
            this->size = size;
        }
        Slice<T> skip(size_t a = 1) {
            return Slice(start+a, size-a);
        }
    };
    std::string ssprintf(const char *fmt, ...);
    size_t istack_pop64(Slice<uint8_t> *s, size_t *sp);
    void istack_push64(Slice<uint8_t> *s, size_t *sp, size_t v);
    size_t interpret(Slice<ByteCodeInst> code, Slice<uint8_t> static_data, std::unordered_map<std::string, size_t> *procs, size_t ip);
    std::string compile(Slice<ByteCodeInst> code, Slice<uint8_t> static_data, std::unordered_map<std::string, size_t> *procs);
}

namespace Parser {
    extern const char *tkn[];
    extern const char *akn[];
    enum TokenKind {
        TK_ID,
        TK_STR,
        TK_NAT,
        TK_PLUS,
        TK_SEMICOLON,
        TK_2COLON,
        TK_COLON,
        TK_LPAR,
        TK_RPAR,
        TK_LCB,
        TK_RCB,
        TK_COMMA,
        TK_HASH,
    };
    struct Token {
        TokenKind kind;
        std::string s;
        size_t row, col;
        std::string to_string();
    };
    std::vector<Token> lexer(const char *code);
    enum AstKind {
        AK_RID,
        AK_STR,
        AK_NUM,
        AK_SUM,
        AK_CPP,
        AK_CALL,
        AK_PROC,
        AK_SEQ,
        AK_FIELD,
        AK_BC,
        AK_RUN,
        AK_IF
    };
    struct Ast {
        AstKind kind;
        size_t row, col;
        std::vector<Ast> inner;
        std::string s;
        size_t n;
        std::vector<Faivy::ByteCodeInst> bcin;
        std::string to_string();
    };
    struct PR {
        Faivy::Slice<Token> rest;
        Ast ast;
        bool is_the;
    };
    PR parse_primary(Faivy::Slice<Token> toks);
    Faivy::Slice<Token> skip(Faivy::Slice<Token> toks, TokenKind tk);
    PR parse_rexpr(Faivy::Slice<Token> toks);
    PR parse_proc(Faivy::Slice<Token> toks);
    PR parse_stmt(Faivy::Slice<Token> toks);
    PR parse_block(Faivy::Slice<Token> toks);
}

// https://stackoverflow.com/questions/313432/c-extend-a-vector-with-another-vector#answer-64102335
#define vector_extend(a, b) (a).insert((a).end(), (b).begin(), (b).end())

namespace Compiler {
    void compile(std::vector<Faivy::ByteCodeInst> *prog, Parser::Ast ast, std::unordered_map<std::string, size_t> *procs, std::vector<uint8_t> *static_data, std::string cfunc);
}

