#include <faivy.hpp>
#include <assert.h>
extern "C" { // Don't worry.
#include "mp_min.h"
}
#define slicefs(t, s) Faivy::Slice<t>(s, sizeof(s)/sizeof(t))

Alloc::Context Alloc::ctx_strings;

std::string read_all(const char *fn) {
    FILE *fptr = fopen(fn, "rb");
    fseek(fptr, 0, SEEK_END);
    size_t sz = ftell(fptr);
    fseek(fptr, 0, SEEK_SET);
    char *buf = (char *)malloc(sz+1);
    fread(buf, sz, 1, fptr);
    buf[sz] = 0;
    fclose(fptr);
    std::string s = buf;
    free(buf);
    return s;
}

extern "C" int mp_user_rebuild(int argc, char **argv) {
    static const char *SRC[] = {"src/faivy.cpp", "src/core.cpp", "src/parser.cpp", "src/memory.cpp", "src/compiler.cpp"};
    static const size_t SRC_SIZE = sizeof(SRC)/sizeof(*SRC);
    #define OCPP "ccache clang++"
    #define CPP "clang++"
    #define CPPFLAGS "-I./include -g -O3"
    #define BIN "./bin"
    bool vv = false;

    if (argc >= 5 && !strcmp(argv[4], "vv")) {
        for (size_t i = 0; i < SRC_SIZE; ++i)
            assert(!mp_systemf(OCPP " -c -DVERY_VERBOSE " CPPFLAGS " %s -o " BIN "/_%zu.o", SRC[i], i));
    }
    else {
        for (size_t i = 0; i < SRC_SIZE; ++i)
            assert(!mp_systemf(OCPP " -c " CPPFLAGS " %s -o " BIN "/_%zu.o", SRC[i], i));
    }
    assert(!mp_systemf(CPP " " CPPFLAGS " " BIN "/_*.o bin/mp_min.o -o " BIN "/faivy"));
    return 0;
}

extern "C" int mp_user_main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <Faivy source file>\n", argv[0]);
        exit(1);
    }
    auto toks = Parser::lexer(read_all(argv[1]).c_str());
#ifdef VERY_VEBOSE
    for (auto tok : toks) {
        std::cout << tok.to_string() << "\n";
    }
#endif // VERY_VEBOSE
    auto stoks = Faivy::Slice<Parser::Token>(toks.data(), toks.size());
    auto ast = Parser::parse_block(stoks).ast;
#ifdef VERY_VEBOSE
    std::cout << ast.to_string() << "\n";
#endif // VERY_VEBOSE
    std::vector<Faivy::ByteCodeInst> insts;
    std::unordered_map<std::string, size_t> procs;
    std::vector<uint8_t> static_data;
    Compiler::compile(ast, {&insts, &procs, &static_data, "<top>"});
#ifdef VERY_VEBOSE
    for (auto inst : insts) {
        std::cout << Faivy::bc_names[inst.kind] << " `" << inst.s << "` ";
        std::cout << "\n";
    }
#endif // VERY_VEBOSE
    char *ofn = strdup(argv[1]);
    ofn[strlen(ofn)-sizeof(".faivy")+1] = 0;
    strcat(ofn, ".c");
    FILE *output = fopen(ofn, "wb");
    fprintf(output, "%s", Faivy::compile(Faivy::Slice<Faivy::ByteCodeInst>(insts.data(), insts.size()), Faivy::Slice<uint8_t>(static_data.data(), static_data.size()), &procs).c_str());
    fclose(output);
    char *ofn0 = strdup(argv[1]);
    ofn0[strlen(ofn0)-sizeof(".faivy")+1] = 0;
    std::cout << Alloc::ctx_strings.tprintf("[RUNNING] cc %s -o %s -Racoon-args\n", ofn, ofn0);
    system(Alloc::ctx_strings.tprintf("cc %s -o %s -fno-pie -fno-pic -no-pie -Wno-int-conversion\n", ofn, ofn0));
    free(ofn);
    free(ofn0);
    return 0;
}
