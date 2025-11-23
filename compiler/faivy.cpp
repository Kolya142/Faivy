#include <faivy.hpp>

#define slicefs(t, s) Faivy::Slice<t>(s, sizeof(s)/sizeof(t))

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

int main(int argc, char **argv) {
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
    Compiler::compile(&insts, ast, &procs, &static_data, "<top>");
#ifdef VERY_VEBOSE
    for (auto inst : insts) {
        std::cout << inst.kind << "\n";
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
    std::cout << Faivy::ssprintf("[RUNNING] cc %s -o %s -fno-pie -fno-pic -no-pie\n", ofn, ofn0);
    system(Faivy::ssprintf("cc %s -o %s -fno-pie -fno-pic -no-pie\n", ofn, ofn0).c_str());
    free(ofn);
    free(ofn0);
    return 0;
}
