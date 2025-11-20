set -xe

CPP=clang++
CPFLAGS="-I. -g"

$CPP $CPFLAGS faivy.cpp core.cpp parser.cpp compiler.cpp -o faivy
./faivy first.faivy
$CPP $CPFLAGS first.cpp -o first
./first
