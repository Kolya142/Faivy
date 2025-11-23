set -e

CPP=clang++
CPFLAGS="-I. -g -O3"

if [ "@"$1 = "@vv" ]; then
    time $CPP $CPFLAGS -DVERY_VEBOSE faivy.cpp core.cpp parser.cpp compiler.cpp -o faivy;
else
    time $CPP $CPFLAGS faivy.cpp core.cpp parser.cpp compiler.cpp -o faivy;
fi
