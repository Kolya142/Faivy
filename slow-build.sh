set -e

CC="cc"
CPP="clang++"
CPFLAGS="-I./include -g -O3"
SRC="src/faivy.cpp src/core.cpp src/parser.cpp src/compiler.cpp bin/mp_min.o"
BIN="./bin"

mkdir -p $BIN
echo -n "*" > $BIN/.gitignore

$CC -c -DMP_IMPLEMENTATION -x c src/mp_min.h -o bin/mp_min.o

[ -e $BIN/faivy ] && echo "Since you're already built $BIN/faivy metaprogram you should use '$BIN/faivy --mp rebuild' instead of slow-build.sh"

if [ "@"$1 = "@vv" ]; then
    time $CPP $CPFLAGS -DVERY_VEBOSE $SRC -o $BIN/faivy;
else
    time $CPP $CPFLAGS $SRC -o $BIN/faivy;
fi
