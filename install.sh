set -e

cd compiler
cargo build
cd ..

sudo ./compiler/target/debug/compiler --dry build.faivy
