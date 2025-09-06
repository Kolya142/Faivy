set -e

cd compiler
cargo build
cd ..

./compiler/target/debug/compiler build.faivy
sudo ./build.faivy.out
