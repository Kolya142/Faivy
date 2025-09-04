set -e

cd compiler
cargo build
cd ..

mkdir -p /opt/Faivy
mkdir -p /opt/Faivy/modules

cp -r modules/* /opt/Faivy/modules
cp compiler/target/debug/compiler /opt/Faivy/faivyc
