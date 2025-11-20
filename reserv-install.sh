set -e

cd compiler
cargo build
cd ..

echo -e -n "Creating directories..."
sudo mkdir -p /opt/Faivy
sudo mkdir -p /opt/Faivy/modules
echo -e -n "End\n"
echo -e -n "Copying files..."
sudo cp -r modules/* /opt/Faivy/modules
sudo cp compiler/target/debug/compiler /opt/Faivy/faivyc
echo -e -n "End\n"
echo -e -n "Use /opt/Faivy/faivyc to build your project\n"
