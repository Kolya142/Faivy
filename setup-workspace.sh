echo "Warning: this script isn't required just to build Faivy compiler"
echo "Press ^C to exit or Return to continue"
read
set -xe

mkdir -p _local
echo -n "*" > _local/.gitignore
echo "# Write private TODOs here" > _local/TODO
echo "https://xkcd.com/303" > _local/THINK
