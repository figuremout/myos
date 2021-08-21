if [[ ! -d ./bin ]]; then
    mkdir ./bin
fi
VHD=/mnt/c/Users/zjm/Desktop/booktool/vhd/bin/vhd
nasm test.asm -o ./bin/test.bin
${VHD} -d ./test.vhd -w0 -b ./bin/test.bin
