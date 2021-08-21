if [[ ! -d ./bin ]]; then
    mkdir ./bin
fi
VHD=/mnt/c/Users/zjm/Desktop/booktool/vhd/bin/vhd
nasm c17_mbr.asm -o ./bin/c17_mbr.bin
nasm c17_core.asm -o ./bin/c17_core.bin
nasm c17-1.asm -o ./bin/c17-1.bin
nasm c17-2.asm -o ./bin/c17-2.bin
${VHD} -d ./myos.vhd -w0 -b ./bin/c17_mbr.bin -w1 -b ./bin/c17_core.bin -w50 -b ./bin/c17-1.bin -w100 -b ./bin/c17-2.bin
