# MYOS
> A toy OS core

## Delay
- 第三章实现了保护模式跳转回实模式的代码 全面学习考虑可以加上 但好像没啥用
- int 0x15 检查内存大小 用于确定要初始化多少页表 但是是BIOS中断 必须在实模式下进行 可以在低端1MB留一块内存存储内存大小等BIOS信息 core读取这块内存进行操作

## 技术选型
- MBR/GPT
- FAT32/EXT2

## Chain loading
> Since the BIOS bootstrap routine loads and runs exactly one sector from the physical disk, having the partition table in the MBR with the boot code simplifies the design of the MBR program. It contains a small program that loads the Volume Boot Record (VBR) of the targeted partition. Control is then passed to this code, which is responsible for loading the actual operating system. This process is known as chain loading.

MBR(boot code, DPT, sig) -> VBR(jmp, BS BPB, boot code, FAT, data) -> core

1. MBR 被 BIOS 加载进 0x0000:0x7c00，然后重定位自己；从 partition table 中获取一个active的 partition，将该 partition 的第一sector加载到 0x0000:0x7c00
2. 该 sector 就是 VBR


## my method
boot -> loader -> kernel(elf)

因为kernel是要C和ASM结合使用的，必须是GCC可链接的
