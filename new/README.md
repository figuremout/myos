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


## 关于链接
> [简单扫盲文](https://blog.csdn.net/a1342772/article/details/77688148)
> [更系统的博客](https://www.cnblogs.com/linhaostudy/p/10544917.html)

目前受到限制的是elf文件的装载、重定位过程，而不是链接过程，即需要实现一个 elf解析器 readelf

- 目前需要`ld -Ttext`指定程序要加载的线性地址，因为程序中包含多模块，模块间是分离的，必须通过 **段间绝对远调用** 进行控制转移，所以需要确定下来被调用模块的线性地址（同一个模块中控制转移是通过相对近调用，与模块加载基地址无关）

- 当然会出现多个程序编译链接出来的线性地址一样的情况，目前不考虑，需要进行装载时重定位(LTR load time relocation)
    - 但是实现分页后，每个任务有4GB的虚拟内存空间，不同任务间可以是相同的线性地址啊，这样的话就按elf中指定的线性地址安装就行了

- x86 汇编采用的方式是PIC (地址无关代码)，把指令中需要的地址分离到数据部分，这样指令可以保持不变，而数据部分则在每个进程有一个副本(即SALT表，加载时重定位这些符号的地址，运行时应用程序在表内获取地址)

- 动态链接器的工作是按程序头表里的信息对文件进行装载，然后将控制转移到ELF的入口地址
    - 动态链接器本身是静态链接的，且不能依赖其他共享对象，即满足自举
