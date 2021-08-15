         ;代码清单17-2
         ;文件名：c17_core.asm
         ;文件说明：保护模式微型核心程序 
         ;创建日期：2012-07-12 23:15
;-------------------------------------------------------------------------------
         ;以下定义常量
         flat_4gb_code_seg_sel  equ  0x0008      ;平坦模型下的4GB代码段选择子 
                                                 ; 描述符索引=0000 0000 0000 1 TI=0 RPL=00
         flat_4gb_data_seg_sel  equ  0x0018      ;平坦模型下的4GB数据段选择子 
                                                 ; 描述符索引=0000 0000 0001 1 TI=0 RPL=00
         idt_linear_address     equ  0x8001f000  ;中断描述符表的线性基地址 
;-------------------------------------------------------------------------------          
         ;以下定义宏
         %macro alloc_core_linear 0              ; 在内核空间中分配虚拟内存
                                                 ; 分配一个线性地址 并配置好该线性地址所对应的页目录、页表、物理页
                                                 ; 返回: ebx 此次分配的内核地址空间线性地址
               mov ebx,[core_tcb+0x06]           ; ebx=内核TCB中的程序加载基地址(下一个可分配的线性地址)
               add dword [core_tcb+0x06],0x1000  ; 为简单起见 每次在内核空间中分配内存时 都固定分配一个页4KB
                                                 ; 因此这里为内核TCB中的程序加载基地址加4096
               call flat_4gb_code_seg_sel:alloc_inst_a_page
         %endmacro 
;-------------------------------------------------------------------------------
         %macro alloc_user_linear 0              ;在任务空间中分配虚拟内存 
                                                 ; 输入 esi 用户任务的tcb线性地址
                                                 ; 输出 ebx 分配的页的线性地址
               mov ebx,[esi+0x06]
               add dword [esi+0x06],0x1000
               call flat_4gb_code_seg_sel:alloc_inst_a_page
         %endmacro
         
;===============================================================================
SECTION  core  vstart=0x80040000                 ; 内核在物理空间中是从0x00040000开始加载的 (定义在mbr的常量) 
                                                 ; 映射到高 2GB 就是 0x80040000
 
         ;以下是系统核心的头部，用于加载核心程序 
         core_length      dd core_end       ; 核心程序总长度 字节数#00

         core_entry       dd start          ; 核心代码段入口点#04 code_entry=0x80040004

;-------------------------------------------------------------------------------
         [bits 32]
;-------------------------------------------------------------------------------
         ;字符串显示例程（适用于平坦内存模型） 
put_string:                                 ; 显示0终止的字符串并移动光标 
                                            ; 输入：EBX=字符串的线性地址
                                            ; 由于这个过程结束时有sti 所以在整个中断系统没有完成初始化之前不能调用这个过程

         push ebx
         push ecx

         cli                                ;硬件操作期间，关中断

  .getc:
         mov cl,[ebx]                       ; 读取字符串中8位(一个字符)
         or cl,cl                           ; 检测串结束标志（0） 
         jz .exit                           ; 显示完毕，返回 
         call put_char
         inc ebx
         jmp .getc

  .exit:

         sti                                ;硬件操作完毕，开放中断

         pop ecx
         pop ebx

         retf                               ;段间返回

;-------------------------------------------------------------------------------
put_char:                                   ;在当前光标处显示一个字符,并推进
                                            ;光标。仅用于段内调用 
                                            ;输入：CL=字符ASCII码 
         pushad

         ;以下取当前光标位置
         mov dx,0x03d4                      ; 0x03d4 显卡的索引寄存器端口
         mov al,0x0e                        ; 8位光标寄存器的索引 提供光标位置的高8位
         out dx,al
         inc dx                             ; 0x03d5 显卡的数据端口
         in al,dx                           ; 读入光标位置的高8位到al
         mov ah,al                          ; 移到ah

         dec dx                             ; 0x03d4
         mov al,0x0f                        ; 8位光标寄存器的索引 提供光标位置的低8位
         out dx,al
         inc dx                             ; 0x3d5
         in al,dx                           ; 读入光标位置的低8位到al
         mov bx,ax                          ; BX=代表光标位置的16位数
         and ebx,0x0000ffff                 ; 准备使用32位寻址方式访问显存 
         
         cmp cl,0x0d                        ; 回车符？
         jnz .put_0a                         
         
         mov ax,bx                          ;以下按回车符处理 
         mov bl,80
         div bl                             ; 商在AL=行数 余数在AH=列数
         mul bl                             ; AX=AL*80=行首的光标位置
         mov bx,ax
         jmp .set_cursor

  .put_0a:
         cmp cl,0x0a                        ; 换行符？
         jnz .put_other
         add bx,80                          ; 光标位置增加一行 
         jmp .roll_screen

  .put_other:                               ;正常显示字符
         shl bx,1                           ; 乘2 将光标位置转化为字节偏移
         mov [0x800b8000+ebx],cl            ; 在光标位置处显示字符 
         mov byte [0x800b8000+ebx+1],0x07   ; 设置字符属性 没有这个也可以 因为默认全是0x0720

         ;以下将光标位置推进一个字符
         shr bx,1
         inc bx

  .roll_screen:
         cmp bx,2000                        ;光标超出屏幕？滚屏
         jl .set_cursor

         cld                                ; 设置DF=0 正向
         ; 小心！32位模式下movsb/w/d 使用的是DS:esi => ES:edi 数量由ecx指定 
         mov esi,0x800b80a0                 ; 对应显存中第二行行首
         mov edi,0x800b8000                 ; 对饮显存中第一行行首
         mov ecx,1920                       ; 传送24行 1920个字符(每个2字节 包括一字节显示属性)
         rep movsw                          ; 个人觉得这里应该是movsw  例程上是movsd 都可以正常运行
         mov bx,3840                        ; 1920*2=最后一行的首个字符在显存中的偏移 清除屏幕最底一行
         mov ecx,80                         ; 32位程序应该使用ECX
  .cls:
         mov word [0x800b8000+ebx],0x0720   ; 0x0720 黑底白字的空白字符
         add bx,2
         loop .cls

         mov bx,1920

  .set_cursor:
         mov dx,0x03d4                      ; 显卡索引端口
         mov al,0x0e                        ; 8位光标寄存器 光标位置高8位
         out dx,al
         inc dx                             ; 0x3d5 显卡数据端口
         mov al,bh
         out dx,al                          ; 传入光标位置高8位
         dec dx                             ; 0x3d4 显卡索引端口
         mov al,0x0f                        ; 8位光标寄存器 光标位置低8位
         out dx,al
         inc dx                             ; 0x3d5 显卡数据端口
         mov al,bl
         out dx,al                          ; 传入光标位置高8位
         
         popad
         
         ret                              

;-------------------------------------------------------------------------------
read_hard_disk_0:                           ;从硬盘读取一个逻辑扇区（平坦模型） 
                                            ;EAX=逻辑扇区号
                                            ;EBX=目标缓冲区线性地址
                                            ;返回：EBX=EBX+512
         ; 这个过程和mbr里的过程相比 只有开始关中断 结束开中断 以及最后是远返回 不同
         ; 在读硬盘时 应当屏蔽硬件中断 以防止对同一个硬盘控制器端口的交叉修改 这会产生很严重的问题
         ; 特别是多任务环境下 当一个任务正在读硬盘时 会被另一个任务打断 如果另一个任务也访问硬盘 将破坏前一个任务对硬盘的操作状态
         cli
         
         push eax 
         push ecx
         push edx
      
         push eax
         
         mov dx,0x1f2
         mov al,1
         out dx,al                          ;读取的扇区数

         inc dx                             ;0x1f3
         pop eax
         out dx,al                          ;LBA地址7~0

         inc dx                             ;0x1f4
         mov cl,8
         shr eax,cl
         out dx,al                          ;LBA地址15~8

         inc dx                             ;0x1f5
         shr eax,cl
         out dx,al                          ;LBA地址23~16

         inc dx                             ;0x1f6
         shr eax,cl
         or al,0xe0                         ;第一硬盘  LBA地址27~24
         out dx,al

         inc dx                             ;0x1f7
         mov al,0x20                        ;读命令
         out dx,al

  .waits:
         in al,dx
         and al,0x88
         cmp al,0x08
         jnz .waits                         ;不忙，且硬盘已准备好数据传输 

         mov ecx,256                        ;总共要读取的字数
         mov dx,0x1f0
  .readw:
         in ax,dx
         mov [ebx],ax
         add ebx,2
         loop .readw

         pop edx
         pop ecx
         pop eax
      
         sti
      
         retf                               ;远返回 

;-------------------------------------------------------------------------------
;汇编语言程序是极难一次成功，而且调试非常困难。这个例程可以提供帮助 
put_hex_dword:                              ;在当前光标处以十六进制形式显示
                                            ;一个双字并推进光标 
                                            ;输入：EDX=要转换并显示的数字
                                            ;输出：无
         pushad

         mov ebx,bin_hex                    ;指向核心地址空间内的转换表
         mov ecx,8                          ; 总共是显示一个32位数 每次以16进制显示4位 要显示8次
  .xlt:    
         rol edx,4                          ; 循环左移4位 高四位循环到低四位
         mov eax,edx
         and eax,0x0000000f                 ; 取低四位
         xlat                               ; 查表 DS:EBX 用AL作为偏移量取一个字节传回AL
      
         push ecx
         mov cl,al                           
         call put_char
         pop ecx
       
         loop .xlt
      
         popad
         retf
      
;-------------------------------------------------------------------------------
set_up_gdt_descriptor:                      ;在GDT内安装一个新的描述符
                                            ;输入：EDX:EAX=描述符 
                                            ;输出：CX=描述符的选择子
         push eax
         push ebx
         push edx

         sgdt [pgdt]                        ; 取得GDTR的界限和线性地址 

         movzx ebx,word [pgdt]              ; movzx带零拓展的传送 GDT界限
         inc bx                             ; GDT总字节数，也是下一个描述符偏移
         add ebx,[pgdt+2]                   ; GDT线性地址+GDT总字节数=下一个描述符的线性地址

         mov [ebx],eax
         mov [ebx+4],edx                    ; 将描述符安装在下一个描述符

         add word [pgdt],8                  ; 给GDT界限值增加一个描述符的大小

         lgdt [pgdt]                        ; 加载回GDTR 对GDT的更改生效

         mov ax,[pgdt]                      ; 得到新GDT界限值
         xor dx,dx
         mov bx,8
         div bx                             ; 界限值除以8得到新描述符的索引号
         mov cx,ax
         shl cx,3                           ; 索引号左移3位构造成 TI=0 RPL=0的选择子

         pop edx
         pop ebx
         pop eax

         retf

;-------------------------------------------------------------------------------
make_seg_descriptor:                        ;构造存储器和系统的段描述符
                                            ;输入：EAX=线性基地址
                                            ;      EBX=段界限
                                            ;      ECX=属性。各属性位都在原始
                                            ;          位置，无关的位清零 
                                            ;返回：EDX:EAX=描述符
         mov edx,eax
         shl eax,16
         or ax,bx                           ; 描述符前32位(EAX)构造完毕

         and edx,0xffff0000                 ; 清除基地址中无关的位
         rol edx,8
         bswap edx                          ; 装配基址的31~24和23~16  (80486+)

         xor bx,bx
         or edx,ebx                         ; 装配段界限的高4位

         or edx,ecx                         ; 装配属性

         retf

;-------------------------------------------------------------------------------
make_gate_descriptor:                       ;构造门的描述符（调用门等）
                                            ;输入：EAX=门代码在段内偏移地址
                                            ;       BX=门代码所在段的选择子 
                                            ;       CX=段类型及属性等（各属
                                            ;          性位都在原始位置）
                                            ;返回：EDX:EAX=完整的描述符
         push ebx
         push ecx
      
         mov edx,eax
         and edx,0xffff0000                 ; 得到偏移地址高16位 
         or dx,cx                           ; 组装属性部分到EDX
       
         and eax,0x0000ffff                 ; 得到偏移地址低16位 
         shl ebx,16                          
         or eax,ebx                         ; 组装段选择子部分
      
         pop ecx
         pop ebx
      
         retf                                   
                             
;-------------------------------------------------------------------------------
allocate_a_4k_page:                         ;分配一个4KB的页
                                            ;输入：无
                                            ;输出：EAX=页的物理地址
         push ebx
         push ecx
         push edx

         xor eax,eax                        ; eax=0
  .b1:
         bts [page_bit_map],eax             ; 从第0位开始搜索值为0的位
         jnc .b2                            ; 位为0 则CF设为0 则跳转
         inc eax
         cmp eax,page_map_len*8
         jl .b1
         
         mov ebx,message_3
         call flat_4gb_code_seg_sel:put_string
         hlt                                ; 没有可以分配的页，停机
                                            ; TODO 对于流行的操作系统来说这是不对的 正确的做法是看哪些已分配的页较少使用
                                            ; (页目录项、页表项中有一个A访问位 指示此表项所指向的页是否被访问过，可以被操作系统用来监视页的使用频率，当内存空间紧张时用以将较少使用的页唤出到磁盘，同时将其P位清零，然后将释放的页分配给马上要运行的程序，以实现虚拟内存管理功能)
         
  .b2:
         shl eax,12                         ; eax是位串中首个未分配的页的索引 由于每个页4KB 乘以4096就是页的物理地址（0x1000） 
         
         pop edx
         pop ecx
         pop ebx
         
         ret
         
;-------------------------------------------------------------------------------
alloc_inst_a_page:                          ; 分配一个物理页，并用页的物理地址和它所对应的线性地址去修改当前内核任务的页目录表和页表(并安装在当前活动的层级分页结构中)
                                            ;输入：EBX=页的线性地址
         push eax
         push ebx
         push esi
         
         ;检查该线性地址所对应的页表是否存在
         mov esi,ebx
         and esi,0xffc00000                 ; 线性地址的高10位 是页目录索引
         shr esi,20                         ; 低12位=页目录索引乘以4  是页目录项在页目录内的偏移
         or esi,0xfffff000                  ; 线性地址0xfffff000高10位1111 1111 11是页目录索引 指向页目录最后一项 存储了页目录自己的物理地址 此时将页目录物理地址当页表物理地址
                                            ; 中间10位是1111 1111 11是页表索引 指向页表的最后一项 存储了页目录自己的物理地址 此时将页目录物理地址当页物理地址
                                            ; 所以0xfffff000+偏移(只有低12位) 组成的线性地址 经过页部件转化出来的物理地址是传入参数页的页目录项的物理地址
                                            ; 所以此时的esi就是对应传入参数页的页目录项的物理地址的线性地址

         test dword [esi],0x00000001        ; 检查该页的页目录项的P位是否为1 为1则该页的页表已在内存中
         jnz .b1                            ; P位为1则跳转
          
         ;该页的页表不在内存中 则创建该线性地址所对应的页表 
         call allocate_a_4k_page            ; 分配一个页做为页表 
                                            ; 返回 eax=分配的页的物理地址
         or eax,0x00000007                  ; 由于页物理地址低12位肯定为0 这步就是给它低12位设置成页目录项的属性
                                            ; AVL=00 G=0 D=0 A=0 PCD=0 PWT=0 US=1 RW=1 P=1
                                            ; 允许所有特权级别的程序访问 可读可写 页表在内存中
         mov [esi],eax                      ; 在页目录中登记该页表
          
  .b1:
         ;开始访问该线性地址所对应的页表 
         mov esi,ebx
         shr esi,10
         and esi,0x003ff000                 ; esi中间10位是参数页的页目录索引 其他为0
         or esi,0xffc00000                  ; 高10为赋为全1 还是利用页目录最后一项的回环性质 得到对应参数页的页表物理地址的线性地址
         
         ;得到该线性地址在页表内的对应条目（页表项） 
         and ebx,0x003ff000                 ; 参数页线性地址的中间10位是该页的页表索引
         shr ebx,10                         ; 相当于右移12位，再乘以4
         or esi,ebx                         ; 得到对应该页的页表项物理地址的线性地址
         call allocate_a_4k_page            ;分配一个页，这才是要安装的页
         or eax,0x00000007                  ; eax=页表项
         mov [esi],eax                      ; 将页表项填入参数页的页表项物理地址处
          
         pop esi
         pop ebx
         pop eax
         
         retf  

;-------------------------------------------------------------------------------
create_copy_cur_pdir:                       ;创建新页目录，并复制当前页目录内容
                                            ;输入：无
                                            ;输出：EAX=新页目录的物理地址 
         push esi
         push edi
         push ebx
         push ecx
         
         call allocate_a_4k_page            ; 返回eax=页的物理地址
         mov ebx,eax
         or ebx,0x00000007                  ; 页物理地址低12位改为属性0x007 G=0 PAT=0 D=0 A=0 PCD=0 PWT=0 US=1 RW=1 P=1
                                            ; 允许所有特权级别的程序访问 可读可写 页存在内存中
         mov [0xfffffff8],ebx               ; 为了访问该页，把它的物理地址登记到当前页目录表(内核页目录表)的倒数第二个目录项
                                            ; 如此一来 0xffffe000是这个新页的线性地址(相对于内核页目录表)

         invlpg [0xfffffff8]                ; invalidate TLB Entry invlpg用于刷新TLB中的单个条目 处理器用给出的线性地址搜索TLB找到那个条目 然后从内存中重新加载它
                                            ; 0xfffffff8是内核页目录表的倒数第二个目录项的物理地址 每次都用它来指向新任务的页目录表

         mov esi,0xfffff000                 ; ESI->当前页目录的线性地址
         mov edi,0xffffe000                 ; EDI->新页目录的线性地址
         mov ecx,1024                       ; ECX=要复制的目录项数
         cld
         repe movsd                         ; 每个目录项4字节
         
         pop ecx
         pop ebx
         pop edi
         pop esi
         
         retf
         
;-------------------------------------------------------------------------------
general_interrupt_handler:                  ;通用的中断处理过程
         push eax
          
         mov al,0x20                        ; 向8259A芯片发送中断结束命令EOI 
         out 0xa0,al                        ; 向从片发送
         out 0x20,al                        ; 向主片发送
         
         pop eax
          
         iretd

;-------------------------------------------------------------------------------
general_exception_handler:                  ;通用的异常处理过程
         mov ebx,excep_msg
         call flat_4gb_code_seg_sel:put_string ; 显示异常信息
         
         hlt                                ; 停机

;-------------------------------------------------------------------------------
rtm_0x70_interrupt_handle:                  ;实时时钟中断处理过程
                                            ; 将当前忙任务放到链表尾并置为空闲 从链表中找一个空闲任务置为忙 并跳转执行
        ; RTC芯片中断号默认是0x70

         pushad                             ; push 所有32位通用寄存器

         ; 由于是硬件中断 先要向8259A发送中断结束命令EOI 否则它不会再向处理器发送另一个中断通知 TODO ?
         mov al,0x20                        ; 中断结束命令EOI
         out 0xa0,al                        ; 向8259A从片发送
         out 0x20,al                        ; 向8259A主片发送

         ; 设置RTC寄存器B
         mov al,0x0b
         or al,0x80                         ; 访问RTC期间 最好阻断NMI TODO why
         out 0x70,al
         mov al,0x12                        ; 设置寄存器B 允许更新周期 禁止周期性中断 禁止闹钟中断 允许更新结束中断 BCD表示 24小时制
         out 0x71,al

         mov al,0x0c                        ; RTC寄存器C的索引。且开放NMI
         out 0x70,al                        ; CMOS RAM 的索引端口
         in al,0x71                         ; CMOS RAM 数据端口 读一下RTC的寄存器C，否则只发生一次中断
                                            ; 每当更新周期结束中断发生时 处理器将寄存器C的第4位置位 读取后清零 如果不清零同样的中断不会再产生
                                            ; 此处不考虑闹钟和周期性中断的情况

         ; 正常情况下 8259是不会允许RTC中断的，所以修改它内部的中断屏蔽寄存器IMR
         in al, 0xa1                        ; 读8259从片的IMR寄存器
         and al,0xfe                        ; 清楚bit 0(对应RTC的中断输入引脚IR0)
         out 0xa1,al                        ; 写回IMR寄存器

         ;找当前任务（状态为忙的任务）在链表中的位置
         mov eax,tcb_chain                  
  .b0:                                      ; EAX=链表头或当前TCB线性地址
         ; TODO 这里有bug？当忙任务是链表最后一个任务时 判断它是链表尾后直接就从中断返回了
         mov ebx,[eax]                      ; EBX=下一个TCB线性地址
         or ebx,ebx                         ; 判断ebx是否为0 即是否到了链表尾
         jz .irtn                           ; 链表为空，或已到末尾，从中断返回
         cmp word [ebx+0x04],0xffff         ; 查看该TCB的任务状态字 0x0000 表示空闲或者挂起 0xffff表示是忙任务（当前任务）？ 链表中只允许有一个忙任务(和TSS中的B位不同)
         je .b1
         mov eax,ebx                        ;定位到下一个TCB（的线性地址）
         jmp .b0         

         ;将当前为忙的任务移到链尾
  .b1:
         ; 此时 eax是忙任务上一个TCB的线性地址 ebx是忙任务TCB的线性地址
         mov ecx,[ebx]                      ; ecx是忙任务下一个TCB的线性地址
         mov [eax],ecx                      ; 让忙任务上一个TCB指向忙任务下一个TCB 将当前任务从链中拆除

  .b2:                                      ; 此时，EBX=忙任务的线性地址
         mov edx,[eax]
         or edx,edx                         ; 已到链表尾端？
         jz .b3
         mov eax,edx
         jmp .b2

  .b3:                                      ; 此时 eax是链表最后一个TCB的线性地址
         mov [eax],ebx                      ; 让链表最后一个TCB指向忙任务 将忙任务的TCB挂在链表尾端
         mov dword [ebx],0x00000000         ; 将忙任务的TCB标记为链尾

         ;从链首搜索第一个空闲任务
         mov eax,tcb_chain
  .b4:
         mov eax,[eax]
         or eax,eax                         ; 已到链尾（未发现空闲任务）
         jz .irtn                           ; 未发现空闲任务，从中断返回
         cmp word [eax+0x04],0x0000         ; 是空闲任务？
         jnz .b4

         ;将空闲任务和当前任务的状态都取反
         ; 此时 eax 是链表上找到的第一个空闲任务TCB
         not word [eax+0x04]                ; 设置空闲任务的状态为忙
         not word [ebx+0x04]                ; 设置当前任务（忙）的状态为空闲

         jmp far [eax+0x14]                 ; 任务转换 注意：中断处理过程和任务不是分离的 这里是旧任务的一部分 只不过是在任务的全局空间罢了 任务切换时旧人物的状态停留在这个中断处理过程中
                                            ; TCB偏移0x14处是四个字节的TSS基地址赋给EIP(丢弃不用) 两字节的TSS选择子赋给CS
                                            ; 处理器用这个选择子访问GDT，通过描述符发现这是一个TSS描述符，就知道应该执行任务切换操作
                                            ; 当前TR指向当前任务的TSS，把当前的每个寄存器快照存到这个TSS中，B位清零
                                            ; 然后因为该TSS描述符有TSS的基地址，处理器从该TSS中恢复各个寄存器的内容，包括通用寄存器、EFLAGS、段寄存器、EIP、ESP、LDTR等，B位置一
                                            ; 让TR指向新任务的TSS，开始执行新任务

         ; 当下一次从其他任务切换到这个旧任务时 回到这里继续执行
  .irtn:
         popad

         iretd                              ; TODO 研究嵌套特性

;-------------------------------------------------------------------------------
terminate_current_task:                     ;终止当前任务
                                            ;注意，执行此例程时，当前任务仍在
                                            ;运行中。此例程其实也是当前任务的
                                            ;一部分 
         ; 找当前任务（状态为忙的任务）在链表中的位置
         mov eax,tcb_chain
  .b0:                                      ; EAX=链表头或当前TCB线性地址
         mov ebx,[eax]                      ; EBX=下一个TCB线性地址
         cmp word [ebx+0x04],0xffff         ; 是忙任务（当前任务）？
         je .b1
         mov eax,ebx                        ; 定位到下一个TCB（的线性地址）
         jmp .b0
         
  .b1:
         mov word [ebx+0x04],0x3333         ; 修改当前任务的状态为退出 TODO 退出为0x3333应该可以随便定义 只要和0x0000空闲 0xffff忙区分就行
         
  .b2:
         hlt                                ; 停机，等待程序管理器恢复运行时，
                                            ; 将其回收 目前并没有回收代码
         jmp .b2 

;------------------------------------------------------------------------------- 
         pgdt             dw  0             ;用于设置和修改GDT 
                          dd  0

         pidt             dw  0             ; 用于存储IDT的界限值
                          dd  0
                          
         ;任务控制块链
         tcb_chain        dd  0             ; 这个双字就是tcb链表头，存储第一个tcb的线性地址

         core_tcb   times  32  db 0         ;内核（程序管理器）的TCB 实际用不到这么大

         ; 页映射位串 用于指示物理页的分配情况
         ; 目前没有检测实际可用内存的代码 所以假定我们只有2MB物理内存可用 2MB可分为512个4KB页 以下声明了512bit的位串
         ; 位0对应物理地址0x00000000的页 位1对应物理地址0x00001000的页...
         ; 前256位差不多都是1 对应了低端1MB内存的页 它们已经整体上划归内核使用了 没有被内核占用的部分多数也被外围硬件占用了 如ROM-BIOS
         ; 其中有一些0x55 => 01010101 是有意造的非连续页 为了说明大的连续的线性地址空间不必对应连续的页
         ; 物理地址0x30000-0x40000是内核的内存空间，但内核不会用到，所以也拿来做这个实验了 TODO 这只是个实验 真正用的时候明明没用到这些页但把它们标记为已分配就浪费了
         page_bit_map     db  0xff,0xff,0xff,0xff,0xff,0xff,0x55,0x55
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
         page_map_len     equ $-page_bit_map
                          
         ;符号地址检索表
         salt:
         salt_1           db  '@PrintString'
                     times 256-($-salt_1) db 0
                          dd  put_string
                          dw  flat_4gb_code_seg_sel

         salt_2           db  '@ReadDiskData'
                     times 256-($-salt_2) db 0
                          dd  read_hard_disk_0
                          dw  flat_4gb_code_seg_sel

         salt_3           db  '@PrintDwordAsHexString'
                     times 256-($-salt_3) db 0
                          dd  put_hex_dword
                          dw  flat_4gb_code_seg_sel

         salt_4           db  '@TerminateProgram'
                     times 256-($-salt_4) db 0
                          dd  terminate_current_task
                          dw  flat_4gb_code_seg_sel

         salt_item_len   equ $-salt_4
         salt_items      equ ($-salt)/salt_item_len

         excep_msg        db  '********Exception encounted********',0

         message_0        db  'Working in system core with protection '
                          db  'and paging are all enabled.System core is mapped '
                          db  'to address 0x80000000.',0x0d,0x0a,0

         message_1        db  'System wide CALL-GATE mounted.',0x0d,0x0a,0
         
         message_3        db  '********No available pages********',0
         
         core_msg0        db  'System core task running!',0x0d,0x0a,0
         
         bin_hex          db '0123456789ABCDEF'
                                            ;put_hex_dword子过程用的查找表 

         core_buf   times 512 db 0          ;内核用的缓冲区

         cpu_vendorHead        db 'Processor Vendor: ',0
         cpu_vendor  times 52 db 0
         cpu_vendorTail        db 0x0d,0x0a,0

         cpu_brandHead        db 'Processor Brand: ',0
         cpu_brand  times 52 db 0
         cpu_brandTail        db 0x0d,0x0a,0

;-------------------------------------------------------------------------------
fill_descriptor_in_ldt:                     ;在LDT内安装一个新的描述符
                                            ;输入：EDX:EAX=描述符
                                            ;          EBX=TCB基地址
                                            ;输出：CX=描述符的选择子
         push eax
         push edx
         push edi

         mov edi,[ebx+0x0c]                 ; 获得LDT基地址
         
         xor ecx,ecx
         mov cx,[ebx+0x0a]                  ; 获得LDT界限
         inc cx                             ; LDT的总字节数，即新描述符偏移地址
         
         mov [edi+ecx+0x00],eax
         mov [edi+ecx+0x04],edx             ; 安装描述符

         add cx,8                           
         dec cx                             ; 得到新的LDT界限值 

         mov [ebx+0x0a],cx                  ; 更新LDT界限值到TCB

         mov ax,cx
         xor dx,dx
         mov cx,8
         div cx                             ; LDT界限值除以8 商在ax 余数在dx
         
         mov cx,ax
         shl cx,3                           ; 左移3位得到描述符索引
         or cx,0000_0000_0000_0100B         ; 使TI位=1，指向LDT，最后使RPL=00 

         pop edi
         pop edx
         pop eax
     
         ret
      
;-------------------------------------------------------------------------------
load_relocate_program:                      ;加载并重定位用户程序
                                            ;输入: PUSH 逻辑扇区号
                                            ;      PUSH 任务控制块起始线性地址
                                            ;输出：无 
         pushad
      
         mov ebp,esp                        ;为访问通过堆栈传递的参数做准备
      
         ;清空当前页目录的前半部分（对应低2GB的局部地址空间） 
         ; 对于每个任务来说 页目录表的前半部分对应它的局部空间 内核用的其页目录表的后半部分 前半部可以临时用来创建只属于任务自己的页目录项
         mov ebx,0xfffff000                 ; 对应页目录表的最后一项 用于回环让线性地址指向页目录表自己
         xor esi,esi
  .b1:
         mov dword [ebx+esi*4],0x00000000
         inc esi
         cmp esi,512                        ; 页目录表共1024项 前半部512项
         jl .b1

         mov eax,cr3
         mov cr3,eax                        ; 刷新TLB 
                                            ; 刚才修改了页目录表 显式刷新TLB
         
         ;以下开始分配内存并加载用户程序
         mov eax,[ebp+40]                   ; 从堆栈中取出用户程序起始扇区号
                                            ; 变址寄存器ebp默认使用SS
         mov ebx,core_buf                   ; 512字节的内核缓冲区 用于读取程序头部数据
         call flat_4gb_code_seg_sel:read_hard_disk_0  ; 将一个扇区加载到了内核缓冲区

         ;以下判断整个程序有多大
         mov eax,[core_buf]                 ; 程序尺寸
         mov ebx,eax
         and ebx,0xfffff000                 ; 使之4KB对齐 
         add ebx,0x1000                     ; 相当于相对于4KB向上取整了   
         test eax,0x00000fff                ; 程序的大小正好是4KB的倍数吗? 
         cmovnz eax,ebx                     ; 不是。使用凑整的结果

         mov ecx,eax
         shr ecx,12                         ; 程序占用的总4KB页数 
         
         mov eax,[ebp+40]                   ; 起始扇区号
         mov esi,[ebp+36]                   ; 从堆栈中取得TCB的基地址
  .b2:
         alloc_user_linear                  ; 宏：在用户任务地址空间上分配内存 
                                            ; 返回 ebx 此次分配的页的线性地址
         
         push ecx
         mov ecx,8
  .b3:
         call flat_4gb_code_seg_sel:read_hard_disk_0               
         inc eax
         loop .b3                           ; 每分配4KB的页 读进来8个512B扇区

         pop ecx
         loop .b2

         ; 在内核地址空间内创建用户任务的TSS
         alloc_core_linear                  ;宏：在内核的地址空间上分配内存
                                            ;用户任务的TSS必须在全局空间上分配 
         
         mov [esi+0x14],ebx                 ; 在TCB中填写TSS的线性地址 
         mov word [esi+0x12],103            ; 在TCB中填写TSS的界限值 
          
         ;在用户任务的局部地址空间内创建LDT 
         alloc_user_linear                  ;宏：在用户任务地址空间上分配内存

         mov [esi+0x0c],ebx                 ; 填写LDT线性地址到TCB中 

         ;建立程序代码段描述符
         mov eax,0x00000000                 ; 段线性基地址
         mov ebx,0x000fffff                 ; 段界限       
         mov ecx,0x00c0f800                ; 属性 G=1 D/B=1 P=1 DPL=11 S=1 TYPE=1000
                                            ; 4KB粒度的代码段描述符(意味着寻址空间到4GB，平坦模型)，特权级3
                                            ; 注意 这是安装在LDT中的段描述符(和GDT中的格式一样) 但不是LDT描述符(安装在GDT中的指示LDT的描述符)
         call flat_4gb_code_seg_sel:make_seg_descriptor
         mov ebx,esi                        ; TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ; 设置选择子的RPL为3
         
         mov ebx,[esi+0x14]                 ; 从TCB中获取TSS的线性地址
         mov [ebx+76],cx                    ; 填写TSS的CS域 填入程序代码段描述符

         ;建立程序数据段描述符
         mov eax,0x00000000                 ; 段线性基地址
         mov ebx,0x000fffff                 ; 段界限
         mov ecx,0x00c0f200                ; 属性 G=1 D/B=1 P=1 DPL=11 S=1 TYPE=0010
                                            ; 4KB粒度的数据段描述符，特权级3
         call flat_4gb_code_seg_sel:make_seg_descriptor
         mov ebx,esi                        ; TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ; 设置选择子的RPL为3
         
         mov ebx,[esi+0x14]                 ; 从TCB中获取TSS的线性地址
         mov [ebx+84],cx                    ; 填写TSS的DS域 填入程序数据段描述符
         mov [ebx+72],cx                    ; 填写TSS的ES域 填入程序数据段描述符
         mov [ebx+88],cx                    ; 填写TSS的FS域 填入程序数据段描述符
         mov [ebx+92],cx                    ; 填写TSS的GS域 填入程序数据段描述符
         
         ;将数据段作为用户任务的3特权级固有堆栈 
         mov [ebx+80],cx                    ; 填写TSS的SS域 填入程序数据段描述符
         alloc_user_linear                  ;宏：在用户任务地址空间上分配内存
                                            ; 返回 ebx 刚才分配的页的线性地址
                                            ; 但是由于这个页用作栈 是向下拓展的 需要的是这个页的高端线性地址
         
         mov ebx,[esi+0x14]                 ; 从TCB中获取TSS的线性地址
         mov edx,[esi+0x06]                 ; TCB中的程序加载基地址 即刚才分配的堆栈的高端线性地址+1
         mov [ebx+56],edx                   ; 填写TSS的ESP域 填入堆栈的高端线性地址+1

         ;在用户任务的局部地址空间内创建0特权级堆栈
         alloc_user_linear                  ;宏：在用户任务地址空间上分配内存
                                            ; 此时TCB的程序加载基地址中存储了这个栈的高端线性地址+1 即栈顶

         mov eax,0x00000000                 ; 段线性基地址
         mov ebx,0x000fffff                 ; 段界限
         mov ecx,0x00c09200                 ; 4KB粒度的堆栈段描述符，特权级0
         call flat_4gb_code_seg_sel:make_seg_descriptor
         mov ebx,esi                        ; TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0000B         ; 设置选择子的RPL为0

         mov ebx,[esi+0x14]                 ; 从TCB中获取TSS的线性地址
         mov [ebx+8],cx                     ; 填写TSS的SS0域 填入程序数据段描述符
         mov edx,[esi+0x06]                 ; TCB中的程序加载基地址 堆栈的高端线性地址
         mov [ebx+4],edx                    ; 填写TSS的ESP0域 

         ;在用户任务的局部地址空间内创建1特权级堆栈
         alloc_user_linear                  ; 宏：在用户任务地址空间上分配内存

         mov eax,0x00000000
         mov ebx,0x000fffff
         mov ecx,0x00c0b200                 ; 4KB粒度的堆栈段描述符，特权级1
         call flat_4gb_code_seg_sel:make_seg_descriptor
         mov ebx,esi                        ; TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0001B         ; 设置选择子的特权级为1

         mov ebx,[esi+0x14]                 ; 从TCB中获取TSS的线性地址
         mov [ebx+16],cx                    ; 填写TSS的SS1域
         mov edx,[esi+0x06]                 ; 堆栈的高端线性地址
         mov [ebx+12],edx                   ; 填写TSS的ESP1域 

         ;在用户任务的局部地址空间内创建2特权级堆栈
         alloc_user_linear                  ; 宏：在用户任务地址空间上分配内存

         mov eax,0x00000000
         mov ebx,0x000fffff
         mov ecx,0x00c0d200                 ; 4KB粒度的堆栈段描述符，特权级2
         call flat_4gb_code_seg_sel:make_seg_descriptor
         mov ebx,esi                        ; TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0010B         ; 设置选择子的特权级为2

         mov ebx,[esi+0x14]                 ; 从TCB中获取TSS的线性地址
         mov [ebx+24],cx                    ; 填写TSS的SS2域
         mov edx,[esi+0x06]                 ; 堆栈的高端线性地址
         mov [ebx+20],edx                   ; 填写TSS的ESP2域 

         ;重定位U-SALT 
         cld                                ; DF=0 正向

         ; 用户程序是从其虚拟地址空间的开始处0x00000000加载的
         ; 偏移量为0x0000000c和0x00000008的地方分别时U-SALT表的条目数及其线性地址
         mov ecx,[0x0c]                     ; U-SALT条目数 外层循环数
         mov edi,[0x08]                     ; U-SALT在4GB空间内的偏移 
  .b4:
         push ecx
         push edi
      
         mov ecx,salt_items                 ; C-SALT条目数 内层循环次数
         mov esi,salt                       ; C-SALT线性地址
  .b5:
         push edi
         push esi
         push ecx

         mov ecx,64                         ; 每条目的比较次数 每次比较4字节 总共也就是比较256字节 即符号名长度
         repe cmpsd                         ; cmpsd 比较DS:ESI和ES:EDI的双字
                                            ; repe 直到ECX=0 或 ZF=0
                                            ; 这里DS ES都是从0x00000000开始的4GB段
         jnz .b6                            ; ZF=0 不匹配则跳转
         mov eax,[esi]                      ; 若匹配，则esi恰好指向C-SALT后的偏移地址 edi指向U-SALT表该符号后一位
         mov [edi-256],eax                  ; 将U-SALT中的字符串改写成偏移地址 
         mov ax,[esi+4]                     ; esi+4指向C-SALT后的代码段选择子
         or ax,0000000000000011B            ; 以用户程序自己的特权级使用调用门
                                            ; 故RPL=3 
         mov [edi-252],ax                   ; 回填调用门选择子 填在偏移地址之后
  .b6:
         pop ecx
         pop esi
         add esi,salt_item_len              ; esi加上条目长度指向C-SALT下一个条目
         pop edi                            ; 从头比较 
         loop .b5                           ; 循环C-SALT条目数次
      
         pop edi
         add edi,256
         pop ecx
         loop .b4                           ; 循环U-SALT条目数次

         ;在GDT中登记LDT描述符
         mov esi,[ebp+36]                   ; esi=从堆栈中取得TCB的基地址
         mov eax,[esi+0x0c]                 ; TCB中取得LDT的起始线性地址
         movzx ebx,word [esi+0x0a]          ; TCB中取得LDT段界限
         mov ecx,0x00008200                 ; LDT描述符属性 G=0 P=1 DPL=00 TYPE=0010
                                            ; 本来是0x00408200 D=1 但LDT描述符D应该为0 TODO
         call flat_4gb_code_seg_sel:make_seg_descriptor
         call flat_4gb_code_seg_sel:set_up_gdt_descriptor  ; 返回cx=描述符选择子
         mov [esi+0x10],cx                  ; 登记LDT描述符选择子到TCB中

         mov ebx,[esi+0x14]                 ; ebx=从TCB中获取TSS的线性地址
         mov [ebx+96],cx                    ; 填写TSS的LDT域 

         mov word [ebx+0],0                 ; TSS的前一个任务指针=0 用于指向嵌套的前一个任务的TSS描述符选择子
      
         mov dx,[esi+0x12]                  ; TCB中的TSS界限值
         mov [ebx+102],dx                   ; 将TSS界限值填入TSS的IO许可位串 以表示没有IO许可位串
      
         mov word [ebx+100],0               ; 填写TSS中的T=0
      
         mov eax,[0x04]                     ; 从任务的4GB地址空间获取入口点 
         mov [ebx+32],eax                   ; 填写TSS的EIP域 

         pushfd                             ; 将EFLAGS压栈
         pop edx                            ; 将EFLAGS的值出栈到edx
         mov [ebx+36],edx                   ; 填写TSS的EFLAGS域 

         ;在GDT中登记TSS描述符
         mov eax,[esi+0x14]                 ; 从TCB中获取TSS的起始线性地址
         movzx ebx,word [esi+0x12]          ; TCB中的TSS界限值
         mov ecx,0x00008900                 ; TSS描述符属性 G=0 P=1 DPL=00 TYPE=1001(B=0)，特权级0
                                            ; 本来是0x00408900 D=1，但TSS描述符应该D=0
         call flat_4gb_code_seg_sel:make_seg_descriptor
         call flat_4gb_code_seg_sel:set_up_gdt_descriptor
         mov [esi+0x18],cx                  ; 登记TSS选择子到TCB

         ;创建用户任务的页目录
         ; 当前的页目录表是借用的内核页目录表 用户任务必须有自己的页目录表 为此申请一个空闲页作为用户任务的页目录表 并将当前页目录表的内容复制过去
         ; 注意！页的分配和使用是由页位图决定的，可以不占用线性地址空间 
         call flat_4gb_code_seg_sel:create_copy_cur_pdir    ; 返回 eax=新页目录的物理地址
         mov ebx,[esi+0x14]                 ; 从TCB中获取TSS的线性地址
         mov dword [ebx+28],eax             ; 填写TSS的CR3(PDBR)域 填入新页目录表的物理地址

         popad
      
         ret 8                              ;丢弃调用本过程前压入的参数 
                                            ; 返回时弹出8字节数据
      
;-------------------------------------------------------------------------------
append_to_tcb_link:                         ;在TCB链上追加任务控制块
                                            ;输入：ECX=TCB线性基地址
         cli                                ; 时钟中断随时可能发生 也要操作TCB链 所以要关中断
         
         push eax
         push ebx

         mov eax,tcb_chain                  ; tcb_chain 链表头
  .b0:                                      ; EAX=链表头或当前TCB线性地址
         mov ebx,[eax]                      ; EBX=下一个TCB线性地址
         or ebx,ebx
         jz .b1                             ; 链表为空，或已到末尾
         mov eax,ebx                        ; 定位到下一个TCB（的线性地址）
         jmp .b0

  .b1:
         mov [eax],ecx                      ; eax 是最后一个TCB的线性地址，让最后一个TCB指向新TCB
         mov dword [ecx],0x00000000         ; 当前TCB指针域清零，以指示这是最后一个TCB
         pop ebx
         pop eax
         
         sti
         
         ret
         
;-------------------------------------------------------------------------------
start:
         ;创建中断描述符表IDT
         ;在此之前，禁止调用put_string过程，以及任何含有sti指令的过程。
          
         ; 前20个向量是处理器异常使用的
         ; intel 定义了保护模式下的中断和异常向量分配 0-19号是已定义的中断类型 20-31 保留不用 32-255 用户自定义中断
         ; 将通用异常处理程序注册到中断描述符表
         mov eax,general_exception_handler  ; 传参 门代码在段内偏移地址
         mov bx,flat_4gb_code_seg_sel       ; 传参 门代码所在段的选择子
         mov cx,0x8e00                      ; 传参 段类型和属性等 P=1 DPL=00 D=1(32位门)
         call flat_4gb_code_seg_sel:make_gate_descriptor ;返回 EDX:EAX=完整的描述符

         mov ebx,idt_linear_address         ; 中断描述符表的线性地址
         xor esi,esi
         ; IDT 第一个描述符是有效的
  .idt0:
         mov [ebx+esi*8],eax
         mov [ebx+esi*8+4],edx
         inc esi
         cmp esi,19                         ; 安装前20个异常中断处理过程，都安装成一样的通用异常处理程序
         jle .idt0

         ;其余为保留或外部硬件中断
         ; 注意这里安装的是通用中断处理程序 上面是通用异常处理程序
         mov eax,general_interrupt_handler  ; 门代码在段内偏移地址
         mov bx,flat_4gb_code_seg_sel       ; 门代码所在段的选择子
         mov cx,0x8e00                      ; 32位中断门，0特权级
         call flat_4gb_code_seg_sel:make_gate_descriptor

         mov ebx,idt_linear_address         ; 中断描述符表的线性地址
  .idt1:
         mov [ebx+esi*8],eax
         mov [ebx+esi*8+4],edx
         inc esi
         cmp esi,255                        ; 安装普通的中断处理过程
         jle .idt1

         ;设置实时时钟中断处理过程
         mov eax,rtm_0x70_interrupt_handle  ; 门代码在段内偏移地址
         mov bx,flat_4gb_code_seg_sel       ; 门代码所在段的选择子
         mov cx,0x8e00                      ; 32位中断门，0特权级
         call flat_4gb_code_seg_sel:make_gate_descriptor

         mov ebx,idt_linear_address         ; 中断描述符表的线性地址
         mov [ebx+0x70*8],eax               ; 将实时时钟中断安装为0x70号中断
         mov [ebx+0x70*8+4],edx

         ;准备开放中断
         mov word [pidt],256*8-1            ; 将IDT的界限写入pidt处的字单元
         mov dword [pidt+2],idt_linear_address  ; 写入IDT的线性基地址
         lidt [pidt]                        ; 将IDT的界限和基地址加载到中断描述符表寄存器IDTR
                                            ; 一旦加载了IDTR 处理器的中断机制就开始起作用了

         ; 需要重新初始化 8259A，因为其主片的中断向量和处理器的异常向量冲突？计算机启动后，主片的中断向量为0x08-0x0F 从片的中断向量是0x70-0x77
         ; 在32位处理器上 0x08-0x0F 已经被处理器用作异常向量(前面注册的前20个中断向量)
         ; 好在 8259A是可编程的，可以把它的中断向量改成0x20-0x27(32-255是用户自定义中断向量)
         ; 设置8259A中断控制器主片
         ; ICW Initialize Command Word
         mov al,0x11                        ; ICW1 设置中断请求触发方式和级联的芯片数量：边沿触发 有多个级联(需要ICW3) 本次初始化需要ICW4
         out 0x20,al                        ; 主片端口号
                                            ; 8259A接到ICW1时，意味着一个新的初始化过程开始了 期待从0x21端口接收ICW2
         mov al,0x20                        ; ICW2 设置每个芯片的中断向量: 起始中断向量为0x20 8个引脚对应着0x20-0x27
         out 0x21,al                        ; 主片端口号
         mov al,0x04                        ; ICW3 指定用哪个引脚实现芯片级联: 主片的IR2(第三个引脚)和从片级联
         out 0x21,al                        
         mov al,0x01                        ; ICW4 控制芯片工作方式: 非自动结束方式 要求在中断处理过程中明确地向8259A写中断结束命令EOI 非总线缓冲，全嵌套，正常EOI
         out 0x21,al                        

         ; 设置8259A从片 同上
         mov al,0x11
         out 0xa0,al                        ;ICW1：边沿触发/级联方式
         mov al,0x70
         out 0xa1,al                        ;ICW2:起始中断向量 0x70-0x7F
         mov al,0x04
         out 0xa1,al                        ;ICW3:从片级联到IR2
         mov al,0x01
         out 0xa1,al                        ;ICW4:非总线缓冲，全嵌套，正常EOI

         ;设置和时钟中断相关的硬件 
         mov al,0x0b                        ; RTC寄存器B
         or al,0x80                         ; 阻断NMI
         out 0x70,al
         mov al,0x12                        ; 设置寄存器B，禁止周期性中断，开放更
         out 0x71,al                        ; 新结束后中断，BCD码，24小时制

         in al,0xa1                         ; 读8259从片的IMR寄存器
         and al,0xfe                        ; 清除bit 0(此位连接RTC)
         out 0xa1,al                        ; 写回此寄存器

         mov al,0x0c
         out 0x70,al
         in al,0x71                         ; 读RTC寄存器C，复位未决的中断状态

         sti                                ; 开放硬件中断
                                            ; 目前已经可能发生时钟周期中断 但因为TCB链为空 所以只会给8259A发送EOI 读一下RTC的寄存器C 然后返回

         mov ebx,message_0
         call flat_4gb_code_seg_sel:put_string

         ; 一般情况下 处理器都支持cpuid指令 因此下面这段完全可以不要 学习用有也没关系
         ; 通过看ID位是否可更改判断是否支持cpuid指令
         pushfd                             ; push eflags
         pop eax                            ; 将eflags存进eax
         mov ebx,eax
         xor eax,0x00200000                 ; 将ID位置1
         push eax
         popfd                              ; 写回eflags
         pushfd                             ; 再次读取
         pop eax
         cmp eax,ebx
         jz .skip_cpuid                     ; 修改后和修改前一样 无法修改则不支持cpuid指令

         ; 0号功能 显示处理器供应商信息 GenuineIntel
         mov eax,0
         cpuid
         mov [cpu_vendor + 0x00],ebx
         mov [cpu_vendor + 0x04],edx
         mov [cpu_vendor + 0x08],ecx
         mov ebx,cpu_vendorHead
         call flat_4gb_code_seg_sel:put_string
         mov ebx,cpu_vendor
         call flat_4gb_code_seg_sel:put_string
         mov ebx,cpu_vendorTail
         call flat_4gb_code_seg_sel:put_string

        ; 80000000号功能 探测处理器最大能支持的功能号
         mov eax,0x80000000
         cpuid
         ; 判断处理器是否支持到80000004号功能
         cmp eax,0x80000004                          ; eax中返回的是最大可支持的功能号 0x80000008
         jl .skip_cpuid                              ; 不支持 则跳过功能调用

         ;80000002-4 号功能 显示处理器品牌信息
         ; Intel(R) Core(TM)  i5-8300H CPU @ 2.30GHZ
         mov eax,0x80000002
         cpuid
         mov [cpu_brand + 0x00],eax
         mov [cpu_brand + 0x04],ebx
         mov [cpu_brand + 0x08],ecx
         mov [cpu_brand + 0x0c],edx

         mov eax,0x80000003
         cpuid
         mov [cpu_brand + 0x10],eax
         mov [cpu_brand + 0x14],ebx
         mov [cpu_brand + 0x18],ecx
         mov [cpu_brand + 0x1c],edx

         mov eax,0x80000004
         cpuid
         mov [cpu_brand + 0x20],eax
         mov [cpu_brand + 0x24],ebx
         mov [cpu_brand + 0x28],ecx
         mov [cpu_brand + 0x2c],edx

         mov ebx,cpu_brandHead                  ;显示处理器品牌信息 
         call flat_4gb_code_seg_sel:put_string
         mov ebx,cpu_brand
         call flat_4gb_code_seg_sel:put_string
         mov ebx,cpu_brandTail
         call flat_4gb_code_seg_sel:put_string

         ;jmp $                             ; 上面的显示信息会被覆盖掉 需要查看可以取消注释这条命令
  .skip_cpuid:

         ;以下开始安装为整个系统服务的调用门。特权级之间的控制转移必须使用门
         ; 即为C-SALT表的每项创建调用门描述符，安装进GDT，然后将描述符选择子回填C-SALT
         mov edi,salt                       ; C-SALT表的起始位置 
         mov ecx,salt_items                 ;C-SALT表的条目数量 
  .b4:
         push ecx   
         mov eax,[edi+256]                  ; 该条目入口点的32位偏移地址 
         mov bx,[edi+260]                   ; 该条目入口点的段选择子 
         mov cx,1_11_0_1100_000_00000B      ; 调用门属性: P=1 DPL=11 TYPE=1100 参数个数0
                                            ; 特权级3的调用门(3以上的特权级才允许访问)，0个参数(因为用寄存器传递参数，而没有用栈) 
                                            ; 一般要用栈 TODO 学习如何用
         call flat_4gb_code_seg_sel:make_gate_descriptor ;返回：EDX:EAX=完整的描述符
         call flat_4gb_code_seg_sel:set_up_gdt_descriptor
         mov [edi+260],cx                   ; 将返回的门描述符选择子回填
         add edi,salt_item_len              ; 指向下一个C-SALT条目 
         pop ecx
         loop .b4

         ; 对门进行测试 
         mov ebx,message_1
         call far [salt_1+256]              ; 通过门显示信息(偏移量将被忽略) 
                                            ; 处理器用该选择子访问GDT 检查这个描述符 发现它是调用门描述符 则忽略32位偏移量
                                            ; 控制转移到描述符中指定的段选择子和段内偏移处

         ;jmp $

         ; 初始化创建内核任务(程序管理器任务)的任务控制块TCB core_tcb中程序基地址项存储的是下一个可分配的虚拟内存空间线性地址
         ; 这个TCB内存不是动态分配的 TODO 可以动态分配吗？
         mov word [core_tcb+0x04],0xffff    ;任务状态：忙碌
         mov dword [core_tcb+0x06],0x80100000  ; 程序加载基地址  
                                               ; 内核自己占据了1MB 0x80000000-0x800FFFFF，可继续分配的空间从0x80100000开始
                                               ; 内核虚拟空间的分配从这里开始。
         mov word [core_tcb+0x0a],0xffff    ; 登记LDT初始的界限到TCB中 这个值用不上因为内核任务没有LDT
         mov ecx,core_tcb
         call append_to_tcb_link            ; 将此TCB添加到TCB链中

         ; 为内核任务的TSS分配内存空间
         alloc_core_linear                  ; 宏：在内核的虚拟地址空间分配一个页的内存
                                            ; 返回ebx中存储的是分配的线性地址

         ;在内核任务的TSS中设置必要的项目
         mov word [ebx+0],0                 ; 反向链=0 前一个任务的指针=0 表明这是唯一的任务
         mov eax,cr3
         mov dword [ebx+28],eax             ; 登记CR3(PDBR)
         ; 偏移32-92是各个寄存器的快照 这里不用填 这个任务因为自从进入保护模式就开始运行了
         mov word [ebx+96],0                ; LDT段选择子=0 没有LDT。处理器允许没有LDT的任务。
         mov word [ebx+100],0               ; T=0 用于软件调试 若T=1每次切换到该任务时将引发一个调试异常中断 调试程序可以接管中断以显示任务状态
         mov word [ebx+102],103             ; IO映射基地址=103 若这个值大于等于TSS的段界限（在TSS描述符中，下面安装）则表明没有IO许可位串
                                            ; 没有I/O位图。0特权级事实上不需要 262 287
         
         ;创建内核任务的TSS描述符，并安装到GDT中
         mov eax,ebx                        ; TSS的起始线性地址
         mov ebx,103                        ; 段长度（界限） 界限值必须至少是103(因为前103字节都是已定义的，后面接可有可无的IO许可位串)
         mov ecx,0x00008900                 ; TSS描述符属性 G=0 D=0 L=0 AVL=0 P=1 DPL=00 S=0 TYPE=1001(B=0)
                                            ; 字节粒度
                                            ; 本来是0x00408900 D=1 但我看TSS描述符D位应该固定为0 TODO 不知道是不是它错了 但是功能不影响
         call flat_4gb_code_seg_sel:make_seg_descriptor ; 返回EDX:EAX描述符
         call flat_4gb_code_seg_sel:set_up_gdt_descriptor ; 返回CX描述符选择子
         mov [core_tcb+0x18],cx             ; 登记内核任务的TSS选择子到其TCB

         ; 任务寄存器TR中的内容是任务存在的标志，该内容也决定了当前任务是谁。
         ; 将TSS选择子加载到TR后 处理器用该选择子访问GDT中对应的TSS描述符 将段界限、段基地址、段属性加载到TR的描述符高速缓存器 同时处理器将该TSS描述符中的B位置1 也就是标志为忙
         ; 下面的指令为当前正在执行的0特权级任务 内核任务后补手续（TSS）
         ltr cx

         ;现在可认为内核任务正执行中 至此内核任务才名正言顺地成为一个合法的任务

         ;创建用户任务A的任务控制块TCB
         ; 为了能够访问和控制所有的任务，每个任务的TCB必须创建在内核的地址空间内
         alloc_core_linear                  ; 宏：在内核的虚拟地址空间分配内存
         
         mov word [ebx+0x04],0              ; 任务状态：空闲 
         mov dword [ebx+0x06],0             ; 程序加载基地址=0 用户任务局部空间的分配从0开始。
         mov word [ebx+0x0a],0xffff         ; LDT当前界限值 登记LDT初始的界限到TCB中
                                            ; LDT界限也是16位 只允许8192个描述符 新LDT的总字节数为0 所以界限值应该是0xffff(0-1)
      
         push dword 50                      ; 用户程序位于逻辑50扇区 TODO 用户程序应该是动态的 不应该写死在内核中
         push ebx                           ; 压入任务控制块起始线性地址 
         call load_relocate_program         ; 加载并重定位用户程序
         mov ecx,ebx         
         call append_to_tcb_link            ; 将此TCB添加到TCB链中


         ;创建用户任务B的任务控制块TCB
         alloc_core_linear                  ; 宏：在内核的虚拟地址空间分配内存

         mov word [ebx+0x04],0              ; 任务状态：空闲
         mov dword [ebx+0x06],0             ; 用户任务局部空间的分配从0开始。
         mov word [ebx+0x0a],0xffff         ; 登记LDT初始的界限到TCB中

         push dword 100                     ; 用户程序位于逻辑100扇区 TODO 动态定义
         push ebx                           ; 压入任务控制块起始线性地址
         call load_relocate_program         ; 加载并重定位用户程序
         mov ecx,ebx
         call append_to_tcb_link            ; 将此TCB添加到TCB链中

  .core:
         mov ebx,core_msg0
         call flat_4gb_code_seg_sel:put_string
         
         ;这里可以编写回收已终止任务内存的代码
          
         jmp .core
            
core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
