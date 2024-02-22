qemu-system-i386 -nographic -drive file=floppy.img
qemu-system-i386 -nographic -boot order=a -fda floppy.img
