all:
	bash -c "bash ./build.sh"

clean:
	rm -rf ./bin

img:
	# create raw img
	dd if=/dev/zero of=myos.img bs=512 count=2880 # 1440 KB
	# make fs
	mkfs -t vfat -F 12 myos.img
mount:
