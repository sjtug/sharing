#!/bin/sh

while true; do
	/usr/games/fortune
	sleep 2
done
[root@centos-demo lazyfortune]# cat lazyfortune-log.sh 
#!/bin/sh

/opt/lazyfortune.sh | tee /mnt/log.txt
