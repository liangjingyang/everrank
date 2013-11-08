#!/bin/bash
IP=192.168.1.255
cd /data/croods/server
tar czf server.tar.gz config deps ebin logs script
scp server.tar.gz $IP:/data/croods/server/
ssh $IP "cd /data/croods/server/; pkill beam; tar xzf server.tar.gz; cd script; ./start.sh" 
