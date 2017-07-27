This project is used to simulate X3 client and X3 server by Erlang.

Author: Xu Xiao, 2017

Usage:

1. How to start X3 server

1.1 Firstly, configure the config.txt file.
 
1.2 cd bin && ./x3_server.sh start

2. How to stop X3 server

./x3_server.sh stop

3. How to stop all X3 server

./x3_server.sh stop all

4. How to collect msg statistics log

./msg_dump.sh start
./msg_dump.sh stop

5. How to generate RTP pcap file for each CC msg.

./rtp_dump.sh start
./rtp_dump.sh stop





