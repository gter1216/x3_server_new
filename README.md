This project is used to simulate X3 client and X3 server by Erlang.

Author: Xu Xiao, 2017

Usage:

1. How to start X3 server

1.1 Firstly, configure the config.txt file.

[50000,50001].              --> the listening port of X3 server
"log1".                     --> basic log level
"/home/.../xx.log".         --> log file path
"/home/.../msg_dump_file"   --> the file contains the msg count
"/home/.../rtp_dump.pcap"   --> RTP pcap file from CC msg    
 
1.2 cd bin && ./x3_server.sh start

2. How to stop X3 server

./x3_server.sh stop

3. How to restart X3 server

./x3_server.sh restart

4. How to run X3 server in debug mode

./x3_server.sh start debug

5. How to check msg count

./msg_dump.sh

6. How to start generate RTP pcap file for each CC msg.

./rtp_dump.sh start

7. How to stop generate RTP pcap file for each CC msg.

./rtp_dump.sh stop





