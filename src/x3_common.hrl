%% @author xxu
%% @copyright 2017 Nokia, Inc
%% Definition of macros used by X3 client and X3 server.

-define(x3_server_ip, "135.251.216.181").

-define(x3_server_port, 10918).

-define(x3_parent_server_port, 31818).

-define(protocol_version, 'io2').

-record(rtp_dump, {rtp_dump_file}).

-record(msg_dump, {msg_dump_file}).
