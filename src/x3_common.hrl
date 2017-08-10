%% @author xxu
%% @copyright 2017 Nokia, Inc
%% Definition of macros used by X3 client and X3 server.

-define(x3_server_ip, "135.251.217.72").
-define(x3_server_ipv6, "7002::217:72").
-define(x3_server_port, 31101).
-define(x3_server_port_ipv6, 31102).
-define(protocol_version, 'io2').

-record(rtp_dump, {rtp_dump_dir,
				   rtp_dump_state}).
-record(msg_dump, {msg_dump_file,
				   msg_dump_state,
				   msg_dump_table}).
