%% @author xxu
%% @copyright 2017 Nokia, Inc
%% This module used to dump msg statistic data


-module(msg_dump).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

%% ==================================================================
%% start
%% 
%% ==================================================================
start() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_PortList, _LogLevel, _LogFile, MsgDumpFile] = Terms,
    
	
    Command = #msg_dump{msg_dump_file = MsgDumpFile},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
								   ?x3_parent_server_port, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
    io:format("~n msg dump file generated ~p~n", [MsgDumpFile]).

%% 	%% close the socket
%%  	ok = gen_tcp:close(Socket).
 
    









