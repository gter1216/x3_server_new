%% @author xxu
%% @copyright 2017 Nokia, Inc
%% This module used to dump msg statistic data


-module(command).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_msg_dump/0,
		 start_rtp_dump/0,
		 stop_rtp_dump/0]).

%% ==================================================================
%% start_msg_dump
%% 
%% ==================================================================
start_msg_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_PortList, _LogLevel, _LogFile, MsgDumpFile] = Terms,
    
	
    Command = {dump_msg, #msg_dump{msg_dump_file = MsgDumpFile}},
	
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
 
    
%% ==================================================================
%% start_rtp_dump
%% 
%% ==================================================================
start_rtp_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_PortList, _LogLevel, _LogFile, _MsgDumpFile, RtpDumpFile] = Terms,
    
	
    Command = {start_dump_rtp, #rtp_dump{rtp_dump_file = RtpDumpFile}},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
								   ?x3_parent_server_port, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes).

%% 	%% close the socket
%%  	ok = gen_tcp:close(Socket).

%% ==================================================================
%% stop_rtp_dump
%% 
%% ==================================================================
stop_rtp_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_PortList, _LogLevel, _LogFile, MsgDumpFile] = Terms,
    
	
    Command = {stop_dump_rtp, #rtp_dump{}},
	
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






