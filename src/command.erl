%% @author xxu
%% @copyright 2017 Nokia, Inc
%% This module used to dump msg statistic data


-module(command).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([stop_server/0,
		 start_msg_dump/0,
		 stop_msg_dump/0,
		 show_msg_dump/0,
		 check_msg_dump/0,
		 start_rtp_dump/0,
		 stop_rtp_dump/0,
		 check_rtp_dump/0]).

%% ==================================================================
%% stop_server
%% 
%% ==================================================================
stop_server() ->
	{ok, Terms} = file:consult("../bin/config.txt"),
	
	[_StartPortList, StopPortList, Ipv4Addr, _Ipv6Addr, SelfPort, _LogLevel] = Terms,
	
	Command = {stop_server, StopPortList},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.

	case gen_tcp:connect(Ipv4Addr,SelfPort,[binary,{packet,0}]) of
		{ok, Socket} ->
			%% send message
            ok = gen_tcp:send(Socket, Bytes),
			%% close the socket
 	        ok = gen_tcp:close(Socket),
			case StopPortList of
				{all, all} ->
					io:format("~nShut Down X3 Server!~n");
				_->
					io:format("~nShut port: ~p! Other port still listening!~n", [StopPortList])
			end,
			init:stop();
		{error,econnrefused} ->
			io:format("~nX3 Server Not Running or Already Shutdown!~n"),
			init:stop()
	end.	
	
 

%% ==================================================================
%% start_msg_dump
%% 
%% ==================================================================
start_msg_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_StartPortList, _StopPortList, Ipv4Addr, _Ipv6Addr, SelfPort, _LogLevel] = Terms,
	
    Command = {start_dump_msg, #msg_dump{}},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(Ipv4Addr, 
								   SelfPort, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
    %% receive message

	receive
		{tcp, Socket, Bin} ->
			Ack = binary_to_term(Bin),
			{ok, _} = Ack
	end,
	
    io:format("~nstart statistic msg~n"),

	%% close the socket
 	ok = gen_tcp:close(Socket),
	
	init:stop().


%% ==================================================================
%% show_msg_dump
%% 
%% ==================================================================
show_msg_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_StartPortList, _StopPortList, Ipv4Addr, _Ipv6Addr, SelfPort, _LogLevel] = Terms,
	
    Command = {show_dump_msg, #msg_dump{}},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(Ipv4Addr, 
								   SelfPort, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
    %% receive message

	receive
		{tcp, Socket, Bin} ->
			Ack = binary_to_term(Bin),
			{ok, #msg_dump{msg_dump_table=MsgDumpTable}} = Ack
	end,
	
    io:format("~nmsg dump table is:~n~p~n", [MsgDumpTable]),

	%% close the socket
 	ok = gen_tcp:close(Socket),
	
	init:stop().

%% ==================================================================
%% stop_msg_dump
%% 
%% ==================================================================
stop_msg_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_StartPortList, _StopPortList, Ipv4Addr, _Ipv6Addr, SelfPort, _LogLevel] = Terms,
	
    Command = {stop_dump_msg, #msg_dump{}},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(Ipv4Addr, 
								   SelfPort, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
	io:format("~nmsg statistic stopped!~n"),

	%% close the socket
 	ok = gen_tcp:close(Socket),
	
	init:stop().

%% ==================================================================
%% check_msg_dump
%% 
%% ==================================================================
check_msg_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_StartPortList, _StopPortList, Ipv4Addr, _Ipv6Addr, SelfPort, _LogLevel] = Terms,
	
    Command = {check_dump_msg, #msg_dump{}},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(Ipv4Addr, 
								   SelfPort, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
	receive
		{tcp, Socket, Bin} ->
			Ack = binary_to_term(Bin),
			{ok, #msg_dump{msg_dump_state=MsgDumpState}} = Ack
	end,
	
	io:format("~nmsg dump state is ~p~n",[MsgDumpState]),

	%% close the socket
 	ok = gen_tcp:close(Socket),
	
	init:stop().
 
    
%% ==================================================================
%% start_rtp_dump
%% 
%% ==================================================================
start_rtp_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_StartPortList, _StopPortList, Ipv4Addr, _Ipv6Addr, SelfPort, _LogLevel] = Terms,
    
    Command = {start_dump_rtp, #rtp_dump{}},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(Ipv4Addr, 
								   SelfPort, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
	io:format("~ndump payload file started ~n"),
	
	%% receive message

	receive
		{tcp, Socket, Bin} ->
			Ack = binary_to_term(Bin),
			{ok, #rtp_dump{rtp_dump_dir=RtpDumpDir}} = Ack
	end,
	
    io:format("~ndump payload started, the payload dir ~p is generated ~n", [RtpDumpDir]),

	%% close the socket
 	ok = gen_tcp:close(Socket),
	
	init:stop().

%% ==================================================================
%% stop_rtp_dump
%% 
%% ==================================================================
stop_rtp_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_StartPortList, _StopPortList, Ipv4Addr, _Ipv6Addr, SelfPort, _LogLevel] = Terms,
	
    Command = {stop_dump_rtp, #rtp_dump{}},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(Ipv4Addr, 
								   SelfPort, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
    io:format("~ndump payload file stopped ~n"),

	%% close the socket
 	ok = gen_tcp:close(Socket),
	
	init:stop().


%% ==================================================================
%% check_rtp_dump
%% 
%% ==================================================================
check_rtp_dump() ->
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[_StartPortList, _StopPortList, Ipv4Addr, _Ipv6Addr, SelfPort, _LogLevel] = Terms,
	
    Command = {check_dump_rtp, #rtp_dump{}},
	
	Bytes = term_to_binary(Command),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(Ipv4Addr, 
								   SelfPort, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
	receive
		{tcp, Socket, Bin} ->
			Ack = binary_to_term(Bin),
			{ok, #rtp_dump{rtp_dump_state=RtpDumpState,
						   rtp_dump_dir=RtpDumpDir}} = Ack
	end,
	
	io:format("~nrtp dump state is ~p~nrtp dump dir is ~p~n",[RtpDumpState,RtpDumpDir]),

	%% close the socket
 	ok = gen_tcp:close(Socket),
	
	init:stop().



