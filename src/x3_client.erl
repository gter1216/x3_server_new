%% @author xxu
%% @copyright 2017 Nokia, Inc
%% This module used to implement X3 client behaviour,
%% i.e. simulate as SBC 7510.


-module(x3_client).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([test_case_001/0, 
		 test_case_001/4,
		 test_case_002/0,
		 test_case_002/3,
		 test_case_003/0
		 ]).


%% ==================================================================
%% test_case_001
%% 
%% send create_lict_req message from SBC to LIC server.
%%
%%       client                  server
%%              create lict req
%%         ----------------------->
%%
%%              create lict ack
%%         <-----------------------
%%
%%
%% ==================================================================
test_case_001() ->
	
	%% assign default value
	MsgSerialNo = 213,
	NeId = "PGW168",
    ICIDValue = "cscf-20170627152245",
    CCCId = 10156,
	
	test_case_001(MsgSerialNo, NeId, ICIDValue, CCCId).

test_case_001(MsgSerialNo, NeId, ICIDValue, CCCId) ->
	
    CreateLICTReq = #'CreateLICTReq'{messageSerialNo = MsgSerialNo,
									 neID = NeId,
									 icidValue = ICIDValue,
									 'cCC-ID' = CCCId},
	
	X3CmdMessage = {createLictReq, CreateLICTReq},
	
	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
	
	io:format("xxx msg byte is ~w~n", [Bytes]),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
								   ?x3_server_port, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	ok = gen_tcp:send(Socket, Bytes),
	ok = gen_tcp:send(Socket, Bytes),
	
	ok = gen_tcp:close(Socket).
%% 
%% 	%% connect to LIC server and return the socket.
%%     {ok, Socket2} = gen_tcp:connect(?x3_server_ip, 
%% 								   ?x3_server_port, 
%% 								   [binary, {packet, 0}]),
%% 	
%% 	DeleteLICTReq = #'DeleteLICTReq'{messageSerialNo = MsgSerialNo,
%% 									 icidValue = ICIDValue,
%% 									 'cCC-ID' = CCCId},
%% 	
%% 	X3CmdMessage2 = {deleteLictReq, DeleteLICTReq},
%% 	
%% 	Bytes2 = x3_lib:encode_x3_interface_msg(X3CmdMessage2),
%% 	
%% 	io:format("msg byte is ~w~n", [Bytes2]),
%% 	
%% 	%% send message
%% 	ok = gen_tcp:send(Socket2, Bytes2).

%% ==================================================================
%% test_case_002
%% 
%% send delete_lict_req message from SBC to LIC server.
%%
%%       client                  server
%%              delete lict req
%%         ----------------------->
%%
%%              delete lict ack
%%         <-----------------------
%%
%% ==================================================================
test_case_002() ->
	
	%% assign default value
	MsgSerialNo = 213,
    ICIDValue = "cscf-20170627152245",
%%     CCCId = 10156,
%%     CCCId = 4294967296,
    CCCId = 10156,
	
	test_case_002(MsgSerialNo, ICIDValue, CCCId).

test_case_002(MsgSerialNo, ICIDValue, CCCId) ->
	
    DeleteLICTReq = #'DeleteLICTReq'{messageSerialNo = MsgSerialNo,
									 icidValue = ICIDValue,
									 'cCC-ID' = CCCId},
	
	X3CmdMessage = {deleteLictReq, DeleteLICTReq},
	
	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
	
	io:format("msg byte is ~w~n", [Bytes]),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(?x3_server_ipv6, 
								   ?x3_server_port_ipv6, 
								   [binary,
									inet6, 
									{packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
	%% close the socket
 	ok = gen_tcp:close(Socket).

%% ==================================================================
%% test_case_003
%% 
%% send check_state_req message from SBC to LIC server.
%%
%%       client                  server
%%              check state req
%%         ----------------------->
%%
%%              check state ack
%%         <-----------------------
%%
%% ==================================================================
test_case_003() ->
	
%%     CheckStateReq = #'X3CheckStateReq'{neID = NeId},
%% 	
%% 	X3CmdMessage = {x3CheckStateReq, CheckStateReq},
%% 	
%% 	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),

    %% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
								   ?x3_server_port, 
								   [binary, {packet, 0}]),

	Bytes1 = <<0,0,247,0,247,48,129,244,128,1,2,161,129,238,164,129,235,128,2,1,235,129,24,77,117,
			   108,116,105,112,108,101,95,76,73,67,84,95,84,117,110,110,101,108,95,48,48,50,130,1,107,
			   131,1,1,164,129,196,48,129,193,128,5,97,117,100,105,111,129,1,17,130,129,180,126,128,195,
			   82,0,180,0,0,128,8,28,125,0,11,212,196,49,42,164,44,142,167,163,167,142,14,39,35,39,14,142,
               167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,
               39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,
               142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,
               167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,
               39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,
               142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14>>,
	
	io:format("msg byte1 is ~w~n", [Bytes1]),
	
	gen_tcp:send(Socket, Bytes1),
	
	timer:sleep(500),

	Bytes2 = <<170,0,0,247,0,247,48,129,244,128,1,2,161,129,238,164,129,235,128,2,1,235,129,24,77,117,
			   108,116,105,112,108,101,95,76,73,67,84,95,84,117,110,110,101,108,95,48,48,50,130,1,107,
			   131,1,1,164,129,196,48,129,193,128,5,97,117,100,105,111,129,1,17,130,129,180,126,128,195,
			   82,0,180,0,0,128,8,28,125,0,11,212,196,49,42,164,44,142,167,163,167,142,14,39,35,39,14,142,
               167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,
               39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,
               142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,
               167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,
               39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14,142,167,163,167,
               142,14,39,35,39,14,142,167,163,167,142,14,39,35,39,14>>,
	
	timer:sleep(200),
	
	io:format("msg byte2 is ~w~n", [Bytes2]),
	
	gen_tcp:send(Socket, Bytes2),
	
	gen_tcp:close(Socket).


%% ====================================================================
%% Internal functions
%% ====================================================================
%% send_bytes(Bytes) ->
%% 	
%% 	%% connect to LIC server and return the socket.
%%     {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
%% 								   ?x3_server_port, 
%% 								   [binary, {packet, 0}]),
%% 	
%% 	%% send message
%% 	ok = gen_tcp:send(Socket, Bytes),
%% 	
%% 	%% close the socket
%%  	ok = gen_tcp:close(Socket).

%% %% ==================================================================
%% %% 
%% %% send_message:
%% %%
%% %% client send message to LIC server by TCP socket.
%% %%
%% %% ==================================================================
%% send_message(Packet) ->
%% 	
%%     %% connect to LIC server and return the socket.
%% 
%%     {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
%% 								   ?x3_server_port, 
%% 								   [binary, {packet, 0}]),
%% 
%%     %% send message by the socket.
%% 
%%     ok = gen_tcp:send(Socket, Packet)













