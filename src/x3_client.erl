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
		 test_case_003/0,
		 test_case_003/1
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
	
	io:format("msg byte is ~w~n", [Bytes]),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
								   ?x3_server_port, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
	%% close the socket
 	ok = gen_tcp:close(Socket).

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
    {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
								   ?x3_server_port, 
								   [binary, {packet, 0}]),
	
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
	
	%% assign default value
	NeId = "PGW168",
	
	test_case_003(NeId).

test_case_003(NeId) ->
	
    CheckStateReq = #'X3CheckStateReq'{neID = NeId},
	
	X3CmdMessage = {x3CheckStateReq, CheckStateReq},
	
	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
	
	io:format("msg byte is ~w~n", [Bytes]),
	
	%% connect to LIC server and return the socket.
    {ok, Socket} = gen_tcp:connect(?x3_server_ip, 
								   ?x3_server_port, 
								   [binary, {packet, 0}]),
	
	%% send message
	ok = gen_tcp:send(Socket, Bytes),
	
	%% close the socket
 	ok = gen_tcp:close(Socket).


%% ====================================================================
%% Internal functions
%% ====================================================================


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













