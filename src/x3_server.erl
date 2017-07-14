%% @author xxu <xiao.a.xu@alcatel-sbell.com.cn>
%% @copyright 2017 Nokia, Inc
%% This module used to implement X3 server behaviour,
%% i.e. simulate as LIC server.

-module(x3_server).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/1]).

-record(state, {pid_name,
				client_ip,
				client_port,
				log_level,
				log_file}).


%% ====================================================================
%% start serverl TCP server(LIC Server) to monitor several 
%% port by PortList.
%%
%% input:  PortList => [Port1, Port2, ..., PortN] 
%%
%%
%% output: PidList = > [<0.79.0>,<0.78.0>,<0.77.0>]
%%
%% ====================================================================
start() ->
	
	io:format("~nLoading config from config.txt ... ~n"),
    
    {ok, Terms} = file:consult("../bin/config.txt"),
	
	[PortList, LogLevel, LogFile] = Terms,
    
    io:format("Listening port list is ~p~n", [PortList]),
    io:format("Log level is ~p~n", [LogLevel]),
    io:format("Log file is ~p~n", [LogFile]),
	
	ok = pm_init(),
    
    lists:foldl(fun(Port, Acc) -> 
								  Pid = spawn(fun() -> start_server(Port, LogLevel, LogFile) end),
								  PidName = list_to_atom(integer_to_list(Port)),
								  register(PidName, Pid),
								  [Pid|Acc]
						  end, [], PortList),
	
	io:format("The X3 server has been successfully started!"),
	
	ok.


%% ====================================================================
%%
%% stop TCP server(LIC Server) by kill Pid in the PidList.
%%
%% ====================================================================
stop([PortList]) ->
    
    lists:foreach(fun(Port) ->
						  PidName = list_to_atom(integer_to_list(Port)),
						  Pid =  whereis(PidName),
						  exit(Pid, kill)
				  end, PortList),
	
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


%% ====================================================================
%% pm_init()
%%
%% used to create pm tables, and init these tables.
%%
%% input: 
%%
%% output: ok
%% ====================================================================
pm_init() ->
	
	ets:new(pm_table, [named_table,
					   public]),
	
	ets:insert(pm_table, [{createLictReq, 0},
						  {deleteLictReq, 0},
						  {x3CheckStateReq, 0},
						  {communicationContentReport, 0}]),
	
	ok.


%% ====================================================================
%% start_server()
%%
%% For each ClientIP+ClientPort pair, start a new worker to handle the 
%% connect, and loop receive the packet form this connection.
%%
%% input: Port => 50000 (integer)
%%
%%
%% ====================================================================
start_server(Port, LogLevel, LogFile) ->
	
	{ok, Listen} = gen_tcp:listen(Port, [binary,
										 {active, false}]),
	
	
	if
		LogLevel >= log1 ->
			{ok, FileDes} =  file:open(LogFile, [write]),
			io:format(FileDes, "Server parent worker ~w start to listen the port ~w ... ~n", 
					  [self(), Port]),
			file:close(FileDes)
	end,
	
	par_connect(Listen, Port, LogLevel, LogFile).


par_connect(Listen, ServerPort, LogLevel, LogFile) ->
	
	{ok, Socket} = gen_tcp:accept(Listen),
	
	{ok, {ClientIP, ClientPort}} = inet:peername(Socket),
	
	PidName = make_pid_name(ServerPort, ClientPort),

	Pid = spawn(fun() -> worker_main(Socket, #state{pid_name = PidName,
													client_ip = ClientIP,
													client_port = ClientPort,
													log_level = LogLevel,
													log_file = LogFile}) end),
	
	register(PidName, Pid),
	
	par_connect(Listen, ServerPort, LogLevel, LogFile).


worker_main(Socket, State) ->
	
	#state{pid_name = PidName,
		   client_ip = ClientIP,
		   client_port = ClientPort,
		   log_level = LogLevel,
		   log_file = LogFile} = State,
	
	{ok, FileDes} =  file:open(LogFile, [append]),
	
	put(pid_name, PidName),
	put(file_des, FileDes),
	put(log_level, LogLevel),
	
	if
		LogLevel >= log1 ->
			io:format(FileDes,
					  "Worker ~w (~w) start receive packets from ip: ~w, port: ~w ... ~n", 
					  [PidName, self(), ClientIP, ClientPort])
	end,

	loop(Socket).



loop(Socket) ->
	
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
			
			LogLevel = get(log_level),
			if
				LogLevel >= log3 ->
					io:format(get(file_des),
							  "Worker ~p received data = ~p~n", 
							  [get(pid_name), Bin])
			end,
			
			put(socket, Socket),
			handle_data(Bin),
            loop(Socket);
		
		{error, closed} ->
			
			LogLevel = get(log_level),
			if
				LogLevel >= log1 ->
					io:format(get(file_des),
							  "Server socket closed~n")
			end,
			file:close(get(log_file))
	end.
	

%% ====================================================================
%% handle_data(Bin)
%%
%% input:
%%
%% output:
%% ====================================================================
handle_data(Bin) ->
	{X3CmdMsgTag, X3CmdMsg} = x3_lib:decode_x3_interface_msg(Bin),
	
    case X3CmdMsgTag of
		createLictReq -> handle_create_lict_req(X3CmdMsg);
		deleteLictReq -> handle_delete_lict_req(X3CmdMsg);
		x3CheckStateReq -> handle_x3_check_state_req(X3CmdMsg);
		communicationContentReport -> handle_ccr(X3CmdMsg)
	end.


%% ====================================================================
%% handle_create_lict_req(Msg)
%%
%% input: 
%%       {createLictReq,{'CreateLICTReq',213,
%%                                       <<80,71,87,49,54,56>>,
%%                                       <<99,115,99,102,45,50,48,49,55,48,54,50,55,49,53,50,50,52,53>>,
%%                                       10156,asn1_NOVALUE}}}
%%
%% output:
%%
%% ====================================================================
handle_create_lict_req(Msg) ->
	
	LogLevel = get(log_level),
	
	if
		LogLevel >= log2 ->
			io:format(get(file_des),
					  "Worker ~w received create lict req msg ~p~n", 
					  [get(pid_name), Msg])
	end,
	
	ets:update_counter(pm_table, createLictReq, 1),
	
	#'CreateLICTReq'{messageSerialNo = MsgSerialNo,
					 icidValue = IcidValue,
					 'cCC-ID' = CCCId} = Msg,
	
    CreateLICTAck = #'CreateLICTAck'{messageSerialNo = MsgSerialNo,
									 icidValue = IcidValue,
									 'cCC-ID' = CCCId,
									 x3TunnelCreateResult = tunnelCreateSuccess},
	
	X3CmdMessage = {createLictAck, CreateLICTAck},
	
	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
	
	Socket = get(socket),
	
	if
		LogLevel >= log2 ->
			io:format(get(file_des),
					  "Worker ~w send create lict ack msg ~p~n", 
					  [get(pid_name), X3CmdMessage])
	end,
	
	if
		LogLevel >= log3 ->
			io:format(get(file_des),
					  "Worker ~w send create lict ack msg ~p~n", 
					  [get(pid_name), Bytes])
	end,
	
	gen_tcp:send(Socket, Bytes).


%% ====================================================================
%% handle_delete_lict_req(Msg)
%%
%% input:
%%
%% output:
%% ====================================================================
handle_delete_lict_req(Msg) ->
	
	LogLevel = get(log_level),
	
	if
		LogLevel >= log2 ->
			io:format(get(file_des),
					  "Worker ~w received delete lict req msg ~p~n", 
					  [get(pid_name), Msg])
	end,
	
	ets:update_counter(pm_table, deleteLictReq, 1),
	
	#'DeleteLICTReq'{messageSerialNo = MsgSerialNo,
					 icidValue = IcidValue,
					 'cCC-ID' = CCCId} = Msg,
	
    DeleteLICTAck = #'DeleteLICTAck'{messageSerialNo = MsgSerialNo,
									 icidValue = IcidValue,
									 'cCC-ID' = CCCId},
	
	X3CmdMessage = {deleteLictAck, DeleteLICTAck},
	
	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
	
	Socket = get(socket),
	
	if
		LogLevel >= log2 ->
			io:format(get(file_des),
					  "Worker ~w send delete lict ack msg ~p~n", 
					  [get(pid_name), X3CmdMessage])
	end,
	
	if
		LogLevel >= log3 ->
			io:format(get(file_des),
					  "Worker ~w send delete lict ack msg ~p~n", 
					  [get(pid_name), Bytes])
	end,
	
	gen_tcp:send(Socket, Bytes).


%% ====================================================================
%% handle_x3_check_state_req(Msg)
%%
%% input:
%%
%% output:
%% ====================================================================
handle_x3_check_state_req(Msg) ->
	
	LogLevel = get(log_level),
	
	if
		LogLevel >= log2 ->
			io:format(get(file_des),
					  "Worker ~w received check state req msg ~p~n", 
					  [get(pid_name), Msg])
	end,
	
	ets:update_counter(pm_table, x3CheckStateReq, 1),
	
    #'X3CheckStateReq'{neID = NeId} = Msg,
	
    CheckStateAck = #'X3CheckStateAck'{neID = NeId},
	
	X3CmdMessage = {x3CheckStateAck, CheckStateAck},
	
	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
	
	Socket = get(socket),
	
	if
		LogLevel >= log2 ->
			io:format(get(file_des),
					  "Worker ~w send check state ack msg ~p~n", 
					  [get(pid_name), X3CmdMessage])
	end,
	
	if
		LogLevel >= log3 ->
			io:format(get(file_des),
					  "Worker ~w send check state ack msg ~p~n", 
					  [get(pid_name), Bytes])
	end,
	
	gen_tcp:send(Socket, Bytes).


%% ====================================================================
%% handle_ccr(Msg)
%%
%% input:
%%
%% output:
%% ====================================================================
handle_ccr(Msg) ->
	
	LogLevel = get(log_level),

	if
		LogLevel >= log2 ->
			io:format(get(file_des),
					  "Worker ~w received cc report msg ~p~n", 
					  [get(pid_name), Msg])
	end,
	
	ets:update_counter(pm_table, communicationContentReport, 1),
	
	ok.


%% ====================================================================
%% make_pid_name()
%%
%% input: Port => 50000 (integer)
%%
%% output: server5000 (atom)
%% ====================================================================
make_pid_name(SPort,CPort) ->
	SName = atom_to_list(s) ++ integer_to_list(SPort),
	CName = atom_to_list(c) ++ integer_to_list(CPort),
	FullName = SName ++ "_" ++ CName,
    list_to_atom(FullName).












