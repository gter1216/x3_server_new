%% @author xxu <xiao.a.xu@alcatel-sbell.com.cn>
%% @copyright 2017 Nokia, Inc
%% This module used to implement X3 server behaviour,
%% i.e. simulate as LIC server.

-module(x3_server).
-include_lib("kernel/include/file.hrl").
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").
-include("enet_types.hrl").
-include("enet_pcap.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
		 logger/3]).

%% -record(state, {pid_name,
%% 				client_ip,
%% 				client_port,
%% 				log_level,
%% 				log_file}).


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
	
	[StartPortList, _StopPortList, Ipv4Addr, Ipv6Addr, SelfPort, LogLevel] = Terms,
	
	{Ipv4PortList, Ipv6PortList} = StartPortList,
	
	io:format("Listening ipv4 addr: ~p, port: ~p~n", [Ipv4Addr, Ipv4PortList]),
	io:format("Listening ipv6 addr: ~p, port: ~p~n", [Ipv6Addr, Ipv6PortList]),
	io:format("Server self port is ~p~n", [SelfPort]),
	io:format("Log level is ~p~n", [LogLevel]),
	
	ok = table_init(),
	
	ok = feature_init(),
	
	{ok, LogFile} = log_init(list_to_atom(LogLevel)),
	
	io:format("Please check server runnning log ~p for details!~n", [LogFile]),
	
	{ok, Ipv4AddrStr} = inet:parse_address(Ipv4Addr),
	
	{ok, Ipv6AddrStr} = inet:parse_address(Ipv6Addr),
	
	logger("Listened ipv4 addr is: ~p, port is: ~w~nListened ipv6 addr is: ~p, port is: ~w~nServer self port is ~p~nLog level is ~p~n", 
		   [Ipv4Addr, Ipv4PortList,Ipv6Addr, Ipv6PortList, SelfPort, LogLevel], 
		   log1),
	
%% 	logger("X3 Server successfully started! ~n", [], log1),
	
    
    lists:foldl(fun(Port, Acc) -> 
								  Pid = spawn(fun() -> start_server(ipv4, 
																	Ipv4AddrStr,
																	Port)
											  end),
								  
								  ets:insert(port_pid_table, [{{ipv4,Port}, Pid}]),
%% 								  PidName = list_to_atom(integer_to_list(Port)),
%% 								  register(PidName, Pid),
								  [Pid|Acc]
						  end, [], Ipv4PortList),
	
	lists:foldl(fun(Port, Acc) ->
								  Pid = spawn(fun() -> start_server(ipv6, 
																	Ipv6AddrStr,
																	Port) 
											  end),
%% 								  PidName = list_to_atom(integer_to_list(Port)),
%% 								  register(PidName, Pid),
                                  ets:insert(port_pid_table, [{{ipv6,Port}, Pid}]),
								  [Pid|Acc]
						  end, [], Ipv6PortList),
	
	start_parent_server(SelfPort),
	
	ok.


%% ====================================================================
%% start parent server to contorl the sever behaviour
%% lisenting port is 31818
%%
%% input:  
%%
%%
%% output: 
%%
%% ====================================================================
start_parent_server(SelfPort) ->
	{Status, Result} = gen_tcp:listen(SelfPort, [binary,
											 {active, false},
											 {packet, 0}]),
	
	ListenSocket = 
		case {Status, Result} of
			{ok, Socket} ->
				Socket;
			{error, eaddrinuse} ->
				io:format("Server Self Port ~p is in used, server startup failed! ~n", [SelfPort]),
				logger("Server Self Port ~p is in used, server startup failed! ~n", [SelfPort], log1),
				init:stop()
		end,
	
	spawn(fun() -> 
				  parent_par_connect(ListenSocket)
		  end),
	
	%% ensure the listened socket never stopped.
	timer:sleep(infinity).

parent_par_connect(ListenSocket) ->
	
	{ok, Socket} = gen_tcp:accept(ListenSocket),
    
	%% each time establish a tcp socket, spawn a new process to 
    %% wait establish new tcp socket
	spawn(fun() -> 
				  parent_par_connect(ListenSocket)
		  end),

	parent_loop(Socket).


parent_loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin}->
			Command = binary_to_term(Bin),
			
			handle_command(Socket, Command),
			
			parent_loop(Socket);
		{error, closed} ->
			pass
%% 			file:close(get(msg_dump_file))
%% 			io:format("parent server closed")
	end.


%% ====================================================================
%% handle command for the parent server
%%
%% input:  
%%
%%
%% output: 
%%
%% ====================================================================
handle_command(Socket, Command) ->
	
	case Command of 
		
		{start_dump_msg, _Arg} ->
			
%% 			#msg_dump{msg_dump_file = MsgDumpFile} = Arg,
%% 			{ok, IoDevice} = file:open(MsgDumpFile, [write]),
%% 			io:format(IoDevice, "message summary is: ~n ~p", [ets:tab2list(pm_table)]);

			%% 
            Timestamp = get_time_sec(erlang:timestamp()),
	
	        %% Dir ===> "/home/xxu/project/x3_project/bin"
	        {ok, Dir} = file:get_cwd(),
	
	        MsgDumpFile = Dir++"/log/msg_statistics/"++Timestamp++".msgdump",
		
		    %% enable msg dump feature
			ets:insert(feature_table, [{dump_msg, on, MsgDumpFile}]),
			
			Ack = {ok, #msg_dump{msg_dump_file=MsgDumpFile}},
			
			gen_tcp:send(Socket, term_to_binary(Ack));
			
		{stop_dump_msg, _Arg} ->
			MsgDumpFile = ets:lookup_element(feature_table, dump_msg, 3),
			{ok, IoDevice} = file:open(MsgDumpFile, [write]),
			io:format(IoDevice, "message summary is: ~n ~p", [ets:tab2list(pm_table)]),
			%% enable msg dump feature
			ets:insert(feature_table, [{dump_msg, off, null}]);
		
		{check_dump_msg, _Arg} ->
			MsgDumpState = ets:lookup_element(feature_table, dump_msg, 2),
			MsgDumpFile = ets:lookup_element(feature_table, dump_msg, 3),
			
			Ack = {ok, #msg_dump{msg_dump_file=MsgDumpFile,
								 msg_dump_state=MsgDumpState}},
			
			gen_tcp:send(Socket, term_to_binary(Ack));
		
		{start_dump_rtp, _Arg}->
			
            Timestamp = get_time_sec(erlang:timestamp()),
	
	        %% Dir ===> "/home/xxu/project/x3_project/bin"
	        {ok, Dir} = file:get_cwd(),
	
	        RtpDumpDir = Dir++"/log/payload_dump/"++Timestamp,
 		
   	        ok = file:make_dir(RtpDumpDir),
			
			%% enable rtp dump feature
			ets:insert(feature_table, [{dump_rtp, on, RtpDumpDir}]),
			
			Ack = {ok, #rtp_dump{rtp_dump_dir=RtpDumpDir}},
			
			gen_tcp:send(Socket, term_to_binary(Ack));
		
		{stop_dump_rtp, _Arg} ->
			
			ets:insert(feature_table, [{dump_rtp, off, null}]);
		
		{check_dump_rtp, _Arg} ->
			RtpDumpState = ets:lookup_element(feature_table, dump_rtp, 2),
			RtpDumpDir = ets:lookup_element(feature_table, dump_rtp, 3),
			
			Ack = {ok, #rtp_dump{rtp_dump_dir=RtpDumpDir,
								 rtp_dump_state=RtpDumpState}},
			
			gen_tcp:send(Socket, term_to_binary(Ack));
		
		
		{stop_server, Arg} ->
			
%% 			init:stop()
            handle_stop_server(Arg)
	
	end.

%% ====================================================================
handle_stop_server(Arg) ->
	case Arg of
		{all, all} ->
%% 			erlang:halt();
			init:stop();
		{[], Ipv6PortList}->
			lists:foreach(fun(Port) ->
								  Pid =
									  ets:lookup_element(port_pid_table, 
														 {ipv6,Port}, 2),
								  Pid ! {ok, shutdown}
						  end, Ipv6PortList);
		{Ipv4PortList, []}->
			lists:foreach(fun(Port) ->
								  Pid =
									  ets:lookup_element(port_pid_table, 
														 {ipv4,Port}, 2),
								  Pid ! {ok, shutdown}
						  end, Ipv4PortList);
		{Ipv4PortList, Ipv6PortList} ->
			lists:foreach(fun(Port) ->
								  Pid =
									  ets:lookup_element(port_pid_table, 
														 {ipv4,Port}, 2),
								  Pid ! {ok, shutdown}
						  end, Ipv4PortList),
			lists:foreach(fun(Port) ->
								  Pid =
									  ets:lookup_element(port_pid_table, 
														 {ipv6,Port}, 2),
								  Pid ! {ok, shutdown}
						  end, Ipv6PortList)
	end.
			

%% %% ====================================================================
%% %%
%% %% stop TCP server(LIC Server) by kill Pid in the PidList.
%% %%
%% %% ====================================================================
%% stop([PortList]) ->
%%     
%%     lists:foreach(fun(Port) ->
%% 						  PidName = list_to_atom(integer_to_list(Port)),
%% 						  Pid =  whereis(PidName),
%% 						  exit(Pid, kill)
%% 				  end, PortList),
%% 	
%% 	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


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
start_server(ipv4, Ipv4Addr, Port) ->
	
	{Status, Result} = gen_tcp:listen(Port, [binary,
										 {active, once},
										 {ip, Ipv4Addr},
										 {keepalive, true},
%% 										 {packet, 0},
										 {nodelay, true}]),
	
	case {Status, Result} of
		{ok, Socket} ->
			logger("Ipv4 Server ~w start to listen the port ~w ...", [self(), Port], log1),
			
			spawn(fun() -> 
						  par_connect(ipv4, Socket, Port)
				  end),
	       
			%% ensure the listened socket never stopped.
	        loop_wait();
		
		{error, eaddrinuse} ->
				io:format("Ipv4 Port ~p is in used, server startup failed! ~n", [Port]),
				init:stop()
	end;

start_server(ipv6, Ipv6Addr, Port) ->
	
	{Status, Result} = gen_tcp:listen(Port, [binary,
										 inet6,
										 {active, once},
										 {ip, Ipv6Addr},
										 {keepalive, true},
%% 										 {packet, 0},
										 {nodelay, true}]),
	
	case {Status, Result} of
		{ok, Socket} ->
			logger("Ipv6 Server ~w start to listen the port ~w ...", [self(), Port], log1),
			
%% 			O = inet:getopts(Socket, [recbuf]),
%% 	        io:format("xxxxx rec buf is:~p~n", [O]),
			
			spawn(fun() -> 
						  par_connect(ipv6, Socket, Port)
				  end),
	       
			%% ensure the listened socket never stopped.
	        loop_wait();
		
		{error, eaddrinuse} ->
				io:format("Ipv6 Port ~p is in used, server startup failed! ~n", [Port]),
				init:stop()
	end.

%% ====================================================================
loop_wait() ->
	receive
		{ok, shutdown} ->
			ok;
		_ ->
			loop_wait()
	end.


%% ====================================================================
par_connect(ipv4, Listen, ServerPort) ->
	
	{ok, Socket} = gen_tcp:accept(Listen),
	
	{ok, {ClientIP, ClientPort}} = inet:peername(Socket),
	
	PidName = make_pid_name(ipv4, ServerPort, ClientPort),
	
	put(pid_name, PidName),
	
	put(socket, Socket),
    
	%% each time establish a tcp socket, spawn a new process to 
    %% wait establish new tcp socket
	spawn(fun() -> 
				  par_connect(ipv4, Listen, ServerPort)
		  end),

	logger("IPV4 TCP socket established , worker ~w (~w) start receive packets from ip: ~w, port: ~w ... ", 
		   [PidName, self(), ClientIP, ClientPort], 
		   log1),

	loop(Socket);

par_connect(ipv6, Listen, ServerPort) ->
	
	{ok, Socket} = gen_tcp:accept(Listen),
	
	{ok, {ClientIP, ClientPort}} = inet:peername(Socket),
	
	PidName = make_pid_name(ipv6, ServerPort, ClientPort),
	
	put(pid_name, PidName),
	put(socket, Socket),

	spawn(fun() -> 
				  par_connect(ipv6, Listen, ServerPort)
		  end),
	
	logger("IPV6 TCP socket established , worker ~w (~w) start receive packets from ip: ~w, port: ~w ... ", 
		   [PidName, self(), inet:ntoa(ClientIP), ClientPort], 
		   log1),
	
	loop(Socket).


%% ====================================================================
loop(Socket) ->

	receive
		{tcp, Socket, Bin} ->
			logger("Worker ~p(~w) received data = ~p", [get(pid_name), self(), Bin], log3),
%% 			handle_data(Bin),
			try
				handle_data(Bin)
			catch
				ExceptionClass:Term ->
					StackTrace = erlang:get_stacktrace(),
					PidName = get(pid_name),
					Pid = self(),
%% 				    io:format("=============== Crash Report =============~n"),
%% 					io:format("Worker ~p(~w) crashed due to exception ~w: ~w~n",
%% 							  [PidName, Pid, ExceptionClass, Term]),
%% 				    io:format("stacktrace:~n~p~n~n",[StackTrace]),
 					generate_crash_dump(PidName,Pid,ExceptionClass,Term,StackTrace),
					exit(crash)
			end,
			inet:setopts(Socket, [{active,once}]),
			loop(Socket);
		{tcp_closed, Socket} ->
			logger("Warning: Worker ~p(~w) died due to tcp socket closed", [get(pid_name), self()], log1);
		{'EXIT', Pid, Reason} ->
			logger("Error: Worker ~p(~w) died due to reason ~p, restart worker!", 
				   [get(pid_name), Pid, Reason], log1)
	end.
	

%% ====================================================================
%% handle_data(Bin)
%%
%% input:
%%
%% output:
%% ====================================================================
handle_data(BinPack) ->
	
	BinList = x3_lib:un_smush(BinPack),
	
	DecodeFun = fun(Bin) ->
	
						{X3CmdMsgTag, X3CmdMsg} = x3_lib:decode_x3_interface_msg(Bin),
	
					    case X3CmdMsgTag of
							createLictReq -> handle_create_lict_req(X3CmdMsg);
							deleteLictReq -> handle_delete_lict_req(X3CmdMsg);
							x3CheckStateReq -> handle_x3_check_state_req(X3CmdMsg);
							communicationContentReport -> handle_ccr(X3CmdMsg)
						end
				end,
						
	
	lists:foreach(DecodeFun, BinList).


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
	
	logger("Worker ~w received create lict req msg ~p", [get(pid_name), Msg], log2),
	
	update_counter(createLictReq),
	
	#'CreateLICTReq'{messageSerialNo = MsgSerialNo,
					 icidValue = IcidValue,
					 'cCC-ID' = CCCId} = Msg,
	CreateLICTAck = 
		case get_x3_state(IcidValue, CCCId) of
			on ->
				logger("Error: 7510 request to create an existed X3 tunnel~p", [Msg], log1),
				#'CreateLICTAck'{messageSerialNo = MsgSerialNo,
								 icidValue = IcidValue,
								 'cCC-ID' = CCCId,
								 x3TunnelCreateResult = tunnelCreateFail,
								 x3TunnelFailureReason = monitorNumberBeSet};
			State when State == off orelse
						   State == undefined ->
				update_x3_state(IcidValue, CCCId, on),
				#'CreateLICTAck'{messageSerialNo = MsgSerialNo,
								 icidValue = IcidValue,
								 'cCC-ID' = CCCId,
								 x3TunnelCreateResult = tunnelCreateSuccess}
	end,

	X3CmdMessage = {createLictAck, CreateLICTAck},
    Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
 	Socket = get(socket),
	
	logger("Worker ~w send create lict ack msg ~p", [get(pid_name), X3CmdMessage], log2),
	logger("Worker ~w send data ~p", [get(pid_name), Bytes], log3),
	
	gen_tcp:send(Socket, Bytes).
	
	


%% ====================================================================
%% handle_delete_lict_req(Msg)
%%
%% input:
%%
%% output:
%% ====================================================================
handle_delete_lict_req(Msg) ->
	
	logger("Worker ~w received delete lict req msg ~p", [get(pid_name), Msg], log2),
	
	update_counter(deleteLictReq),
	
	#'DeleteLICTReq'{messageSerialNo = MsgSerialNo,
					 icidValue = IcidValue,
					 'cCC-ID' = CCCId} = Msg,
	
	DeleteLICTAck = 
		case get_x3_state(IcidValue, CCCId) of
			on ->
				update_x3_state(IcidValue, CCCId, off),
				#'DeleteLICTAck'{messageSerialNo = MsgSerialNo,
								 icidValue = IcidValue,
								 'cCC-ID' = CCCId};
			State when State == off orelse 
						   State == undefined ->
				logger("Error: 7510 request to delete an non-exist X3 tunnel~p", [Msg], log1),
			    #'DeleteLICTAck'{messageSerialNo = MsgSerialNo,
								 icidValue = IcidValue,
								 'cCC-ID' = CCCId}

	end,
	
	X3CmdMessage = {deleteLictAck, DeleteLICTAck},
	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
	Socket = get(socket),
	
	logger("Worker ~w send delete lict ack msg ~p", [get(pid_name), X3CmdMessage], log2),
	logger("Worker ~w send data ~p", [get(pid_name), Bytes], log3),
	
	gen_tcp:send(Socket, Bytes).


%% ====================================================================
%% handle_x3_check_state_req(Msg)
%%
%% input:
%%
%% output:
%% ====================================================================
handle_x3_check_state_req(Msg) ->
	
	logger("Worker ~w received check state req msg ~p", [get(pid_name), Msg], log2),
	
	update_counter(x3CheckStateReq),
	
    #'X3CheckStateReq'{neID = NeId} = Msg,
	
    CheckStateAck = #'X3CheckStateAck'{neID = NeId},
	
	X3CmdMessage = {x3CheckStateAck, CheckStateAck},
	
	Bytes = x3_lib:encode_x3_interface_msg(X3CmdMessage),
	
	Socket = get(socket),
	
	logger("Worker ~w send check state ack msg ~p", [get(pid_name), X3CmdMessage], log2),
	
	logger("Worker ~w send data ~p", [get(pid_name), Bytes], log3),
	
	gen_tcp:send(Socket, Bytes).


%% ====================================================================
%% handle_ccr(Msg)
%%
%% input:
%%
%% output:
%% ====================================================================
handle_ccr(Msg) ->
	
	logger("Worker ~w received cc report msg ~p", [get(pid_name), Msg], log2),
	
	update_counter(communicationContentReport),
	
	{'CommunicationContentReport', 
	 _DataSerialNo, 
	 IcidValue, 
	 CCCId, 
	 _DataDirect, 
	 PayLodList} = Msg,
	
	case get_x3_state(IcidValue, CCCId) of
		on ->
				case ets:lookup_element(feature_table, dump_rtp, 2) of
					off ->
						ok;
					on ->
						dump_rtp_file(IcidValue, CCCId, PayLodList)
				end;	
		State when State == off orelse 
						   State == undefined ->
			logger("Error: 7510 send CC msg on an non-existed X3 tunnel~p", [Msg], log1)
	end,
	
	ok.


%% ====================================================================
%% update_counter()
%%
%%
%% input: key
%%
%% output: 
%% ====================================================================
update_counter(Key) ->
	
	case ets:lookup_element(feature_table, dump_msg, 2) of
		off ->
			pass;
		on ->
			ets:update_counter(pm_table, Key, 1)
	end.


%% ====================================================================
%% pm_init()
%%
%% used to create pm tables, and init these tables.
%%
%% input: 
%%
%% output: ok
%% ====================================================================
table_init() ->
	
	ets:new(port_pid_table, [named_table,
							 public]),
	
	ets:new(pm_table, [named_table,
					   public]),
	
	ets:insert(pm_table, [{createLictReq, 0},
						  {deleteLictReq, 0},
						  {x3CheckStateReq, 0},
						  {communicationContentReport, 0}]),
	
	ets:new(user_table, [named_table,
						 public]),
	
%% 	ets:new(log_table, [named_table,
%% 						public]),
	
	ok.

%% ====================================================================
%% feature_init()
%%
%% init some feature switches.
%%
%% input: 
%%
%% off = ets:lookup_element(feature_table, dump_rtp, 2).
%%
%% null = ets:lookup_element(feature_table, dump_rtp, 3).
%%
%% output: ok
%% ====================================================================
feature_init() ->
	
	ets:new(feature_table, [named_table,public]),
	
	ets:insert(feature_table, [{dump_rtp, off, null}]),
	
	ets:insert(feature_table, [{dump_msg, off, null}]),
	
	ok.


%% ====================================================================
%% log_init(LogLevel)
%%
%% init some log parameters.
%%
%% input: LogLevel
%%
%% output:
%% ====================================================================
log_init(LogLevel) ->
	
    Timestamp = get_time_sec(erlang:timestamp()),
	
	%% Dir ===> "/home/xxu/project/x3_project/bin"
	{ok, Dir} = file:get_cwd(),
	
	LogFile = Dir++"/log/trace/"++Timestamp++".log",
	
%% 	CrashDumpFile = Dir++"/log/crash_log/"++Timestamp++".crashdump",
	
%% 	CrashReportFile = Dir++"/log/trace/"++Timestamp++".crashdump",	
%% 	ok = error_logger:logfile({open, CrashReportFile}),
	
	Pid = spawn(fun() -> log_server_start(LogFile, LogLevel) end),
	
	register(logproc, Pid),
	
	{ok, LogFile}.

%% ====================================================================
%% log_server_start(LogFile, LogLevel)
%%
%% start a server to do trace work
%%
%% input: LogFile, LogLevel
%%
%% output:
%% ====================================================================
log_server_start(LogFile, LogLevel) ->
	
	{ok, TraceFileDes} = file:open(LogFile, [write]),
	
	log_server_loop(TraceFileDes, LogLevel).

log_server_loop(TraceFileDes, LogLevel) ->
	
	receive
		{_From, {trace, RxLogLevel, Info}} ->
			if
				LogLevel >= RxLogLevel ->
					TS = get_time_micro(erlang:timestamp()),
					io:format(TraceFileDes, "~s~n", [TS]),
					io:format(TraceFileDes, "~s~n", [Info]),
					log_server_loop(TraceFileDes, LogLevel);
				true -> 
					log_server_loop(TraceFileDes, LogLevel)
			end;
		{_From, {crash, PidName, Pid, ExceptionClass, Term, StackTrace}} ->
			TS = get_time_micro(erlang:timestamp()),
			io:format(TraceFileDes, "~n~s~n", [TS]),
			io:format(TraceFileDes, "=========================== Crash Report Begin ===========================~n",[]),
            io:format(TraceFileDes, "Worker ~p(~w) crashed due to exception ~w: ~w~n",
					  [PidName, Pid, ExceptionClass, Term]),
			io:format(TraceFileDes, "stacktrace:~n~p~n",[StackTrace]),
			io:format(TraceFileDes, "=========================== Crash Report End ===========================~n~n",[])
	end.


%% ====================================================================
%% log(Format, VarList, LogLevel)
%%
%% start a server to do trace work
%%
%% input: Format, VarList, LogLevel
%%
%% output:
%% ====================================================================
logger(Format, VarList, LogLevel) ->
	
	Info = io_lib:format(Format, VarList),
	
	logproc ! {self(), {trace, LogLevel, Info}}.

%% ====================================================================
generate_crash_dump(PidName,Pid,ExceptionClass,Term,StackTrace) ->
	
	logproc ! {self(), {crash, PidName, Pid, ExceptionClass, Term, StackTrace}}.


%% %% ====================================================================
%% generate_crash_dump(PidName, Pid, ExceptionClass, Term, StackTrace) ->
%% 
%% 	logproc ! {self(), {crash, PidName, Pid, ExceptionClass, Term, StackTrace}}.	



%% ====================================================================
%% get_time_sec(Timestamp)
%%
%%
%% input: {MegaSecs, Secs, MicroSecs}
%%
%% output: 20170725163443
%% ====================================================================
get_time_sec(Timestamp) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
	
	%% "20170725163443"
	lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",
								[Year, Month, Day, Hour, Minute, Second])).

%% ====================================================================
%% get_time_mirco(Timestamp)
%%
%%
%% input: {MegaSecs, Secs, MicroSecs}
%%
%% output: 20170725163443
%% ====================================================================
get_time_micro({_,_,Micro} = Timestamp) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
	
	%% "2017-07-25 08:12:51.885Z"
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0s:",
								[Year, Month, Day, Hour, Minute, Second, integer_to_list(Micro)])).


%% ====================================================================
%% make_pid_name()
%%
%% input: Port => 50000 (integer)
%%
%% output: server5000 (atom)
%% ====================================================================
make_pid_name(ipv4, SPort,CPort) ->
	SName = atom_to_list(s) ++ integer_to_list(SPort),
	CName = atom_to_list(c) ++ integer_to_list(CPort),
	FullName = "ipv4_" ++ SName ++ "_" ++ CName,
    list_to_atom(FullName);

make_pid_name(ipv6, SPort,CPort) ->
	SName = atom_to_list(s) ++ integer_to_list(SPort),
	CName = atom_to_list(c) ++ integer_to_list(CPort),
	FullName = "ipv6_" ++ SName ++ "_" ++ CName,
    list_to_atom(FullName).


%% ====================================================================
%% dump_rtp_file(IcidValue, Cccid, PayLodList)
%%
%% Add UDP, IP, MAC, and PCAP header to constuct wireshark understood 
%% ethernet packets,
%%
%% input: 
%%      IcidValue, Cccid, PayLodList
%%
%%      RtpDumpFile -> 
%% ====================================================================
dump_rtp_file(IcidValue, Cccid, PayLodList) ->
	
	RtpDumpDir = ets:lookup_element(feature_table, dump_rtp, 3),
	
	[{'PayloadList_SEQOF', _MediaType, _ProtocolType, PayLoadBin}] = PayLodList,
	
	
    Icid = binary_to_list(IcidValue),
	SubIcid = string:right(Icid, 10),
	
	
	%% icid01_110.pcap
	PcapFileName = 
		string:trim(SubIcid, leading) ++ "_" ++ integer_to_list(Cccid, 16) ++ ".pcap",
%%         binary_to_list(IcidValue) ++ "_" ++ integer_to_list(Cccid) ++ ".pcap",
	
	PcapFileDes = get_pcap_file_des(RtpDumpDir, PcapFileName),
	
%% 	Pkt = #udp{src_port = <<SrcPort:16/big>>,
%% 			   dst_port = <<DstPort:16/big>>,
%% 			   data = PayLoadBin},
%% 	
%% 	UdpPkt = enet_udp:encode(Pkt, []),
	
	Pkt2 = #ipv4{proto = udp,
				 hdr_csum = 0,
				 src = <<10,11,13,95>>,
				 dst = <<10,11,13,234>>,
				 options = <<>>,
				 data = PayLoadBin},
	
	Ipv4Pkt = enet_ipv4:encode(Pkt2, []),
	
	Pkt3 = #eth{dst = <<0, 3, 186, 166, 12, 64>>,
				src = <<224, 48, 5, 250, 28, 226>>,
				type = 2048, %% Ipv4
				data = Ipv4Pkt},
	
	EthPkt = enet_eth:encode(Pkt3, []),
	
	PCAPPkt = #pcap_pkt{ts = enet_pcap:now_to_ts(),
						orig_len = byte_size(EthPkt),
						data = EthPkt},
	
	ok = file:write(PcapFileDes, enet_pcap:encode_packet(default_header(), PCAPPkt)),
	
    file:close(PcapFileDes).
	
%% 	init:stop().


%% ==================================================================
get_pcap_file_des(RtpDumpDir, PcapFileName) ->

	File = RtpDumpDir ++ "/" ++ PcapFileName,
	
	{ok, F} = file:open(File, [append]),
	
	case file:read_file_info(File) of
		{ok, #file_info{size=0}} ->
			%% file is new created, write pcap header
	        PCAPHdr = default_header(),
	        ok = file:write(F, enet_pcap:encode_header(PCAPHdr));
		{ok, #file_info{size=_Sz}} ->
			ok
	end,
	
	F.


%% ==================================================================

default_header() ->
    #pcap_hdr{version={2, 4},
              tz_correction=0,
              sigfigures=0,
              snaplen=65535,
              datalinktype=1,
              endianness=little}.

%% ==================================================================
update_x3_state(Icid, CCCId, State) ->
	ets:insert(user_table, [{{Icid, CCCId}, State}]).
	

%% ==================================================================
get_x3_state(Icid, CCCId) ->
	case ets:lookup(user_table, {Icid, CCCId}) of
		[] ->
			undefined;
		[{{Icid, CCCId}, Value}] ->
			Value
	end.




















