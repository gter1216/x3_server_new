%% @author xxu <xiao.a.xu@alcatel-sbell.com.cn>
%% @copyright 2017 Nokia, Inc
%% This module used to implement X3 server behaviour,
%% i.e. simulate as LIC server.

-module(x3_server).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").
-include("enet_types.hrl").
-include("enet_pcap.hrl").

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
	
	[PortList, SelfPort, LogLevel, LogFile, _MsgDumpFile, _RtpDumpFile] = Terms,
    
    io:format("Listening port list is ~p~n", [PortList]),
	io:format("Server self port is ~p~n", [SelfPort]),
    io:format("Log level is ~p~n", [LogLevel]),
    io:format("Log file is ~p~n", [LogFile]),
	
	ok = pm_init(),
	
	ok = feature_init(),
    
    lists:foldl(fun(Port, Acc) -> 
								  Pid = spawn(fun() -> start_server(Port, 
																	list_to_atom(LogLevel), 
																	LogFile) 
											  end),
								  PidName = list_to_atom(integer_to_list(Port)),
								  register(PidName, Pid),
								  [Pid|Acc]
						  end, [], PortList),
	
	io:format("The X3 server has been successfully started!"),
	
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
	{ok, Listen} = gen_tcp:listen(SelfPort, [binary,
										  {active, false},
										  {packet, 0},
										  {reuseaddr, true},
										  {keepalive, true},
										  {exit_on_close, false}]),
	
	parent_par_connect(Listen).

parent_par_connect(Listen) ->
	
	{ok, Socket} = gen_tcp:accept(Listen),

	spawn(fun() -> parent_loop(Socket) end),
	
	parent_par_connect(Listen).

parent_loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin}->
			Command = binary_to_term(Bin),
			
			handle_command(Command),
			
			parent_loop(Socket);
		{error, closed} ->
			file:close(get(msg_dump_file))
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
handle_command(Command) ->
	
	case Command of 
		
		{dump_msg, Arg} ->
			
			#msg_dump{msg_dump_file = MsgDumpFile} = Arg,
			
			put(msg_dump_file, MsgDumpFile) ,
			
			{ok, IoDevice} = file:open(MsgDumpFile, [write]),
			
			io:format(IoDevice, "message summary is: ~n ~p", [ets:tab2list(pm_table)]);
		
		{start_dump_rtp, Arg}->
			 
			#rtp_dump{rtp_dump_file = RtpDumpFile} = Arg,
 			
			%% write PCAP header to the rtp dump file
   	        {ok, FileDes} = file:open(RtpDumpFile, [write]),
	
	        PCAPHdr = default_header(),
	
	        ok = file:write(FileDes, enet_pcap:encode_header(PCAPHdr)),
			
			%% enable rtp dump feature
			ets:insert(feature_table, [{dump_rtp, on, RtpDumpFile}]),
			
			file:close(FileDes);
		
		{stop_dump_rtp, _Arg} ->
			
			ets:insert(feature_table, [{dump_rtp, off, null}]);
		
		{stop_server, _Arg} ->
			
			init:stop()
	
	end.
			
			

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
			file:close(FileDes);
		true ->
			pass
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
	put(socket, Socket),
	
	if
		LogLevel >= log1 ->
			io:format(FileDes,
					  "Worker ~w (~w) start receive packets from ip: ~w, port: ~w ... ~n", 
					  [PidName, self(), ClientIP, ClientPort]);
		true ->
			pass
	end,
	

	loop(Socket).



loop(Socket) ->
	
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
			
			LogLevel = get(log_level),
			
%% 			io:format("Worker ~p received data = ~p~n", 
%% 					  [get(pid_name), Bin]),
			
			if
				LogLevel >= log3 ->
					io:format(get(file_des),
							  "Worker ~p received data = ~p~n", 
							  [get(pid_name), Bin]);
				true ->
					pass
			end,
			
			handle_data(Bin),
            loop(Socket);
		
		{error, closed} ->
			
			LogLevel = get(log_level),
			
			FileDes = get(file_des),
			
			if
				LogLevel >= log1 ->
					io:format(FileDes,
							  "Server socket closed~n",[]);
				true ->
					pass
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
					  [get(pid_name), Msg]);
	    true ->
			pass
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
					  [get(pid_name), X3CmdMessage]);
		true ->
			pass
	end,
	
	if
		LogLevel >= log3 ->
			io:format(get(file_des),
					  "Worker ~w send data ~p~n", 
					  [get(pid_name), Bytes]);
		true ->
			pass
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
					  [get(pid_name), Msg]);
		true ->
			pass
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
					  [get(pid_name), X3CmdMessage]);
		true ->
			pass
	end,
	
	if
		LogLevel >= log3 ->
			io:format(get(file_des),
					  "Worker ~w send data ~p~n", 
					  [get(pid_name), Bytes]);
		true ->
			pass
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
					  [get(pid_name), Msg]);
		true ->
			pass
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
					  [get(pid_name), X3CmdMessage]);
		true ->
			pass
	end,
	
	if
		LogLevel >= log3 ->
			io:format(get(file_des),
					  "Worker ~w send data ~p~n", 
					  [get(pid_name), Bytes]);
		true ->
			pass
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
					  [get(pid_name), Msg]);
		true ->
			pass
	end,
	
	ets:update_counter(pm_table, communicationContentReport, 1),
	
	case ets:lookup_element(feature_table, dump_rtp, 2) of
		off ->
			pass;
		on ->
			RtpDumpFile = ets:lookup_element(feature_table, dump_rtp, 3),
			
			{'CommunicationContentReport', _T1, _T2, _T3, _T4, PayLodList} = Msg,
			
			[{'PayloadList_SEQOF', _MediaType, _ProtocolType, PayLoad}] = PayLodList,
			
			dump_rtp_file(PayLoad, RtpDumpFile)
	end,
	
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


%% ====================================================================
%% dump_rtp_file(PayLoad, RtpDumpFile)
%%
%% Add UDP, IP, MAC, and PCAP header to constuct wireshark understood 
%% ethernet packets,
%%
%% input: 
%%      PayLoad -> Binary, pure rtp bytes
%%
%%      RtpDumpFile -> 
%% ====================================================================
dump_rtp_file(PayLoadBin, RtpDumpFile) ->
	
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
	
	{ok, FileDes} = file:open(RtpDumpFile, [append]),
	
	PCAPPkt = #pcap_pkt{ts = enet_pcap:now_to_ts(),
						orig_len = byte_size(EthPkt),
						data = EthPkt},
	
	ok = file:write(FileDes, enet_pcap:encode_packet(default_header(), PCAPPkt)),
	
    file:close(FileDes).
	
%% 	init:stop().


%% ==================================================================

default_header() ->
    #pcap_hdr{version={2, 4},
              tz_correction=0,
              sigfigures=0,
              snaplen=65535,
              datalinktype=1,
              endianness=little}.









