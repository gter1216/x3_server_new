%% @author xxu
%% only for test purpose


-module(test_01).
-include("X3-PROTOCOL.hrl").
-include("enet_types.hrl").
-include("enet_pcap.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([construct_x3_check_state_req/1,
		 decode_x3_interface/1,
		 start/0,start2/0]).



start()->
%% 	PayLoadBin = <<64,50,195,82,0,180,0,0,128,
    PayLoadBin = <<128,
				   136,251,105,0,9,237,161,
				   176,160,57,46,142,167,163,
				   167,142,14,39,35,39,14,142,
				   167,163,167,142,14,39,35,
				   39,14,142,167,163,167,142,
				   14,39,35,39,14,142,167,163,
				   167,142,14,39,35,39,14,142,
				   167,163,167,142,14,39,35,
				   39,14,142,167,163,167,142,
				   14,39,35,39,14,142,167,163,
				   167,142,14,39,35,39,14,142,
				   167,163,167,142,14,39,35,
				   39,14,142,167,163,167,142,
				   14,39,35,39,14,142,167,163,
				   167,142,14,39,35,39,14,142,
				   167,163,167,142,14,39,35,
				   39,14,142,167,163,167,142,
				   14,39,35,39,14,142,167,163,
				   167,142,14,39,35,39,14,142,
				   167,163,167,142,14,39,35,
				   39,14,142,167,163,167,142,
				   14,39,35,39,14,142,167,163,
				   167,142,14,39,35,39,14>>,
	
	SrcPort = 32438,
	DstPort = 32439,
	
	Pkt = #udp{src_port = <<SrcPort:16/big>>,
			   dst_port = <<DstPort:16/big>>,
			   data = PayLoadBin},
	
	UdpPkt = enet_udp:encode(Pkt, []),
	
	Pkt2 = #ipv4{proto = udp,
				 hdr_csum = 0,
				 src = <<10,11,13,95>>,
				 dst = <<10,11,13,234>>,
				 options = <<>>,
				 data = UdpPkt},
	
	Ipv4Pkt = enet_ipv4:encode(Pkt2, []),
	
	Pkt3 = #eth{dst = <<0, 3, 186, 166, 12, 64>>,
				src = <<224, 48, 5, 250, 28, 226>>,
				type = 2048, %% Ipv4
				data = Ipv4Pkt},
	
	EthPkt = enet_eth:encode(Pkt3, []),
	
	{ok, FileDes} = file:open("/home/X3_Project/x3_server_erlang/x3_server_erl/src/xx.pcap", [append]),
	
	PCAPHdr = default_header(),
	
	ok = file:write(FileDes, enet_pcap:encode_header(PCAPHdr)),
	
	PCAPPkt = #pcap_pkt{ts = enet_pcap:now_to_ts(),
						orig_len = byte_size(EthPkt),
						data = EthPkt},
	
	ok = file:write(FileDes, enet_pcap:encode_packet(PCAPHdr, PCAPPkt)),
	
    file:close(FileDes).


start2()->
	
	Bin = <<170,0,0,229,0,229,
			48,129,226,128,1,2,
			161,129,220,164,129,217,
			128,2,1,53,129,6,
			105,99,105,100,48,49,
			130,1,110,131,1,1,
			164,129,196,48,129,193,
			128,5,97,117,100,105,
			111,129,1,17,130,129,
			180,64,54,195,82,0,
			180,0,0,128,8,117>>,
	
	{X3CmdMsgTag, X3CmdMsg} = x3_lib:decode_x3_interface_msg(Bin),
	
	io:format("msg tag is ~p~n, msg content is ~p~n", [X3CmdMsgTag, X3CmdMsg]).

	
	
%% 	io:format(FileDes, "~s~n", [enet_if_dump:hexblock(EthPkt)]).
	

	
%% ==================================================================

default_header() ->
    #pcap_hdr{version={2, 4},
              tz_correction=0,
              sigfigures=0,
              snaplen=65535,
              datalinktype=1,
              endianness=little}.


%% ==================================================================
%% input:
%%      NeId => OCTET STRING(SIZE(1..256))
%%              "sbc7510"
%%
%% ouput:
%%      Bytes Msg => Check State Req
%%
%% ==================================================================
construct_x3_check_state_req(NeId) ->
	
	X3CheckStateReq = #'X3CheckStateReq'{neID = NeId},
	
	ProtocolVersion = 'io2',
	
	X3CmdMessage = {x3CheckStateReq, X3CheckStateReq},
	
	X3InterfaceMsg = #'X3Interface'{protocolVersion = ProtocolVersion,
									x3cmdMessage = X3CmdMessage},
	
    {ok, Bytes} = 'X3-PROTOCOL':encode('X3Interface', X3InterfaceMsg),

    Bytes.


%% ==================================================================
%% input:
%%      Data => Byte Stream
%%         <<16#30,16#0d,16#80,16#01,16#02,16#a1,16#08,16#a5,16#06,16#80,16#04,16#50,16#47,16#57,16#31>>
%%
%% ouput:
%%      {ok,{'X3Interface',io2,
%%                 {x3CheckStateReq,{'X3CheckStateReq',<<"PGW1">>}}}}
%%
%%
%% 
%%
%%
%%
%%
%%
%% ==================================================================
decode_x3_interface(Data) ->
	
	'X3-PROTOCOL':decode('X3Interface', Data).
	



%% ====================================================================
%% Internal functions
%% ====================================================================


