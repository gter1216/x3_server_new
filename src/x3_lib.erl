%% @author xxu
%% @copyright 2017 Nokia, Inc
%% This module used to constuct all X3 interface message.


-module(x3_lib).
-include("X3-PROTOCOL.hrl").
-include("x3_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([un_smush/1,
		 encode_x3_interface_msg/1,
		 decode_x3_interface_msg/1]).

%% ==================================================================
%% un_smush(BinPack)
%%
%% input: 
%%
%% ouput:
%%
%% Result is:
%%
%% example:
%%
%% ==================================================================
un_smush(BinPack) ->
	
	decode_tcp_msg(BinPack, []).


decode_tcp_msg(<<>>, Acc) ->
	
	Acc;

decode_tcp_msg(BinPack, Acc) ->
	
	<<16#aa, 16#00, Len:16, Len:16, TotalBin/binary>> = BinPack,
	
	Length =  Len*8,
	
	<<Bin:Length, LeftBin/binary>> = TotalBin,
	
	decode_tcp_msg(LeftBin, Acc++[<<Bin:Length>>]).



%% ==================================================================
%% encode_x3_interface_msg(MsgTag, Msg)
%%
%% input: 
%%
%% ouput: 
%%
%% Result is:
%%
%% example:
%%
%% ==================================================================
encode_x3_interface_msg(X3CmdMessage) ->
	
	X3InterfaceMsg = #'X3Interface'{protocolVersion = ?protocol_version,
									x3cmdMessage = X3CmdMessage},
	
    {ok, Bytes} = 'X3-PROTOCOL':encode('X3Interface', X3InterfaceMsg),

    add_x3_header(Bytes).


%% ==================================================================
%% decode_x3_interface_msg(Bin)
%%
%% input: Binary from cLient
%%
%% ouput: X3CmdMessage
%%
%% Result is #'X3Interface'{protocolVersion, x3cmdMessage}
%%
%% example:
%% {'X3Interface',io2,{createLictReq,{'CreateLICTReq',213,<<80,71,87,49,54,56>>,<<99,115,99,102,45,50,48,49,55,48,54,50,55,49,53,50,50,52,53>>,10156,asn1_NOVALUE}}}
%%
%% ==================================================================
decode_x3_interface_msg(Bin) ->
	
	{ok, Result} = 'X3-PROTOCOL':decode('X3Interface', Bin),
	
	#'X3Interface'{x3cmdMessage = X3CmdMsg} = Result,
	
	X3CmdMsg.


%% %% ==================================================================
%% %% cons_delete_lict_req
%% %%
%% %% input:
%% %%
%% %% ouput:
%% %%
%% %% ==================================================================
%% cons_delete_lict_req(MsgSerialNo, ICIDValue, CCCId) ->
%% 	
%% 	DeleteLICTReq = #'DeleteLICTReq'{messgeSerialNo = MsgSerialNo,
%% 									 icidValue = ICIDValue,
%% 									 'cCC-ID' = CCCId},
%% 	
%% 	X3CmdMessage = {deleteLictReq, DeleteLICTReq},
%% 	
%% 	X3InterfaceMsg = #'X3Interface'{protocolVersion = ?protocol_version,
%% 									x3cmdMessage = X3CmdMessage},
%% 	
%%     {ok, Bytes} = 'X3-PROTOCOL':encode('X3Interface', X3InterfaceMsg),
%% 
%%     add_x3_header(Bytes).
%% 
%% 
%% %% ==================================================================
%% %% cons_delete_lict_ack
%% %%
%% %% input:
%% %%
%% %% ouput:
%% %%
%% %% ==================================================================
%% cons_delete_lict_ack(MsgSerialNo, ICIDValue, CCCId) ->
%% 	
%% 	CreateLICTAck = #'CreateLICTAck'{messageSerialNo = MsgSerialNo,
%% 									 icidValue = ICIDValue,
%% 									 'cCC-ID' = CCCId,
%% 									 x3TunnelCreateResult = X3TunnelCreateResult},
%% 	
%% 	X3CmdMessage = {createLictAck, CreateLICTAck},
%% 	
%% 	X3InterfaceMsg = #'X3Interface'{protocolVersion = ?protocol_version,
%% 									x3cmdMessage = X3CmdMessage},
%% 	
%%     {ok, Bytes} = 'X3-PROTOCOL':encode('X3Interface', X3InterfaceMsg),
%% 
%%     add_x3_header(Bytes).
%% 
%% 
%% %% ==================================================================
%% %% cons_x3_check_state_req
%% %%
%% %% input:
%% %%
%% %% ouput:
%% %%
%% %% ==================================================================
%% cons_x3_check_state_req(NeId) ->
%% 	
%% 	X3CheckStateReq = #'X3CheckStateReq'{neID = NeId},
%% 	
%% 	X3CmdMessage = {x3CheckStateReq, X3CheckStateReq},
%% 	
%% 	X3InterfaceMsg = #'X3Interface'{protocolVersion = ?protocol_version,
%% 									x3cmdMessage = X3CmdMessage},
%% 	
%%     {ok, Bytes} = 'X3-PROTOCOL':encode('X3Interface', X3InterfaceMsg),
%% 
%%     Bytes.
%% 
%% 



%% ====================================================================
%% Internal functions
%% ====================================================================


%% ==================================================================
%% 
%% add x3 header for the input bytes
%%
%% x3 header format => 
%%       16#aa, 16#00, 2 bytes size, 2 bytes size
%% 
%%       16#aa: sycn bytes
%%       16#00: not ciphered
%%       message bytes size
%%
%% ==================================================================
add_x3_header(Bytes) ->
	
    Len = size(Bytes),

    <<16#aa, 16#00, Len:16, Len:16, Bytes/binary>>.
	







