%% @author xxu
%% only for test purpose


-module(test_01).
-include("X3-PROTOCOL.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([construct_x3_check_state_req/1,decode_x3_interface/1]).


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


