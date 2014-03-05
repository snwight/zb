%%%-------------------------------------------------------------------
%%% @author Steve Wight <northwight@gmail.com>
%%% @copyright (C) 2014, Steve Wight
%%% @doc
%%%     Zero Bureau data source connector application
%%% @end
%%% Created :  3 Mar 2014 by Steve Wight
%%%-------------------------------------------------------------------
-module(zb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec start(StartType, StartArgs) -> {ok, Pid} 
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case zb_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
		end.

%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
