%%%-------------------------------------------------------------------
%%% @author Steve Wight <northwight@gmail.com>
%%% @copyright (C) 2014, Steve Wight
%%% @doc
%%%     The Zero Bureau implementation
%%%     ...this module encapsulates all data sources REST API secrets
%%% @end
%%% Created :  2 Mar 2014 by Steve Wight
%%%-------------------------------------------------------------------
-module(zb_connector).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([ping/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name :: atom(), 
		base_uri="" :: string(),
		fmt="" :: string(),
		ping="" :: string(),
		queries=[] :: list()}).

%%%===================================================================
%%% API
%%%===================================================================
ping(Name) ->
    gen_server:cast(Name, ping).

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%%--------------------------------------------------------------------
start_link({Name, Config}) ->
    gen_server:start_link({local, Name}, ?MODULE, Config, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}
%%--------------------------------------------------------------------
init({Name, {BaseUri, Fmt, ApiKey, Ping, _Queries}}) ->
    State = #state{name=Name, 
		   base_uri=BaseUri ++ Fmt ++ ApiKey, 
		   fmt=Fmt,
		   ping=Ping},
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State}
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State}
%%--------------------------------------------------------------------
handle_cast(ping, State) ->
    case httpc:request(State#state.ping) of
	{ok, Result} -> 
	    Extract = extract(Result),
	    io:format("Extracted:~n~p~n", [Extract]);
	{error, Reason} -> 
	    io:format("error! ~p~n", [Reason])
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
extract({_Vers, _Hdr, Data}) ->
    HitList = mochijson2:decode(Data),
    lists:map(
      fun(Row) ->
	      {struct, JsonData} = Row,
	      JsonData
      end, HitList).
