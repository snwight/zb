%%%-------------------------------------------------------------------
%%% @author Steve Wight <northwight@gmail.com>
%%% @copyright (C) 2014, Steve Wight
%%% @doc
%%%     Zero Bureau connector launcher
%%% @end
%%% Created :  3 Mar 2014 by Steve Wight 
%%%-------------------------------------------------------------------
-module(zb_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([launch_connector/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% -record(source_rec, 
%% 	{name :: atom(),
%% 	 base_uri :: string(),
%% 	 api_key :: string()
%% 	}).

-record(state, 
	{sources=[] :: list(),
	 active=[] :: list()
	}).

%%%===================================================================
%%% API
%%%===================================================================
launch_connector(SourceAtom) ->
    gen_server:cast(zb_server, {launch, SourceAtom}).

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%%--------------------------------------------------------------------
start_link() ->
    {ok, ConfigFile} = application:get_env(zb, sources_config),
    {ok, [Sources]} = file:consult(ConfigFile),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Sources, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}
%%--------------------------------------------------------------------
init(Sources) ->
    {ok, #state{sources=Sources}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State}
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State}
%%--------------------------------------------------------------------
handle_cast({launch, SourceAtom}, State=#state{sources=Sources}) ->
    SourceRec = lists:keyfind(SourceAtom, 1, Sources),
    {ok, Pid} = zb_connector:start_link({SourceAtom, SourceRec}),
    Active = [{SourceAtom, Pid} | State#state.active],
    {noreply, State#state{active=Active}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
