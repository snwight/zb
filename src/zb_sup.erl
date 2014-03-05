%%%-------------------------------------------------------------------
%%% @author Steve Wight <northwight@gmail.com>
%%% @copyright (C) 2014, Steve Wight
%%% @doc
%%%     Generic supervisor for Zero Bureau data connections
%%% @end
%%% Created :  3 Mar 2014 by Steve Wight
%%%-------------------------------------------------------------------
-module(zb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} 
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ZBServer = {'zb_server', {'zb_server', start_link, []},
		   Restart, Shutdown, Type, ['zb_zerver']},

    {ok, {SupFlags, [ZBServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
