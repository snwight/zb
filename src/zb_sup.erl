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
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ZBConnector = {'zb_connector', {'zb_connector', start_link, []},
		   Restart, Shutdown, Type, ['zb_connector']},

    {ok, {SupFlags, [ZBConnector]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
