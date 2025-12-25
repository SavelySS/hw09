%%%-------------------------------------------------------------------
%% @doc iotserv top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(iotserv_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    
    ChildSpecs = [
        #{id => iotserv,
          start => {iotserv, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [iotserv]}
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
