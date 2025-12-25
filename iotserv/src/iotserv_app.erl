%%%-------------------------------------------------------------------
%% @doc iotserv public API
%% @end
%%%-------------------------------------------------------------------

-module(iotserv_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc Запуск приложения
-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    case iotserv_sup:start_link() of

        {ok, Pid} ->{ok, Pid};

        Error -> Error
    end.

%% @doc Остановка приложения
-spec stop(term()) -> ok.
stop(_State) ->
    ok.