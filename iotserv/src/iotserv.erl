%% @doc сервер
-module(iotserv).
-behaviour(gen_server).

-export([
    start_link/0,
    start/0,
    stop/0,
    add/4,
    delete/1,
    change/2,
    lookup/1,
    get_all/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {next_id = 1 :: integer()}).

-type device_id() :: integer().
-type metric() :: {atom(), number()}.
-type metrics() :: [metric()].

%% @doc Запуск сервера
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Запуск сервера
-spec start() -> ok | {error, term()}.
start() ->
    case start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        Error -> Error
    end.

%% @doc Остановка сервера
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% @doc Добавление устройства
-spec add(string(), string(), float(), metrics()) -> {ok, device_id()} | {error, term()}.
add(Name, Address, Temperature, Metrics) ->
    gen_server:call(?MODULE, {add, Name, Address, Temperature, Metrics}).

%% @doc Удаление устройства по ID
-spec delete(device_id()) -> ok | {error, not_found}.
delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

%% @doc Изменение параметров устройства
-spec change(device_id(), map()) -> ok | {error, not_found}.
change(Id, Updates) ->
    gen_server:call(?MODULE, {change, Id, Updates}).

%% @doc Поиск устройства по ID
-spec lookup(device_id()) -> {ok, map()} | {error, not_found}.
lookup(Id) ->
    gen_server:call(?MODULE, {lookup, Id}).

%% @doc Получение всех устройств
-spec get_all() -> [map()].
get_all() ->
    gen_server:call(?MODULE, get_all).

%% @doc Инициализация сервера
-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    process_flag(trap_exit, true),
    
    case iotserv_db:init() of
        ok ->
            Devices = iotserv_db:get_all(),
            MaxId = case Devices of
                [] -> 0;
                _ -> lists:max([Id || #{id := Id} <- Devices])
            end,
            {ok, #state{next_id = MaxId + 1}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @doc Завершение работы
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Добавление устройства
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {stop, term(), #state{}}.
handle_call({add, Name, Address, Temperature, Metrics}, _From, State) ->
    Id = State#state.next_id,
    
    case iotserv_db:add(Id, Name, Address, Temperature, Metrics) of
        ok ->
            {reply, {ok, Id}, State#state{next_id = Id + 1}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% @doc Удаление устройства
handle_call({delete, Id}, _From, State) ->
    Reply = iotserv_db:delete(Id),
    {reply, Reply, State};

%% @doc Изменение устройства
handle_call({change, Id, Updates}, _From, State) ->
    Reply = iotserv_db:update(Id, Updates),
    {reply, Reply, State};

%% @doc Поиск устройства
handle_call({lookup, Id}, _From, State) ->
    Reply = iotserv_db:lookup(Id),
    {reply, Reply, State};

%% @doc Получение всех устройств
handle_call(get_all, _From, State) ->
    Devices = iotserv_db:get_all(),
    {reply, Devices, State};

handle_call(Request, _From, State) ->
    {reply, {error, unknown_call, Request}, State}.

handle_cast(stop, State) ->
    iotserv_db:stop(),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Обработка системных сообщений
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Обновление кода
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.