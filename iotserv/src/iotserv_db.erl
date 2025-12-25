%% @doc для работы с хранилищем
-module(iotserv_db).
-export([
    init/0,
    add/5,
    delete/1,
    update/2,
    lookup/1,
    get_all/0,
    stop/0
]).

-define(DEFAULT_DETS_FILE, "data/devices.dets").
-define(TABLE_ID, iotserv_table).

-record(device, {id :: integer(), name :: list(), address :: list(), temperature :: float(), metrics :: list()}).

%% @doc Инициализация
-spec init() -> ok | {error, term()}.
init() ->
    case read_config() of
        {ok, DetsPath} ->
            try
                ets:new(?TABLE_ID, [named_table, public, {keypos, #device.id}])
            catch
                error:badarg -> 
                    io:format("ETS table already exists, reusing~n"),
                    ok
            end,

            io:format("Opening DETS file: ~p~n", [DetsPath]),
            case dets:open_file(iotserv_dets, [{file, DetsPath}, {type, set}, {keypos, #device.id}]) of
                {ok, _} ->
                    load_from_dets(),
                    ok;
                {error, Reason} ->
                    {error, {dets_open_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Чтение конфигурации из JSON файла
-spec read_config() -> {ok, string()} | {error, term()}.
read_config() ->
    case file:read_file("config.json") of
        {ok, ConfigJson} ->
            Config = jsx:decode(ConfigJson, [return_maps]),
            
            case maps:get(<<"dets_path">>, Config, undefined) of
                undefined ->
                    {ok, ?DEFAULT_DETS_FILE};
                DetsPathBin when is_binary(DetsPathBin) ->
                    DetsPath = binary_to_list(DetsPathBin),
                    {ok, DetsPath};
                DetsPath when is_list(DetsPath) ->
                    {ok, DetsPath}
            end;
        {error, enoent} ->
            {ok, ?DEFAULT_DETS_FILE};
        {error, Reason} ->
            {error, {config_read_failed, Reason}}
    end.

%% @doc Загрузка данных из DETS в ETS
-spec load_from_dets() -> ok.
load_from_dets() ->
    case dets:info(iotserv_dets, size) of
        undefined -> ok;
        0 -> ok;
        _ ->
            dets:traverse(iotserv_dets, fun(Device) -> 
                ets:insert(?TABLE_ID, Device),
                continue 
            end)
    end.

%% @doc Добавление устройства
-spec add(integer(), list(), list(), float(), list()) -> ok | {error, term()}.
add(Id, Name, Address, Temperature, Metrics) ->
    Device = #device{
        id = Id,
        name = Name,
        address = Address,
        temperature = Temperature,
        metrics = Metrics
    },

    true = ets:insert(?TABLE_ID, Device),
    
    ok = dets:insert(iotserv_dets, Device),
    ok = dets:sync(iotserv_dets),
    
    ok.

%% @doc Удаление устройства
-spec delete(integer()) -> ok | {error, not_found}.
delete(Id) ->
    case ets:lookup(?TABLE_ID, Id) of
        [_Device] ->
            true = ets:delete(?TABLE_ID, Id),
            ok = dets:delete(iotserv_dets, Id),
            ok = dets:sync(iotserv_dets),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc Обновление устройства
-spec update(integer(), map()) -> ok | {error, not_found}.
update(Id, Updates) ->
    case ets:lookup(?TABLE_ID, Id) of
        [Device] ->
            UpdatedDevice = update_device(Device, Updates),
            true = ets:insert(?TABLE_ID, UpdatedDevice),
            ok = dets:insert(iotserv_dets, UpdatedDevice),
            ok = dets:sync(iotserv_dets),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc Поиск устройства
-spec lookup(integer()) -> {ok, map()} | {error, not_found}.
lookup(Id) ->
    case ets:lookup(?TABLE_ID, Id) of
        [#device{id = Id, name = Name, address = Address, 
                 temperature = Temp, metrics = Metrics}] ->
            DeviceMap = #{
                id => Id,
                name => Name,
                address => Address,
                temperature => Temp,
                metrics => Metrics
            },
            {ok, DeviceMap};
        [] ->
            {error, not_found}
    end.

%% @doc Получение всех устройств
-spec get_all() -> [map()].
get_all() ->
    Devices = ets:tab2list(?TABLE_ID),
    lists:map(fun(#device{id = Id, name = Name, address = Address, 
                          temperature = Temp, metrics = Metrics}) ->
        #{
            id => Id,
            name => Name,
            address => Address,
            temperature => Temp,
            metrics => Metrics
        }
    end, Devices).

%% @doc Остановка
-spec stop() -> ok.
stop() ->
    save_to_dets(),
    catch dets:close(iotserv_dets),
    ok.

%% @doc Сохранение данных в DETS
-spec save_to_dets() -> ok.
save_to_dets() ->
    Devices = ets:tab2list(?TABLE_ID),
    ok = dets:delete_all_objects(iotserv_dets),
    lists:foreach(fun(Device) -> 
        ok = dets:insert(iotserv_dets, Device) 
    end, Devices),
    ok = dets:sync(iotserv_dets),
    ok.

%% @doc Обновление устройства
-spec update_device(#device{}, map()) -> #device{}.
update_device(Device, Updates) ->
    lists:foldl(fun({Field, Value}, Acc) ->
        case Field of
            name -> Acc#device{name = Value};
            address -> Acc#device{address = Value};
            temperature -> Acc#device{temperature = Value};
            metrics -> Acc#device{metrics = Value};
            _ -> Acc
        end
    end, Device, maps:to_list(Updates)).