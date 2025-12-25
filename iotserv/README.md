# Задание 9 (поведение gen_server)
На основе материалов лекции 16 (пример usr-v3) реализовать ОТР- приложение для управления сервером IoT-устройств.
IoT-устройство имеет следующие характеристики:
- id;
- название;
- адрес установки;
- температура;
- список значений показателей, например: ```[{water_consumption, 124}]``` или ```[{temp, 43}, {mem_load, 57}]```.

Сервер ```iotserv.erl``` должен pеализовывать поведение ```gen_server```, предоставлять пользовательский интерфейс (API) и обработчики для сообщений (callback-функции gen_server).

Пользователю должны быть предоставлены функции:
- добавление устройства (add)
- удаление устройства по id (delete)
- изменение параметров устройства по id (change)
- поиск устройств по id (lookup)

Сервер должен хранить информацию в таблице ETS и синхронно в таблице DETS. 

При запуске сервера ETS таблица заполняется данными из DETS. 

Путь к файлу таблицу DETS нужно прочитать из конфигурационного файла в формате JSON (формат разработать свой, используйте библиотеку jsx).

Функции работы с хранилищем вынести в модуль iotserv_db.

Супервизор (iotserv_sup) должен перезапускать сервер iotserv при его аварийном завершении (см. iotserv_sup.erl).

Приложение разработать с использованием rebar3 (шаблон app).

Приложить сценарий тестирования сервера из shell: привести примеры вызовов всех клиентских API сервера.


iotserv
=====

Build
-----

    $ rebar3 compile
    $ rebar3 shell

# Пример работы

```
savely@savelyPC:~/erlang/hw09/iotserv$ rebar3 compile
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling iotserv
savely@savelyPC:~/erlang/hw09/iotserv$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling iotserv
Erlang/OTP 25 [erts-13.2.2.5] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit:ns]

Eshell V13.2.2.5  (abort with ^G)
1> Opening DETS file: "data/devices.dets"
===> Booted jsx
===> Booted iotserv
iotserv:start().
ok
2> iotserv:add("Thermometer1", "Kitchen", 22.5, [{temp, 23}, {humidity, 45}]).       
{ok,1}
3> iotserv:add("Meter2", "Bathroom", 24.0, [{water_consumption, 124}]).          
{ok,2}
4> iotserv:add("Light Sensor", "Living Room", 21.8, [{light_level, 78}, {motion, 0}]).
{ok,3}
5> iotserv:lookup(1).
{ok,#{address => "Kitchen",id => 1,
      metrics => [{temp,23},{humidity,45}],
      name => "Thermometer1",temperature => 22.5}}
6> iotserv:lookup(2).
{ok,#{address => "Bathroom",id => 2,
      metrics => [{water_consumption,124}],
      name => "Meter2",temperature => 24.0}}
7> iotserv:change(1, #{name => "T2", temperature => 23.0}).                         
ok
8> iotserv:lookup(1).                                      
{ok,#{address => "Kitchen",id => 1,
      metrics => [{temp,23},{humidity,45}],
      name => "T2",temperature => 23.0}}
9> iotserv:get_all().
[#{address => "Kitchen",id => 1,
   metrics => [{temp,23},{humidity,45}],
   name => "T2",temperature => 23.0},
 #{address => "Bathroom",id => 2,
   metrics => [{water_consumption,124}],
   name => "Meter2",temperature => 24.0},
 #{address => "Living Room",id => 3,
   metrics => [{light_level,78},{motion,0}],
   name => "Light Sensor",temperature => 21.8}]
10> 
[2]+  Остановлен    rebar3 shell

savely@savelyPC:~/erlang/hw09/iotserv$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling iotserv
Erlang/OTP 25 [erts-13.2.2.5] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit:ns]

Eshell V13.2.2.5  (abort with ^G)
1> Opening DETS file: "data/devices.dets"
===> Booted jsx
===> Booted iotserv
1> iotserv:
add/4          change/2       code_change/3  delete/1       get_all/0      
handle_call/3  handle_cast/2  handle_info/2  init/1         lookup/1       
module_info/0  module_info/1  start/0        start_link/0   stop/0         
terminate/2 
1> iotserv:start().
ok
2> iotserv:get_all().
[#{address => "Kitchen",id => 1,
   metrics => [{temp,23},{humidity,45}],
   name => "T2",temperature => 23.0},
 #{address => "Bathroom",id => 2,
   metrics => [{water_consumption,124}],
   name => "Meter2",temperature => 24.0},
 #{address => "Living Room",id => 3,
   metrics => [{light_level,78},{motion,0}],
   name => "Light Sensor",temperature => 21.8}]
3> iotserv:delete(2).
ok
4> iotserv:lookup(2).
{error,not_found}
5> exit(whereis(iotserv), kill).
true
6> =SUPERVISOR REPORT==== 25-Dec-2025::21:10:17.526235 ===
    supervisor: {local,iotserv_sup}
    errorContext: child_terminated
    reason: killed
    offender: [{pid,<0.156.0>},
               {id,iotserv},
               {mfargs,{iotserv,start_link,[]}},
               {restart_type,permanent},
               {significant,false},
               {shutdown,5000},
               {child_type,worker}]

Opening DETS file: "data/devices.dets"
6> iotserv:lookup(1). 
{ok,#{address => "Kitchen",id => 1,
      metrics => [{temp,23},{humidity,45}],
      name => "T2",temperature => 23.0}}
```