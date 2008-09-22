-module(wifbfe).
-author('Nick Gerakines <nick@gerakines.net>').

-behaviour(application).

-export([start/2, stop/1, start_phase/3, init/1]).
-export([build_templates/0, base_url/0]).

-record(wiicode, {user, wiicode}).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

start_phase(facebook, _, _) ->
    erlang_facebook:start(),
    catch erlang_facebook:update("APIKEY", "SECRET"),
    ok;

start_phase(mnesia, _, _) ->
    case mnesia:table_info(schema, storage_type) of
        ram_copies -> mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ -> ok
    end,
    ExistingTables = mnesia:system_info(tables) -- [schema],
    Tables = [wiicode],
    [begin
        create_table(Table)
    end || Table <- Tables, not lists:member(Table, ExistingTables)],
    ok.

create_table(wiicode) ->
    mnesia:create_table(wiicode, [{disc_copies, [node()]}, {attributes, record_info(fields, wiicode)}]).

init(_) ->
    {ok, {{one_for_one, 2, 10}, [
        {wifbfe_yaws, {wifbfe_yaws, start_link, [ok]}, permanent, 2000, worker, [wifbfe_yaws]}
    ]}}.

build_templates() ->
    [begin
        erltl:compile(F, [{outdir, "ebin"}, report_errors, report_warnings, nowarn_unused_vars])
    end || F <- filelib:wildcard("templates/*.et")].

base_url() -> "http://apps.facebook.com/wiiinfo/".
