%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(wifbfe).
-author('Nick Gerakines <nick@gerakines.net>').

-behaviour(application).

-export([start/2, stop/1, start_phase/3, init/1]).
-export([build_templates/0, base_url/0]).

-record(wiicode, {user, wiicode}).
-record(game, {id, user, game, code}).

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
    mnesia:create_table(wiicode, [{disc_copies, [node()]}, {attributes, record_info(fields, wiicode)}]);

create_table(game) ->
    mnesia:create_table(game, [{disc_copies, [node()]}, {attributes, record_info(fields, game)}]).

init(_) ->
    {ok, {{one_for_one, 2, 10}, [
        {wifbfe_yaws, {wifbfe_yaws, start_link, [ok]}, permanent, 2000, worker, [wifbfe_yaws]}
    ]}}.

build_templates() ->
    [begin
        erltl:compile(F, [{outdir, "ebin"}, report_errors, report_warnings, nowarn_unused_vars])
    end || F <- filelib:wildcard("templates/*.et")].

base_url() -> "http://apps.facebook.com/wiiinfo/".
