-module(wifbfe_yaws).
-behaviour(gen_server).

-include_lib("yaws/include/yaws.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, set_conf/0]).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    wifbfe_yaws:set_conf().

set_conf() ->
    GC = yaws_config:make_default_gconf(false, "wifbfe"),
    SC1 = #sconf{
        port = 6010,
        servername = "wiiinfo.iplaywowapp.com",
        listen = {0, 0, 0, 0},
        docroot = "www",
        appmods = [{"/", wifbfe_handler}]
    },
    try yaws_api:setconf(GC, [[SC1]]) of
        ok -> {ok, started};
        Errora -> {stop, Errora}
    catch
        Errorb -> {stop, Errorb}
    end.

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
