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
-module(wifbfe_utils).

-export([get_wiicode/1, set_wiicode/2, get_gamecode/1, set_gamecode/3]).
-export([get_games/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(wiicode, {user, wiicode}).
-record(game, {id, user, game, code}).

%% mnesia:activity(transaction, fun() -> qlc:e(qlc:q([E || E <- mnesia:table(wiicode), E#wiicode.user == <<"500025891">>])) end).
get_wiicode(User) when is_list(User) ->
    get_wiicode(list_to_binary(User));

get_wiicode(User) when is_binary(User) ->
    F = fun() ->
        qlc:e(qlc:q([E || E <- mnesia:table(wiicode),
            E#wiicode.user =:= User]))
    end,
    mnesia:activity(transaction, F);

get_wiicode(_) -> [].

get_gamecode(User) when is_list(User) ->
    get_gamecode(list_to_binary(User));

get_gamecode(User) when is_binary(User) ->
    F = fun() ->
        qlc:e(qlc:q([E || E <- mnesia:table(game),
            E#game.user =:= User]))
    end,
    mnesia:activity(transaction, F);

get_gamecode(_) -> [].

%% mnesia:activity(transaction, fun() -> mnesia:write(#wiicode{ user = <<"500025891">>, wiicode = <<"0002-8039-8968-xxxx">> }) end).

set_wiicode(User, WiiCode) ->
    F = fun() ->
        mnesia:write(#wiicode{ user = User, wiicode = WiiCode })
    end,
    mnesia:activity(transaction, F).

set_gamecode(User, Game, Code) ->
    F = fun() ->
        mnesia:write(#game{ id = {User, Game}, user = User, game = Game, code = Code})
    end,
    mnesia:activity(transaction, F).

get_games() -> [
        "Guitar Hero: Aerosmith",
        "Guitar Hero III: Legends of Rock",
        "Mario Kart Wii",
        "Super Smash Bros. Brawl"
    ].
