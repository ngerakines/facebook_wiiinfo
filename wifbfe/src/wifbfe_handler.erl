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
-module(wifbfe_handler).

-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").

-record(wiicode, {user, wiicode}).

out(Arg) ->
    Req = Arg#arg.req,
    handle_request(Req#http_request.method, Arg#arg.server_path, Arg).

wrap_body(Outer, {Inner, Args}) ->
    InnerBody = apply(Outer, Inner, [Args]),
    iolist_to_binary(InnerBody).

handle_request('POST', "/facebook/", Arg) ->
    FBFun = erlang_facebook:facebook_fun(yaws_api:parse_post(Arg)),
    Code = try wifbfe_utils:get_wiicode(FBFun(user)) of
        [Record] -> wiicode_fun(Record);
        _ -> none
    catch _ -> none end,
    make_response(200, wrap_body(wifbfe_hometmpl, {index, {FBFun, Code}}));

handle_request('POST', "/facebook/update", Arg) ->
    FBFun = erlang_facebook:facebook_fun(yaws_api:parse_post(Arg)),
    ReqVars = lists:keysort(1, yaws_api:parse_query(Arg)),
    case [lists:keysearch("fb_sig_user", 1, ReqVars), lists:keysearch("wiicode", 1, ReqVars)] of
        [{value, {_, User}}, {value, {_, WiiCode}}] ->
            wifbfe_utils:set_wiicode(list_to_binary(User), list_to_binary(WiiCode));
        _ -> ok
    end,
    make_response(200, wrap_body(wifbfe_hometmpl, {dialog, {"Updated", "Your Wii code has been set."}}));

%% Catchall
handle_request(_, _, _Arg) -> % catchall
    make_response(501, "<h1>Error!</h1><p>Action not implemented.</p>").

make_response(Status, Message) ->
    make_response(Status, "text/html", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].

wiicode_fun(Record) ->
    fun (user) -> Record#wiicode.user;
        (wiicode) -> Record#wiicode.wiicode
    end.
