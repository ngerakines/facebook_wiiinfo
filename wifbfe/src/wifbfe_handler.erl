-module(wifbfe_handler).

-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    handle_request(Req#http_request.method, Arg#arg.server_path, Arg).

wrap_body(Outer, {Inner, Args}) ->
    InnerBody = apply(Outer, Inner, [Args]),
    iolist_to_binary(InnerBody).

handle_request('POST', "/facebook/", Arg) ->
    FBFun = erlang_facebook:facebook_fun(yaws_api:parse_post(Arg)),
    make_response(200, wrap_body(wifbfe_hometmpl, {index, {FBFun}}));

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
