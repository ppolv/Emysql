-module(emysql_parse).
-export([ parse_buffer/3, init/0]).


parse_buffer(_, _, _) ->
    not_loaded(<<"??">>).

init() ->
    Path = filename:dirname(code:which(?MODULE)) ++ "/../priv/emysql_parse",
    ok = erlang:load_nif(Path, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).