-module(tetromino_native).
-export([split_position/1, stdin_read_all/0]).

split_position(<<X, Y>>) -> {ok, {<<X>>, <<Y>>}};
split_position(Other) -> {error, <<"Invalid input ", Other/binary>>}.

stdin_read_all() ->
    do_stdin_read_all([]).

do_stdin_read_all(Acc) ->
    case io:get_line("") of
        Line when is_binary(Line); is_list(Line) -> do_stdin_read_all([Acc | Line]);
        eof -> {ok, iolist_to_binary(Acc)};
        Error = {error, _} -> Error
    end.
