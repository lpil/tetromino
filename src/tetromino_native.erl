-module(tetromino_native).
-export([split_position/1]).

split_position(<<X, Y>>) -> {ok, {<<X>>, <<Y>>}};
split_position(Other) -> {error, <<"Invalid input ", Other/binary>>}.
