# tetromino

A little game of Tetris! Though currently without graphics I'm afraid.

To build and run this program you will need
[Gleam](https://gleam.run/getting-started/installing-gleam.html),
[Erlang](https://gleam.run/getting-started/installing-erlang.html) and the
Erlang build tool [rebar3](https://www.rebar3.org/) installed.

```sh
cd path/to/this/project

# Run the unit tests
rebar3 eunit

# Build the executable
rebar3 escriptize

# Run the program
cat input.txt | ./_build/default/bin/tetromino
```
