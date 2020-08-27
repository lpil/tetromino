import gleam/io
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam/string_builder
import tetromino/piece
import tetromino/board

pub external fn stdin_read_all() -> Result(String, Nil) =
  "tetromino_native" "stdin_read_all"

fn parse_input(input) {
  input
  |> string.split(",")
  |> list.try_map(piece.parse)
}

fn flip(f) {
  fn(a, b) { f(b, a) }
}

pub fn play_single_game(input) {
  input
  |> parse_input
  |> result.map(list.fold(_, board.new(), flip(board.play_round)))
  |> result.map(board.height)
}

pub fn play_tetris(input) {
  input
  |> string.split("\n")
  |> list.filter(fn(i) { i != "" })
  |> list.try_map(fn(i) { play_single_game(i) })
}

external fn halt_program(Int) -> Nil =
  "erlang" "halt"

pub fn main(_argv) {
  let conclusion =
    stdin_read_all()
    |> result.map_error(fn(_) { "Unable to read from stdin" })
    |> result.then(fn(i) { play_tetris(i) })
    |> result.map(fn(results) {
      results
      |> list.map(int.to_string)
      |> list.intersperse("\n")
      |> string_builder.from_strings
      |> string_builder.append("\n")
      |> string_builder.to_string
    })

  case conclusion {
    Ok(height) -> io.print(height)

    Error(reason) -> {
      io.println(reason)
      halt_program(1)
    }
  }
}
