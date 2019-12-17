import gleam/io
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam/iodata
import tetromino/piece
import tetromino/board

fn parse_input(input) {
  input
  |> string.split(_, ",")
  |> list.traverse(_, piece.parse)
}

fn flip(f) {
  fn(a, b) { f(b, a) }
}

pub fn play_single_game(input) {
  input
  |> parse_input
  |> result.map(_, list.fold(_, board.new(), flip(board.play_round)))
  |> result.map(_, board.height)
}

pub fn play_tetris(input) {
  input
  |> string.split(_, "\n")
  |> list.filter(_, fn(i) { i != "" })
  |> list.traverse(_, fn(i) { play_single_game(i) })
}

external fn halt_program(Int) -> Nil = "erlang" "halt"

pub fn main(_argv) {
  let conclusion =
    io.stdin_read_all()
    |> result.map_error(_, fn(_) { "Unable to read from stdin" })
    |> result.then(_, fn(i) { play_tetris(i) })
    |> result.map(_, fn(results) {
      results
      |> list.map(_, int.to_string)
      |> list.intersperse(_, "\n")
      |> iodata.from_strings
      |> iodata.append(_, "\n")
      |> iodata.to_string
    })

  case conclusion {
    Ok(height) -> io.print(height)

    Error(reason) -> {
      io.write(string.append(reason, "\n"), io.stderr())
      halt_program(1)
    }
  }
}
