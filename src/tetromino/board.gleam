import gleam/bitwise

const columns = 10

const max_height = 100

external fn pow(Int, Int) -> Float =
  "math" "pow"

external fn floor(Float) -> Int =
  "erlang" "floor"

fn power(a, b) {
  floor(pow(a, b))
}

fn full_bottom_row() {
  power(2, columns) - 1
}

// Create a new board with no pieces places
pub fn new() {
  0
}

// Lower pieces on a board by a number of rows, dropping any rows that are
// pushed below row zero.
pub fn lower_by_rows(board, rows) {
  bitwise.shift_right(board, rows * columns)
}

// Raise piece on a board.
pub fn raise_by_rows(board, rows) {
  bitwise.shift_left(board, rows * columns)
}

// Merge pieces in two boards. Does not check for collisions.
pub fn merge(a, b) {
  bitwise.or(a, b)
}

// Create a new board that has cells filled where the cells were filled in both
// given boards.
pub fn intersection(a, b) {
  bitwise.and(a, b)
}

// Fold over the rows in a board, starting from the bottom
pub fn fold_bottom(board, from init, with reduce) {
  case board > 0 {
    False -> init
    True -> {
      let row = intersection(full_bottom_row(), board)
      let board = lower_by_rows(board, 1)
      fold_bottom(board, reduce(row, init), reduce)
    }
  }
}

// Filter out any rows in a board for which a predicate is not True
pub fn filter(board, with predicate) {
  let reduce = fn(row, acc) {
    let tuple(index, inner_board) = acc
    case predicate(row) {
      False -> tuple(index, inner_board)
      True -> tuple(index + 1, merge(inner_board, raise_by_rows(row, index)))
    }
  }
  let tuple(_, filtered) = fold_bottom(board, tuple(0, new()), reduce)
  filtered
}

// Remove any rows that have a piece in every column
pub fn remove_complete_rows(board) {
  filter(board, fn(row) { row != full_bottom_row() })
}

// Test to see whether two boards have overlapping pieces
pub fn has_collision(a, b) {
  intersection(a, b) != new()
}

// Drop the piece onto the board. Assumed the initial position is empty.
fn drop_piece(board, piece) {
  case has_collision(piece, full_bottom_row()) {
    // We have reached the bottom
    True -> merge(piece, board)

    // We have further to fall
    False -> {
      let lowered_piece = lower_by_rows(piece, 1)
      case has_collision(lowered_piece, board) {
        False -> drop_piece(board, lowered_piece)
        True -> merge(piece, board)
      }
    }
  }
}

fn count_height(board, row_count) {
  case board {
    0 -> row_count
    _ -> {
      let lowered = lower_by_rows(board, 1)
      count_height(lowered, row_count + 1)
    }
  }
}

pub fn height(board) {
  count_height(board, 0)
}

// Drop a single piece onto the board and remove any complete rows after
// it has landed
pub fn play_round(board, piece) {
  piece
  |> raise_by_rows(max_height)
  |> drop_piece(board, _)
  |> remove_complete_rows
}
