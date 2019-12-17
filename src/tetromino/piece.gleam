import gleam/int
import gleam/string
import gleam/result
import gleam/bitwise

// We're using a little endian integer representation while the document uses
// big endian, so the pieces are flipped compared to the images in the document.

// 0000000011
// 0000000011
pub fn q() {
  3075
}

// 0000000110
// 0000000011
pub fn s() {
  6147
}

// 0000000011
// 0000000110
pub fn z() {
  3078
}

// 0000000111
// 0000000010
pub fn t() {
  7170
}

// 0000001111
pub fn i() {
  15
}

// 0000000010
// 0000000010
// 0000000011
pub fn j() {
  2099203
}

// 0000000001
// 0000000001
// 0000000011
pub fn l() {
  1049603
}

fn parse_piece(char) {
  case char {
    "Q" -> Ok(q())
    "Z" -> Ok(z())
    "S" -> Ok(s())
    "T" -> Ok(t())
    "I" -> Ok(i())
    "L" -> Ok(l())
    "J" -> Ok(j())
    _ -> Error(string.append("Unknown piece ", char))
  }
}

fn parse_offset(offset) {
  offset
  |> int.parse
  |> result.map_error(_, fn(_) {
    string.append("Invalid piece offset ", offset)
  })
}

external fn split_position(String) -> Result(struct(String, String), String)
  = "tetromino_native" "split_position"

external fn trim(String) -> String
  = "string" "trim"

// Parse a piece positioned in a column from an input of a letter kind and a
// column offset, i.e. I2
//
// This function is a little naive as it assumed that any single digit
// numberical offset is OK, when for many pieces this would shift the piece
// more than a row, causing it to overflow into the next. Fix this when we have
// more time.
pub fn parse(input) {
  input
  |> trim
  |> split_position
  |> result.then(_, fn(input) {
    let struct(kind, offset) = input
    offset
    |> parse_offset
    |> result.then(_, fn(offset) {
      kind |> parse_piece |> result.map(_, bitwise.shift_left(_, offset))
    })
  })
}
