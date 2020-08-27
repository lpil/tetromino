import gleam/int
import gleam/string
import gleam/result
import gleam/bitwise

// We're using a little endian integer representation while the document uses
// big endian, so the pieces are flipped compared to the images in the document.
//
// 11
// 11
pub fn q() {
  0b0000000011_0000000011
}

// 11
//  11
pub fn s() {
  0b0000000110_0000000011
}

//  11
// 11
pub fn z() {
  0b0000000011_0000000110
}

// 111
//  1
pub fn t() {
  0b0000000111_0000000010
}

// 1111
pub fn i() {
  0b0000001111
}

// 1
// 1
// 11
pub fn j() {
  0b0000000010_0000000010_0000000011
}

//  1
//  1
// 11
pub fn l() {
  0b0000000001_0000000001_0000000011
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
  |> result.map_error(fn(_) { string.append("Invalid piece offset ", offset) })
}

external fn split_position(String) -> Result(tuple(String, String), String) =
  "tetromino_native" "split_position"

external fn trim(String) -> String =
  "string" "trim"

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
  |> result.then(fn(input) {
    let tuple(kind, offset) = input
    offset
    |> parse_offset
    |> result.then(fn(offset) {
      kind
      |> parse_piece
      |> result.map(bitwise.shift_left(_, offset))
    })
  })
}
