import tetromino
import gleam/should

pub fn play_single_game_test() {
  ""
  |> tetromino.play_single_game
  |> should.equal(Error("Invalid input "))

  "I0,I"
  |> tetromino.play_single_game
  |> should.equal(Error("Invalid input I"))

  "I0,I10"
  |> tetromino.play_single_game
  |> should.equal(Error("Invalid input I10"))

  "K0,I10"
  |> tetromino.play_single_game
  |> should.equal(Error("Unknown piece K"))

  "I0"
  |> tetromino.play_single_game
  |> should.equal(Ok(1))

  "I0,I0"
  |> tetromino.play_single_game
  |> should.equal(Ok(2))

  "   I0  ,\tI0\n\n  "
  |> tetromino.play_single_game
  |> should.equal(Ok(2))

  "I0,I0,I0"
  |> tetromino.play_single_game
  |> should.equal(Ok(3))

  "I0,I4"
  |> tetromino.play_single_game
  |> should.equal(Ok(1))

  "Q0"
  |> tetromino.play_single_game
  |> should.equal(Ok(2))

  "Q8"
  |> tetromino.play_single_game
  |> should.equal(Ok(2))

  "I0,Q8"
  |> tetromino.play_single_game
  |> should.equal(Ok(2))

  "I0,I4,Q7"
  |> tetromino.play_single_game
  |> should.equal(Ok(3))

  "T0,I2"
  |> tetromino.play_single_game
  |> should.equal(Ok(3))

  // From example
  "I0,I4,Q8"
  |> tetromino.play_single_game
  |> should.equal(Ok(1))

  // From example
  "T1,Z3,I4"
  |> tetromino.play_single_game
  |> should.equal(Ok(4))

  // From example
  "Q0,I2,I6,I0,I6,I6,Q2,Q4"
  |> tetromino.play_single_game
  |> should.equal(Ok(3))
}
