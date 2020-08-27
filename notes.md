## Reflections

I opted to use a bit array for representing the board as it's nice and memory
efficient while fast to manipluate. I'm quite happy with it, though there's
some places where I'm iterating where I suspect someone with better bitwise-fu
than me could do it in constant time (i.e. dropping pieces onto the board).

I attempted to rush through this one a little so test coverage isn't as good
as I would like, and I didn't take the time to make a board-to-string function
which would have been useful for debugging. Gleam's type system helped quite a
bit but there's enough complexity here that fine grained tests would have paid
off even in this short challenge.

With a little more time it would have been good to add a type wrapper to the
integer that represents a board, so that the type system can detect invalid
uses.
