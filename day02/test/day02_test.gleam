import day02
import gleam/result
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const input = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"

pub fn count_safe_test() {
  let answer = day02.count_safe(input)

  should.be_ok(answer)

  let safe = result.unwrap(answer, -1)

  should.equal(safe, 2)
}

pub fn count_safe_damp_test() {
  let answer = day02.count_safe_dampered(input)

  should.be_ok(answer)

  let safe = result.unwrap(answer, -1)

  should.equal(safe, 4)
}
