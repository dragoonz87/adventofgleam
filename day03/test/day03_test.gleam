import day03
import gleam/result
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn count_sum_mul_test() {
  let input =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
    mul(4* mul(6,9! ?(12,34) mul ( 2 , 4 )"
  let output = day03.get_sum_muls(input)

  should.be_ok(output)

  let count = result.unwrap(output, -1)

  should.equal(count, 161)
}

pub fn count_sum_mul_logic_test() {
  let input =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  let output = day03.get_sum_muls_logic(input)

  should.be_ok(output)

  let count = result.unwrap(output, -1)

  should.equal(count, 48)
}
