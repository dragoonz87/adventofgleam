import gleam/result
import day03
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
mul(4* mul(6,9! ?(12,34) mul ( 2 , 4 )"

pub fn count_sum_mul_test() {
    let output = day03.get_sum_muls(input)

    should.be_ok(output)

    let count = result.unwrap(output, -1)

    should.equal(count, 161)
}
