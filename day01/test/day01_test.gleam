import gleam/result
import gleeunit/should
import day01
import gleeunit

pub fn main() {
  gleeunit.main()
}

const input = "3   4
4   3
2   5
1   3
3   9
3   3
"

pub fn find_distance_test() {
    let dist = day01.find_sum_distances(input)
    
    should.be_ok(dist)

    let dist = result.unwrap(dist, -1)

    should.equal(dist, 11)
}

pub fn find_similarities_test() {
    let dist = day01.find_similarities(input)
    
    should.be_ok(dist)

    let dist = result.unwrap(dist, -1)

    should.equal(dist, 31)
}
