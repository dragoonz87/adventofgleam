import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(input) = simplifile.read("input.txt")
  let assert Ok(distance) = find_sum_distances(input)
  io.println("part 1")
  io.debug(distance)

  let assert Ok(similarity) = find_similarities(input)
  io.println("part 2")
  io.debug(similarity)
}

pub fn find_sum_distances(input: String) -> Result(Int, Nil) {
  use #(l1, l2) <- result.try(get_lists(string.split(input, on: "\n")))

  let l1 = list.sort(l1, int.compare)
  let l2 = list.sort(l2, int.compare)

  list.zip(l1, l2)
  |> list.map(fn(val: #(Int, Int)) -> Int { int.absolute_value(val.0 - val.1) })
  |> list.reduce(fn(a: Int, acc: Int) -> Int { acc + a })
}

pub fn find_similarities(input: String) -> Result(Int, Nil) {
  use #(l1, l2) <- result.try(get_lists(string.split(input, on: "\n")))

  let l1 = list.sort(l1, int.compare)
  let l2 = list.sort(l2, int.compare)

  l1
  |> list.map(fn(a: Int) -> Int { a * num_occurences(a, l2) })
  |> list.reduce(fn(a: Int, acc: Int) -> Int { acc + a })
}

fn get_lists(combined: List(String)) -> Result(#(List(Int), List(Int)), Nil) {
  get_lists_internal(combined, [], [])
}

fn get_lists_internal(
  combined: List(String),
  l1: List(Int),
  l2: List(Int),
) -> Result(#(List(Int), List(Int)), Nil) {
  case combined {
    [] | [""] -> Ok(#(l1, l2))
    [first, ..rest] -> {
      let nums = string.split(first, on: " ")
      use n1 <- result.try(list.first(nums))
      use n2 <- result.try(list.last(nums))
      use n1 <- result.try(int.parse(n1))
      use n2 <- result.try(int.parse(n2))

      let l1 = list.append(l1, [n1])
      let l2 = list.append(l2, [n2])

      get_lists_internal(rest, l1, l2)
    }
  }
}

fn num_occurences(val: t, list: List(t)) -> Int {
  num_occurences_internal(val, list, 0)
}

fn num_occurences_internal(val: t, list: List(t), acc: Int) -> Int {
  case list {
    [] -> acc
    [first, ..rest] if first == val ->
      num_occurences_internal(val, rest, acc + 1)
    [_, ..rest] -> num_occurences_internal(val, rest, acc)
  }
}
