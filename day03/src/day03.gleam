import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option}
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(input) = simplifile.read("input.txt")
  let assert Ok(sum_muls) = get_sum_muls(input)
  io.println("part 1")
  io.debug(sum_muls)
}

pub fn get_sum_muls(input: String) -> Result(Int, Nil) {
  input
  |> string.to_graphemes()
  |> to_muls()
}

fn to_muls(input: List(String)) -> Result(Int, Nil) {
  to_muls_internal(input, option.None, 0)
}

fn to_muls_internal(
  input: List(String),
  prev: Option(String),
  acc: Int,
) -> Result(Int, Nil) {
  case input {
    [] -> Ok(acc)
    [first, ..rest] -> {
      case prev {
        option.Some(val) ->
          case val, first {
            "m", "u" | "u", "l" | "l", "(" ->
              to_muls_internal(rest, option.Some(first), acc)
            "(", _ ->
              case get_nums(input) {
                #(l, Ok(#(n1, n2))) ->
                  to_muls_internal(l, option.None, acc + n1 * n2)
                #(l, Error(Nil)) -> to_muls_internal(l, option.None, acc)
              }
            _, _ -> to_muls_internal(rest, option.None, acc)
          }
        option.None ->
          case first {
            "m" -> to_muls_internal(rest, option.Some(first), acc)
            _ -> to_muls_internal(rest, option.None, acc)
          }
      }
    }
  }
}

fn get_nums(input: List(String)) -> #(List(String), Result(#(Int, Int), Nil)) {
  get_nums_internal(input, [], option.None)
}

fn get_nums_internal(
  input: List(String),
  acc: List(String),
  num: Option(Int),
) -> #(List(String), Result(#(Int, Int), Nil)) {
  case input {
    [] -> #([], Error(Nil))
    [first, ..rest] ->
      case first, num {
        ")", option.Some(num1) ->
          case list.length(acc) {
            1 | 2 ->
              case acc |> string.join(with: "") |> int.parse() {
                Ok(num2) -> #(rest, Ok(#(num1, num2)))
                Error(Nil) -> #(rest, Error(Nil))
              }
            _ -> #(rest, Error(Nil))
          }
        ",", option.None ->
          case list.length(acc) {
            1 | 2 ->
              case acc |> string.join(with: "") |> int.parse() {
                Ok(num1) -> get_nums_internal(rest, [], option.Some(num1))
                Error(Nil) -> #(rest, Error(Nil))
              }
            _ -> #(rest, Error(Nil))
          }
        d, option.Some(num1) ->
          case int.parse(d) {
            Ok(_) ->
              case list.length(acc) {
                0 | 1 ->
                  get_nums_internal(
                    rest,
                    list.append(acc, [d]),
                    option.Some(num1),
                  )
                2 -> {
                  case
                    acc
                    |> list.append([d])
                    |> string.join(with: "")
                    |> int.parse(),
                    rest
                  {
                    Ok(num2), [")", ..r] -> #(r, Ok(#(num1, num2)))
                    _, _ -> #(rest, Error(Nil))
                  }
                }
                _ -> #(rest, Error(Nil))
              }
            Error(Nil) -> #(rest, Error(Nil))
          }
        d, option.None ->
          case int.parse(d) {
            Ok(_) ->
              case list.length(acc) {
                0 | 1 ->
                  get_nums_internal(rest, list.append(acc, [d]), option.None)
                2 -> {
                  case
                    acc
                    |> list.append([d])
                    |> string.join(with: "")
                    |> int.parse(),
                    rest
                  {
                    Ok(num), [",", ..r] ->
                      get_nums_internal(r, [], option.Some(num))
                    _, _ -> #(rest, Error(Nil))
                  }
                }
                _ -> #(rest, Error(Nil))
              }
            Error(Nil) -> #(rest, Error(Nil))
          }
      }
  }
}
