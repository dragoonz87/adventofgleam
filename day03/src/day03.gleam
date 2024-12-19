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

  let assert Ok(input) = simplifile.read("input.txt")
  let assert Ok(sum_muls) = get_sum_muls_logic(input)
  io.println("part 2")
  io.debug(sum_muls)
}

pub fn get_sum_muls(input: String) -> Result(Int, Nil) {
  input
  |> string.to_graphemes()
  |> to_muls()
}

pub fn get_sum_muls_logic(input: String) -> Result(Int, Nil) {
  input
  |> string.to_graphemes()
  |> to_muls_logic()
}

fn to_muls(input: List(String)) -> Result(Int, Nil) {
  to_muls_internal(input, 0)
}

fn to_muls_internal(input: List(String), acc: Int) -> Result(Int, Nil) {
  case input {
    [] -> Ok(acc)
    [first, ..rest] -> {
      case first {
        "m" -> {
          case get_mul(input) {
            #(l, Ok(#(n1, n2))) -> to_muls_internal(l, acc + n1 * n2)
            #(l, Error(Nil)) -> to_muls_internal(l, acc)
          }
        }
        _ -> to_muls_internal(rest, acc)
      }
    }
  }
}

fn to_muls_logic(input: List(String)) -> Result(Int, Nil) {
  to_muls_logic_internal(input, True, 0)
}

fn to_muls_logic_internal(
  input: List(String),
  enabled: Bool,
  acc: Int,
) -> Result(Int, Nil) {
  case input {
    [] -> Ok(acc)
    [first, ..rest] -> {
      case first {
        "d" -> {
          case get_logic(input) {
            #(l, Ok(e)) -> to_muls_logic_internal(l, e, acc)
            #(l, Error(Nil)) -> to_muls_logic_internal(l, enabled, acc)
          }
        }
        "m" ->
          case enabled {
            True ->
              case get_mul(input) {
                #(l, Ok(#(n1, n2))) ->
                  to_muls_logic_internal(l, enabled, acc + n1 * n2)
                #(l, Error(Nil)) -> to_muls_logic_internal(l, enabled, acc)
              }
            False -> to_muls_logic_internal(rest, enabled, acc)
          }
        _ -> to_muls_logic_internal(rest, enabled, acc)
      }
    }
  }
}

fn get_mul(input: List(String)) -> #(List(String), Result(#(Int, Int), Nil)) {
  get_mul_internal(input, option.None)
}

fn get_mul_internal(
  input: List(String),
  prev: Option(String),
) -> #(List(String), Result(#(Int, Int), Nil)) {
  case input {
    [] -> #(input, Error(Nil))
    [first, ..rest] -> {
      case prev {
        option.Some(val) -> {
          case val, first {
            "m", "u" | "u", "l" | "l", "(" ->
              get_mul_internal(rest, option.Some(first))
            "(", _ -> get_nums(input)
            _, _ -> #(rest, Error(Nil))
          }
        }
        option.None -> {
          case first {
            "m" -> get_mul_internal(rest, option.Some(first))
            _ -> #(rest, Error(Nil))
          }
        }
      }
    }
  }
}

fn get_logic(input: List(String)) -> #(List(String), Result(Bool, Nil)) {
  get_logic_internal(input, option.None)
}

fn get_logic_internal(
  input: List(String),
  prev: Option(String),
) -> #(List(String), Result(Bool, Nil)) {
  case input {
    [] -> #(input, Error(Nil))
    [first, ..rest] -> {
      case prev {
        option.Some(val) -> {
          case val, first {
            "d", "o" | "o", "n" | "n", "'" | "'", "t" ->
              get_logic_internal(rest, option.Some(first))
            "o", "(" ->
              case rest {
                [")", ..r] -> #(r, Ok(True))
                _ -> #(rest, Error(Nil))
              }
            "t", "(" ->
              case rest {
                [")", ..r] -> #(r, Ok(False))
                _ -> #(rest, Error(Nil))
              }
            _, _ -> #(rest, Error(Nil))
          }
        }
        option.None -> {
          case first {
            "d" -> get_logic_internal(rest, option.Some(first))
            _ -> #(rest, Error(Nil))
          }
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
    [first, ..rest] -> {
      case first, num {
        ")", option.Some(num1) -> {
          case list.length(acc) {
            1 | 2 ->
              case acc |> string.join(with: "") |> int.parse() {
                Ok(num2) -> #(rest, Ok(#(num1, num2)))
                Error(Nil) -> #(rest, Error(Nil))
              }
            _ -> #(rest, Error(Nil))
          }
        }
        ",", option.None -> {
          case list.length(acc) {
            1 | 2 ->
              case acc |> string.join(with: "") |> int.parse() {
                Ok(num1) -> get_nums_internal(rest, [], option.Some(num1))
                Error(Nil) -> #(rest, Error(Nil))
              }
            _ -> #(rest, Error(Nil))
          }
        }
        d, option.Some(num1) -> {
          case int.parse(d) {
            Ok(_) -> {
              case list.length(acc) {
                0 | 1 -> {
                  get_nums_internal(
                    rest,
                    list.append(acc, [d]),
                    option.Some(num1),
                  )
                }
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
            }
            Error(Nil) -> #(rest, Error(Nil))
          }
        }
        d, option.None -> {
          case int.parse(d) {
            Ok(_) -> {
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
            }
            Error(Nil) -> #(rest, Error(Nil))
          }
        }
      }
    }
  }
}
