import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(input) = simplifile.read("input.txt")
  let assert Ok(count) = count_safe(input)
  io.println("part 1")
  io.debug(count)

  let assert Ok(count) = count_safe_dampered(input)
  io.println("part 2")
  io.debug(count)

  io.debug(count_safe("5 5 4"))
}

pub fn count_safe(input: String) -> Result(Int, Nil) {
  use reports <- result.try(get_reports(string.split(input, on: "\n")))

  let count =
    reports
    |> map_safe()
    |> list.filter(fn(r) { r })
    |> list.length()

  Ok(count)
}

pub fn count_safe_dampered(input: String) -> Result(Int, Nil) {
  use reports <- result.try(get_reports(string.split(input, on: "\n")))

  let count =
    reports
    |> map_safe_dampered()
    |> list.filter(fn(r: #(Bool, List(Int))) { r.0 || damper(r.1) })
    |> list.length()

  Ok(count)
}

fn get_reports(input: List(String)) -> Result(List(List(Int)), Nil) {
  get_reports_internal(input, [])
}

fn get_reports_internal(
  input: List(String),
  output: List(List(Int)),
) -> Result(List(List(Int)), Nil) {
  case input {
    [] | [""] -> Ok(output)
    [first, ..rest] -> {
      let nums =
        first
        |> string.split(on: " ")
        |> list.map(fn(n) { int.parse(n) })
      case list.any(nums, fn(n) { result.is_error(n) }) {
        True -> Error(Nil)
        False -> {
          get_reports_internal(
            rest,
            list.append(output, [list.map(nums, fn(n) { result.unwrap(n, -1) })]),
          )
        }
      }
    }
  }
}

fn map_safe(input: List(List(Int))) -> List(Bool) {
  map_safe_internal(input, [])
}

fn map_safe_dampered(input: List(List(Int))) -> List(#(Bool, List(Int))) {
  list.zip(map_safe_internal(input, []), input)
}

fn map_safe_internal(results: List(List(Int)), output: List(Bool)) -> List(Bool) {
  case results {
    [] -> output
    [first, ..rest] -> {
      map_safe_internal(rest, list.append(output, [is_safe(first)]))
    }
  }
}

fn is_safe(result: List(Int)) -> Bool {
  is_safe_internal(result, False, False, -1)
}

fn is_safe_internal(
  result: List(Int),
  increasing: Bool,
  decreasing: Bool,
  last: Int,
) -> Bool {
  case result {
    [] -> True
    [level, ..rest] -> {
      case increasing, decreasing {
        False, False ->
          case last {
            -1 -> is_safe_internal(rest, False, False, level)
            l ->
              case level, int.absolute_value(level - l) {
                _, diff if level > l && { diff >= 1 && diff <= 3 } ->
                  is_safe_internal(rest, True, False, level)
                _, diff if level < l && { diff >= 1 && diff <= 3 } ->
                  is_safe_internal(rest, False, True, level)
                _, _ -> False
              }
          }
        True, False ->
          case level - last {
            val if val >= 1 && val <= 3 ->
              is_safe_internal(rest, True, False, level)
            _ -> False
          }
        False, True ->
          case last - level {
            val if val >= 1 && val <= 3 ->
              is_safe_internal(rest, False, True, level)
            _ -> False
          }
        True, True -> False
      }
    }
  }
}

// fn is_safe_dampered(
//   result: List(Int),
//   increasing: Bool,
//   decreasing: Bool,
//   last: Int,
//   // damped: Bool,
// ) -> Bool {
//   case result {
//     [] -> True
//     [level, ..rest] -> {
//       // io.debug(Nil)
//       // io.debug(result)
//       // io.debug(level)
//       // io.debug(rest)
//       // io.debug(increasing)
//       // io.debug(decreasing)
//       // io.debug(last)
//       // io.debug(damped)
//       // io.debug(Nil)
//       case increasing, decreasing {
//         False, False ->
//           case last {
//             -1 -> is_safe_dampered(rest, False, False, level)
//             l ->
//               case int.absolute_value(level - l) {
//                 diff if level > l && { diff >= 1 && diff <= 3 } ->
//                   is_safe_dampered(rest, True, False, level)
//                 diff if level < l && { diff >= 1 && diff <= 3 } ->
//                   is_safe_dampered(rest, False, True, level)
//                 _ ->
//                   is_safe_damperless(rest, False, False, l)
//                   || is_safe_damperless(rest, False, False, level)
//               }
//           }
//         True, False ->
//           case level - last {
//             val if val >= 1 && val <= 3 ->
//               is_safe_dampered(rest, True, False, level)
//             _ -> is_safe_damperless(rest, True, False, last)
//           }
//         False, True ->
//           case last - level {
//             val if val >= 1 && val <= 3 ->
//               is_safe_dampered(rest, False, True, level)
//             _ -> is_safe_damperless(rest, False, True, last)
//           }
//         True, True -> False
//       }
//     }
//   }
// }

fn damper(result: List(Int)) -> Bool {
  damper_internal(result, [])
}

fn damper_internal(result: List(Int), arr: List(Int)) -> Bool {
  case result {
    [] -> False
    [level, ..rest] ->
      is_safe(list.append(arr, rest))
      || damper_internal(rest, list.append(arr, [level]))
  }
}
