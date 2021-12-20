library(tidyverse)
library(testthat)

parse_draws <- \(draws) {
  draws |>
    str_split(",") |>
    first() |>
    parse_integer()
}

parse_board <- \(board) {
  board |>
    read_lines() |>
    str_trim() |>
    str_split(" +") |>
    map(parse_integer)
}

parse_boards <- \(boards) {
  boards |>
    str_split("\n *\n") |>
    first() |>
    map(parse_board)
}

mark_boards <- \(boards, draw) {
  mark_row <-
      \(a) if_else(a == draw, NA_integer_, a)

  mark_board <-
    \(board, draw) map(board, mark_row)

  map(boards, mark_board, draw)
}

check_winners <- \(boards) {
  complete_row <-
      compose(any, \(a) map_lgl(a, every, .p = is.na))

  complete_col <-
    compose(
      complete_row,
      \(a) map(a, flatten_int),
      transpose
    )

  check_winner <-
    \(board) complete_row(board) || complete_col(board)

  map_lgl(boards, check_winner)
}

play_round_part_1 <- \(boards, draw) {
  marked_boards <-
    mark_boards(boards, draw)

  winners <-
    check_winners(marked_boards)

  if (any(winners))
    done(
      list(
        board = pluck(marked_boards, which(winners)),
        draw = draw
      )
    )

  else
    marked_boards
}

play_round_part_2 <- \(boards, draw) {
  marked_boards <-
    mark_boards(boards, draw)

  winners <-
    check_winners(marked_boards)

  if (all(winners))
    done(
      list(
        board = last(marked_boards),
        draw = draw
      )
    )

  else
    marked_boards[which(!winners)]
}

play_bingo <- \(play_fn, boards, draws) {
  score_board <-
    \(a) reduce(a, partial(sum, na.rm = TRUE))

  winner <-
    reduce(draws, play_fn, .init = boards)

  score_board(winner$board) * winner$draw
}

day_4 <- \(play_fn, boards, draws) {
  parsed_boards <-
    parse_boards(boards)

  parsed_draws <-
    parse_draws(draws)

  play_bingo(play_fn, parsed_boards, parsed_draws)
}

day_4_part_1 <-
  partial(day_4, play_round_part_1)

day_4_part_2 <-
  partial(day_4, play_round_part_2)

test_draws <-
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"

test_boards <-
 "22 13 17 11  0
   8  2 23  4 24
  21  9 14 16  7
   6 10  3 18  5
   1 12 20 15 19

   3 15  0  2 22
   9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
   2  0 12  3  7"

test_that("part 1 matches example result", {
  expect_equal(day_4_part_1(test_boards, test_draws), 4512)
})

test_that("part 1 matches example result", {
  expect_equal(day_4_part_2(test_boards, test_draws), 1924)
})

