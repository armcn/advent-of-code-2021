library(tidyverse)
library(testthat)

parse_move <-
  compose(
    parse_integer,
    partial(str_extract, pattern = "[0-9+]")
  )

moves <- \(direction, input) {
  \(input) {
    if_else(
      str_detect(input, direction),
      parse_move(input),
      0L
    )
  }
}

moves_forward <-
  moves("forward")

moves_down <-
  moves("down")

moves_up <-
  moves("up")

calc_position_1 <- \(input) {
  horizontal_pos <-
    cumsum(moves_forward(input))

  vertical_pos <-
    cumsum(moves_down(input) - moves_up(input))

  last(horizontal_pos) * last(vertical_pos)
}

calc_position_2 <- \(input) {
  horizontal_moves <-
    moves_forward(input)

  aim <-
    cumsum(moves_down(input) - moves_up(input))

  vertical_pos <-
    cumsum(horizontal_moves * aim)

  sum(horizontal_moves) * last(vertical_pos)
}

day_2_part_1 <-
  compose(calc_position_1, read_lines)

day_2_part_2 <-
  compose(calc_position_2, read_lines)

test_data <-
 "forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2"

test_that("part 1 matches example result", {
  expect_equal(day_2_part_1(test_data), 150)
})

test_that("part 2 matches example result", {
  expect_equal(day_2_part_2(test_data), 900)
})
