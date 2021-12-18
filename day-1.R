library(tidyverse)
library(testthat)

parse_input <-
  compose(parse_integer, read_lines)

is_positive <-
  partial(`>`, e2 = 0)

n_positive <-
  compose(partial(sum, na.rm = TRUE), is_positive)

sliding_window <- \(n, nums) {
  \(nums) map(0:n, \(a) lead(nums, n = a))
}

sum_list <- \(a) reduce(a, `+`)

day_1_part_1 <-
  compose(n_positive, diff, parse_input)

day_1_part_2 <-
  compose(
    n_positive,
    diff,
    sum_list,
    sliding_window(2),
    parse_input
  )

test_data <-
 "199
  200
  208
  210
  200
  207
  240
  269
  260
  263"

test_that("part 1 matches example result", {
  expect_equal(day_1_part_1(test_data), 7)
})

test_that("part 2 matches example result", {
  expect_equal(day_1_part_2(test_data), 5)
})
