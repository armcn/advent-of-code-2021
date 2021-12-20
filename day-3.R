library(tidyverse)
library(testthat)

parse_input <-
  compose(str_trim, read_lines)

str_collapse <-
  partial(str_c, collapse = "")

as_decimal <-
  partial(strtoi, base = 2L)

binary_str_length <-
  compose(str_length, \(a) pluck(a, 1))

transpose_binary <- \(binary) {
  binary |>
    str_split("") |>
    transpose() |>
    map(compose(str_collapse, flatten_chr)) |>
    flatten_chr()
}

most_common_bit <- \(bits) {
  transposed <-
    transpose_binary(bits)

  one_count <-
    str_count(transposed, "1")

  zero_count <-
    str_count(transposed, "0")

  if_else(one_count < zero_count, 1L, 0L)
}

least_common_bit <- \(bits) {
  if_else(most_common_bit(bits) == 1L, 0L, 1L)
}

filter_binary <- \(digit_fn, binary, i) {
  at_index <-
    \(a) str_split(a, "") |>
      pluck(1, i) |>
      as.integer()

  digit <-
    digit_fn(binary)[i]

  if (length(binary) == 1)
    binary
  else
    keep(binary, \(a) at_index(a) == digit)
}

filter_most_common <- \(binary) {
  reduce(
    seq(1, binary_str_length(binary)),
    partial(filter_binary, most_common_bit),
    .init = binary
  )
}

filter_least_common <- \(binary) {
  reduce(
    seq(1, binary_str_length(binary)),
    partial(filter_binary, least_common_bit),
    .init = binary
  )
}

power_consumption_1 <- \(input) {
  gamma <-
    compose(as_decimal, str_collapse, most_common_bit)

  epsilon <-
    compose(as_decimal, str_collapse, least_common_bit)

  gamma(input) * epsilon(input)
}

power_consumption_2 <- \(input) {
  oxygen_generator <-
    compose(as_decimal, filter_most_common)

  co2_scrubber <-
    compose(as_decimal, filter_least_common)

  oxygen_generator(input) * co2_scrubber(input)
}

day_3_part_1 <-
  compose(power_consumption_1, parse_input)

day_3_part_2 <-
  compose(power_consumption_2, parse_input)

test_data <-
 "00100
  11110
  10110
  10111
  10101
  01111
  00111
  11100
  10000
  11001
  00010
  01010"

test_that("part 1 matches example result", {
  expect_equal(day_3_part_1(test_data), 198)
})

test_that("part 2 matches example result", {
  expect_equal(day_3_part_2(test_data), 230)
})
