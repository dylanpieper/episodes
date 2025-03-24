library(lubridate)

test_that("validate_units throws appropriate errors", {
  expect_error(
    episodes:::validate_units("days", "invalid"),
    "`inactive_unit` must be one of: days, weeks, months"
  )

  expect_error(
    episodes:::validate_units("invalid", "days"),
    "`gap_unit` must be one of: days, weeks, months"
  )

  expect_error(
    episodes:::validate_units("days", "days"),
    NA
  )
})

test_that("categorize_variables correctly identifies fixed and varying variables", {
  df <- tibble(
    id = rep(c(1, 2), each = 3),
    fixed_across_all = rep("value", 6),
    fixed_within_group = rep(c("A", "B"), each = 3),
    varying_within_group = c(1, 2, 3, 4, 5, 6)
  )

  result_no_groups <- episodes:::categorize_variables(
    df, character(0),
    c("fixed_across_all", "fixed_within_group", "varying_within_group")
  )

  expect_equal(result_no_groups$fixed_vars, "fixed_across_all")
  expect_equal(result_no_groups$varying_vars, c("fixed_within_group", "varying_within_group"))

  result_with_groups <- episodes:::categorize_variables(
    df, "id",
    c("fixed_across_all", "fixed_within_group", "varying_within_group")
  )

  expect_equal(result_with_groups$fixed_vars, c("fixed_across_all", "fixed_within_group"))
  expect_equal(result_with_groups$varying_vars, "varying_within_group")
})

test_that("calculate_statistics produces expected results", {
  single_date <- episodes:::calculate_statistics(as.Date("2023-01-01"))
  expect_equal(single_date$dates_count, 1)
  expect_true(is.na(single_date$dates_avg_days_between))
  expect_true(is.na(single_date$dates_sd_days_between))

  multiple_dates <- episodes:::calculate_statistics(seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5))
  expect_equal(multiple_dates$dates_count, 5)
  expect_equal(multiple_dates$dates_avg_days_between, 1)
  expect_equal(multiple_dates$dates_sd_days_between, 0)

  varied_dates <- episodes:::calculate_statistics(as.Date(c("2023-01-01", "2023-01-02", "2023-01-05", "2023-01-10")))
  expect_equal(varied_dates$dates_count, 4)
  expect_equal(varied_dates$dates_avg_days_between, 3)
  expect_true(varied_dates$dates_sd_days_between > 0)
})

test_that("determine_status correctly assesses episode status", {
  today <- Sys.Date()

  active_end <- today - days(2)
  inactive_end <- today - days(20)

  active_result <- episodes:::determine_status(active_end, TRUE, TRUE, 5, days(5), today)
  expect_false(active_result$discontinued)
  expect_equal(active_result$status, "Active")

  inactive_result <- episodes:::determine_status(inactive_end, TRUE, TRUE, 5, days(5), today)
  expect_true(inactive_result$discontinued)
  expect_equal(inactive_result$status, "Inactive")

  gap_result <- episodes:::determine_status(active_end, FALSE, TRUE, 5, days(5), today)
  expect_true(gap_result$discontinued)
  expect_equal(gap_result$status, "Gap")

  ongoing_result <- episodes:::determine_status(active_end, TRUE, FALSE, 5, days(5), today)
  expect_false(ongoing_result$discontinued)
  expect_equal(ongoing_result$status, "Ongoing")

  infinite_threshold_result <- episodes:::determine_status(inactive_end, TRUE, TRUE, Inf, days(Inf), today)
  expect_false(infinite_threshold_result$discontinued)
  expect_equal(infinite_threshold_result$status, "Active")
})

test_that("calculate_gap_days calculates gap correctly", {
  current_end <- as.Date("2023-01-05")
  next_start <- as.Date("2023-01-10")

  expect_equal(episodes:::calculate_gap_days(current_end, next_start), 4)
})

test_that("find_episode_boundaries finds correct episode breaks", {
  dates <- seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 10)

  no_gap_result <- episodes:::find_episode_boundaries(dates, 5, days(5))
  expect_equal(no_gap_result$breaks, c(0, 10))
  expect_equal(no_gap_result$indices, rep(1, 10))

  dates_with_gap <- c(
    seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
    seq.Date(from = as.Date("2023-01-10"), by = "days", length.out = 5)
  )

  gap_result <- episodes:::find_episode_boundaries(dates_with_gap, 2, days(2))
  expect_equal(gap_result$breaks, c(0, 5, 10))
  expect_equal(gap_result$indices, c(rep(1, 5), rep(2, 5)))

  infinite_threshold_result <- episodes:::find_episode_boundaries(dates_with_gap, Inf, days(Inf))
  expect_equal(infinite_threshold_result$breaks, c(0, 10))
  expect_equal(infinite_threshold_result$indices, rep(1, 10))
})

test_that("make_period creates correct period objects", {
  day_period <- episodes:::make_period(5, "days")
  expect_s4_class(day_period, "Period")
  expect_equal(as.numeric(day_period, "days"), 5)

  week_period <- episodes:::make_period(2, "weeks")
  expect_s4_class(week_period, "Period")
  expect_equal(as.numeric(week_period, "days"), 14)

  month_period <- episodes:::make_period(3, "months")
  expect_s4_class(month_period, "Period")
  expect_equal(month_period@month, 3)

  infinite_day_period <- episodes:::make_period(Inf, "days")
  expect_true(is.infinite(as.numeric(infinite_day_period, "days")))

  infinite_week_period <- episodes:::make_period(Inf, "weeks")
  expect_true(is.infinite(as.numeric(infinite_week_period, "days")))

  infinite_month_period <- episodes:::make_period(Inf, "months")
  expect_true(is.infinite(infinite_month_period@month))
})
