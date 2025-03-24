test_that("split_episode requires episode_start and episode_end", {
  df <- tibble(
    id = 1,
    date = as.Date("2023-01-01")
  )

  expect_error(
    split_episode(df, thresholds = c(30, 60)),
    "Data must contain both `episode_start` and `episode_end` columns"
  )
})

test_that("split_episode requires non-empty thresholds", {
  df <- tibble(
    episode_start = as.Date("2023-01-01"),
    episode_end = as.Date("2023-03-01")
  )

  expect_error(
    split_episode(df, thresholds = numeric(0)),
    "`thresholds` must not be empty"
  )
})

test_that("split_episode validates units", {
  df <- tibble(
    episode_start = as.Date("2023-01-01"),
    episode_end = as.Date("2023-03-01")
  )

  expect_error(
    split_episode(df, thresholds = c(30, 60), units = "invalid"),
    "units must be one of: days, weeks, months"
  )

  expect_error(
    split_episode(df, thresholds = c(30, 60), units = c("days", "invalid")),
    "units must be one of: days, weeks, months"
  )

  expect_error(
    split_episode(df, thresholds = c(30, 60), units = c("days")),
    NA
  )

  expect_error(
    split_episode(df, thresholds = c(30, 60), units = c("days", "days")),
    NA
  )
})

test_that("split_episode validates unit length", {
  df <- tibble(
    episode_start = as.Date("2023-01-01"),
    episode_end = as.Date("2023-03-01")
  )

  expect_error(
    split_episode(df, thresholds = c(30, 60), units = c("days", "days", "days")),
    "If providing multiple units, length must match thresholds"
  )
})

test_that("split_episode works with basic data", {
  df <- tibble(
    episode_start = as.Date("2023-01-01"),
    episode_end = as.Date("2023-03-01")
  )

  result <- split_episode(df, thresholds = c(30, 60, 90), units = "days")

  expect_equal(names(result), c(
    "episode_start", "episode_end",
    "days_30_date", "days_30_eligible", "days_30_continued",
    "days_60_date", "days_60_eligible", "days_60_continued",
    "days_90_date", "days_90_eligible", "days_90_continued"
  ))

  expect_equal(result$days_30_date, as.Date("2023-01-31"))
  expect_equal(result$days_60_date, as.Date("2023-03-02"))
  expect_equal(result$days_90_date, as.Date("2023-04-01"))

  expect_true(result$days_30_eligible)
  expect_true(result$days_30_continued)

  expect_true(result$days_60_eligible)
  expect_false(result$days_60_continued)

  expect_true(result$days_90_eligible)
  expect_false(result$days_90_continued)
})

test_that("split_episode handles different units", {
  df <- tibble(
    episode_start = as.Date("2023-01-01"),
    episode_end = as.Date("2023-05-01")
  )

  result <- split_episode(df,
    thresholds = c(30, 8, 2),
    units = c("days", "weeks", "months")
  )

  expect_equal(names(result), c(
    "episode_start", "episode_end",
    "days_30_date", "days_30_eligible", "days_30_continued",
    "weeks_8_date", "weeks_8_eligible", "weeks_8_continued",
    "months_2_date", "months_2_eligible", "months_2_continued"
  ))

  expect_equal(result$days_30_date, as.Date("2023-01-31"))
  expect_equal(result$weeks_8_date, as.Date("2023-02-26"))
  expect_equal(result$months_2_date, as.Date("2023-03-01"))

  expect_true(result$days_30_eligible)
  expect_true(result$days_30_continued)

  expect_true(result$weeks_8_eligible)
  expect_true(result$weeks_8_continued)

  expect_true(result$months_2_eligible)
  expect_true(result$months_2_continued)
})

test_that("split_episode handles custom max_date", {
  df <- tibble(
    episode_start = as.Date("2023-01-01"),
    episode_end = as.Date("2023-03-01")
  )

  result1 <- split_episode(df, thresholds = c(30, 60), max_date = as.Date("2023-01-15"))

  expect_false(result1$days_30_eligible)
  expect_false(result1$days_30_continued)
  expect_false(result1$days_60_eligible)
  expect_false(result1$days_60_continued)

  result2 <- split_episode(df, thresholds = c(30, 60), max_date = as.Date("2023-02-15"))

  expect_true(result2$days_30_eligible)
  expect_true(result2$days_30_continued)
  expect_false(result2$days_60_eligible)
  expect_false(result2$days_60_continued)
})

test_that("split_episode works with multiple rows", {
  df <- tibble(
    id = c(1, 2, 3),
    episode_start = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    episode_end = as.Date(c("2023-02-15", "2023-04-01", "2023-03-15"))
  )

  result <- split_episode(df, thresholds = c(30), units = "days")

  expect_equal(nrow(result), 3)
  expect_equal(result$id, c(1, 2, 3))

  expect_equal(
    result$days_30_date,
    as.Date(c("2023-01-31", "2023-03-03", "2023-03-31"))
  )

  expect_equal(result$days_30_eligible, c(TRUE, TRUE, TRUE))
  expect_equal(result$days_30_continued, c(TRUE, TRUE, FALSE))
})

test_that("split_episode preserves other columns", {
  df <- tibble(
    id = 1,
    episode_start = as.Date("2023-01-01"),
    episode_end = as.Date("2023-03-01"),
    other_col = "value"
  )

  result <- split_episode(df, thresholds = c(30), units = "days")

  expect_equal(result$other_col, "value")
})
