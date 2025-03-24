library(lubridate)

test_that("segment_episodes_by_covars works with basic data", {
  df <- tibble(
    id = rep(1, 5),
    date = seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
    covar = c(1, 1, 2, 2, 2)
  ) |>
    group_by(id)

  result <- segment_episodes_by_covars(df, date, covar_cols = covar)

  expect_equal(nrow(result), 2)
  expect_equal(result$episode_id, c(1, 1))
  expect_equal(result$segment_id, c("1.1", "1.2"))
  expect_equal(result$segment_start, c(as.Date("2023-01-01"), as.Date("2023-01-03")))
  expect_equal(result$segment_end, c(as.Date("2023-01-02"), as.Date("2023-01-05")))
  expect_equal(result$covar, c(1, 2))
})

test_that("segment_episodes_by_covars respects gap thresholds", {
  df <- tibble(
    id = rep(1, 10),
    date = c(
      seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
      seq.Date(from = as.Date("2023-01-10"), by = "days", length.out = 5)
    ),
    covar = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
  ) |>
    group_by(id)

  result <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$episode_id, c(1, 2))
  expect_equal(result$segment_id, c("1.1", "2.1"))
  expect_equal(result$covar, c(1, 2))
})

test_that("segment_episodes_by_covars handles multiple covariates", {
  df <- tibble(
    id = rep(1, 5),
    date = seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
    covar1 = c(1, 1, 2, 2, 2),
    covar2 = c("A", "A", "A", "B", "B")
  ) |>
    group_by(id)

  result <- segment_episodes_by_covars(df, date, covar_cols = c(covar1, covar2))

  expect_equal(nrow(result), 3)
  expect_equal(result$episode_id, c(1, 1, 1))
  expect_equal(result$segment_id, c("1.1", "1.2", "1.3"))
  expect_equal(result$covar1, c(1, 2, 2))
  expect_equal(result$covar2, c("A", "A", "B"))
})

test_that("segment_episodes_by_covars validates units correctly", {
  df <- tibble(
    id = rep(1, 5),
    date = seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
    covar = c(1, 1, 2, 2, 2)
  ) |>
    group_by(id)

  expect_error(
    segment_episodes_by_covars(df, date, covar_cols = covar, gap_unit = "invalid"),
    "`gap_unit` must be one of: days, weeks, months"
  )

  expect_error(
    segment_episodes_by_covars(df, date, covar_cols = covar, inactive_unit = "invalid"),
    "`inactive_unit` must be one of: days, weeks, months"
  )
})

test_that("segment_episodes_by_covars handles single observation", {
  df <- tibble(
    id = 1,
    date = as.Date("2023-01-01"),
    covar = 1
  ) |>
    group_by(id)

  result <- segment_episodes_by_covars(df, date, covar_cols = covar)

  expect_equal(nrow(result), 1)
  expect_equal(result$episode_id, 1)
  expect_equal(result$segment_id, "1.1")
  expect_equal(result$episode_start, as.Date("2023-01-01"))
  expect_equal(result$episode_end, as.Date("2023-01-01"))
  expect_equal(result$segment_start, as.Date("2023-01-01"))
  expect_equal(result$segment_end, as.Date("2023-01-01"))
  expect_equal(result$episode_days, 0)
  expect_equal(result$segment_days, 0)
  expect_equal(result$covar, 1)
})

test_that("segment_episodes_by_covars handles NA covariate values", {
  df <- tibble(
    id = rep(1, 5),
    date = seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
    covar = c(1, 1, NA, NA, 2)
  ) |>
    group_by(id)

  result <- segment_episodes_by_covars(df, date, covar_cols = covar)

  expect_equal(nrow(result), 3)
  expect_equal(result$segment_id, c("1.1", "1.2", "1.3"))
  expect_equal(result$covar, c(1, NA, 2))
})

test_that("segment_episodes_by_covars includes fixed variables", {
  df <- tibble(
    id = rep(1, 5),
    date = seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
    covar = c(1, 1, 2, 2, 2),
    fixed_var = "value"
  ) |>
    group_by(id)

  result <- segment_episodes_by_covars(df, date, covar_cols = covar)

  expect_true("fixed_var" %in% names(result))
  expect_equal(unique(result$fixed_var), "value")
})

test_that("segment_episodes_by_covars excludes varying variables", {
  df <- tibble(
    id = rep(1, 5),
    date = seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
    covar = c(1, 1, 2, 2, 2),
    varying_var = 1:5
  ) |>
    group_by(id)

  expect_message(
    result <- segment_episodes_by_covars(df, date, covar_cols = covar),
    "Varying variables excluded"
  )

  expect_false("varying_var" %in% names(result))
})

test_that("segment_episodes_by_covars handles episodes filtering", {
  df <- tibble(
    id = rep(1, 10),
    date = c(
      seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
      seq.Date(from = as.Date("2023-01-10"), by = "days", length.out = 5)
    ),
    covar = c(1, 1, 2, 2, 2, 3, 3, 4, 4, 4)
  ) |>
    group_by(id)

  all_episodes <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days"
  )
  expect_equal(nrow(all_episodes), 5)

  first_episode <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days",
    episodes = "first"
  )
  expect_equal(nrow(first_episode), 2)
  expect_equal(unique(first_episode$episode_id), 1)

  last_episode <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days",
    episodes = "last"
  )
  expect_equal(nrow(last_episode), 3)
  expect_equal(unique(last_episode$episode_id), 2)

  specific_episode <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days",
    episodes = 2
  )
  expect_equal(nrow(specific_episode), 3)
  expect_equal(unique(specific_episode$episode_id), 2)

  non_existent_episode <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days",
    episodes = 3
  )
  expect_equal(nrow(non_existent_episode), 0)
})

test_that("segment_episodes_by_covars calculates gap_days correctly", {
  df <- tibble(
    id = rep(1, 10),
    date = c(
      seq.Date(from = as.Date("2023-01-01"), by = "days", length.out = 5),
      seq.Date(from = as.Date("2023-01-10"), by = "days", length.out = 5)
    ),
    covar = c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4)
  ) |>
    group_by(id)

  result <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days"
  )

  expect_equal(result$gap_days[1], 0)
  expect_equal(result$gap_days[2], 4)
  expect_equal(result$gap_days[3], 0)
  expect_true(is.na(result$gap_days[5]))
})

test_that("segment_episodes_by_covars handles status correctly", {
  today <- Sys.Date()
  df <- tibble(
    id = rep(1, 10),
    date = c(
      seq.Date(from = today - 100, by = "days", length.out = 5),
      seq.Date(from = today - 10, by = "days", length.out = 5)
    ),
    covar = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
  ) |>
    group_by(id)

  result1 <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days",
    inactive_threshold = 5, inactive_unit = "days"
  )
  expect_equal(result1$status[1], "Gap")
  expect_equal(result1$status[2], "Active")

  result2 <- segment_episodes_by_covars(df, date,
    covar_cols = covar,
    gap_threshold = 1, gap_unit = "days",
    inactive_threshold = 20, inactive_unit = "days"
  )
  expect_equal(result2$status[1], "Gap")
  expect_equal(result2$status[2], "Inactive")
})
