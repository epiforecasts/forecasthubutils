test_that("make_weekly function works", {

  df <- data.frame(value = 1:30,
                   date = seq.Date(from = as.Date("2020-12-20"),
                                   to = as.Date("2021-01-18"), by = "days"),
                   location = "GM",
                   location_name = "Germany")

  computed <- make_weekly(df, group_by = "location")

  result <- data.table(location = "GM",
                       value = c(28, 77, 126, 175),
                       target_end_date = as.Date(c("2020-12-26",
                                                   "2021-01-02",
                                                   "2021-01-09",
                                                   "2021-01-16")))

  expect_equal(computed, result)
})

