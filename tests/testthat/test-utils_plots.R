test_that("basic", {

  test_date <- as.Date("2000-01-10")

  df_test <- data.frame(
    project_id = c(1, 1, 2, 2, 3, 3, 3),
    cohort = c("A", "A", "A", "A", "B", "B", "B"),
    start_datetime = c(
      test_date + 100,
      test_date + 200,
      test_date + 300,
      test_date + 400,
      test_date + 200,
      test_date + 300,
      test_date - 100
    )
  )
  df_test$end_datetime <- df_test$start_datetime + c(rep(c(10,20,30),2),100)

  df_id_cohort <- df_test %>%
    dplyr::distinct(project_id, cohort)

  df_event_length <- driveanalytics::get_event_length(df_test, "cohort", TRUE)
  df_event_length_expected <- df_id_cohort %>%
    dplyr::mutate(length = c(32,42,153))

  testthat::expect_s3_class(df_event_length, "data.frame")
  testthat::expect_equal(
    df_event_length$length,
    df_event_length_expected$length)

})
