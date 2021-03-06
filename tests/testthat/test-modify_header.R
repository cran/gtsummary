context("test-modify_header")
testthat::skip_on_cran()

tbl_summary_noby <- trial %>% tbl_summary()
tbl_summary_by <- trial %>% tbl_summary(by = trt)
tbl_svysummary_by <-
  survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) %>%
  tbl_svysummary(by = Survived)

test_that("input checks", {
  expect_error(
    tbl_summary_noby %>% modify_header(stat_0 = "test"),
    NA
  )

  expect_error(
    tbl_summary_noby %>% modify_header(stat_0 ~ "test"),
    NA
  )

  expect_error(
    tbl_summary_noby %>% modify_header(),
    NA
  )

  expect_error(
    tbl_summary_noby %>% modify_header(label = c("test", "test2")),
    "*"
  )
})

test_that("checking glue inserts to headers", {
  expect_error(
    tbl1 <-
      tbl_summary_by %>%
      modify_header(
        list(
          all_stat_cols() ~ "{level} ({n}/{N}; {style_percent(p)}%)",
          label ~ "Variable (N = {N})"
        )
      ),
    NA
  )

  expect_equal(
    tbl1$table_header %>% filter(hide == FALSE) %>% pull(label),
    c("Variable (N = 200)", "Drug A (98/200; 49%)", "Drug B (102/200; 51%)")
  )

  expect_error(
    tbl2 <-
      tbl_svysummary_by %>%
      modify_header(
        list(
          all_stat_cols() ~ "{level} ({n}/{N}; {style_percent(p)}%): Unweighted {n_unweighted}/{N_unweighted}; {style_percent(p_unweighted)}%",
          label ~ "Variable (N = {N}: Unweighted {N_unweighted})"
        )
      ),
    NA
  )

  expect_equal(
    tbl2$table_header %>% filter(hide == FALSE) %>% pull(label),
    c("Variable (N = 2201: Unweighted 32)",
      "No (1490/2201; 68%): Unweighted 16/32; 50%",
      "Yes (711/2201; 32%): Unweighted 16/32; 50%")
  )

  expect_error(
    tbl3 <-
      lm(mpg ~ hp, mtcars) %>%
      tbl_regression() %>%
      modify_header(label ~ "Variable (N = {N})"),
    NA
  )
  expect_equal(
    tbl3$table_header %>% filter(column == "label") %>% pull(label),
    c("Variable (N = 32)")
  )
})
