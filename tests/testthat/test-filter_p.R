context("test-filter_p")
testthat::skip_on_cran()

test_that("no errors/warnings with standard use after tbl_summary() and add_p()", {
  table1 <- trial %>%
    tbl_summary(by = trt) %>%
    add_p()

  expect_error(filter_p(table1, t = 0.2), NA)
  expect_warning(filter_p(table1, t = 0.2), NA)
})


test_that("no errors/warnings with standard use after tbl_regression() and add_global_p()", {
  regress1 <- glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
    tbl_regression(exponentiate = TRUE) %>%
    add_global_p()

  expect_error(filter_p(regress1, t = 0.2), NA)
  expect_warning(filter_p(regress1, t = 0.2), NA)
})

test_that("no errors/warnings with standard use after tbl_regression() and add_global_p()", {
  regress2 <-
    tbl_uvregression(
      trial %>% dplyr::select(response, age, grade),
      method = glm,
      y = response,
      method.args = list(family = binomial),
      exponentiate = TRUE
    ) %>%
    add_global_p()

  expect_error(filter_p(regress2, t = 0.2), NA)
  expect_warning(filter_p(regress2, t = 0.2), NA)
})


test_that("expect error if no p value in table 1", {
  table1 <- trial %>% tbl_summary(by = trt)

  expect_error(
    filter_p(table1),
    NULL
  )
})

test_that("expect error if q = TRUE and no q values in table 1", {
  table1 <- trial %>%
    tbl_summary(by = trt) %>%
    add_p()

  expect_error(
    filter_p(table1, q = TRUE),
    NULL
  )
})
