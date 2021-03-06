context("test-tbl_merge")
testthat::skip_on_cran()

library(survival)
library(purrr)
# univariate regression models
t0 <-
  trial %>%
  dplyr::select(response, trt, grade, age) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
  )
# MVA logistic regression
t1 <-
  glm(response ~ trt + grade + age, trial, family = binomial) %>%
  tbl_regression(
    exponentiate = TRUE
  )
# MVA cox regression
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(
    exponentiate = TRUE
  )


# tbl_stack adjusted model
covars <- c("trt", "age")

# get model covariates adjusted by stage and grade
adj_mods <- map(covars, ~
                  coxph(
                    as.formula(
                      paste("Surv(ttdeath, death) ~ grade + ", .x)
                    ),
                    trial
                  ) %>%
                  tbl_regression(
                    include = .x,
                    exponentiate = TRUE
                  ))

# now get stage and grade models adjusted for each other
adj_mods[["grade_mod"]] <- coxph(
  as.formula(
    paste("Surv(ttdeath, death) ~ grade")
  ),
  trial
) %>%
  tbl_regression(
    exponentiate = TRUE
  )

# stack all your adjusted models
t3 <- tbl_stack(adj_mods)


# putting all tables together
t4 <-
  tbl_merge(
    tbls = list(t0, t1, t2, t3),
    tab_spanner = c("UVA Tumor Response", "MVA Tumor Response", "MVA Time to Death", "TTD Adjusted for grade")
  )

t5 <-
  trial %>%
  dplyr::select(age, grade, response) %>%
  tbl_summary(missing = "no") %>%
  add_n()
t6 <-
  tbl_uvregression(
    trial %>% dplyr::select(ttdeath, death, age, grade, response),
    method = coxph,
    y = Surv(ttdeath, death),
    exponentiate = TRUE,
    hide_n = TRUE
  )

test_that("no errors/warnings with standard use", {
  expect_error(t4, NA)
  expect_warning(t4, NA)
  expect_error(tbl_merge(tbls = list(t5, t6)), NA)
  expect_warning(tbl_merge(tbls = list(t5, t6)), NA)
})

test_that("number of rows the same after joining", {
  expect_true(nrow(t0$table_body) == nrow(t4$table_body))
  expect_true(nrow(t1$table_body) == nrow(t4$table_body))
  expect_true(nrow(t2$table_body) == nrow(t4$table_body))
  expect_true(nrow(t3$table_body) == nrow(t4$table_body))
})

test_that("tbl_merge throws errors", {
  expect_error(tbl_merge(t1), NULL)
  expect_error(tbl_merge(list(mtcars)), NULL)
  expect_error(tbl_merge(tbls = list(t5)), NULL)
  expect_error(tbl_merge(tbls = list(t5, t6), tab_spanner = c("Table")), NULL)
})

test_that("tbl_merge throws errors", {
  expect_equal(
    trial %>%
    split(.$trt) %>%
    purrr::map(tbl_summary, by = stage) %>%
    tbl_merge(tab_spanner = c("Drug A", "Drug B")) %>%
    purrr::pluck("table_header", "spanning_header") %>%
    unique(),
    c(NA, "Drug A", "Drug B")
  )
})
