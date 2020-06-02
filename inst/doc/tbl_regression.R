## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(gtsummary)
library(dplyr)

## ---- message=FALSE-----------------------------------------------------------
# build logistic regression model
m1 = glm(response ~ age + stage + grade, trial, family = binomial(link = "logit"))

# view raw model results
summary(m1)$coefficients


## ---- message=FALSE-----------------------------------------------------------
# format results
tbl_regression(m1, exponentiate = TRUE)

## ---- message=FALSE-----------------------------------------------------------
# format results into data frame with global p-values
m1 %>%
  tbl_regression(
    exponentiate = TRUE, 
    pvalue_fun = function(x) style_pvalue(x, digits = 2),
    estimate_fun = function(x) style_ratio(x, digits = 3)
  ) %>% 
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>% 
  italicize_levels()

## -----------------------------------------------------------------------------
tbl_regression(m1) %>% names()

## -----------------------------------------------------------------------------
tbl_regression(m1) %>% as_gt(return_calls = TRUE) %>% head(n = 3)

## ----as_gt2-------------------------------------------------------------------
tbl_regression(m1, exponentiate = TRUE) %>%
  as_gt(include = -tab_footnote)

## ----tbl_uvregression---------------------------------------------------------
trial %>%
  select(-death, -ttdeath, -stage) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = function(x) style_pvalue(x, digits = 2)
  ) %>%
  # overrides the default that shows p-values for each level
  add_global_p() %>%
  # adjusts global p-values for multiple testing (default method: FDR)
  add_q() %>%
  # bold p-values under a given threshold (default 0.05)
  bold_p() %>%
  # now bold q-values under the threshold of 0.10
  bold_p(t = 0.10, q = TRUE) %>%
  bold_labels()

