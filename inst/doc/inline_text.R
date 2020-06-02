## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(gtsummary)
library(dplyr)

## -----------------------------------------------------------------------------
trial2 =
  trial %>%
  select(trt, marker, stage)

## -----------------------------------------------------------------------------
tab1 <- tbl_summary(trial2, by = trt)
tab1

## -----------------------------------------------------------------------------
# build logistic regression model
m1 = glm(response ~ age + stage, trial, family = binomial(link = "logit"))


## -----------------------------------------------------------------------------
tbl_m1 <- tbl_regression(m1, exponentiate = TRUE)
tbl_m1

