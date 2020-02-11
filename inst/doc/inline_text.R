## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----exit_early, include = FALSE, eval = !requireNamespace("gt")--------------
#  knitr::knit_exit()

## ---- include=FALSE-----------------------------------------------------------
library(gtsummary)
library(dplyr)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("gtsummary")
#  remotes::install_github("rstudio/gt", ref = gtsummary::gt_sha)
#  
#  library(gtsummary)
#  library(dplyr)

## -----------------------------------------------------------------------------
trial2 =
  trial %>%
  dplyr::select(trt, marker, stage)

## -----------------------------------------------------------------------------
tab1 <- tbl_summary(trial2, by = trt)
tab1

## -----------------------------------------------------------------------------
# build logistic regression model
m1 = glm(response ~ age + stage, trial, family = binomial(link = "logit"))


## -----------------------------------------------------------------------------
tbl_m1 <- tbl_regression(m1, exponentiate = TRUE)
tbl_m1
