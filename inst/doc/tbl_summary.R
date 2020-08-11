## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(gtsummary)
library(dplyr)

## ---- message=FALSE-----------------------------------------------------------
head(trial)

## -----------------------------------------------------------------------------
trial2 =
  trial %>%
  select(trt, marker, stage)

## ---- message=FALSE-----------------------------------------------------------
tbl_summary(trial2)

## -----------------------------------------------------------------------------
tbl_summary(trial2, by = trt) %>% add_p()

## ---- eval = FALSE------------------------------------------------------------
#  trial %>%
#    tbl_summary(by = trt, missing = "no") %>%
#    add_n() %>%
#    as_gt() %>%
#    <gt functions>

## -----------------------------------------------------------------------------
trial2 %>%
  # build base summary table
  tbl_summary(
    # split table by treatment variable
    by = trt,
    # change variable labels
    label = list(marker ~ "Marker, ng/mL",
                 stage ~ "Clinical T Stage"),
    # change statistics printed in table
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = list("marker" ~ c(1, 2))
  ) %>%
  # add p-values, report t-test, round large pvalues to two decimal place
  add_p(test = list(marker ~ "t.test"),
                 pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  # add statistic labels
  add_stat_label() %>%
  # bold variable labels, italicize levels
  bold_labels() %>%
  italicize_levels() %>%
  # bold p-values under a given threshold (default is 0.05)
  bold_p(t = 0.2) %>%
  # include percent in headers
  modify_header(stat_by = "**{level}**, N = {n} ({style_percent(p, symbol = TRUE)})")

## ---- eval=FALSE--------------------------------------------------------------
#  all_continuous()      all_categorical()      all_dichotomous()

## ---- eval=FALSE--------------------------------------------------------------
#  all_numeric()         all_integer()          all_logical()
#  all_factor()          all_character()        all_double()

## -----------------------------------------------------------------------------
trial %>%
  select(trt, response, age, stage, marker, grade) %>%
  tbl_summary(
    by = trt,
    type = list(c(response, grade) ~ "categorical"), # select by variables in c()
    statistic = list(all_continuous() ~ "{mean} ({sd})", 
                     all_categorical() ~ "{p}%") # select by summary type
  ) %>%
  add_p(test = list(contains("response") ~ "fisher.test", # select using functions in tidyselect
                    all_continuous() ~ "t.test"))

## -----------------------------------------------------------------------------
tbl_summary(trial2) %>% names()

## -----------------------------------------------------------------------------
tbl_summary(trial2) %>% as_gt(return_calls = TRUE) %>% head(n = 4)

## ----as_gt2-------------------------------------------------------------------
tbl_summary(trial2, by = trt) %>%
  as_gt(include = -tab_footnote) %>%
  gt::tab_spanner(label = gt::md("**Treatment Group**"),
                  columns = gt::starts_with("stat_"))

## -----------------------------------------------------------------------------
# loading the api data set
data(api, package = "survey")

## -----------------------------------------------------------------------------
svy_apiclus1 <- 
  survey::svydesign(
    id = ~dnum, 
    weights = ~pw, 
    data = apiclus1, 
    fpc = ~fpc
  ) 

## -----------------------------------------------------------------------------
svy_apiclus1 %>%
  tbl_svysummary(
    # stratify summary statistics by the "both" column
    by = both, 
    # summarize a subset of the columns
    include = c(cname, api00, api99, both),
    # adding labels to table
    label = list(
      cname ~ "County",
      api00 ~ "API in 2000",
      api99 ~ "API in 1999"
    )
  ) %>%
  # comparing values by "both" column
  add_p() %>%
  add_overall() %>%
  # adding spanning header
  modify_spanning_header(starts_with("stat_") ~ "**Met Both Targets**")

## -----------------------------------------------------------------------------
d <- dplyr::as_tibble(Titanic)
head(d, n = 10)

## -----------------------------------------------------------------------------
d %>%
  survey::svydesign(data = ., ids = ~ 1, weights = ~ n) %>%
  tbl_svysummary()

## -----------------------------------------------------------------------------
trial %>%
  tbl_cross(row = stage,
    col = trt,
    percent = "cell") %>%
  add_p()

