## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(gtsummary)
library(purrr); library(dplyr); library(tibble)

tbl_regression_ex <-
  lm(age ~ grade + marker, trial) %>%
  tbl_regression() %>%
  bold_p(t = 0.5) 

tbl_summary_ex <-
  trial %>%
  select(trt, age, grade, response) %>%
  tbl_summary(by = trt)

## -----------------------------------------------------------------------------
tbl_summary_ex$table_body

## ---- echo=FALSE--------------------------------------------------------------
tribble(
  ~Column, ~Description,
  "column", "Column name from table_body",
  "label", "Label that will be displayed (if column is displayed in output)",
  "hide", "Logical indicating whether the column is hidden in the output",
  "text_interpret", "the {gt} function that is used to interpret the column label",
  "fmt_fun", "If the column needs to be formatted, this list column contains the function that performs the formatting.  Note, this is the function object; not the character name of a function.",
  "bold", "For columns that bold row conditionally, the column includes the threshold to bold below.  The most common use for this is to bold p-value below a threshold.",
  "footnote_abbrev", "Lists the abbreviation footnotes for a table.  All abbreviation footnotes are collated into a single footnote.  For example, 'OR = Odds Ratio' and 'CI = Confidence Interval' appear in a single footnote.",
  "footnote", "Lists the footnotes that will appear for each column.  Duplicates abbreviations will appear once."
) %>%
  knitr::kable() 

## -----------------------------------------------------------------------------
tbl_regression_ex$table_header

## -----------------------------------------------------------------------------
tbl_regression_ex$gt_calls

## -----------------------------------------------------------------------------
tbl_regression_ex$kable_calls

## -----------------------------------------------------------------------------
tbl_regression_ex$fmt_fun %>% names()

## -----------------------------------------------------------------------------
tbl_regression_ex %>%
  pluck("table_body") %>%
  select(variable, row_type, label)

## -----------------------------------------------------------------------------
gtsummary:::table_header_fill_missing(
  table_header = tibble(column = names(tbl_regression_ex$table_body))) 

## ---- eval = FALSE------------------------------------------------------------
#  print.gtsummary <- function(x) {
#    if (getOption("gtsummary.print_engine") == "gt") {
#      return(as_gt(x) %>% print())
#    }
#    else if (getOption("gtsummary.print_engine") == "kable") {
#      return(as_kable(x) %>% print())
#    }
#  }
#  

