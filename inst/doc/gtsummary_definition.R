## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(gtsummary)

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
tibble::tribble(
  ~Column, ~Description,
  "column", "Column name from `.$table_body`",
  "hide", "Logical indicating whether the column is hidden in the output",
  "align", "Specifies the alignment/justification of the column, e.g. 'center' or 'left'",
  "label", "Label that will be displayed (if column is displayed in output)",
  "interpret_label", "the {gt} function that is used to interpret the column label, `gt::md()` or `gt::html()`",
  "spanning_header", "Includes text printed above columns as spanning headers.",
  "interpret_spanning_header", "the {gt} function that is used to interpret the column spanning headers, `gt::md()` or `gt::html()`"
) %>%
  knitr::kable() 

## ---- echo=FALSE--------------------------------------------------------------
tibble::tribble(
  ~Column, ~Description,
  "column", "Column name from `.$table_body`",
  "rows", "expression selecting rows in `.$table_body`, `NA` indicates to add footnote to header",
  "footnote", "string containing footnote to add to column/row"
) %>%
  knitr::kable() 

## ---- echo=FALSE--------------------------------------------------------------
tibble::tribble(
  ~Column, ~Description,
  "column", "Column name from `.$table_body`",
  "rows", "expression selecting rows in `.$table_body`",
  "fmt_fun", "list of formatting/styling functions"
) %>%
  knitr::kable() 

## ---- echo=FALSE--------------------------------------------------------------
tibble::tribble(
  ~Column, ~Description,
  "column", "Column name from `.$table_body`",
  "rows", "expression selecting rows in `.$table_body`",
  "format_type", "one of `c('bold', 'italic', 'indent')`",
  "undo_text_format", "logical indicating where the formatting indicated should be undone/removed."
) %>%
  knitr::kable() 

## ---- echo=FALSE--------------------------------------------------------------
tibble::tribble(
  ~Column, ~Description,
  "column", "Column name from `.$table_body`",
  "rows", "expression selecting rows in `.$table_body`",
  "symbol", "string to replace missing values with, e.g. an em-dash"
) %>%
  knitr::kable() 

## ---- echo=FALSE--------------------------------------------------------------
tibble::tribble(
  ~Column, ~Description,
  "column", "Column name from `.$table_body`",
  "rows", "expression selecting rows in `.$table_body`",
  "pattern", "glue pattern directing how to combine/merge columns. The merged columns will replace the column indicated in 'column'."
) %>%
  knitr::kable() 

## -----------------------------------------------------------------------------
tbl_regression_ex$table_styling

## -----------------------------------------------------------------------------
tbl_regression_ex %>%
  purrr::pluck("table_body") %>%
  select(variable, row_type, label)

## ---- eval = FALSE------------------------------------------------------------
#  print.gtsummary <- function(x) {
#    get_theme_element("pkgwide-str:print_engine") %>%
#      switch(
#        "gt" = as_gt(x),
#        "flextable" = as_flex_table(x),
#        "huxtable" = as_hux_table(x),
#        "kable_extra" = as_kable_extra(x),
#        "kable" = as_kable(x)
#      ) %>%
#      print()
#  }

