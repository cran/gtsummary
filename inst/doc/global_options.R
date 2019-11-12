## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## ----exit_early, include = FALSE, eval = !requireNamespace("gt")---------
#  knitr::knit_exit()

## ------------------------------------------------------------------------
#  options(gtsummary.pvalue_fun = function(x) style_pvalue(x, digits = 2))

## ------------------------------------------------------------------------
#  options(gtsummary.pvalue_fun = purrr::partial(style_pvalue, digits = 2))

## ------------------------------------------------------------------------
#  options(gtsummary.pvalue_fun = function(x) sprintf(x * 100, fmt='%#.1f'))

## ------------------------------------------------------------------------
#  options(gtsummary.print_engine = "kable")

## ------------------------------------------------------------------------
#  options(gtsummary.print_engine = "gt")

## ---- eval = TRUE--------------------------------------------------------
library(gtsummary)
tbl_summary(trial)$gt_calls %>% head(n = 4)

## ------------------------------------------------------------------------
#  options(gtsummary.as_gt.addl_cmds = "gt::tab_options(table.font.size = 'small', row.padding = gt::px(1))")

