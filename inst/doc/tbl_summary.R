## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

gt_compact_fun <- function(x) {
  gt::tab_options(x, 
                  table.font.size = 'small',
                  data_row.padding = gt::px(1),
                  summary_row.padding = gt::px(1),
                  grand_summary_row.padding = gt::px(1),
                  footnotes.padding = gt::px(1),
                  source_notes.padding = gt::px(1),
                  row_group.padding = gt::px(1))
}

## ---- echo = FALSE, results = 'asis'------------------------------------------
if (TRUE) {
# if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  msg <- 
    paste("View this vignette on the",
          "[package website](http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html).")
  cat(msg)
  knitr::knit_exit()
}

