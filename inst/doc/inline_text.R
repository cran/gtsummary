## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>"
)
gt_compact_fun <- function(x) {
  gt::tab_options(x,
    table.font.size = "small",
    data_row.padding = gt::px(1),
    summary_row.padding = gt::px(1),
    grand_summary_row.padding = gt::px(1),
    footnotes.padding = gt::px(1),
    source_notes.padding = gt::px(1),
    row_group.padding = gt::px(1)
  )
}

## ---- echo = FALSE, results = 'asis'------------------------------------------
# we do NOT want the vignette to build on CRAN...it's taking too long
if (!identical(Sys.getenv("IN_PKGDOWN"), "true") &&
  !tolower(as.list(Sys.info())$user) %in% c("sjobergd", "currym", "whitingk", "whiting")) {
  msg <-
    paste(
      "View this vignette on the",
      "[package website](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)."
    )
  cat(msg)
  knitr::knit_exit()
}

## ----message = FALSE, warning=FALSE-------------------------------------------
# install.packages("gtsummary")
library(gtsummary)

## -----------------------------------------------------------------------------
trial2 <-
  trial %>%
  select(trt, marker, stage)

## -----------------------------------------------------------------------------
tab1 <- tbl_summary(trial2, by = trt)
tab1

## -----------------------------------------------------------------------------
# build logistic regression model
m1 <- glm(response ~ age + stage, trial, family = binomial(link = "logit"))

## -----------------------------------------------------------------------------
tbl_m1 <- tbl_regression(m1, exponentiate = TRUE)
tbl_m1

## ---- echo = FALSE------------------------------------------------------------
tibble::tribble(
  ~Parameter,       ~Description,
  "`{estimate}`",   "primary estimate (e.g. model coefficient, odds ratio)",
  "`{conf.low}`",   "lower limit of confidence interval",
  "`{conf.high}`",  "upper limit of confidence interval",
  "`{p.value}`",    "p-value",
  "`{conf.level}`", "confidence level of interval",
  "`{N}`",          "number of observations"
) %>%
  gt::gt() %>%
  gt::fmt_markdown(columns = c(Parameter)) %>%
  gt_compact_fun()

