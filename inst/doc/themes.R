## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(gtsummary)

trial |> 
  tbl_summary(by = trt, include = c(age, grade)) |> 
  add_p()

## ----message=TRUE-------------------------------------------------------------
theme_gtsummary_journal(journal = "jama")

## ----message=FALSE, echo=FALSE------------------------------------------------
trial |> 
  tbl_summary(by = trt, include = c(age, grade)) |> 
  add_p()

## ----message=TRUE-------------------------------------------------------------
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

## ----message=FALSE, echo=FALSE------------------------------------------------
trial |> 
  tbl_summary(by = trt, include = c(age, grade)) |> 
  add_p()

## ----message=FALSE, echo = FALSE----------------------------------------------
set_gtsummary_theme(theme_gtsummary_journal(journal = "jama"))
set_gtsummary_theme(theme_gtsummary_compact())
set_gtsummary_theme(theme_gtsummary_language("es"))

## ----message=FALSE, echo=FALSE------------------------------------------------
trial |> 
  tbl_summary(by = trt, include = c(age, grade)) |> 
  add_p()

## -----------------------------------------------------------------------------
reset_gtsummary_theme()

## -----------------------------------------------------------------------------
my_theme <-
  list(
    # round large p-values to two places
    "pkgwide-fn:pvalue_fun" = label_style_pvalue(digits = 2),
    "pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 2, prepend_p = TRUE),
    # report median (Q1 - Q2) and n (percent) as default stats in `tbl_summary()`
    "tbl_summary-arg:statistic" = list(all_continuous() ~ "{median} ({p25} - {p75})",
                                       all_categorical() ~ "{n} ({p})")
  )

## ----eval=FALSE---------------------------------------------------------------
#  set_gtsummary_theme(my_theme)

## ----echo=FALSE---------------------------------------------------------------
gtsummary:::df_theme_elements %>%
  dplyr::filter(argument == FALSE, deprecated == FALSE) %>%
  dplyr::select(-argument, -deprecated) %>%
  dplyr::mutate(
    name = ifelse(!is.na(name), glue::glue("`{name}`"), NA_character_),
    example = ifelse(!is.na(example), glue::glue("`{example}`"), NA_character_)
  ) %>%
  dplyr::group_by(fn) %>%
  dplyr::arrange(dplyr::desc(fn == "Package-wide")) %>%
  gt::gt() %>%
  gt::cols_align(columns = everything(), align = "left") %>%
  gt::cols_label(
    name = "Theme Element", desc = "Description",
    example = "Example"
  ) %>%
  gt::fmt_markdown(columns = c(name, desc, example)) %>%
  gt::sub_missing(columns = everything(), missing_text = "") %>%
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1),
    row_group.padding = gt::px(1)
  )

## ----echo=FALSE---------------------------------------------------------------
gtsummary:::df_theme_elements %>%
  dplyr::filter(argument == TRUE, deprecated == FALSE) %>%
  dplyr::select(fn, name) %>%
  dplyr::group_by(fn) %>%
  dplyr::mutate(arg_list = paste0("`", name, "`", collapse = ", ")) %>%
  dplyr::select(fn, arg_list) %>%
  dplyr::distinct() %>%
  gt::gt() %>%
  gt::cols_label(arg_list = "Theme Element") %>%
  gt::fmt_markdown(columns = c(arg_list)) %>%
  gt::tab_options(
    table.font.size = "small",
    data_row.padding = gt::px(1),
    row_group.padding = gt::px(1)
  )

