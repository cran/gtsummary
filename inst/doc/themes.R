## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE-----------------------------------------------------------
library(gtsummary); library(gt); library(dplyr)

trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  add_stat_label()

## ---- message=TRUE------------------------------------------------------------
set_gtsummary_theme(theme_gtsummary_journal(journal = "jama"))

## ---- message=FALSE, echo=FALSE-----------------------------------------------
trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  add_stat_label()

## ---- message=TRUE------------------------------------------------------------
set_gtsummary_theme(theme_gtsummary_journal(journal = "jama"))
set_gtsummary_theme(theme_gtsummary_compact())

## ---- message=FALSE, echo=FALSE-----------------------------------------------
trial %>%
  select(trt, age, grade) %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  add_stat_label()

## -----------------------------------------------------------------------------
reset_gtsummary_theme()

## -----------------------------------------------------------------------------
my_theme <-   
  list(
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
    "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
    "tbl_summary-str:continuous_stat" = "{median} ({p25} - {p75})",
    "tbl_summary-str:categorical_stat" = "{n} ({p})"
  )

## ---- eval=FALSE--------------------------------------------------------------
#  set_gtsummary_theme(my_theme)

## ---- echo=FALSE--------------------------------------------------------------
gtsummary:::df_theme_elements %>%
  filter(argument == FALSE) %>%
  select(-argument) %>%
  mutate(name = glue::glue("`{name}`"),
         example = glue::glue("`{example}`")) %>%
  group_by(fn) %>%
  gt() %>%
  cols_align(columns = everything(), align = "left") %>%
  cols_label(name = "Theme Element", desc = "Description",
             example = "Example") %>%
  fmt_markdown(columns = vars(name, example)) %>%
  tab_options(table.font.size = 'small',
                  data_row.padding = gt::px(1),
                  row_group.padding = gt::px(1))


## ---- echo=FALSE--------------------------------------------------------------
gtsummary:::df_theme_elements %>%
  filter(argument == TRUE) %>%
  select(fn, name) %>%
  group_by(fn) %>%
  mutate(arg_list = paste0("`", name, "`", collapse = ", ")) %>%
  select(fn, arg_list) %>%
  distinct() %>%
  gt() %>%
  cols_label(arg_list = "Theme Element") %>%
  fmt_markdown(columns = vars(arg_list)) %>%
  tab_options(table.font.size = 'small',
                  data_row.padding = gt::px(1),
                  row_group.padding = gt::px(1))

