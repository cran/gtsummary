#' Calculates and formats N's and percentages for categorical and dichotomous data
#'
#' @param data Data frame
#' @param variable Character variable name in `data` that will be tabulated
#' @param by Character variable name in `data` that Summary statistics for
#' `variable` are stratified
#' @param var_label String label
#' @param stat_display String that specifies the format of the displayed statistics.
#' The syntax follows \code{\link[glue]{glue}} inputs with n, N, and p as input options.
#' @param dichotomous_value If the output is dichotomous, then this is the value
#' of the variable that will be displayed.
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @return formatted summary statistics in a tibble.
#' @keywords internal

summarize_categorical <- function(data, variable, by, var_label,
                                  stat_display, dichotomous_value, missing) {

  # counting total missing
  tot_n_miss <- sum(is.na(data[[variable]]))

  # tidyr::complete throws warning `has different attributes on LHS and RHS of join`
  # when variable has label.  So deleting it.
  attr(data[[variable]], "label") <- NULL
  if (!is.null(by)) attr(data[[by]], "label") <- NULL
  # same thing when the class "labelled" is included when labeled with the Hmisc package
  class(data[[variable]]) <- setdiff(class(data[[variable]]), "labelled")
  if (!is.null(by)) {
    class(data[[by]]) <- setdiff(class(data[[by]]), "labelled")
  }

  # keeping the variable and by and renaming each
  data <-
    data %>%
    dplyr::select(dplyr::one_of(
      c(variable, by)
    )) %>%
    purrr::set_names(
      c(".variable", ".by")[1:length(c(variable, by))]
    )

  # grouping by variable (if applicable)
  if (!is.null(by)) {
    data <-
      data %>%
      dplyr::mutate(
        .by = paste0("stat_by", as.numeric(factor(.data$.by)))
      ) %>%
      dplyr::group_by(.data$.by)
  }

  # counting observations within variable (and by, if specified)
  results_var_count_n <-
    data %>%
    dplyr::filter(!is.na(.data$.variable)) %>%
    dplyr::count(.data$.variable) %>%
    dplyr::ungroup() %>%
    tidyr::complete(!!!rlang::syms(c(dplyr::group_vars(data), ".variable")), fill = list(n = 0))

  # grouping by variable (if applicable)
  if (!is.null(by)) {
    results_var_count_n <-
      results_var_count_n %>%
      dplyr::group_by(.data$.by)
  }

  # counting big N, and calculating percent
  results_var_count <-
    results_var_count_n %>%
    dplyr::mutate(
      N = sum(.data$n),
      p = ifelse(.data$N > 0, fmt_percent(.data$n / .data$N), "NA"),
      stat = glue::glue(stat_display) %>% as.character(),
      row_type = "level",
      label = as.character(.data$.variable)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(c(dplyr::group_vars(data), "row_type", ".variable", "label", "stat")))

  # counting missing vars
  results_missing <-
    data %>%
    dplyr::summarise(
      row_type = "missing",
      label = "Unknown",
      stat = sum(is.na(.data$.variable)) %>% as.character()
    )

  # appending missing N to bottom of data frame
  results_long_missing <-
    dplyr::bind_rows(
      results_var_count,
      results_missing
    )

  # transposing to wide (by levels have their own columns)
  if (!is.null(by)) {
    results_wide <-
      results_long_missing %>%
      tidyr::spread_(".by", "stat")
  }
  # if no by var, just rename stat to stat_overall
  if (is.null(by)) {
    results_wide <-
      results_long_missing %>%
      purrr::set_names("row_type", ".variable", "label", "stat_overall")
  }

  # only keeping dichotomous value (and NAs) is specified
  if (!is.null(dichotomous_value)) {
    results_final <-
      results_wide %>%
      dplyr::filter(.data$.variable %in% c(dichotomous_value, NA)) %>%
      dplyr::mutate(
        label = ifelse(!is.na(.data$.variable), var_label, .data$label),
        row_type = ifelse(!is.na(.data$.variable), "label", .data$row_type)
      )
  } else { # otherwise adding in a header row on top
    results_final <-
      dplyr::bind_rows(
        tibble::tibble(row_type = "label", label = var_label),
        results_wide
      )
  }


  # removing .variable from results
  results_final <- results_final %>% dplyr::select(-dplyr::one_of(".variable"))

  # excluding missing row if indicated
  if (missing == "no" | (missing == "ifany" & tot_n_miss == 0)) {
    results_final <-
      results_final %>%
      dplyr::filter(.data$row_type != 'missing')
  }

  return(results_final)
}

# summarize_categorical(
#   data = lung, variable = "ph.karno", by = "sex", var_label = "WTF",
#   stat_display = "{n} ({p}%)", dichotomous_value = NULL
# )
#
# summarize_categorical(
#   data = mtcars, variable = "cyl", by = NULL, var_label = "WTF",
#   stat_display = "{n} ({p}%)", dichotomous_value = NULL
# )
#
# summarize_categorical(
#   data = mtcars, variable = "cyl", by = "am", var_label = "WTF",
#   stat_display = "{n} ({p}%)", dichotomous_value = NULL
# )
