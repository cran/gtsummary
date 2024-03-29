% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_custom_summary.R
\name{tbl_custom_summary}
\alias{tbl_custom_summary}
\title{Create a table of summary statistics using a custom summary function}
\usage{
tbl_custom_summary(
  data,
  by = NULL,
  label = NULL,
  stat_fns,
  statistic,
  digits = NULL,
  type = NULL,
  value = NULL,
  missing = NULL,
  missing_text = NULL,
  include = everything(),
  overall_row = FALSE,
  overall_row_last = FALSE,
  overall_row_label = NULL
)
}
\arguments{
\item{data}{A data frame}

\item{by}{A column name (quoted or unquoted) in \code{data}.
Summary statistics will be calculated separately for each level of the \code{by}
variable (e.g. \code{by = trt}). If \code{NULL}, summary statistics
are calculated using all observations. To stratify a table by two or more
variables, use \code{tbl_strata()}}

\item{label}{List of formulas specifying variables labels,
e.g. \code{list(age ~ "Age", stage ~ "Path T Stage")}.  If a
variable's label is not specified here, the label attribute
(\code{attr(data$age, "label")}) is used.  If
attribute label is \code{NULL}, the variable name will be used.}

\item{stat_fns}{Formula or list of formulas specifying the function to be
used to compute the statistics (see below for details and examples). You can
also use dedicated helpers such as \code{\link[=continuous_summary]{continuous_summary()}}, \code{\link[=ratio_summary]{ratio_summary()}}
or \code{\link[=proportion_summary]{proportion_summary()}}.}

\item{statistic}{List of formulas specifying the \code{\link[glue:glue]{glue::glue()}} pattern to
display the statistics for each variable. The statistics should be returned
by the functions specified in \code{stat_fns} (see below for details and
examples).}

\item{digits}{List of formulas specifying the number of decimal
places to round summary statistics. If not specified,
\code{tbl_summary} guesses an appropriate number of decimals to round statistics.
When multiple statistics are displayed for a single variable, supply a vector
rather than an integer.  For example, if the
statistic being calculated is \code{"{mean} ({sd})"} and you want the mean rounded
to 1 decimal place, and the SD to 2 use \code{digits = list(age ~ c(1, 2))}. User
may also pass a styling function: \code{digits = age ~ style_sigfig}}

\item{type}{List of formulas specifying variable types. Accepted values
are \code{c("continuous", "continuous2", "categorical", "dichotomous")},
e.g. \code{type = list(age ~ "continuous", female ~ "dichotomous")}.
If type not specified for a variable, the function
will default to an appropriate summary type. See below for details.}

\item{value}{List of formulas specifying the value to display for dichotomous
variables. gtsummary selectors, e.g. \code{all_dichotomous()}, cannot be used
with this argument. See below for details.}

\item{missing}{Indicates whether to include counts of \code{NA} values in the table.
Allowed values are \code{"no"} (never display NA values),
\code{"ifany"} (only display if any NA values), and \code{"always"}
(includes NA count row for all variables). Default is \code{"ifany"}.}

\item{missing_text}{String to display for count of missing observations.
Default is \code{"Unknown"}.}

\item{include}{variables to include in the summary table. Default is \code{everything()}}

\item{overall_row}{Logical indicator to display an overall row. Default is
\code{FALSE}. Use \code{\link[=add_overall]{add_overall()}} to add an overall column.}

\item{overall_row_last}{Logical indicator to display overall row last in
table. Default is \code{FALSE}, which will display overall row first.}

\item{overall_row_label}{String indicating the overall row label. Default is
\code{"Overall"}.}
}
\value{
A \code{tbl_custom_summary} and \code{tbl_summary} object
}
\description{
\lifecycle{experimental}
The \code{tbl_custom_summary()} function calculates descriptive statistics for
continuous, categorical, and dichotomous variables.
This function is similar to \code{\link[=tbl_summary]{tbl_summary()}} but allows you to provide
a custom function in charge of computing the statistics (see Details).
}
\section{Similarities with \code{tbl_summary()}}{

Please refer to the help file of \code{\link[=tbl_summary]{tbl_summary()}} regarding the use of select
helpers, and arguments \code{include}, \code{by}, \code{type}, \code{value}, \code{digits}, \code{missing} and
\code{missing_text}.
}

\section{\code{stat_fns} argument}{

The \code{stat_fns} argument specify the custom function(s) to be used for computing
the summary statistics. For example, \code{stat_fns = everything() ~ foo}.

Each function may take the following arguments:
\code{foo(data, full_data, variable, by, type, ...)}
\itemize{
\item \verb{data=} is the input data frame passed to \code{tbl_custom_summary()}, subset
according to the level of \code{by} or \code{variable} if any, excluding \code{NA}
values of the current \code{variable}
\item \verb{full_data=} is the full input data frame passed to \code{tbl_custom_summary()}
\item \verb{variable=} is a string indicating the variable to perform the
calculation on
\item \verb{by=} is a string indicating the by variable from \verb{tbl_custom_summary=},
if present
\item \verb{type=} is a string indicating the type of variable
(continuous, categorical, ...)
\item \verb{stat_display=} a string indicating the statistic to display (for the
\code{statistic} argument, for that variable)
}

The user-defined does not need to utilize each of these inputs. It's
encouraged the user-defined function accept \code{...} as each of the arguments
\emph{will} be passed to the function, even if not all inputs are utilized by
the user's function, e.g. \code{foo(data, ...)} (see examples).

The user-defined function should return a one row \code{\link[dplyr:reexports]{dplyr::tibble()}} with
one column per summary statistics (see examples).
}

\section{statistic argument}{

The statistic argument specifies the statistics presented in the table. The
input is a list of formulas that specify the statistics to report. For example,
\code{statistic = list(age ~ "{mean} ({sd})")}.
A statistic name that appears between curly brackets
will be replaced with the numeric statistic (see \code{\link[glue:glue]{glue::glue()}}).
All the statistics indicated in the statistic argument should be returned
by the functions defined in the \code{stat_fns} argument.

When the summary type is \code{"continuous2"}, pass a vector of statistics. Each element
of the vector will result in a separate row in the summary table.

For both categorical and continuous variables, statistics on the number of
missing and non-missing observations and their proportions are also available
to display.
\itemize{
\item \code{{N_obs}} total number of observations
\item \code{{N_miss}} number of missing observations
\item \code{{N_nonmiss}} number of non-missing observations
\item \code{{p_miss}} percentage of observations missing
\item \code{{p_nonmiss}} percentage of observations not missing
}

Note that for categorical variables, \code{{N_obs}}, \code{{N_miss}} and \code{{N_nonmiss}} refer
to the total number, number missing and number non missing observations
in the denominator, not at each level of the categorical variable.

It is recommended to use \code{\link[=modify_footnote]{modify_footnote()}} to properly describe the
displayed statistics (see examples).
}

\section{Caution}{


The returned table is compatible with all \code{gtsummary} features applicable
to a \code{tbl_summary} object, like \code{\link[=add_overall]{add_overall()}}, \code{\link[=modify_footnote]{modify_footnote()}} or
\code{\link[=bold_labels]{bold_labels()}}.

However, some of them could be inappropriate in such case. In particular,
\code{\link[=add_p]{add_p()}} do not take into account the type of displayed statistics and
always return the p-value of a comparison test of the current variable
according to the \code{by} groups, which may be incorrect if the displayed
statistics refer to a third variable.
}

\section{Example Output}{

\if{html}{Example 1}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/ddsjoberg/gtsummary/main/man-images/tbl_custom_summary_ex1.png" alt="image of rendered example table" style="width:45\%;">
}}

\if{html}{Example 2}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/ddsjoberg/gtsummary/main/man-images/tbl_custom_summary_ex2.png" alt="image of rendered example table" style="width:45\%;">
}}

\if{html}{Example 3}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/ddsjoberg/gtsummary/main/man-images/tbl_custom_summary_ex3.png" alt="image of rendered example table" style="width:35\%;">
}}
}

\examples{
\donttest{
# Example 1 ----------------------------------
my_stats <- function(data, ...) {
  marker_sum <- sum(data$marker, na.rm = TRUE)
  mean_age <- mean(data$age, na.rm = TRUE)
  dplyr::tibble(
    marker_sum = marker_sum,
    mean_age = mean_age
  )
}

my_stats(trial)

tbl_custom_summary_ex1 <-
  trial \%>\%
  tbl_custom_summary(
    include = c("stage", "grade"),
    by = "trt",
    stat_fns = everything() ~ my_stats,
    statistic = everything() ~ "A: {mean_age} - S: {marker_sum}",
    digits = everything() ~ c(1, 0),
    overall_row = TRUE,
    overall_row_label = "All stages & grades"
  ) \%>\%
  add_overall(last = TRUE) \%>\%
  modify_footnote(
    update = all_stat_cols() ~ "A: mean age - S: sum of marker"
  ) \%>\%
  bold_labels()

# Example 2 ----------------------------------
# Use `data[[variable]]` to access the current variable
mean_ci <- function(data, variable, ...) {
  test <- t.test(data[[variable]])
  dplyr::tibble(
    mean = test$estimate,
    conf.low = test$conf.int[1],
    conf.high = test$conf.int[2]
  )
}

tbl_custom_summary_ex2 <-
  trial \%>\%
  tbl_custom_summary(
    include = c("marker", "ttdeath"),
    by = "trt",
    stat_fns = ~mean_ci,
    statistic = ~"{mean} [{conf.low}; {conf.high}]"
  ) \%>\%
  add_overall(last = TRUE) \%>\%
  modify_footnote(
    update = all_stat_cols() ~ "mean [95\% CI]"
  )

# Example 3 ----------------------------------
# Use `full_data` to access the full datasets
# Returned statistic can also be a character
diff_to_great_mean <- function(data, full_data, ...) {
  mean <- mean(data$marker, na.rm = TRUE)
  great_mean <- mean(full_data$marker, na.rm = TRUE)
  diff <- mean - great_mean
  dplyr::tibble(
    mean = mean,
    great_mean = great_mean,
    diff = diff,
    level = ifelse(diff > 0, "high", "low")
  )
}

tbl_custom_summary_ex3 <-
  trial \%>\%
  tbl_custom_summary(
    include = c("grade", "stage"),
    by = "trt",
    stat_fns = ~diff_to_great_mean,
    statistic = ~"{mean} ({level}, diff: {diff})",
    overall_row = TRUE
  ) \%>\%
  bold_labels()
}
}
\seealso{
Review \link[=syntax]{list, formula, and selector syntax} used throughout gtsummary

Other tbl_summary tools: 
\code{\link{add_n.tbl_summary}()},
\code{\link{add_overall}()},
\code{\link{add_p.tbl_summary}()},
\code{\link{add_q}()},
\code{\link{add_stat_label}()},
\code{\link{bold_italicize_labels_levels}},
\code{\link{inline_text.tbl_summary}()},
\code{\link{inline_text.tbl_survfit}()},
\code{\link{modify}},
\code{\link{separate_p_footnotes}()},
\code{\link{tbl_merge}()},
\code{\link{tbl_split}()},
\code{\link{tbl_stack}()},
\code{\link{tbl_strata}()},
\code{\link{tbl_summary}()}

Other tbl_custom_summary tools: 
\code{\link{add_overall}()},
\code{\link{continuous_summary}()},
\code{\link{proportion_summary}()},
\code{\link{ratio_summary}()}
}
\author{
Joseph Larmarange
}
\concept{tbl_custom_summary tools}
\concept{tbl_summary tools}
