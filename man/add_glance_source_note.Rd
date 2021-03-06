% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_glance_source_note.R
\name{add_glance_source_note}
\alias{add_glance_source_note}
\title{Add glance statistics}
\usage{
add_glance_source_note(
  x,
  include = everything(),
  label = NULL,
  fmt_fun = NULL,
  glance_fun = broom::glance,
  sep1 = " = ",
  sep2 = "; ",
  ...
)
}
\arguments{
\item{x}{'tbl_regression' object}

\item{include}{tidyselect list of statistics to include. Default is \code{everything()}}

\item{label}{use to update statistic labels}

\item{fmt_fun}{use to update default formatting function. Default is
\code{everything() ~ purrr::partial(style_sigfig, digits = 3)}}

\item{glance_fun}{function to calculate and return glance statistics.
Default is \code{broom::glance()}}

\item{sep1}{Separator between statistic name and statistic.
Default is \code{" = "}, e.g. \code{"R2 = 0.456"}}

\item{sep2}{Separator between statistics. Default is \code{"; "}}

\item{...}{additional arguments passed to \code{broom::glance()}}
}
\description{
\lifecycle{experimental}
Add the statistics returned in \code{broom::glance()} as a table source note.
}
\section{Default Labels}{

The following statistics have set default labels when being printed.
When there is no default, the label is the column name from \code{broom::glance()}.\tabular{ll}{
   Statistic Name \tab Default Label \cr
   r.squared \tab R² \cr
   adj.r.squared \tab Adjusted R² \cr
   p.value \tab p-value \cr
   logLik \tab log-likelihood \cr
   statistic \tab Statistic \cr
   df.residual \tab Residual df \cr
   null.deviance \tab Null deviance \cr
   df.null \tab Null df \cr
   nevent \tab N events \cr
   concordance \tab c-index \cr
   std.error.concordance \tab c-index SE \cr
}
}

\section{Example Output}{

\if{html}{Example 1}

\if{html}{\figure{add_glance_source_note_ex1.png}{options: width=64\%}}
}

\examples{
# Example 1 ----------------------------------
add_glance_source_note_ex1 <-
  lm(age ~ marker + grade, trial) \%>\%
  tbl_regression() \%>\%
  add_glance_source_note(
    label = list(df  ~ "Degrees of Freedom", sigma ~ "\U03C3"),
    fmt_fun = df ~ style_number,
    include = c(r.squared, AIC, sigma, df)
  )
}
