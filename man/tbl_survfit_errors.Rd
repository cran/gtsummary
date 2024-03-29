% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_survfit_errors.R
\name{tbl_survfit_errors}
\alias{tbl_survfit_errors}
\title{Common Sources of Error with \code{tbl_survfit()}}
\description{
When functions \code{add_n()} and \code{add_p()} are run after \code{tbl_survfit()},
the original call to \code{survival::survfit()}
is extracted and the \verb{formula=} and \verb{data=} arguments are used to calculate
the N or p-value.

When the values of the \verb{formula=} and \verb{data=} are unavailable, the functions
cannot execute. Below are some tips to modify your code to ensure all functions
run without issue.
\enumerate{
\item Let \code{tbl_survfit()} construct the \code{survival::survfit()} for you by passing
a data frame to \code{tbl_survfit()}. The survfit model will be constructed
in a manner ensuring the formula and data are available.
This only works if you have a stratified model.

Instead of the following line

\if{html}{\out{<div class="sourceCode r">}}\preformatted{survfit(Surv(ttdeath, death) ~ trt, trial) \%>\%
  tbl_survfit(times = c(12, 24))
}\if{html}{\out{</div>}}

Use this code

\if{html}{\out{<div class="sourceCode r">}}\preformatted{trial \%>\%
  select(ttdeath, death, trt) \%>\%
  tbl_survfit(y = Surv(ttdeath, death), times = c(12, 24))
}\if{html}{\out{</div>}}
\item Construct an expression of the \code{survival::survfit()} before evaluating it.
Ensure the formula and data are available in the call by using the tidyverse
bang-bang operator, \verb{!!}.

Use this code

\if{html}{\out{<div class="sourceCode r">}}\preformatted{formula_arg <- Surv(ttdeath, death) ~ 1
data_arg <- trial
rlang::expr(survfit(!!formula_arg, !!data_arg)) \%>\%
  eval() \%>\%
  tbl_survfit(times = c(12, 24))
}\if{html}{\out{</div>}}
}
}
\keyword{internal}
