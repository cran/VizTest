% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{print.viztest}
\alias{print.viztest}
\title{Print Method for viztest Objects}
\usage{
\method{print}{viztest}(x, ..., best = TRUE, missed_tests = TRUE, level = NULL)
}
\arguments{
\item{x}{An object of class \code{viztest}.}

\item{...}{Other arguments, currently not implemented.}

\item{best}{Logical indicating whether the results should be filtered to include only the best level(s) or include all levels}

\item{missed_tests}{Logical indicating whether the tests not represented by the optimal visual testing intervals should be displayed}

\item{level}{Which level should be used as the optimal one.  If \code{NULL}, the easiest optimal level will be used.  Easiness is measured by the sum of the overlap
in confidence intervals for insignificant tests plus the distance between the lower and upper bound for tests that are significant.}
}
\value{
Printed results that give the level(s) that correspond most closely with the pairwise test results.  The values returned are the smallest,
largest, middle and easiest.  By default this function also reports the tests that are not captured by the (non-)overlaps in confidence intervals
when each different level is used.
}
\description{
Prints a summary of the results from the \code{viztest()} function.
}
\details{
The results are printed in such a way that the range of optional levels is produced including the range along with two candidates for the
best levels to use - middle and easiest.

Prints the results from the viztest function
}
