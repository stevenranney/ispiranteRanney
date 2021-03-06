\name{PlotCallVolumeByDay}
\alias{PlotCallVolumeByDay}
\title{Create a plot of the resolution time by day of week}
\description{This function plots the resolution time (in hours) and produces 
a box plot of those values as a function of the day of week. }
\usage{PlotCallVolumeByDay(dF, ...)}
\arguments{
  \item{dF}{The dataFrame for which to plot resolution time by date level.}
  \item{\dots}{Arguments to be passed to other functions.  Specifically, this can take
the form of identifying which \code{date} value is of interest.  For example
the sample datasets provided all had either at least \code{assigned_at} 
or \code{created_at} values.  The \code{\link{AssignDateAndDay}} can used
either of these values as the date of interest. }
}
\details{This functions converts dates that are not represented in the data
frame to \code{0} values.  However, \code{0} value dates are only 'filled in;'
that is, the function only fills in dates.  It does not extend the date field.
For example, if the data frame has dates \code{2014-01-01}, \code{2014-01-03}, 
and \code{2014-01-05}, this function fills in the date gaps.

Plots are generated by the function \code{\link{ggplot}}}


\author{Steven H. Ranney
Contact: \email{Steven.Ranney@gmail.com}
Steven Ranney}





