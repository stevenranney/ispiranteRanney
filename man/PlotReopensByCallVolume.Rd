\name{PlotReopensByCallVolume}
\alias{PlotReopensByCallVolume}
\title{Create a plot of the reopens by call volume}
\description{This function plots the number of reopens and produces a plot 
of those values as a function of call volume. }
\usage{PlotReopensByCallVolume(dF, ...)}
\arguments{
  \item{dF}{The dataFrame for which to plot the number of reopens as a function
of call volume.}
  \item{\dots}{Arguments to be passed to other functions.  Specifically, this can take
the form of identifying which \code{date} value is of interest.  For example
the sample datasets provided all had either at least \code{assigned_at} 
or \code{created_at} values.  The \code{\link{AssignDateAndDay}} can used
either of these values as the date of interest. }
}
\details{Plots are generated with \code{\link{ggplot}}

This function uses a linear model to evaluate the relationship between
the number of reopens and call volume (i.e., calls per day).  If the red line 
(the result of the \code{\link{lm}} in the produced plot trends down, then 
the higher the call volume the more times a ticket gets reopened.  If the red 
line is parallel or very near to parallel, then there is no relationship 
between the number of reopens and call volume.

In most cases, there are very few or no reopen data.  In many cases, 
there are no reopens for a given ticket.  As a result, these data are sparse.}


\author{Steven H. Ranney
Contact: \email{Steven.Ranney@gmail.com}
Steven Ranney}





