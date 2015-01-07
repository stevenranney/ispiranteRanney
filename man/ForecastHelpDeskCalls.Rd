\name{ForecastHelpDeskCalls}
\alias{ForecastHelpDeskCalls}
\title{Forecast the number of calls that will come into a help desk }
\description{This function extracts the number of calls received daily at a 
help desk and forecasts using an Autoregressive Integrated Moving Average (ARIMA) 
model for a number of days into the future.  }
\usage{ForecastHelpDeskCalls(dF, distance = 31, ...)}
\arguments{
  \item{dF}{The dF that needs an \code{weekday} value.}
  \item{distance}{How far into the future should the forecast be made?  A \code{numeric}
value passed to the \code{forecast} statement that shows the number of 
calls to the helpdesk in \code{distance} number of days.  For a monthly 
forecast, \code{distance = 365/12} is appropriate.  \code{distance = 365/4}
is appropriate for quarterly predictions.  Values could theoretically 
range from \code{1} to anything \code{>1}.  Here, the defaults
is to forecast \code{31} days into the future.}
  \item{\dots}{Arguments to be passed to other functions.  Specifically, this can take
the form of identifying which \code{date} value is of interest.  For example
the sample datasets provided all had either at least \code{assigned_at} 
or \code{created_at} values.  The \code{\link{AssignDateAndDay}} can used
either of these values as the date of interest.}
}
\details{This function calls the \code{\link{AssignDateAndDay}} function to 
assign the \code{day} values used.  The \code{dayOfWeek} values are used
in calculating the number of calls in a given day.

Dates in the \code{dF} passed to this function do not need to be 
sequential.  This function 'fills in' date gaps that are not represented in 
the dataFrame and assigns dates that do not appear in the \code{dF} a 
value of \code{0}.

For the purposes of this proposal, this function only forecasts with 
\code{\link{arima}} though other forecasting models are available.  In many 
cases, selecting the right model and correct model parameters requires 
significant familiarity with the data and would otherwise be left up to the 
analyst.  Because this process is designed to be automated and simple, a 
14-order \code{\link{arima}} model is used.

\code{\link{arima}} has a number of arguments that can be changed.  
For the purposes of this proposal, the user cannot change these arguments.

The prediction intervals that appear in the subsequent plot from 
calling this formula are the defaults for the \code{\link{forecast}} function.
those defaults are \code{c(80, 95)} confidence levels for the prediction 
interval.}


\author{Steven H. Ranney Contact: \email{Steven.Ranney@gmail.com}
Steven Ranney}



\seealso{\code{\link{ts}}

\code{\link{forecast}}

\code{\link{arima}}

\code{\link{ggplot}}

\code{\link{geom_line}}}

