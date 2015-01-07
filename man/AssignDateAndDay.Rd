\name{AssignDateAndDay}
\alias{AssignDateAndDay}
\title{Assign a date to each row }
\description{This function uses a dataFrame and extracts the date from the field
\code{assigned_at} or \code{created_at}.  It appends a vector of \code{character} 
values with the date and the day of the week associated with those values.}
\usage{AssignDateAndDay(dF, value = "assigned")}
\arguments{
  \item{dF}{The dF that needs an \code{weekday} value.}
  \item{value}{The field that will be used the determine which day
of the week the call came into the call center.  
Additional values include \code{"created"}.  The 
default value will use the \code{assigned_at} field 
in the \code{dF}.}
}
\details{The \code{dF} passed to this function must have either the values
\code{assigned_at} or \code{created_at} as a vector of character date values.

The assumed format for the date values is from the \code{\link{strptime}} 
function where \code{format = "\%Y-\%m-\%d \%H:\%M:\%S"}.

This function uses \code{\link{weekdays}} to convert dates to the 
day of the week.}


\author{Steven H. Ranney
Contact: \email{Steven.Ranney@gmail.com}
Steven Ranney}



\seealso{\code{\link{weekdays}}

\code{\link{strptime}}}
\examples{

assigned_at <- c("2014-01-01 11:37:00 -0800", "2014-01-02 09:24:05 -0800", 
                "2014-01-02 10:04:41 -0800")
dF<-as.data.frame(assigned_at)
newDf <- AssignDateAndDay(dF)

}
