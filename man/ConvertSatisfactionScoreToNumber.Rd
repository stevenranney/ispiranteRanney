\name{ConvertSatisfactionScoreToNumber}
\alias{ConvertSatisfactionScoreToNumber}
\title{Convert a character to a number}
\description{This function converts the \code{satisfaction_score} from a 
character value into a numeric value \code{$scoreNumber} that can be used 
in regression or in other ways.}
\usage{ConvertSatisfactionScoreToNumber(dF)}
\arguments{
  \item{dF}{The dataFrame for which to convert \code{satisfaction_score} into a
number.}
}
\details{Values for \code{satisfaction_score} include \code{Good}, 
\code{Not Offered}, \code{Offered}, and \code{Bad}.  This function converts 
\code{Good} values into the numeric \code{4}, \code{Bad} into \code{1}, and
the others to \code{NA} values.

If all values for \code{satisfaction_score} are \code{NA}, then 
the resulting \code{$scoreNumber} will be filled with \code{NA} values.}


\author{Steven H. Ranney
Contact: \email{Steven.Ranney@gmail.com}
Steven Ranney}





