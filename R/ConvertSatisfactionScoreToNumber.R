
ConvertSatisfactionScoreToNumber <- structure(
function # Convert a character to a number

  ##############################################################################
  # File:  ConvertSatisfactionScoreToNumber.R
  ##author<<  Steven H. Ranney
  ## Contact: \email{Steven.Ranney@gmail.com}
  # Created: 12/31/14  
  # Last Edited: 12/31/14 by SHR
  ##description<< This function converts the \code{satisfaction_score} from a 
  ## character value into a numeric value \code{$scoreNumber} that can be used 
  ## in regression or in other ways.
  #
  # TODO: add RData for example
  # TODO: add testing section
  ###############################################################################
  
  (dF ##<< The dataFrame for which to convert \code{satisfaction_score} into a 
      ## number.
  ){
  
  ##details<< Values for \code{satisfaction_score} include \code{Good}, 
  ## \code{Not Offered}, \code{Offered}, and \code{Bad}.  This function converts 
  ## \code{Good} values into the numeric \code{4}, \code{Bad} into \code{1}, and
  ## the others to \code{NA} values.
  
  ##details<< If all values for \code{satisfaction_score} are \code{NA}, then 
  ## the resulting \code{$scoreNumber} will be filled with \code{NA} values.
  
  if(length(which(is.na(dF$satisfaction_score))) == nrow(dF)) dF$scoreNumber <- NA
  
  dF$scoreNumber[dF$satisfaction_score == "Good"] <- 4
  dF$scoreNumber[dF$satisfaction_score == "Offered"] <- NA
  dF$scoreNumber[dF$satisfaction_score == "Not Offered"] <- NA
  dF$scoreNumber[dF$satisfaction_score == "Bad"] <- 1
  
  return(dF)
  
  })
