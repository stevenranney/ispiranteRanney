
AssignDateAndDay <- structure(
function 
##title<< Assign a date to each row 

  ###############################################################################
  # File:  AssignDateAndDay.R
  ##author<< Steven H. Ranney
  ## Contact: \email{Steven.Ranney@gmail.com}
  # Created: 12/21/14  
  # Last Edited: 12/30/14 by SHR
  ##description<< This function uses a dataFrame and extracts the date from the field
  ## \code{assigned_at} or \code{created_at}.  It appends a vector of \code{character} 
  ## values with the date and the day of the week associated with those values.
  # Returns: The original dataFrame with a new vector of Dates. 
  #
  # TODO: add testing section
  ###############################################################################

  (dF, ##<< The dF that needs an \code{weekday} value.
  value = "assigned" ##<< The field that will be used the determine which day
                    ## of the week the call came into the call center.  
                    ## Additional values include \code{"created"}.  The 
                    ## default value will use the \code{assigned_at} field 
                    ## in the \code{dF}.
  ){

  #Resolution time by day of week
  
  ##details<< The \code{dF} passed to this function must have either the values
  ## \code{assigned_at} or \code{created_at} as a vector of character date values.
  
  ##details<< The assumed format for the date values is from the \code{\link{strptime}} 
  ## function where \code{format = "%Y-%m-%d %H:%M:%S"}.

  if(value == "assigned") dF$dayOfWeek <- weekdays(strptime(dF$assigned_at, format = "%Y-%m-%d %H:%M:%S"))
  if(value == "created") dF$dayOfWeek <- weekdays(strptime(dF$created_at, format = "%Y-%m-%d %H:%M:%S"))
  
  if(value == "assigned") dF$assignedDate <- as.Date(dF$assigned_at, format = "%Y-%m-%d %H:%M:%S")
  if(value == "created") dF$assignedDate <- as.Date(dF$created_at, format = "%Y-%m-%d %H:%M:%S")

  ##details<< This function uses \code{\link{weekdays}} to convert dates to the 
  ## day of the week.
  
  ##seealso<< \code{\link{weekdays}}
  ##seealso<< \code{\link{strptime}}
  
  return(dF)
  
  }, ex = function() {
  
  assigned_at <- c("2014-01-01 11:37:00 -0800", "2014-01-02 09:24:05 -0800", 
                  "2014-01-02 10:04:41 -0800")
  dF<-as.data.frame(assigned_at)
  newDf <- AssignDateAndDay(dF)
  
  })
