
ForecastHelpDeskCalls <- structure(
function 
##title<< Forecast the number of calls that will come into a help desk 

  # #############################################################################
  # File:  ForecastHelpDeskCalls.R
  ##author<<Steven H. Ranney Contact: \email{Steven.Ranney@gmail.com}
  # Created: 12/29/14  
  # Last Edited: 12/29/14 by SHR
  ##description<< This function extracts the number of calls received daily at a 
  ## help desk and forecasts using an Autoregressive Integrated Moving Average (ARIMA) 
  ## model for a number of days into the future.  
  #
  # TODO: add example section
  # TODO: add testing section
  # #############################################################################

  (dF, ##<< The dF that needs an \code{weekday} value.
  distance = 31, ##<< How far into the future should the forecast be made?  A \code{numeric}
               ## value passed to the \code{forecast} statement that shows the number of 
               ## calls to the helpdesk in \code{distance} number of days.  For a monthly 
               ## forecast, \code{distance = 365/12} is appropriate.  \code{distance = 365/4}
               ## is appropriate for quarterly predictions.  Values could theoretically 
               ## range from \code{1} to anything \code{>1}.  Here, the defaults
               ## is to forecast \code{31} days into the future.
    ... ##<< Arguments to be passed to other functions.  Specifically, this can take 
        ## the form of identifying which \code{date} value is of interest.  For example
        ## the sample datasets provided all had either at least \code{assigned_at} 
        ## or \code{created_at} values.  The \code{\link{AssignDateAndDay}} can used
        ## either of these values as the date of interest.
  ){
  #FORECAST OUT BY N DAYS
  
  ##details<< This function calls the \code{\link{AssignDateAndDay}} function to 
  ## assign the \code{day} values used.  The \code{dayOfWeek} values are used
  ## in calculating the number of calls in a given day.
  
  dF <- AssignDateAndDay(dF, ...)

  #Create the structure by which the timeSeries of information will be calculated
  #Aggregate the number of calls/day 
  totalByDate <- aggregate(dF$dayOfWeek, by = list(dF$assignedDate), length)
  names(totalByDate) <- c("date", "number")
  totalByDate$date <- as.Date(totalByDate$date, format = "%Y-%m-%d")
  totalByDate$number <- as.numeric(totalByDate$number)
  
  #Create a sequence of dates from the minimum value in the aggregated dataFrame
  #above through the final value.
  
  ##details<< Dates in the \code{dF} passed to this function do not need to be 
  ##sequential.  This function 'fills in' date gaps that are not represented in 
  ## the dataFrame and assigns dates that do not appear in the \code{dF} a 
  ## value of \code{0}.
  tmpDate <- as.data.frame(seq.Date(as.Date(min(totalByDate$date, na.rm = T), format = "%Y-%m-%d"), 
                                    as.Date(max(totalByDate$date, na.rm = T), format = "%Y-%m-%d"), 
                                    1))
  names(tmpDate) <- c("date")
  
  #Merge data from dF to a vector of sequential dates
  tByDate <- merge(totalByDate, tmpDate, by = "date", all = TRUE)
  
  #Fill in NA values with zero
  tByDate$number[is.na(tByDate$number)] <- 0

  #Create daily time series of date and number of calls
  x = ts(tByDate$number, start = c(as.numeric(strsplit(as.character(tByDate[1,1]), "-")[[1]][1]), 
                                  min(as.numeric(strftime(tByDate[,1], format= "%j")))), 
        frequency = 365)
  
  ##seealso<< \code{\link{ts}}
  ##seealso<< \code{\link{forecast}}
  ##seealso<< \code{\link{arima}}
  ##seealso<< \code{\link{ggplot}}
  ##seealso<< \code{\link{geom_line}}

  
  ##details<< For the purposes of this proposal, this function only forecasts with 
  ## \code{\link{arima}} though other forecasting models are available.  In many 
  ## cases, selecting the right model and correct model parameters requires 
  ## significant familiarity with the data and would otherwise be left up to the 
  ## analyst.  Because this process is designed to be automated and simple, a 
  ## 14-order \code{\link{arima}} model is used.

  ##details<< \code{\link{arima}} has a number of arguments that can be changed.  
  ## For the purposes of this proposal, the user cannot change these arguments.

  ##details<< The prediction intervals that appear in the subsequent plot from 
  ## calling this formula are the defaults for the \code{\link{forecast}} function.
  ## those defaults are \code{c(80, 95)} confidence levels for the prediction 
  ## interval.
  
  #Add forecast values to the same dataFrame as the original values so they can
  # be plotted with ggplot()

  tByDate$forecast <- NA    
  tByDate$lo80 <- NA
  tByDate$lo95 <- NA
  tByDate$hi80 <- NA
  tByDate$hi95 <- NA
    
  #Create vector of dates beyond original data that go out to the total distance
  #required
  moreDates <- as.data.frame(seq.Date(max(tByDate$date)+1, 
                                      max(tByDate$date)+distance, 
                                      1))

  #Create forecast
  forVals <- forecast(arima(x, order = c(14, 1, 0)), distance)

  tmpFor <- as.data.frame(forVals$mean)
  tmpBlank <- NA
  tmpLow <- as.data.frame(forVals$lower)
  tmpUp <- as.data.frame(forVals$upper)

  tmpData <- data.frame(moreDates, tmpBlank, tmpFor, tmpLow, tmpUp)
  names(tmpData) <- c("date", "number", "forecast", "lo80", "lo95", "hi80", "hi95")

  #Merge all data--original and predicted--into one dataFrame for plotting
  allData <- rbind(tByDate, tmpData)
  
  #Create ggplot item
  p1a <- ggplot(data=allData, aes(x=date,y=number)) 
  p1a <- p1a + geom_line()
  p1a <- p1a + geom_line(aes(y=forecast), colour='blue')
  p1a <- p1a + geom_ribbon(aes(ymin=lo95, ymax=hi95), 
                           alpha=.25, colour = "yellow", fill = "yellow")
  p1a <- p1a + geom_ribbon(aes(ymin=lo80, ymax=hi80), 
                           alpha=.25, colour = "red", fill = "red")  
  p1a <- p1a + labs(y = "Number of calls/day", x = "Date", title = "ARIMA Forecast")

  return(p1a)

  })
 
