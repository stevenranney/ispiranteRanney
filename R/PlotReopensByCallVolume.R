
PlotReopensByCallVolume <- structure(
function # Create a plot of the reopens by call volume

  ##############################################################################
  # File:  PlotReopensByCallVolume.R
  ##author<<  Steven H. Ranney
  ## Contact: \email{Steven.Ranney@gmail.com}
  # Created: 12/30/14  
  # Last Edited: 12/30/14 by SHR
  ##description<< This function plots the number of reopens and produces a plot 
  ## of those values as a function of call volume. 
  #
  # TODO: add RData for example
  # TODO: add testing section
  ###############################################################################

  (dF, ##<< The dataFrame for which to plot the number of reopens as a function 
       ## of call volume.
  ... ##<< Arguments to be passed to other functions.  Specifically, this can take 
      ## the form of identifying which \code{date} value is of interest.  For example
      ## the sample datasets provided all had either at least \code{assigned_at} 
      ## or \code{created_at} values.  The \code{\link{AssignDateAndDay}} can used
      ## either of these values as the date of interest. 
  ){
  
 
  dF <- AssignDateAndDay(dF, ...)

  #Eliminate rows where there is no date assigned
  dF <- dF[which(!is.na(dF$assignedDate)), ]
    
  #aggregate call volume by date
  volData <- aggregate(dF$dayOfWeek, by = list(dF$assignedDate), length)
  names(volData) <- c("assignedDate", "totalCalls")
  volData <- volData[order(volData$assignedDate), ]
  
  #aggregate scores by Date
  reopensData <- aggregate(dF$reopens, by = list(dF$assignedDate), sum, na.rm = T)
  names(reopensData) <- c("assignedDate", "meanReopens")
  reopensData <- reopensData[order(reopensData$assignedDate), ]
  
  allData <- cbind(volData, reopensData)

  if(length(which(is.na(dF$reopens))) == nrow(dF)){
    stop("There is no reopen data in this data frame.")
    } else {
    
  ##details<< Plots are generated with \code{\link{ggplot}}
  
  ##details<< This function uses a linear model to evaluate the relationship between
  ## the number of reopens and call volume (i.e., calls per day).  If the red line 
  ## (the result of the \code{\link{lm}} in the produced plot trends down, then 
  ## the higher the call volume the more times a ticket gets reopened.  If the red 
  ## line is parallel or very near to parallel, then there is no relationship 
  ## between the number of reopens and call volume.
  
  ##details<< In most cases, there are very few or no reopen data.  In many cases, 
  ## there are no reopens for a given ticket.  As a result, these data are sparse.
  
  xlabel <- "Call volume (calls/day)"  
  ylabel <- "Number of reopens"
  
  #Create a linear model of score as a function of wait time (hours)
  lmMod <- lm(allData$meanReopens~allData$totalCalls) 

  g <- ggplot(allData, aes(x = totalCalls, y = meanReopens))#, fill = dayOfWeek))

    p1 <- g + geom_point() + labs(ylab(ylabel)) + labs(xlab(xlabel)) +  
              geom_abline(intercept=lmMod$coefficients[1], slope=lmMod$coefficients[2], 
                          colour = "red", size = 1.01)
    
    } # end else
    return(p1)    

})   
