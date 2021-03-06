
PlotReopensByDay <- structure(
function 
##title<< Plot the number of reopens by day of week 

  ###############################################################################
  # File:  PlotReopensByDay.R
  ##author<< Steven H. Ranney Contact: \email{Steven.Ranney@gmail.com}
  # Created: 12/29/14  
  # Last Edited: 12/29/14 by SHR
  ##description<< This function plots the number of reopens and produces a box 
  ## plot of those values as a function of \code{day}.  Day levels include those 
  ## assigned from the \code{\link{AssignDateAndDay}} function.  
  #
  # TODO: add RData for example
  # TODO: add testing section
  ###############################################################################

  (dF, ##<< The dataFrame for which to plot resolution time by priority level.
  ... ##<< Arguments to be passed to other functions.  Specifically, this can take 
      ## the form of identifying which \code{date} value is of interest.  For example
      ## the sample datasets provided all had either at least \code{assigned_at} 
      ## or \code{created_at} values.  The \code{\link{AssignDateAndDay}} can used
      ## either of these values as the date of interest.
  ){

  
  ##details<< This function calls the \code{\link{AssignDateAndDay}} function to 
  ## assign the \code{day} values used.  
  
  dF <- AssignDateAndDay(dF, ...)
  
  #Eliminate rows where there is no data
  dF <- dF[which(!is.na(dF$dayOfWeek)), ]
  
  #Ordering the dayOfWeek factors correctly
  dF$dayOfWeek <- factor(dF$dayOfWeek, levels=c("Monday", "Tuesday", "Wednesday", 
                                                "Thursday", "Friday", "Saturday",
                                                "Sunday"))
  
  #Handling 'nan.0' values
  dF$reopens[dF$reopens == "nan.0"] <- NA
  dF$reopsn <- as.numeric(dF$reopens)
  dF$value <- dF$reopens
  
  if(length(which(is.na(dF$value))) == nrow(dF)){
    stop(print("There is no reopen data in this data frame."))
    } else {
  
  ##details<< This functions converts values that are \code{nan.0} to \code{NA} values.
  
  ##details<< Day of week levels in some of the sample data are represented by the 
  ## days of the week.  For the purposes of this function, weeks begin on 
  ## \code{Monday} and continue through \code{Sunday}.
  
  ##details<< Boxes in the plot that is produced from this function can help
  ## managers understand the resolution time on any given \code{dayOfWeek}.  If the 
  ## notches in a box overlap the heavy bar (i.e., the median resolution time)
  ## in an adjacent box, then the resolution time for that priority level is \emph{not}
  ## significantly different than the resolution time in the adjacent plot.  This 
  ## provides a quick, easy method of determining if \code{dayOfWeek} really 
  ## has an affect on resolution time.
  
  ##details<< The red dots on a plot indicate outliers in the data set.  In the 
  ## \code{\link{geom_boxplot}} documentation, outliers are defined as data points 
  ## fall outside of \emph{1.5 * IQR} where \emph{IQR} stands for the "Inter-Quartile
  ## Range" of the data.

  ylabel <- "Number of reopens"
  
  xlabel <- "Day of Week"
  
  ##details<< Plots are generated by the function \code{\link{ggplot}}
  
  g <- ggplot(dF, aes(x = dayOfWeek, y = (value), fill = dayOfWeek))

  p1 <- g + geom_boxplot(mapping = NULL, data = dF, stat = "boxplot", 
                    position = "dodge", outlier.colour = "red", outlier.shape = 16, 
                    outlier.size = 2, notch = TRUE) + 
        labs(ylab(ylabel)) + labs(xlab(xlabel)) + labs(ggtitle("Resolution time")) + 
        theme(legend.position="none") +
        scale_fill_brewer()
  } #end else

  return(p1)
  
  }) 
 
