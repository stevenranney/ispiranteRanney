
PlotResolutionTimeByReopens <- structure(
function 
##title<< Plot hours to resolution time by the number of reopens 

  ###############################################################################
  # File:  PlotResolutionTimeByReopens.R
  ##author<<  Steven H. Ranney
  ## Contact: \email{Steven.Ranney@gmail.com}
  # Created: 1/07/14  
  # Last Edited: 01/07/14 by SHR
  ##description<< This function plots the chosen value of resolution time (in 
  ## hours) and produces a plot of those values as a function of \code{reopens}. 
  #
  # TODO: add RData for example
  # TODO: add testing section
  ###############################################################################

  (dF, ##<< The dataFrame for which to plot resolution time by priority level.
  resTime = "first" ##<< A character vector that indicates which resolution time
                    ## to plot.  Other values include \code{"full"}.
  ){
    
  dF$reopens <- as.factor(dF$reopens)
  
  #Handling 'nan.0' values
  dF$first_resolution_time_in_minutes_within_business_hours[dF$first_resolution_time_in_minutes_within_business_hours == "nan.0"] <- NA
  dF$full_resolution_time_in_minutes_within_business_hours[dF$full_resolution_time_in_minutes_within_business_hours == "nan.0"] <- NA

  value <- NA
  if(resTime == "first") dF$value <- as.numeric(dF$first_resolution_time_in_minutes_within_business_hours)
  if(resTime == "full") dF$value <- as.numeric(dF$first_resolution_time_in_minutes_within_business_hours)
  
  if(length(which(is.na(dF$value))) == nrow(dF)){
    stop(print("Data frame has no data to display"))
    } else {
  
  ##details<< Resolution time in all sample dataFrames is \code{factor}; this function
  ## converts apparently numeric values to \code{as.numeric}
  
  ##details<< This functions converts values that are \code{nan.0} to \code{NA}.
  
  ##details<< Boxes in the plot that is produced from this function can help
  ## managers understand the resolution time of \code{reopens} level.  If the 
  ## notches in a box overlap the heavy bar (i.e., the median resolution time)
  ## in an adjacent box, then the resolution time for that number of reopens is \emph{not}
  ## significantly different than the resolution time in the adjacent plot.  This 
  ## provides a quick, easy method of determining if \code{priority} level really 
  ## has an affect on resolution time.
  
  ##details<< The red dots on a plot indicate outliers in the data set.  In the 
  ## \code{\link{geom_boxplot}} documentation, outliers are defined as data points 
  ## fall outside of \emph{1.5 * IQR} where \emph{IQR} stands for the "Inter-Quartile
  ## Range" of the data.
  
  ylabel <- NA
  if(resTime == "first") ylabel <- "First resolution (hours)"
  if(resTime == "full") ylabel <- "Full resolution (hours)"
  
  xlabel <- "Number of Reopens"
 
  ##details<< Plots are generated by the function \code{\link{ggplot}}
  g <- ggplot(dF, aes(x = reopens, y = round(value/60, 2), fill = reopens))
 
  p1 <- g + geom_boxplot(mapping = NULL, data = dF, stat = "boxplot", 
                    position = "dodge", outlier.colour = "red", outlier.shape = 16, 
                    outlier.size = 2, notch = TRUE) + 
        labs(ylab(ylabel)) + labs(xlab(xlabel)) + 
        theme(legend.position="none") +
        scale_fill_brewer()  
  
    } #end else
    
  return(p1)
  
  })  
