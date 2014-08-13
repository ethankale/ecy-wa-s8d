
#####
#
# Create simple summary plots, suitable for in-line use,
#  of parameter concentration ranges by land use & season.
#
# Must run Plot_byParam_ver9_Apr2014.r first.
#####

library(ggplot2)
library(grid)
library(gridExtra)

# Make the output directory.  Only really required the first time...
dir.create(file.path(outputDirectory, "inlineSummaries"), showWarnings = FALSE)

for (i in 1:length(ParamList)) {
  
  # Pull out the subset of data for the parameter, calculate min, max, mean
  stormFiltered <- Storm[which(Storm$Parameter.string == ParamList[i]), ]
  stormFiltered$LanduseCode <- factor(stormFiltered$LanduseCode)
  
  landuses      <- aggregate(new_Result_Value ~ LanduseCode, 
                             data = stormFiltered, 
                             FUN = function(x) c(theMean = mean(x), 
                                                 theMin  = min(x), 
                                                 theMax  = max(x)
                                                 )
                             )
  landusesDF    <- data.frame(landuses[1], landuses[2]$new_Result_Value)
  
  seasons       <- aggregate(new_Result_Value ~ WetSeason, 
                             data = stormFiltered, 
                             FUN = function(x) c(theMean = mean(x), 
                                                 theMin  = min(x), 
                                                 theMax  = max(x)
                                                 )
                            )
  seasonsDF     <- data.frame(seasons[1], seasons[2]$new_Result_Value)
  
  # Create a new column, for plotting purposes
  seasonsDF$season <- "DRY"
  seasonsDF$season[which(seasonsDF$WetSeason)] <- "WET"
  
  # Overall minima & maxima, for plot labels
  landuses.min <- min(landusesDF$theMin)
  landuses.max <- max(landusesDF$theMax)
  seasons.min  <- min(seasonsDF$theMin)
  seasons.max  <- max(seasonsDF$theMax)
  
  # Plot a simple graph with landuse on the left & seasons on the right
  
  # Make a theme with just a plain white background
  thm <- theme(
    plot.background  = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_blank(),
    axis.ticks       = element_blank(),
    axis.text.x      = element_blank(),
    axis.text.y      = element_text(size = 7),
    axis.title       = element_blank(),
    axis.line.y      = element_line(color = 'black')
  )
  
  # If the plot is percent, use a linear axis; if it's concentration, us a log axis.
  
  if (is.na(grep("%", ParamList[i])[1])) {
    scale <- scale_y_log10(breaks = c(landuses.min, landuses.max))
  } else {
    scale <- scale_y_continuous(breaks = c(landuses.min, landuses.max))
  }
  
  # Plot landuses (text at mean, with lines to indicate data range)
  # Need to deal with cases where minimum values = 0 (issue for log scale)
  p1 <- ggplot(landusesDF) + 
  #ggplot(landusesDF) +
    geom_segment(aes(x = LanduseCode, xend = LanduseCode, y = theMin, yend = theMax), 
                 size  = 1, 
                 color = "gray") +
    geom_text(aes(label = LanduseCode, x = LanduseCode, y = theMean), 
              size = 2.5) +
    scale +
    theme_bw() +
    thm
  
  # Plot seasons (text at mean, with lines to indicate data range)
  p2 <- ggplot(seasonsDF) + 
    geom_segment(aes(x = season, xend = season, y = theMin, yend = theMax),
                 size  = 1,
                 color = "gray",
                 linetype = "82") +
    geom_text(aes(label = season, x = season, y = theMean),
              size = 2.5) +
    scale +
    theme_bw() +
    thm +
    theme(axis.text.y = element_blank())
  
  # Set up the output file & write to it
  
  # Not all characters can be included in a filename
  parameterName <- sub("/", "", ParamList[i])
  parameterName <- sub("%", "", parameterName)
  
  filename <- paste("inline_", sub("/", "", parameterName), ".png", sep="") 
    
  png(file = paste(outputDirectory, "inlineSummaries", filename, sep="/"), 
      width  = 3, 
      height = 1.5,
      units  = "in",
      res    = 150
  )
  
  # Actually draw the plot
  grid.arrange(arrangeGrob(p1,p2,ncol=2,widths=c(2/3,1/3)))
  
  dev.off()
}
