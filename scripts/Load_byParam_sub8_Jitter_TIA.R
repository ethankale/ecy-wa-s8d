
#####
# Jitter plot of area loads by total impervious area
#
# Intended to be called by Load_byParam_sub3_Load_Summary_Plots.R
#####

# Create bins
bins <- c(0,20,40,60,80)

# Set up universal jitter plot parameters
xlimits <- c(0, 100)

plot(x=c(1:10), y=c(1:10), 
     xlim = xlimits, 
     ylim = ylimits, 
     ylab = "Kg/Ha", 
     type = "n", 
     xaxt = "n", 
     xlab = "",
     log  = "y", 
     xaxs = "i", 
     yaxs = "i", 
     main = "\nArea Loads\nby Impervious Area (kg/ha)", 
     las  = 2, 
     cex.axis = 0.8
)

# Visually separate the bins
abline(v = seq(from = 20, to = 80, by = 20))

axis(side   = 1, 
     at     = seq(from = 10, to = 90, by = 20), 
     labels = rep("",5)
)

# In the "col" property, the first should be detects, the second ND.
#   Hence, first item in "palette" -> detects, second -> nd.
palette(c("blue", "darkgray"))

# Initial set-up of parameters for creating the labels
percentCensor <- c()
detCount      <- c()
nd            <- c()

# Loop through each type of land use & plot
for (j in 1:length(bins)) {
  
  minTIA <- bins[j]
  
  # Find data within the bin range, then count detects & non-detects
  data       <- ParamData[which(ParamData$TIAPercent >= minTIA & ParamData$TIAPercent < minTIA+20), ] 
  nonDetects <- data[which(data$nonDetect_Flag == 1),]
  Detects    <- data[which(data$nonDetect_Flag == 0),]
  
  numRows    <- nrow(data)
  margin     <- minTIA
  
  # Create the horizontal "jittering" for the plot (only if enough rows exist)
  if ( numRows > 0) {
    if (numRows > 2) {
      jitterX <- sample(seq(from=(3+margin), to=(17+margin), length.out=numRows))
    } else {
      if (numRows == 2) {
        jitterX <- c((7+margin), (13+margin))
      } else {
        jitterX <- c(10+margin)
      }
    }
    
    # Create the dots that represent the ND values
    if (nrow(nonDetects) > 0 ) {
      jitterX_ND <- jitterX[1:nrow(nonDetects)]
      
      points(y   = nonDetects$sample_area_loads, 
             x   = jitterX_ND, 
             pch = 16, 
             cex = 1.2, 
             col = 2)
    }
    
    # Create the dots that represent the detected values
    if (nrow(Detects) > 0 ) {
      jitterX_det <- jitterX[nrow(nonDetects)+1:nrow(Detects)]
      
      points(y   = Detects$sample_area_loads, 
             x   = jitterX_det, 
             pch = 16, 
             cex = 1.2, 
             col = 1)
    }
  }

  # Create label values
  # Count both the non-detects and the detects for each parameter/land use
  #  subset, then create a dummy list for percent censored values.
  detCount[j]      <- nrow(data[which(data$nonDetect_Flag == FALSE), ])
  nd[j]            <- nrow(data[which(data$nonDetect_Flag == TRUE),  ])
  percentCensor[j] <- nd[j]
}

##### Add the labes & customize the axis

# Calculate the percent of censored values for each land use type
percentCensor <- percentCensor / (nd + detCount)
percentCensor <- round(100*percentCensor, 1)

# In the rare case where there are no numbers (neither non-detects nor detects),
#  percentCensor will return NaN.  lapply(...) replaces with "-".
percentCensor <- lapply( percentCensor, FUN = function(x) ifelse(is.nan(x),"-",x))

axislabels <- paste(c("0-20", "20-40", "40-60", "60-80", "80-100"), 
                    rep("\n Det=",4), detCount,
                    rep("\n ND=",4),  nd, 
                    rep("\n",4),      percentCensor,
                    rep("%",4),
                    sep="")

mtext(side = 1, 
      line = 3.5, 
      at   = seq(from = 10, to = 90, by = 20), 
      text = axislabels, 
      adj  = 0.5, 
      padj = 0, 
      cex  = 0.6)
