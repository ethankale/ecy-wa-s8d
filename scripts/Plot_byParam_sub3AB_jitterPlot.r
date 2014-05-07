
# Jitter Plots ####
# Creates four empty boxes, then fills them with jittered data from each of the four land use types
# This script is called from Plot_byParam_ver9_Apr2014.r in a loop, once for each parameter

  # Set up universal jitter plot parameters
  xlimits <- c(0, 4)
  plot(x=c(1:10), y=c(1:10), 
       xlim = xlimits, 
       ylim = ylimits, 
       ylab = ParamList[i], 
       type = "n", 
       xaxt = "n", 
       xlab = "", 
       log  = "y", 
       xaxs = "i", 
       yaxs = "i", 
       main = "\nJitter Plot", 
       las  = 2, 
       cex.axis = 0.8
       )
  abline(v = c(1:4))
  axis(side   = 1, 
       at     = c(0.5, 1.5, 2.5, 3.5), 
       labels = rep("",4)
       )

  palette(c("violet", "purple", "gray80", "gray60"))
  
  # Initial set-up of parameters for creating the labels
  percentCensor <- c()
  detCount      <- c()
  nd            <- c()

  # Set up landuse-specific parameters
  landuseInfo <- data.frame(
             useCode    = c("IND","COM","HDR", "LDR"), 
             leftMargin = c(0,1,2,3)
             )

  # Loop through each type of land use & plot
  for (j in 1:nrow(landuseInfo)) {
    
    # Pull out landuse subset & useful data
    data       <- ParamData[which(as.character(ParamData$Type) == as.character(landuseInfo[j,1])),] 
    nonDetects <- data[which(data$nonDetect_Flag == 1),]
    Detects    <- data[which(data$nonDetect_Flag == 0),]
    
    numRows    <- nrow(data)
    margin     <- landuseInfo[j,2]
    
    # Create the horizontal "jittering" for the plot (only if enough rows exist)
    if ( numRows > 0) {
      if (numRows > 2) {
        jitterX <- sample(seq(from=(0.05+margin), to=(0.95+margin), length.out=numRows))
      } else {
        if (numRows == 2) {
          jitterX <- c((0.3+margin), (0.7+margin))
        } else {
          jitterX <- c(0.5+margin)
        }
      }
      
      # Create the vertical rows that represent non-detects
      if (nrow(nonDetects) > 0 ) {
        jitterX_ND <- jitterX[1:nrow(nonDetects)]
        segments(y0=rep(min(ylimits),
                        nrow(nonDetects)), 
                 x0=jitterX_ND, 
                 y1=nonDetects$new_Result_Value, 
                 x1=jitterX_ND, 
                 col=3+nonDetects$WetSeason)
      }
      
      # Create the dots that represent the detected values
      if (nrow(Detects) > 0 ) {
        jitterX_Det <- jitterX[nrow(nonDetects)+1:nrow(Detects)]
        points(y=Detects$new_Result_Value, x=jitterX_Det, pch=16, cex=1.2, col=1+Detects$WetSeason)
      } 
      
      # Create label values
      detCount[j]      <- sum(data$nonDetect_Flag == FALSE)
      nd[j]            <- nrow(data[which(data$nonDetect_Flag == TRUE),])
      percentCensor[j] <- nd[j]
    }
  }

  box()
  

  # Create & apply the labels for each box ####

  # Calculate the percent of censored values for each land use type
  percentCensor <- percentCensor / (nd + detCount)
  percentCensor <- round(100*percentCensor, 1)

  axislabels <- paste(c("Ind", "Com", "HRes", "LRes"), 
                      rep("\n Det=",4), detCount,
                      rep("\n ND=",4),  nd, 
                      rep("\n",4),      percentCensor,
                      rep("%",4),
                      sep="")

  axis(side=1, at=c(1:4), labels=rep("",4))
  mtext(side=1, line=3.5, at=-0.5+c(1:4), text=axislabels, adj=0.5, padj=0, cex=0.6)

  ymax <- par("usr")[4]
  ymin <- par("usr")[3]

  ymin_legend <- 10^(ymax + 0.01*(ymax-ymin))
  ymax_legend <- 10^(ymax + 0.22*(ymax-ymin))

  legend(x=c(2.6,4.3), 
            y      = c(ymin_legend, ymax_legend), 
            legend = c("Detect-DrySeas", "Detect-WetSeas", "NonDetect-DrySeas", "NonDetect-WetSeas"),
            lty    = c("blank", "blank", "solid", "solid"), 
            pch    = c(16,16,NA, NA), 
            pt.cex = c(1.2,1.2,1.2,1.2), 
            col    = c("violet", "purple", "gray80", "gray60"), 
            xpd    = NA, 
            cex    = 0.8, 
            bty    = "o", 
            bg     = "white"
         )
