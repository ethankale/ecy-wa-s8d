#####
# Determine the criteria level for each measured parameter
#  for which there exist criteria.
# Depends on Plot_byParam_ver9_Apr2014.r (or newer),
#  and on Load_byParam_sub1_lookup.r.
#####

require(sqldf)

# Include the function for calculating criteria
source(paste(scriptDirectory, "Calculate_Criteria.R", sep="/"))

##### Add columns to "Storm" dataframe for pH and hardness. -------------------------

# Add hardness, and average repeated values in the same day & location
hardnessTable <- Storm[which(Storm$Parameter_string == "Hardness as CaCO3 water  (ug/L)"),
                        c("Location_ID", 
                          "Field_Collection_Start_Date", 
                          "Field_Collection_End_Date", 
                          "start",
                          "end",
                          "new_Result_Value")
                        ]
hardnessTable$new_Result_Value <- hardnessTable$new_Result_Value / 1000
hardnessTable <- aggregate(new_Result_Value ~ Location_ID + Field_Collection_Start_Date + Field_Collection_End_Date + start + end, data = hardnessTable, mean)

# Add pH, and average repeated values in the same day & location
phTable <- Storm[which(Storm$Parameter_string == "pH water  (pH)"),
                       c("Location_ID", 
                         "Field_Collection_Start_Date", 
                         "Field_Collection_End_Date", 
                         "start",
                         "end",
                         "new_Result_Value")
                       ]
phTable <- aggregate(new_Result_Value ~ Location_ID + Field_Collection_Start_Date + Field_Collection_End_Date + start + end, data = phTable, mean)

# Combined the tables
Storm <- sqldf(c("CREATE INDEX s1 ON Storm(Location_ID, start, end)",
                "CREATE INDEX s2 ON hardnessTable(Location_ID, start, end)",
                "CREATE INDEX s3 ON phTable(Location_ID, start, end)",
                "SELECT Storm.*, 
                   hardnessTable.new_Result_Value AS hardness,
                   phTable.new_Result_Value AS pH
                 FROM Storm
                 LEFT OUTER JOIN hardnessTable 
                  ON Storm.Location_ID = hardnessTable.Location_ID
                    AND Storm.start >= hardnessTable.start
                    AND Storm.end <= hardnessTable.end
                 LEFT OUTER JOIN phTable 
                  ON Storm.Location_ID = phTable.Location_ID
                    AND Storm.start >= phTable.start
                    AND Storm.end <= phTable.end")
)

##### Use the new function & rows to calculate applicable criteria -------------------------

# We create new lists, rather than adding values directly to Storm,
#  to avoid subsetting the data.frame (i.e., Storm[,]) as much as possible.
#  Avoiding subsetting improves performance.

acuteList   <- c()
chronicList <- c()
hhList      <- c()

paramList    <- Storm$Parameter_string
pHList       <- Storm$pH
hardnessList <- Storm$hardness

for (j in 1:nrow(Storm)) {
  
  # The criteria function returns acute, chronic, and human health in a single-row data.frame.
  #  Standards that aren't applicable get NA values.
  standards <- criteria(parameter = paramList[j],
                        pH        = pHList[j],
                        hardness  = hardnessList[j]
                        )
  
  acuteList[j]   <- standards$acute
  chronicList[j] <- standards$chronic
  hhList[j]      <- standards$hh
}

Storm$acute   <- acuteList
Storm$chronic <- chronicList
Storm$hh      <- hhList

Storm$acuteExceedPercent   <- (Storm$new_Result_Value / Storm$acute)   * 100
Storm$chronicExceedPercent <- (Storm$new_Result_Value / Storm$chronic) * 100
Storm$hhExceedPercent      <- (Storm$new_Result_Value / Storm$hh)      * 100

Storm$acuteExceeds   <- Storm$acuteExceedPercent > 100
Storm$chronicExceeds <- Storm$chronicExceedPercent > 100
Storm$hhExceeds      <- Storm$hhExceedPercent > 100

##### Plot out criteria by parameter (per Will Hobbs suggestion) -------------------------
pdf(paste(outputDirectory, "concentration_criteria_plots.pdf", sep="/"), width=11, height=8.5)

mar.default = c(5, 4, 4, 2) + 0.1

for (type in c("acute", "chronic", "hh")) {

  par(mar = mar.default + c(0, 12, 0, 0))
  
  # Values vs. criteria
  storm.current <- Storm[-which(is.na(Storm[, type])), ]
  storm.current$Parameter_string <- factor(storm.current$Parameter_string)
  
  ylimits <- c(1, length(levels(storm.current$Parameter_string)))
  xlimits <- c(min(storm.current$new_Result_Value), max(storm.current$new_Result_Value))
  
  plot(storm.current$new_Result_Value, 
       storm.current$Parameter_string,
       pch  = -124,
       cex  = 0.5,
       col  = "darkgray",
       log  = "x",
       xlab = "Sampled Concentration (ug/L)",
       ylab = "",
       main = paste("Range of Concentrations with", type, "criteria"),
       yaxt = "n",
       # Print out the background lines, text, etc. underneath the data
       panel.first = {
         j <- 1
         for (param in levels(storm.current$Parameter_string)) {
           
           # Create background horizontal lines for each parameter
           xMin <- min(storm.current$new_Result_Value)
           xMax <- max(storm.current$new_Result_Value)
           abline(h   = j,
                  col = "lightgray",
                  lty = 2
           )
           
           # Add min and max labels at the ends of each data series
           theMin <- min(storm.current$new_Result_Value[which(storm.current$Parameter_string == param)])
           theMax <- max(storm.current$new_Result_Value[which(storm.current$Parameter_string == param)])
           
           text(theMin, j, labels = c(as.character(theMin)), pos = 3, offset = 0.5, cex = 0.85 )
           text(theMax, j, labels = c(as.character(theMax)), pos = 3, offset = 0.5, cex = 0.85 )
           
           # Add the min and max of the criteria to each data series
           acuteMin <- min(storm.current[which(storm.current$Parameter_string == param), type])
           acuteMax <- max(storm.current[which(storm.current$Parameter_string == param), type])
           
           text(acuteMin, j, labels = c("|"))
           text(acuteMax, j, labels = c("|"))
           line <- c()
           line$x <- c(acuteMin, acuteMax)
           line$y <- c(j, j)
           lines(x = line)
           
           j <- j + 1
           
         }
       }
       )
  
  labelList <- levels(storm.current$Parameter_string)
  
  axis(side   = 2, 
       at     = 1:length(labelList), 
       las    = 1,
       labels = labelList
       )
}

dev.off()

##### Similar plots to above, but as percent exceedence rather than actual values -------------------------
pdf(paste(outputDirectory, "criteria_exceedence_plots.pdf", sep="/"), width=11, height=8.5)

mar.default = c(5, 4, 4, 2) + 0.1

# Loop through each type of exceedence
for (type in c("acute", "chronic", "hh")) {
  
  colName <- paste(type, "ExceedPercent", sep="")
  
  par(mar = mar.default + c(0, 12, 0, 0))
  
  # Values vs. criteria
  
  storm.current <- Storm[-which(is.na(Storm[, type])), ]
  storm.current$Parameter_string <- factor(storm.current$Parameter_string)
  
  ylimits <- c(1, length(levels(storm.current$Parameter_string)))
  xlimits <- c(min(storm.current[, colName]), max(storm.current[, colName]))
  
  plot(storm.current[, colName], 
       storm.current$Parameter_string,
       pch  = -124,
       cex  = 0.5,
       col  = "darkgray",
       log  = "x",
       xlab = "Concentration as Percent of Criteria \n(vertical line = 100%)",
       ylab = "",
       main = paste("Percent Exceedance with", type, "criteria"),
       yaxt = "n",
       # Print out the background lines, text, etc. underneath the data
       panel.first = {
         j <- 1
         for (param in levels(storm.current$Parameter_string)) {
           
           # Create background horizontal lines for each parameter
           xMin <- min(storm.current[, colName])
           xMax <- max(storm.current[, colName])
           abline(h   = j,
                  col = "lightgray",
                  lty = 2
           )
           
           # Add min and max labels at the ends of each data series
           theMin <- min(storm.current[which(storm.current$Parameter_string == param), colName])
           theMax <- max(storm.current[which(storm.current$Parameter_string == param), colName])
           
           text(theMin, j, labels = c(paste(formatC(theMin, format = "f", digits = 2, big.mark = ","), "%", sep = "")), pos = 3, offset = 0.5, cex = 0.85 )
           text(theMax, j, labels = c(paste(formatC(theMax, format = "f", digits = 2, big.mark = ","), "%", sep = "")), pos = 3, offset = 0.5, cex = 0.85 )
          
           # Add a line at 100 (above which = exceedance of criteria)
           abline(v = 100, col = "gray")
           
           j <- j + 1
           
         }
       }
  )
  
  labelList <- levels(storm.current$Parameter_string)
  
  axis(side   = 2, 
       at     = 1:length(labelList), 
       las    = 1,
       labels = labelList
  )
  
  
  # Plot count of exceedences vs. total count of samples.
  
  colCountName <- paste(type, "Exceeds", sep="")
  
  # Table of exceedance counts.
  exceedsTbl   <- table(storm.current[, colCountName], storm.current$Parameter_string)
  
  barplot(exceedsTbl, 
          horiz = TRUE,
          las   = 1,
          xlab  = "Number of Samples",
          main  = paste("Samples That Exceed", type, "Criteria")
          )

}



dev.off()
