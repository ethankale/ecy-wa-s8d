#####
# Determine the criteria level for each measured parameter
#  for which there exist criteria.
#
# Depends on Plot_byParam_ver9_Apr2014.r (or newer),
#  and on Load_byParam_sub1_lookup.r.
#####

require(sqldf)
require(devEMF)

# Include the function for calculating criteria
source(paste(scriptDirectory, "Calculate_Criteria.R", sep="/"))

##### Add columns to "Storm" dataframe for pH and hardness. -------------------------
# Helper for multiple runs - skip these steps if already complete

if (length(Storm$hardness) == 0) {
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
  phTable <- aggregate(new_Result_Value ~ Location_ID + Field_Collection_Start_Date + Field_Collection_End_Date + start + end, 
                       data = phTable, 
                       mean)


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
}

##### Use the new function & rows to calculate applicable criteria -------------------------

# We create new lists, rather than adding values directly to Storm,
#  to avoid subsetting the data.frame (i.e., Storm[,]) as much as possible.
#  Avoiding subsetting improves performance.  Or so I've heard.

acuteList     <- c()
chronicList   <- c()
hhList        <- c()
cleanupList   <- c()
screeningList <- c()

# Names of new columns to add to Storm w/ associated criteria types
criteriaColumnNames <- data.frame(exceedPercent = c("acuteExceedPercent", 
                                                    "chronicExceedPercent", 
                                                    "hhExceedPercent", 
                                                    "cleanupExceedPercent", 
                                                    "screeningExceedPercent"
                                                    ),
                                  
                                  exceeds       = c("acuteExceeds", 
                                                    "chronicExceeds", 
                                                    "hhExceeds", 
                                                    "cleanupExceeds", 
                                                    "screeningExceeds"
                                                    ),
                                  
                                  row.names     = c("acute", 
                                                    "chronic", 
                                                    "hh", 
                                                    "cleanup", 
                                                    "screening"
                                                    ),
                                  
                                  stringsAsFactors=FALSE)

paramList    <- Storm$Parameter_string
pHList       <- Storm$pH
hardnessList <- Storm$hardness

# Calculate all five water quality and sediment criteria for each measurement.

#calculatedCriteria <- apply(X = head(Storm), MARGIN = "Parameter_string", FUN = criteria, pH = Storm$pH, hardness = Storm$hardness )

for (j in 1:nrow(Storm)) {
  
  # The criteria function returns acute, chronic, and human health in a single-row data.frame.
  #  Standards that aren't applicable get NA values.
  standards <- criteria(parameter = paramList[j],
                        pH        = pHList[j],
                        hardness  = hardnessList[j]
                        )
  
  acuteList[j]     <- standards$acute
  chronicList[j]   <- standards$chronic
  hhList[j]        <- standards$hh
  cleanupList[j]   <- standards$cleanup
  screeningList[j] <- standards$screening
}

Storm$acute     <- acuteList
Storm$chronic   <- chronicList
Storm$hh        <- hhList
Storm$cleanup   <- cleanupList
Storm$screening <- screeningList

# Calculate the exceedence % and
#  the does/does not exceed columns.
for (criterion in rownames(criteriaColumnNames)) {
  
  percentColumnName <- criteriaColumnNames[criterion, "exceedPercent"]
  exceedsColumnName <- criteriaColumnNames[criterion, "exceeds"]
  
  Storm[, percentColumnName] <- ((Storm$new_Result_Value / Storm[criterion]) * 100)
  Storm[, exceedsColumnName] <- (Storm[, percentColumnName] > 100) & !Storm$nonDetect_Flag
        
}

Crit_acute     <- table(Storm$Parameter_string, Storm$acuteExceeds)
Crit_chronic   <- table(Storm$Parameter_string, Storm$chronicExceeds)
Crit_hh        <- table(Storm$Parameter_string, Storm$hhExceeds)
Crit_cleanup   <- table(Storm$Parameter_string, Storm$cleanupExceeds)
Crit_screening <- table(Storm$Parameter_string, Storm$screeningExceeds)

Crit_exceed  <- data.frame(Crit_acute,Crit_chronic,Crit_hh)

write.csv(Crit_exceed,paste(outputDirectory, "Criteria_exceedance.csv", sep="/"))

##### Plot out criteria by parameter (per Will Hobbs suggestion) -------------------------
#   Either output in PDF (using line below, & commenting out "png()" lines) or png.
#pdf(paste(outputDirectory, "concentration_criteria_plots.pdf", sep="/"), width=11, height=8.5)

png.width   = 8.5 #inches
png.height  = 11  #inches
mar.default = c(5, 4, 4, 2) + 0.1

# Just two colors - do not exceed and exceed
palette(c("darkgray","#FF3300"))

for (type in c("acute", "chronic", "hh", "cleanup", "screening")) {

  # Set values specific to each type
  # Water
  if (type == "acute") {
    exceedCol <- "acuteExceeds"
    typeTitle <- "Acute"
  } else if (type == "chronic") {
    exceedCol <- "chronicExceeds"
    typeTitle <- "Chronic"
  } else if (type == "hh") {
    exceedCol <- "hhExceeds"
    typeTitle <- "Human Health"
  
  # Sediment
  } else if (type == "cleanup") {
    exceedCol <- "cleanupExceeds"
    typeTitle <- "Sediment Cleanup"
  } else if (type == "screening") {
    exceedCol <- "screeningExceeds"
    typeTitle <- "Sediment Screening"
  }
  
  # File output values.  Comment out next two lines for PDF output.
  # This line has to be BEFORE you specify the margins.
  filename = paste("concentration", type, ".png", sep="")
  png(file = paste(outputDirectory, filename, sep="/"), 
      width  = png.width, 
      height = png.width,
      units  = "in",
      res    = 1000
  )
  
  par(mar = mar.default + c(0, 15, 0, 3) )

  # Values vs. criteria
  storm.current <- Storm[-which(is.na(Storm[, type])), ]
  storm.current$Parameter_string <- factor(storm.current$Parameter_string)
  
  ylimits <- c(1, length(levels(storm.current$Parameter_string)))
  xlimits <- c(min(storm.current$new_Result_Value), max(storm.current$new_Result_Value))
  
  # List of exceedence percents, for the right-hand axis
  exceeds   <- array()
  
  # Shift the data down slightly, so that the values show up below the horizontal line
  #storm.current$y <- as.numeric(storm.current$Parameter_string) - 0.15
    
  plot(storm.current$new_Result_Value, 
       storm.current$Parameter_string,
       pch  = -124,
       cex  = 0.75,
       log  = "x",
       xlab = "Sampled Concentration (ug/L)",
       ylab = "",
       main = paste("Range of Concentrations with", typeTitle, "Criteria"),
       col  = as.numeric(storm.current[, exceedCol])+1,
       yaxt = "n"
  )

   j <- 1
   for (param in levels(storm.current$Parameter_string)) {
     
     # Create background horizontal lines for each parameter
     xMin <- min(storm.current$new_Result_Value)
     xMax <- max(storm.current$new_Result_Value)
     abline(h   = j,
            col = "lightgray",
            lty = 2
     )
     
     # Add min, max, and censored count labels at the ends of each data series
     #theMin      <- min(storm.current$new_Result_Value[which(storm.current$Parameter_string == param)])
     #theMax      <- max(storm.current$new_Result_Value[which(storm.current$Parameter_string == param)])
     exceedColValues <- storm.current[, exceedCol][which(storm.current$Parameter_string == param)]
     exceedCount     <- sum(exceedColValues)
     exceedLength    <- length(exceedColValues)
     exceedPercent   <- (exceedCount / exceedLength) * 100
     
     #text(theMin, j, labels = c(as.character(theMin)), pos = 3, offset = 0.5, cex = 0.85 )
     #text(theMax, j, labels = c(as.character(theMax)), pos = 3, offset = 0.5, cex = 0.85 )
     exceeds[j] <- paste(as.character(round(exceedPercent)), "%", sep="")
     
     # Add the min and max of the criteria to each data series
     acuteMin <- min(storm.current[which(storm.current$Parameter_string == param), type])
     acuteMax <- max(storm.current[which(storm.current$Parameter_string == param), type])
     
     #text(acuteMin, j, labels = c("|"))
     #text(acuteMax, j, labels = c("|"))
     line <- c()
     line$x <- c(acuteMin, acuteMin, acuteMin, acuteMax, acuteMax, acuteMax)
     line$y <- c(j - 0.4, j - 0.2, j - 0.3, j - 0.3, j - 0.2, j - 0.4)
     lines(x   = line,
           lwd = 2)
     
     j <- j + 1
     
   }
  
  # Parameter name labels
  
  labelList <- levels(storm.current$Parameter_string)
  
  axis(side   = 2, 
       at     = 1:length(labelList), 
       las    = 1,
       labels = labelList
       )
  
  # Censored value labels (second Y axis)
  
  axis(side   = 4, 
       at     = 1:length(exceeds), 
       las    = 1,
       labels = exceeds,
       hadj   = 1,
       lwd    = 0,
       lwd.ticks = 0,
       line   = 1.5

  )
  
  mtext(side = 4, line = 3, "Percent of Values that Exceed Criteria")
  
  dev.off()
}
