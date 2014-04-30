#####
# Determine the criteria level for each measured parameter
#  for which there exist criteria.
# Depends on Plot_byParam_ver9_Apr2014.r (or newer),
#  and on Load_byParam_sub1_lookup.r.
#####

require(sqldf)
require(reshape)

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

acute   <- c()
chronic <- c()
hh      <- c()

for (i in 1:nrow(Storm)) {
  acute[i] <- criteria(parameter = Storm[i,"Parameter_string"],
                        standard  = "acute",
                        pH        = Storm[i, "pH"],
                        hardness  = Storm[i, "hardness"]
                        )
  chronic[i] <- criteria(parameter = Storm[i,"Parameter_string"],
                        standard  = "chronic",
                        pH        = Storm[i, "pH"],
                        hardness  = Storm[i, "hardness"]
                        )
  hh[i] <- criteria(parameter = Storm[i,"Parameter_string"],
                        standard  = "hh",
                        pH        = Storm[i, "pH"],
                        hardness  = Storm[i, "hardness"]
                        )
}

Storm$acute   <- acute
Storm$chronic <- chronic
Storm$hh      <- hh

Storm$acuteExceedPercent <- Storm$new_Result_Value / Storm$acute
Storm$chronicExceedPercent <- Storm$new_Result_Value / Storm$chronic
Storm$hhExceedPercent <- Storm$new_Result_Value / Storm$hh

##### Plot out criteria by parameter (per Will Hobbs suggestion)
pdf(paste(outputDirectory, "concentration_criteria_plots.pdf", sep="/"), width=11, height=8.5)

mar.default = c(5, 4, 4, 2) + 0.1

# Acute values vs. criteria
par(mar = mar.default + c(0, 12, 0, 0))
storm.acute <- Storm[-which(is.na(Storm$acute)), ]
storm.acute$Parameter_string <- factor(storm.acute$Parameter_string)

ylimits <- c(1, length(levels(storm.acute$Parameter_string)))
xlimits <- c(min(storm.acute$new_Result_Value), max(storm.acute$new_Result_Value))

plot(storm.acute$new_Result_Value, 
     storm.acute$Parameter_string,
     pch  = -124,
     cex  = 0.5,
     col  = "darkgray",
     log  = "x",
     xlab = "Sampled Concentration (ug/L)",
     ylab = "",
     main = "Range of Concentrations with acute criteria",
     yaxt = "n"
     )

axis(side = 2, 
     at   = 1:16, 
     las  = 1,
     labels = levels(storm.acute$Parameter_string))

# Add additional labelling/decoration to each data series
j <- 1
for (param in levels(storm.acute$Parameter_string)) {
  
  xMin <- min(storm.acute$new_Result_Value)
  xMax <- max(storm.acute$new_Result_Value)
  abline(h   = j,
         col = "lightgray"
         )
  
  # Add min and max labels at the ends of each data series
  theMin <- min(storm.acute$new_Result_Value[which(storm.acute$Parameter_string == param)])
  theMax <- max(storm.acute$new_Result_Value[which(storm.acute$Parameter_string == param)])
   
  text(theMin, j, labels = c(as.character(theMin)), pos = 2, offset = 0.5, cex = 0.85 )
  text(theMax, j, labels = c(as.character(theMax)), pos = 4, offset = 0.5, cex = 0.85 )
  
  # Add the min and max of the criteria to each data series
  acuteMin <- min(storm.acute$acute[which(storm.acute$Parameter_string == param)])
  acuteMax <- max(storm.acute$acute[which(storm.acute$Parameter_string == param)])
  
  text(acuteMin, j, labels = c("|"), col = "blue")
  text(acuteMax, j, labels = c("|"), col = "red" )
  
  j <- j + 1
  
}
dev.off()
