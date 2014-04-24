#####
# Process the Storm data frame (created by Plot_ByParam_ver9_Apr2014.r
#  and further proceesed by Load_byParam_sub1_lookup.R)
#  to make loads for each parameter (parameter group?) per event.
#####

storm_load <- Storm[-which(Storm$paramClass=="Measurement"),]                ### remove all the flow data and conventional parameters
storm_load <- storm_load[-which(storm_load$Sample_Matrix=="Sediment"),]      ### remove sediment samples
storm_load <- storm_load[which(storm_load$new_Result_Units=="ug/L"),]        ### ensure all remaining samples are in appropriate units

storm_load$Field_Collection_End_Date<-as.Date(storm_load$Field_Collection_End_Date,"%m/%d/%Y")
storm_load$sample.year<-as.numeric(format(storm_load$Field_Collection_End_Date,"%Y"))

### calculate sample event loads -------------------------
# Unit conversion note - sample and storm volumes are in m3.  Desired load units are Kg.
#  All samples should be in ug/L.  Multiply ug/L by 1000 to get ug/m3.  Then divide
#  the resulting load by 1e9 (1,000,000,000) to convert ug to Kg.
storm_load$sample_loads <- (storm_load$sample_event_flow_volume*(storm_load$new_Result_Value*1000))/1e+09
storm_load$storm_loads  <- (storm_load$storm_event_flow_volume*(storm_load$new_Result_Value*1000))/1e+09
storm_load$load_units   <- "Kg"

# Update Parameter_string to remove load units (which should now be identical for all parameters)
storm_load$Parameter_string <- sub("\\s+$", "", paste(storm_load$Parameter, tolower(storm_load$new_Fraction_Analyzed), sep=" "))

noStorm  <- storm_load[which(is.na(storm_load$storm_loads)), ]
noSample <- storm_load[which(is.na(storm_load$sample_loads)), ]

loadCount <- aggregate(storm_load$sample_loads, list(parameter=storm_load$Parameter_string, location=storm_load$Location_ID), length)
loadNA    <- aggregate(storm_load$sample_loads, list(parameter=storm_load$Parameter_string, location=storm_load$Location_ID), function(x) sum(is.na(x)))

write.csv(loadNA, paste(outputDirectory, "loadNA.csv", sep="/"))
write.csv(loadCount, paste(outputDirectory, "loadCount.csv", sep="/"))

### Future reference - plots

#layout(matrix(c(1:2), 2, 1, byrow=TRUE))
#cuLoad <- storm_load[which(storm_load$Parameter_string == "Copper"),]
#boxplot(x = cuLoad$sample_loads, horizontal = TRUE, log = "x")
#plot(x = cuLoad$sample_loads, y = rep(1, nrow(cuLoad)), pch = -124, col = rgb(0,0,0,0.15), log = "x")


###THIS IS SIMPLY A COPY OF THE SCRIPT FOR CALCULATING DATA SUMMARIES FROM CONCENTRATIONS AND NEEDS EDITING FOR LOADS -------------------------
          
GroupList <- unique(storm_load[,c("Parameter.string", "paramGroup", "Parameter", "new_Fraction_Analyzed", "new_Result_Units")])
ParamList <- as.vector(sort(unique(storm_load$Parameter.string)))

### Store Detection counts in a matrix for export.
Case.list <- data.frame(Parameter=character(),
                 CaseCode=integer(), 
                 pctCensor=numeric(), 
                 nSamples=integer(),
                 num.Detects=integer(),
                 num.nonDetects=integer(),
                 min.Detect=numeric(),
                 max.Detect=numeric(),
                 min.nonDetect=numeric(),
                 max.nonDetect=numeric(),
                 KM.mean=numeric(),
                 KM.mean.SE=numeric(),
                 KM.mean.95LCL=numeric(),
                 KM.mean.95UCL=numeric(),
                 KM.median=numeric(),
                 KM.SD=numeric(),
                 PetoPrentice.pvalue=numeric(),
                 PetoPrentice.chisq=numeric(),
                 PetoPrentice.df=numeric(),
                 ROS_correlation=numeric(), 
                 PPCC_test=character())


sink(paste(outputDirectory, "PetoPrentice.txt", sep=""))

for (i in c(1:length(ParamList))) {
###for (i in c(120:133)) {
  ParamData <- Storm[which(Storm$Parameter.string == ParamList[i]), ]
  Detects <- ParamData$new_Result_Value[which(ParamData$nonDetect_Flag == FALSE) ]
  nonDetects <- ParamData$new_Result_Value[which(ParamData$nonDetect_Flag == TRUE) ]

  if (length(nonDetects) == 0) {
    num.nonDetects <- 0
    min.nonDetects <- NA
    max.nonDetects <- NA
  } else {
    num.nonDetects <- length(nonDetects)
    min.nonDetects <- min(nonDetects)
    max.nonDetects <- max(nonDetects)
  }

  if (length(Detects) == 0) {
    num.Detects <- 0
    min.Detects <- NA
    max.Detects <- NA
  } else {
    num.Detects <- length(Detects)
    min.Detects <- min(Detects)
    max.Detects <- max(Detects)
  }

  nSamples <- c(num.nonDetects + num.Detects)
  pctCensor <- round(100*num.nonDetects / nSamples,1)
  if (pctCensor <= 50) {
    case.code <- "A"
  } else if (pctCensor <= 80) {
    case.code <- "B"
  } else {
    case.code <- "C"  
  }

  cat(paste("Row:  ", i, "      Parameter=", ParamList[i], sep=""))
  cat("\n")

  if (case.code == "A") {
    KM.fit <- cenfit(ParamData$new_Result_Value, ParamData$nonDetect_Flag)
    print(KM.fit)
    print(mean(KM.fit))
    cat("\n")
    KM.mean.data <- mean(KM.fit)
    KM.mean <- as.vector(KM.mean.data[1])
    KM.mean.SE <- as.vector(KM.mean.data[2])
    KM.mean.95LCL <- as.vector(KM.mean.data[3])
    KM.mean.95UCL <- as.vector(KM.mean.data[4])
    KM.median <- as.vector(median(KM.fit))
    KM.SD <- as.vector(sd(KM.fit))
  } else {
    KM.mean <- NA
    KM.mean.SE <- NA
    KM.mean.95LCL <- NA
    KM.mean.95UCL <- NA
    KM.median <- NA
    KM.SD <- NA
  }

  nGroups <- length(unique(ParamData$useCode))
  if( case.code == "A" & num.Detects > 9 & nGroups > 2) {
    PetoPrentice <- cendiff(ParamData$new_Result_Value, ParamData$nonDetect_Flag, groups=ParamData$Type)
    print(PetoPrentice)
    cat("\n\n\n")
    PetoPrentice.chisq <- PetoPrentice$chisq
    PetoPrentice.df <- nGroups-1
    if (PetoPrentice.chisq < 60) {
      PetoPrentice.pvalue <- 1 - pchisq(PetoPrentice$chisq, nGroups-1)
    } else {
      PetoPrentice.pvalue <- -expm1(pchisq(PetoPrentice$chisq, df=PetoPrentice.df, log.p=TRUE))
    }

  } else {
    PetoPrentice.chisq <- NA
    PetoPrentice.df <- NA
    PetoPrentice.pvalue <- NA
  }


  ROS_correlation <- NA
  PPCC_test <- NA
  
   if (case.code == "B" & nSamples>50) {
    MLE.fit <- cenmle(ParamData$new_Result_Value, ParamData$nonDetect_Flag)
    print(MLE.fit)
    print(mean(MLE.fit))
    cat("\n")
    MLE.mean.data <- mean(MLE.fit)
    MLE.mean <- as.vector(MLE.mean.data[1])
    MLE.mean.SE <- as.vector(MLE.mean.data[2])
    MLE.mean.95LCL <- as.vector(MLE.mean.data[3])
    MLE.mean.95UCL <- as.vector(MLE.mean.data[4])
    MLE.median <- as.vector(median(MLE.fit))
    MLE.SD <- as.vector(sd(MLE.fit))
  } else {
    MLE.mean <- NA
    MLE.mean.SE <- NA
    MLE.mean.95LCL <- NA
    MLE.mean.95UCL <- NA
    MLE.median <- NA
    MLE.SD <- NA
  }
   
  Case.list <- rbind(Case.list, data.frame(ParamList[i], case.code, pctCensor, nSamples, num.Detects, 
                      num.nonDetects, min.Detects, max.Detects, min.nonDetects, max.nonDetects, 
                      KM.mean, KM.mean.SE, KM.mean.95LCL, KM.mean.95UCL, KM.median, KM.SD, 
                      PetoPrentice.pvalue, PetoPrentice.chisq, PetoPrentice.df, ROS_correlation, PPCC_test,
                      MLE.mean,MLE.mean.SE,MLE.mean.95LCL,MLE.mean.95UCL,MLE.median,MLE.SD))

}
sink()

print(table(Case.list$case.code))

