#####
# Initial stantistical analysis of data, including:
#  - chi-squared of each unique parameter/matrix/unit combination by land use type
#  - creation of a Case.list table with summary statistics and grading (A,B,C) of each unique combination of parameter/matrix/unit

GroupList <- unique(Storm[,c("Parameter.string", "paramGroup", "Parameter", "Sample_Matrix", "new_Fraction_Analyzed", "new_Result_Units")])
ParamList <- as.vector(sort(unique(Storm$Parameter.string)))

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


sink(paste(outputDirectory, "PetoPrentice.txt", sep="/"))
#sink(paste(outputDirectory, "PetoPrentice_landuse.txt", sep=""))   ###use for parameter + landuse summary
#sink(paste(outputDirectory, "PetoPrentice_season.txt", sep=""))   ###use for parameter + season summary

for (i in c(1:length(ParamList))) {
###for (i in c(120:133)) {
  ParamData  <- Storm[which(Storm$Parameter.string == ParamList[i]), ]
  Detects    <- ParamData$new_Result_Value[which(ParamData$nonDetect_Flag == FALSE) ]
  nonDetects <- ParamData$new_Result_Value[which(ParamData$nonDetect_Flag == TRUE) ]

  # Count & categorize detects & non-detects
  num.nonDetects <- length(nonDetects)
  if (num.nonDetects == 0) {
    min.nonDetects <- NA
    max.nonDetects <- NA
  } else {
    min.nonDetects <- min(nonDetects)
    max.nonDetects <- max(nonDetects)
  }

  num.Detects <- length(Detects)
  if (num.Detects == 0) {
    min.Detects <- NA
    max.Detects <- NA
  } else {
    min.Detects <- min(Detects)
    max.Detects <- max(Detects)
  }

  # Calculate percent of censored samples for each group, and therefore statistical "grade" of the group (A,B, or C)
  nSamples  <- c(num.nonDetects + num.Detects)
  pctCensor <- round(100*num.nonDetects / nSamples, 1)
  if (pctCensor <= 50) {
    case.code <- "A"
  } else if (pctCensor <= 80) {
    case.code <- "B"
  } else {
    case.code <- "C"  
  }

  # Print out each unique group, calculating statistical details if the group is of grade "A"
  cat(paste("Row:", i, "        Parameter=", ParamList[i], sep=""))
  cat("\n")

  if (case.code == "A") {
    
    KM.fit <- cenfit(ParamData$new_Result_Value, ParamData$nonDetect_Flag)
    print(KM.fit)
    print(mean(KM.fit))
    cat("\n")
    
    KM.mean.data   <- mean(KM.fit)
    KM.mean        <- as.vector(KM.mean.data[1])
    KM.mean.SE     <- as.vector(KM.mean.data[2])
    KM.mean.95LCL  <- as.vector(KM.mean.data[3])
    KM.mean.95UCL  <- as.vector(KM.mean.data[4])
    KM.median      <- as.vector(median(KM.fit))
    KM.SD          <- as.vector(sd(KM.fit))
  } else {
    KM.mean       <- NA
    KM.mean.SE    <- NA
    KM.mean.95LCL <- NA
    KM.mean.95UCL <- NA
    KM.median     <- NA
    KM.SD         <- NA
  }

  nGroups <- length(unique(ParamData$Type))
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

