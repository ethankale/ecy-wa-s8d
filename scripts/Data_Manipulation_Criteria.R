dataDirectory <- "X:\\Hobbs_Will\\Final 2007-2012 Stormwater Data\\"

scriptDirectory <- paste(dataDirectory, "R\\", sep="")
outputDirectory <- paste(scriptDirectory, "Output\\", sep="")

### set up name of the input csv file.
csvFile <- paste(dataDirectory, "R\\FinalMasterFile_4-15-2014.csv", sep="")
Storm <- read.csv(csvFile) 
           
### Make a few adjustments to the Storm dataframe:
Storm <- Storm[-which(Storm$nonDetect_Flag=="WARNING"),]                ### Remove REJ and B data flagged as 'WARNING'
Storm <- Storm[-which(Storm$paramStatus=="Omit"),]                      ### Remove the parameters that are designated 'Omit' for analysis and plotting
Storm <- Storm[-which(substr(Storm$Parameter,1,8) == "Particle"),]         ### Remove grain size data.
Storm$Season <- factor(Storm$Season)                                                   
Storm$nonDetect_Flag <- as.logical(Storm$nonDetect_Flag)         #and the nonDetect_Flag is logical 
Storm$WetSeason<- as.logical(Storm$WetSeason)

### Assign abbreviations to the permittees:
Storm$Permittee <- as.vector(Storm$Study_Name)
Storm$Permittee[which(Storm$Permittee == "City of Seattle Phase I Municipal Stormwater Permit")] <- "CoS"
Storm$Permittee[which(Storm$Permittee == "City of Tacoma Phase I Municipal Stormwater Permit")] <- "TAC"
Storm$Permittee[which(Storm$Permittee == "Clark County Phase I Municipal Stormwater Permit")] <- "CLK"
Storm$Permittee[which(Storm$Permittee == "Port of Seattle Marine Division Phase I Municipal Stormwater Permit")] <- "PoS"
Storm$Permittee[which(Storm$Permittee == "Snohomish County Phase I Municipal Stormwater Permit")] <- "SNO"
Storm$Permittee[which(Storm$Permittee == "King County Phase I Municipal Stormwater Permit")] <- "KNG"
Storm$Permittee[which(Storm$Permittee == "Pierce County Phase I Municipal Stormwater Permit")] <- "PRC"
Storm$Permittee[which(Storm$Permittee == "Port of Tacoma Phase I Municipal Stormwater Permit")] <- "PoT"
Storm$Permittee <- factor(Storm$Permittee)

### Create a list of parameter names to group data for plotting/analysis:
Parameter.string <- paste(Storm$Parameter, " ", tolower(Storm$Sample_Matrix), " ", 
          tolower(Storm$new_Fraction_Analyzed), " (", Storm$new_Result_Units, ")", sep="")
#Parameter.string <- paste(Storm$Type," ",Storm$Result_Parameter_Name, " ", tolower(Storm$Sample_Matrix), " ", 
#         tolower(Storm$new_Fraction_Analyzed), " (", Storm$new_Result_Units, ")", sep="")      ##Use the lower Parameter String to group data by parameter and land use for statistical summaries
Storm <- droplevels(cbind(Storm, Parameter.string))

columns.keep <- c("Parameter", "paramClass", "Type", "new_Result_Value", "new_Result_Units","new_Fraction_Analyzed","Field_Collection_Start_Date", "Field_Collection_End_Date", "WetSeason","Season","Result_Data_Qualifier","Permittee","Parameter.string")
                   
Storm.long <- Storm[,columns.keep]
parameters<-as.vector(sort(unique(Storm$Parameter.string)))
columns.id <- "Result_Data_Qualifier"
Storm.wide <- reshape(data=Storm.long, v.names="Parameter.string", idvar=columns.id, timevar="new_Result_Value", direction="wide")
write.csv(Storm.wide, file="Storm dataset WIDE FORMAT for criteria.csv")


anyDuplicated(EIM$Sample_ID,EIM$Result_Parameter_Name)
EIM.dups<-EIM[duplicated(paste(EIM$Sample_ID,EIM$Result_Parameter_Name)),]
