
##### Read Stormwater Data -------------

# This file is called by Plot_byParam_ver9_[date].r.
#  It assumes the data have been pre-processed in MS Access.
# The file "FinalMasterFile_4-28-2014.csv" should be included
#  with this script, and used as the data source.


### Read the data from the csv file:
print("Reading the data...")
Storm <- read.csv(csvFile) 
print("Data read.  Making adjustments...")

### Make a few adjustments to the Storm dataframe:
Storm <- Storm[-which(Storm$nonDetect_Flag=="WARNING"),]                ### Remove REJ and B data flagged as 'WARNING'
Storm <- Storm[-which(Storm$paramStatus=="Omit"),]                      ### Remove the parameters that are designated 'Omit' for analysis and plotting
Storm <- Storm[-which(substr(Storm$Parameter,1,8) == "Particle"),]      ### Remove grain size data.
##Storm<-Storm[-which(Storm$new_Result_Units=="%"),]                    ### temporarily remove all grain size and TOC

#####
### Make the sample matrix names consistent

sedimentNames <- c("solid/sediment", "Solid/sediment", "Solid/Sediment", "SOLID/SEDIMENT")
waterNames <- c("WATER", "water")

Storm$Sample_Matrix<-as.vector(Storm$Sample_Matrix)

# Switch every variation of "solid/sediment" to "Sediment"
for (name in sedimentNames) {
  Storm$Sample_Matrix[which(Storm$Sample_Matrix == name)] <- "Sediment"
}

# Switch every variation of "water" to "Water"
for (name in waterNames) {
  Storm$Sample_Matrix[which(Storm$Sample_Matrix == name)] <- "Water"
}

Storm$Sample_Matrix <- factor(Storm$Sample_Matrix)

#table(Storm$new_Fraction_Analyzed)                           ###verify the fraction analyzed only contains blanks and Dissolved
#table(Storm$Sample_Matrix)

######
### Make sure a couple of the integer columns are factors - boxplots need this
Storm$Season <- factor(Storm$Season)                                                   
Storm$nonDetect_Flag <- as.logical(Storm$nonDetect_Flag)         #and the nonDetect_Flag is logical 
Storm$WetSeason<- as.logical(Storm$WetSeason)

#####
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

### copy the new_Result_Reported_Value_Number column over to the old one to make sure
### we never accidentally use the numbers with mixed units of measurement.

#Storm$new_Result_Reported_Value_Number <- Storm$new_Result_Reported_Value                  

#####
### Create a list of parameter names to group data for plotting/analysis:
Parameter.string <- paste(Storm$Parameter,  " ", 
                          tolower(Storm$Sample_Matrix),  " ",
                          tolower(Storm$new_Fraction_Analyzed),
                          " (", Storm$new_Result_Units,  ")",
                          sep="")

###Use the lower Parameter String to group data by parameter and land use for statistical summaries
#Parameter.string <- paste(Storm$Type," ",Storm$Parameter, " ", tolower(Storm$Sample_Matrix), " ", tolower(Storm$new_Fraction_Analyzed), " (", Storm$new_Result_Units, ")", sep="")     

###Use the lower Parameter String to group data by parameter and season for statistical summaries
#Storm$WetSeason[which(Storm$WetSeason=="TRUE")]<-"wet"
#Storm$WetSeason[which(Storm$WetSeason=="FALSE")]<-"dry"
#Parameter.string <- paste(Storm$WetSeason," ",Storm$Parameter, " ", tolower(Storm$Sample_Matrix), " ", tolower(Storm$new_Fraction_Analyzed),
#                          " (", Storm$new_Result_Units, ")", sep="") 

Storm <- droplevels(cbind(Storm, Parameter.string))

print("Summary of non-Detect flags and data qualifiers.  Make Sure there are no REJ results.")
print(table(Storm$Result_Data_Qualifier, Storm$nonDetect_Flag))
