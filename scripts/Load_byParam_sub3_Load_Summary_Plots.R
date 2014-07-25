
####Script for creating plot output to summarize contaminant loads by mass and area###

###IMPORTANT###
# Presumes that Plot_byParam_ver9_Apr2014.r and Load_byParam_sub1_lookup.R
#  have already been run in the same workspace.

# Contains a variety of helper functions
source(paste(scriptDirectory, "loadCalculate.R", sep="/"))



##### Data cleaning & analysis -------------------------

# Remove unnecessary data, calculate loads, perform unit conversions
storm_load <- loadCalc(Storm)

ParamList <- as.vector(sort(unique(storm_load$Parameter_string)))

# ditch the samples with no sample loads
storm_load <- storm_load[-which(is.na(storm_load$sample_loads) | is.na(storm_load$storm_loads)), ]

# create the data summaries for the mass loads
Storm_mass_loads <- massLoadSummary(storm_load, ParamList)

# create the data summaries for the aerial loads
#source(paste(scriptDirectory, "Load_byParam_sub4b_Aerial_Load_Summary.r", sep="/"))
Storm_area_loads <- arealLoadSummary(storm_load, ParamList)

##### Produce summary plots -------------------------

pdf(file = paste(outputDirectory, "Plots of Load Summaries.pdf", sep="/"),
    height = 8.5,
    width = 11)

par(mgp=c(2.8,0.5,0))

# Set up the display with three plots on top, two on bottom

# Would like to do this, but can't figure out how to do
#  multiple page PDFs using split.screen.  C'mon, Google.

#split.screen(c(2,1))
#split.screen(c(1,3), 1)
#split.screen(c(1,2), 2)

layout(matrix(c(1,2,3,4,5,5), nrow = 2, ncol = 3, byrow = TRUE))

#i<-20

for (i in 1:length(ParamList)) {
  # VERY IMPORTANT - the value of "i" refers to the parameter being plotted throughout
  #  this loop.  DO NOT set the value of "i" within any of the subscripts.
  
    ParamData <- storm_load[which(storm_load$Parameter_string == ParamList[i]), ]
      
    # Produce boxplots of the mass loads by land use
    source( paste(scriptDirectory, "Load_byParam_sub5a_Mass_Boxplot_LandUse.r", sep="/"))
    
    #Produce boxplots of the aerial loads by land use
    source( paste(scriptDirectory, "Load_byParam_sub5b_Aerial_Boxplot_LandUse.r", sep="/"))
    
    #ylimits <-  c(min(ParamData$storm_area_loads)/2, max(ParamData$storm_area_loads)*2)
    
    # Produce boxplots of the aerial loads by Wet season
    source( paste(scriptDirectory, "Load_byParam_sub6_Boxplot_Season.r", sep="/"))
    
    # Produce ECDFs for aerial loads
    source( paste(scriptDirectory, "Load_byParam_sub7_ECDF_Landuse.R", sep="/"))
    
    # Produce jitter plots of aerial loads vs. binned % impervious surface
    source( paste(scriptDirectory, "Load_byParam_sub4b_Aerial_Load_Summary.R", sep="/"))
    
    print(ParamList[i])
    
}

close.screen(all = TRUE)
dev.off()
