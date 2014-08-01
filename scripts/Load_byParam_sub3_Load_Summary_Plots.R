
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

par(mgp = c(2.8,0.5,0))

mar.default = c(5, 4, 4, 2) + 0.1

# Set up the display with three plots on top, two on bottom

layout(matrix(c(1,1,2,2,3,3,
                4,4,4,5,5,5), 
              nrow = 2, 
              ncol = 6, 
              byrow = TRUE))

# The original list of parameter strings differs from the list
#  of parameter strings available in storm_load
ParameterList <- as.vector(sort(unique(storm_load$Parameter_string)))

#i<-20
#for (i in c(1, 2, 22, 55)) {

for (i in 1:length(ParameterList)) {
  # VERY IMPORTANT - the value of "i" refers to the parameter being plotted throughout
  #  this loop.  DO NOT set the value of "i" within any of the subscripts.
  
    ParamData <- storm_load[which(storm_load$Parameter_string == ParameterList[i]), ]
    ylimits   <- c(min(ParamData$sample_area_loads)/2, max(ParamData$sample_area_loads)*2)
    paramName <- sub(" (ug/L)", "", ParameterList[i], fixed=TRUE)
    
    # Row 1
    par(mar = mar.default + c(2,0,4,0))
    
    # Produce boxplots of the mass loads by land use
    source( paste(scriptDirectory, "Load_byParam_sub5a_Mass_Boxplot_LandUse.r", sep="/"))
    
    #Produce boxplots of the aerial loads by land use; also include the legend for top three plots
    source( paste(scriptDirectory, "Load_byParam_sub5b_Aerial_Boxplot_LandUse.r", sep="/"))
    
    # Produce boxplots of the aerial loads by Wet season
    source( paste(scriptDirectory, "Load_byParam_sub6_Boxplot_Season.r", sep="/"))
    
    
    # Row 2
    par(mar = mar.default)
    
    # Produce ECDFs for aerial loads
    source( paste(scriptDirectory, "Load_byParam_sub7_ECDF_Landuse.R", sep="/"))
    
    # Produce jitter plots of areal loads vs. binned % impervious surface
    source( paste(scriptDirectory, "Load_byParam_sub8_Jitter_TIA.R", sep="/"))
    
    # Title
    title(paramName, 
          outer = TRUE, 
          line  = -2,
          cex.main   = 1.75)
    
    print(ParameterList[i])
    
}

close.screen(all = TRUE)
dev.off()
