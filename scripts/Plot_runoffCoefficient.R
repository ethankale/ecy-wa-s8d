#####
# Calculate the runoff coefficient for each year & location,
#  and plot them.
#
# Depends on Plot_byParam_ver9_Apr2014.r (or newer),
#  and on Load_byParam_sub1_lookup.r.
#####

##### Data Analysis & Calculation -----------------------------

# Make a new dataset, so we're not messing with the existing one
runoffEvents <- Storm

# Get basin size in square meters
runoffEvents$m2 <- round(runoffEvents$Acres * 4046.86)

# Sum rainfall and storm event flow by site id and year
runoffEvents$year <- substr(runoffEvents$Field_Collection_Start_Date, 1, 4)

rainfall <- runoffEvents[which(runoffEvents$Parameter == "Precipitation"), ]
rainfall <- rainfall[, c("new_Result_Value", "storm_event_flow_volume", "year", "Location_ID", "m2", "TIAPercent", "LanduseCode")]

runoffAgg <- aggregate(. ~ year + Location_ID + m2 + TIAPercent + LanduseCode, data = rainfall, sum)

# Clean up the aggregated data (remove empty factor levels, etc.)
runoffAgg$LanduseCode <- factor(runoffAgg$LanduseCode)

# Unit conversion - inches to meters, precip depth to volume
runoffAgg$precipm   <- runoffAgg$new_Result_Value * 0.0254
runoffAgg$precipVol <- runoffAgg$precipm * runoffAgg$m2

# Calculate runoff coefficient
runoffAgg$coeff <- (runoffAgg$storm_event_flow_volume / runoffAgg$precipVol)

##### Plotting the Result -----------------------------

mar.default = c(5, 4, 4, 2) + 0.1
par(mfrow = c(2,2),
    mar   = mar.default
    )

plot(runoffAgg$storm_event_flow_volume, 
     runoffAgg$coeff,
     xlab = "Storm Volume",
     ylab = "Runoff Coefficient")

plot(runoffAgg$TIAPercent, 
     runoffAgg$coeff,
     xlab = "Impervious Percent",
     ylab = "Runoff Coefficient")

plot(runoffAgg$LanduseCode, 
     runoffAgg$coeff,
     xlab = "Land Use",
     ylab = "Runoff Coefficient")

plot(runoffAgg$m2, 
     runoffAgg$coeff,
     xlab = "Basin Size (m2)",
     ylab = "Runoff Coefficient")

