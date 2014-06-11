#####
# Summary information, including map, about S8.D date.
# 
# Relies on Plot_byParam_ver9_Apr2014.r
#
#####

library(maps)

#bbox <- c(left   = min(sites$CalculatedNAD83Longitude),
#          right  = max(sites$CalculatedNAD83Longitude),
#          bottom = max(sites$CalculatedNAD83Latitude),
#          top    = min(sites$CalculatedNAD83Latitude)
#          )

# Map limits.  Generally easier to set manually to western
#  Washington than to try to assign dynamically (though you can).

#xlim <- range(sites$CalculatedNAD83Longitude)
#xlim <- c(xlim[1] * 1.01, xlim[2] * 0.99)
xlim <- c(-125, -120)
#ylim <- range(sites$CalculatedNAD83Latitude)
#ylim <- c(ylim[1] * 0.99, ylim[2] * 1.01)
ylim <- c(45.2, 48.3)

# Display the outline of the state, and fainter county boundaries.
map("county", "washington", interior = FALSE, xlim = xlim, ylim = ylim)
map("county", "washington", boundary = FALSE, col="gray", add = TRUE)

# Add points for each site, color-coded by land use.
points(x   = sites$CalculatedNAD83Longitude, 
       y   = sites$CalculatedNAD83Latitude, 
       col = sites$LanduseCode,
       pch = 15
       )

# Legend for land use.
legend(x = "bottomright", 
       legend = unique(sites$LanduseCode), 
       pch = 15,
       col = unique(sites$LanduseCode)
       )

# List the most commonly sampled parameters.
theSum <- summary(Storm$Parameter)
theSum <- sort(theSum, decreasing = TRUE)
theSum[c(1:15)]

# Count qualifiers
qualifiedDataSum <- summary(Storm$Result_Data_Qualifier)
qualifiedDataSum <- sort(qualifiedDataSum, decreasing = TRUE)

# Count Labs
labSum <- summary(Storm$Result_Lab_Name)
labSum <- sort(labSum, decreasing = TRUE)

# Count of samples by site owner & landuse type
locationType <- aggregate(TIAPercent ~ SiteOwner + Type, data = Storm, mean)
