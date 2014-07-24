
#####
# Made by: Nat Kale, WA Dept. of Ecology
#####

criteria <- function(parameter, pH, hardness) {
  # Return WA state numeric Do Not Exceed criteria for a given parameter
  #
  # Args:
  #   parameter (string): Which water quality parameter to check 
  #   pH (numeric): pH value associated with sample (1-14)
  #   hardness (numeric): hardness as CaCO3 water  (ug/L)
  # 
  # Return:
  #   criteria (data.frame, 3 numeric values): The do not exceed values, for that parameter, with the
  #     given hardness and pH.
  #
  # Reference is WAC-173-201A-240.
  # By default returns accute, but can specify a standard of "chronic" or "hh" (human health).
  # Returns NA if there are no matching criteria; returns an error if pH or hardness is required
  #  and is not supplied.
  
  if(missing(parameter)) {
    stop("Parameter required, at a minimum.")
  }
  
  missingpH   <- paste("For parameter", parameter, "pH is required.")
  missingHard <- paste("For parameter", parameter, "hardness is required.")
  
  criteriaList <- data.frame(acute     = numeric(0), 
                             chronic   = numeric(0), 
                             hh        = numeric(0), 
                             cleanup   = numeric(0), 
                             screening = numeric(0)
                             )
  
  ##### Water concentration criteria -----------------------
  
  if (parameter == "Acenaphthylene water  (ug/L)") {
    criteriaList[1,"hh"] <- 670
  
  } else if(parameter == "Ammonia water  (ug/L)") {
    if(missing(pH)) {stop(missingpH)}
    
    criteriaList[1,"acute"] <- (0.275 / (1 + (10^(7.204 - pH)))) + (39 / (1 + (10^(pH - 7.204))) * 1000)
    
  } else if (parameter == "Anthracene water  (ug/L)") {
    criteriaList[1,"hh"] <- 9600
    
  } else if (parameter == "Arsenic water dissolved (ug/L)") {
    
    criteriaList[1,"acute"]   <- 360
    criteriaList[1,"chronic"] <- 190
    criteriaList[1,"hh"]      <-   0.018
  
  } else if (parameter == "Benz(a)anthracene water  (ug/L)") {
    criteriaList[1,"hh"] <- 0.0028
      
  } else if (parameter == "Benzo(a)pyrene water  (ug/L)") {
    criteriaList[1,"hh"] <- 0.0028
        
  } else if (parameter == "Benzo(b)fluoranthene water  (ug/L)") {
    criteriaList[1,"hh"] <- 0.0028
    
  } else if (parameter == "Benzo(b,k)fluoranthene water  (ug/L)") {
    criteriaList[1,"hh"] <- 0.0028
    
  } else if (parameter == "Bis(2-ethylhexyl) phthalate water  (ug/L)" ) {
    criteriaList[1,"hh"] <- 1.8
    
  } else if (parameter == "Butyl benzyl phthalate water  (ug/L)") {
    criteriaList[1,"hh"] <- 1500
    
  } else if (parameter == "Cadmium water  (ug/L)"){
    if(missing(hardness)) {stop(missingHard)}
    
    criteriaList[1,"acute"]   <- exp((1.1280 * log(hardness)) - 3.828)
    criteriaList[1,"chronic"] <- exp((0.7852 * log(hardness)) - 3.490)
    
  } else if (parameter == "Cadmium water dissolved (ug/L)") {
    if(missing(hardness)) {stop(missingHard)}
    
    criteriaList[1,"acute"]   <- 1.136672 - ((log(hardness) * 0.041838) * (exp((1.1280 * log(hardness)) - 3.828)))
    criteriaList[1,"chronic"] <- 1.101672 - ((log(hardness) * 0.041838) * (exp((0.7852 * log(hardness)) - 3.490)))
    
  } else if (parameter == "Chloride water  (ug/L)"){
    
    criteriaList[1,"acute"]   <- 860000
    criteriaList[1,"chronic"] <- 230000
    
  } else if (parameter == "Chlorpyrifos water  (ug/L)"){
    
    criteriaList[1,"acute"]   <- 0.083
    criteriaList[1,"chronic"] <- 0.041
  
  } else if (parameter == "Chrysene water  (ug/L)") {
    criteriaList[1,"hh"] <- 0.0028
    
  } else if (parameter == "Copper water  (ug/L)") {
    if(missing(hardness)) {stop(missingHard)}
    
    criteriaList[1,"acute"]   <- exp((0.9422 * (log(hardness))) - 1.464)
    criteriaList[1,"chronic"] <- exp((0.8545 * (log(hardness))) - 1.465)
    
  } else if (parameter == "Copper water dissolved (ug/L)"){
    if(missing(hardness)) {stop(missingHard)}
    
    criteriaList[1,"acute"]   <- 0.960 * exp((0.9422 * (log(hardness))) - 1.464)
    criteriaList[1,"chronic"] <- 0.960 * exp((0.8545 * (log(hardness))) - 1.465)
    
  } else if (parameter == "Diazinon water  (ug/L)") {
    
    criteriaList[1,"acute"]   <- 0.3397
    criteriaList[1,"chronic"] <- 0.1699
    
  } else if (parameter == "Dibenzo(a,h)anthracene water  (ug/L)") {
    criteriaList[1,"hh"] <- 0.0028
    
  } else if (parameter == "Dibutyl phthalate water  (ug/L)") {
    criteriaList[1,"hh"] <- 2700
    
  } else if (parameter == "Diethyl phthalate water  (ug/L)") {
    criteriaList[1,"hh"] <- 23000
    
  } else if (parameter == "Dimethyl phthalate water  (ug/L)") {
    criteriaList[1,"hh"] <- 313000
    
  } else if (parameter == "Fluoranthene water  (ug/L)") {
    criteriaList[1,"hh"] <- 300
    
  } else if (parameter == "Fluorene water  (ug/L)") {
    criteriaList[1,"hh"] <- 1300
    
  } else if (parameter == "Indeno(1,2,3-cd)pyrene water  (ug/L)") {
    criteriaList[1,"hh"] <- 0.0028
    
  } else if (parameter == "Lead water  (ug/L)") {
    if(missing(hardness)) {stop(missingHard)}
    
    criteriaList[1,"acute"]   <- exp((1.273 * (log(hardness))) - 1.460)
    criteriaList[1,"chronic"] <- exp((1.273 * (log(hardness))) - 4.705)
    
  } else if (parameter == "Lead water dissolved (ug/L)") {
    if(missing(hardness)) {stop(missingHard)}
    
    criteriaList[1,"acute"]   <- (1.46203 - (log(hardness) * 0.145712)) * exp(1.273 * (log(hardness)) - 1.460)
    criteriaList[1,"chronic"] <- (1.46203 - (log(hardness) * 0.145712)) * exp(1.273 * (log(hardness)) - 4.705)
    
  } else if (parameter == "Mercury water  (ug/L)") {
    
    criteriaList[1,"acute"]   <- 2.1
    criteriaList[1,"chronic"] <- 0.012
    criteriaList[1,"hh"]      <- 0.14
    
  } else if (parameter == "Mercury water dissolved (ug/L)") {
    
    criteriaList[1,"acute"]   <- 1.785
    criteriaList[1,"chronic"] <- 0.0102
        
  } else if (parameter == "Pentachlorophenol water  (ug/L)") {
    if(missing(pH)) {stop(missingpH)}
    
    criteriaList[1,"acute"]   <- exp((1.005 * pH) - 4.830)
    criteriaList[1,"chronic"] <- exp((1.005 * pH) - 5.29)
    
  } else if (parameter == "Pyrene water  (ug/L)") {
    criteriaList[1,"hh"] <- 960
    
  } else if (parameter == "Total PCB water  (ug/L)") {
    
    criteriaList[1,"acute"]   <- 2
    criteriaList[1,"chronic"] <- 0.014
    
  } else if (parameter == "Zinc water  (ug/L)") {
    if(missing(hardness)) {stop(missingHard)}
    
    criteriaList[1,"acute"]   <- exp((0.8473 * log(hardness)) + 0.8604)
    criteriaList[1,"chronic"] <- exp((0.8473 * log(hardness)) + 0.7614)
    
  } else if (parameter == "Zinc water dissolved (ug/L)") {
    if(missing(hardness)) {stop(missingHard)}
    
    criteriaList[1,"acute"]   <- 0.978 * exp((0.8473 * log(hardness)) + 0.8604)
    criteriaList[1,"chronic"] <- 0.986 * exp((0.8473 * log(hardness)) + 0.7614)
  
  ##### Sediment concentration criteria -----------------------
  
  } else if (parameter == "Cadmium sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 2100
    criteriaList[1,"screening"] <- 5400
    
  } else if (parameter == "Copper sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 400000
    criteriaList[1,"screening"] <- 1200000
    
  } else if (parameter == "Lead sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 360000
    criteriaList[1,"screening"] <- 1300000
    
  } else if (parameter == "Mercury sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 660
    criteriaList[1,"screening"] <- 800
    
  } else if (parameter == "Zinc sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 3200000
    criteriaList[1,"screening"] <- 4200000

  } else if (parameter == "Bis(2-ethylhexyl) phthalate sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 500
    criteriaList[1,"screening"] <- 22000
    
  } else if (parameter == "Bis(2-ethylhexyl) phthalate sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 500
    criteriaList[1,"screening"] <- 22000
    
  } else if (parameter == "Di-N-Octyl Phthalate sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 39
    criteriaList[1,"screening"] <- 1100
    
  } else if (parameter == "Pentachlorophenol sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 1200
    criteriaList[1,"screening"] <- 1200
    
  } else if (parameter == "Phenol sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 120
    criteriaList[1,"screening"] <- 210
    
  } else if (parameter == "Total PAH sediment  (ug/Kg)") {
    
    criteriaList[1,"cleanup"]   <- 17000
    criteriaList[1,"screening"] <- 30000
    
  } else {
    # Return a criteria set of all NA
    criteriaList[1, ] <- c(NA, NA, NA, NA, NA)
    
  }
  
  return(criteriaList)
  
}
