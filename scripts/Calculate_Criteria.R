

criteria <- function(parameter, standard="acute", pH, hardness) {
  ##### Return WA state numeric Do Not Exceed criteria for a given parameter -------------------------
  #
  # Args:
  #   parameter (string): Which water quality parameter to check 
  #   standard (string): in the list ("acute", "chronic", "hh")
  #   pH (numeric): pH value associated with sample (1-14)
  #   hardness (numeric): hardness as CaCO3 water  (ug/L)
  # 
  # Return:
  #   criterion (numeric): The do not exceed value, for that parameter & standard, with the
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
  criterion   <- ""
  criteriaList <- c()
  
  # Acute standards, from WAC 173-201A-240
  if (standard == "acute") {
    
    if(parameter == "Ammonia water  (ug/L)") {
      if(missing(pH)) {stop(missingpH)}
      criterion <- (0.275/(1+(10^(7.204-pH))))+(39/(1+(10^(pH-7.204)))*1000)
      
    } else if (parameter == "Arsenic water dissolved (ug/L)") {
      criterion <- 360
      
    } else if (parameter == "Cadmium water  (ug/L)"){
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.944 * exp((1.128 * log(hardness)) - 3.828)
      
    } else if (parameter == "Cadmium water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 1.136672 - ((log(hardness) * 0.041838) * 0.944 * (exp((1.128 * log(hardness)) - 3.828)))
      
    } else if (parameter == "Chloride water  (ug/L)"){
      criterion <- 860000
      
    } else if (parameter == "Chlorpyrifos water  (ug/L)"){
      criterion <- 0.083
      
    } else if (parameter == "Copper water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- exp((0.9422 * (log(hardness))) - 1.464)
      
    } else if (parameter == "Copper water dissolved (ug/L)"){
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.96 * (0.960 * exp((0.9422 * (log(hardness))) - 1.464))
      
    } else if (parameter == "Diazinon water  (ug/L)") {
      criteria <- 0.3397
      
    } else if (parameter == "Lead water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.791 * exp((1.273 * (log(hardness))) - 1.460)
      
    } else if (parameter == "Lead water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- (1.46203 - ((log(hardness) * 0.145712))) * 0.791 * (exp(1.273 * (log(hardness)) - 1.460)) 
      
    } else if (parameter == "Mercury water  (ug/L)") {
      criterion <- 2.1
      
    } else if (parameter == "Mercury water dissolved (ug/L)") {
      criterion <- 1.785
      
    } else if (parameter == "Pentachlorophenol water  (ug/L)") {
      if(missing(pH)) {stop(missingpH)}
      criterion <- exp((1.005 * pH) - 4.830)
      
      
    } else if (parameter == "Total PCB water  (ug/L)") {
      criterion <- 2
      
    } else if (parameter == "Zinc water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.978 * (exp(0.8473 * (log(hardness)) + 0.8604)) / 0.978
      
    } else if (parameter == "Zinc water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.978 * (exp(0.8473 * (log(hardness)) + 0.8604))
      
    } else {criterion <- NA}
    
    # Chronic standards, from WAC 173-201A-240
  } else if (standard == "chronic"){
    
    if (parameter == "Arsenic water dissolved (ug/L)") {
      criterion <- 190
      
    } else if (parameter == "Cadmium water  (ug/L)"){
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.909 * exp((0.7852 * log(hardness)) - 3.490)
      
    } else if (parameter == "Cadmium water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 1.101672 - ((log(hardness) * 0.041838) * 0.909 * (exp((0.7852 * log(hardness)) - 3.490)))
      
    } else if (parameter == "Chloride water  (ug/L)"){
      criterion <- 230000
      
    } else if (parameter == "Chlorpyrifos water  (ug/L)"){
      criterion <- 0.041
      
    } else if (parameter == "Copper water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.960 * exp((0.8545 * (log(hardness))) - 1.464)
      
    } else if (parameter == "Copper water dissolved (ug/L)"){
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.96 * (0.960 * exp((0.8545 * (log(hardness))) - 1.465))
      
    } else if (parameter == "Diazinon water  (ug/L)") {
      criterion <- 0.1699
      
    } else if (parameter == "Lead water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.791 * (exp(1.273 * (log(hardness)) - 4.705)) 
      
    } else if (parameter == "Lead water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- (1.46203 - (log(hardness) * (0.145712))) * 0.791 * (exp(1.273 * (log(hardness)) - 4.705)) 
      
    } else if (parameter == "Mercury water  (ug/L)") {
      criterion <- 0.012
      
    } else if (parameter == "Mercury water dissolved (ug/L)") {
      criterion <- 0.0102
      
    } else if (parameter == "Pentachlorophenol water  (ug/L)") {
      if(missing(pH)) {stop(missingpH)}
      criterion <- exp((1.005 * pH) - 5.29)
      
    } else if (parameter == "Total PCB water  (ug/L)") {
      criterion <- 0.014
      
    } else if (parameter == "Zinc water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- exp((0.8473 * (log(hardness))) + 0.7614)
      
      
    } else if (parameter == "Zinc water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.986 * (exp((0.8473 * (log(hardness))) + 0.7614))
      
    } else {criterion <- NA}
    
    # Human health standards, from ???
  } else if (standard == "hh"){
    
    if (parameter == "Acenaphthylene water  (ug/L)") {
      criterion <- 670
      
    } else if (parameter == "Anthracene water  (ug/L)") {
      criterion <- 9600
      
    } else if (parameter == "Arsenic water dissolved (ug/L)") {
      criterion <- 0.018
      
    } else if (parameter == "Benz(a)anthracene water  (ug/L)") {
      criterion <- 0.0028
      
    } else if (parameter == "Benzo(a)pyrene water  (ug/L)") {
      criterion <- 0.0028
      
    } else if (parameter == "Benzo(b)fluoranthene water  (ug/L)") {
      criterion <- 0.0028
      
    } else if (parameter == "Benzo(b,k)fluoranthene water  (ug/L)") {
      criterion <- 0.0028
      
    } else if (parameter == "Bis(2-ethylhexyl) phthalate water  (ug/L)" ) {
      criterion <- 1.8
      
    } else if (parameter == "Butyl benzyl phthalate water  (ug/L)") {
      criterion <- 1500
      
    } else if (parameter == "Chrysene water  (ug/L)") {
      criterion <- 0.0028
      
    } else if (parameter == "Dibenzo(a,h)anthracene water  (ug/L)") {
      criterion <- 0.0028
      
    } else if (parameter == "Dibutyl phthalate water  (ug/L)") {
      criterion <- 2700
      
    } else if (parameter == "Diethyl phthalate water  (ug/L)") {
      criterion <- 23000
      
    } else if (parameter == "Dimethyl phthalate water  (ug/L)") {
      criterion <- 313000
      
    } else if (parameter == "Fluoranthene water  (ug/L)") {
      criterion <- 300
      
    } else if (parameter == "Fluorene water  (ug/L)") {
      criterion <- 1300
      
    } else if (parameter == "Indeno(1,2,3-cd)pyrene water  (ug/L)") {
      criterion <- 0.0028
      
    } else if (parameter == "Mercury water  (ug/L)") {
      criterion <- 0.14
      
    } else if (parameter == "Pyrene water  (ug/L)") {
      criterion <- 960
      
    } else {criterion <- NA}
    
  }
  
  return(as.numeric(criterion))
  
}