##Return the hospital in the state with lowest mortality rate determined by specified outcome
##For ties, returns the first hospital by alphabetical order

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") [,c(2,7,11,17,23)]
  ## Check that state is valid  
  if (!state %in% data$State) {
    stop("invalid state")
  }
  ## Remove all columns except hospital name and specified outcome
  ## Stop and report if outcome is invalid
  if (outcome == "heart attack") {
    data = data[,c(1,2,3)]
  } else if (outcome == "heart failure") {
    data = data[,c(1,2,4)]
  } else if (outcome == "pneumonia") {
    data = data[,c(1,2,5)]
  } else {
    stop("invalid outcome")
  }
  ## Remove entries not in specified state
  data = data[data$State == state,]
  names(data)[3] = "Mortality"
  data[,3] = suppressWarnings(as.numeric(data$Mortality))
  ##Remove NA
  data = data[!is.na(data$Mortality),]
  ## Order by deaths then alphabetical
  data = data[order(data$Mortality, data$Hospital.Name),]
  ##Result
  return (data$Hospital.Name[1])
}