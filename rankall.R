##Return the nth best hospital in every state for given outcome
##For ties, returns the first hospital by alphabetical order

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") [,c(2,7,11,17,23)]
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
  ##Remove NA
  names(data)[3] = "Mortality"
  data[,3] = suppressWarnings(as.numeric(data$Mortality))
  data = data[!is.na(data$Mortality),]
  ##Look at each state individually
  bystate = split(data, data$State)
  result = lapply(bystate, function(x, num) {
    ## Order by deaths then alphabetical
    x = x[order(x$Mortality, x$Hospital.Name),]
    ##Result in state
    if (num == "best") { ##Check if looking for best
      return (x$Hospital.Name[1])
    } else if (num == "worst") { ##Check if looking for worst
      return (x$Hospital.Name[nrow(x)])
    } else if (class(num) != "numeric") { ##Check if invalid word for placement was used
      stop("invalid placement")
    } else if (num > nrow(x)) { ##Check if placement is too low for number of valid hospitals
      return (NA)
    }else {
      return (x$Hospital.Name[num])
    } 
  }, num)
  ##Results
  return (data.frame(hospital=unlist(result), state=names(result)))
}