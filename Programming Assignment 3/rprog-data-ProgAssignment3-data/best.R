best <- function(state, outcomes) {
  ##read outcome data
  data1 <- read.csv('outcome-of-care-measures.csv')
  
  ## Check that state and outcome are valid
  if (!any(data1['State']==state)) {
    stop('invalid date')
  } 
  
  #taking outcomes input and converting it for use
  library('stringi')
  substrg <- unlist(strsplit(stri_trans_totitle(outcomes), ' '), '\\.')
  substrg1 <- paste(substrg, collapse = '.')
  outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", substrg1, collapse = '', 
                   sep = '')
  
  #check if outcome is valid
  if (!any(colnames(data1) == outcome)) {
    stop('invalid outcome')
  }
  
  ##return hospital name in that state with lowest 30-day death rate
  data2 <- subset(data1, State == state)
  xind <- which(data2[[outcome]] == min(data2[[outcome]][!(is.na(data2[[outcome]]))]))
  hosname <- sort(data2$Hospital.Name[xind])
  hosname
}

