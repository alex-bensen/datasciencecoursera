rankhospital <- function(state, outcomes, num = "best") {
  ##read outcome data
  data1 <- read.csv('outcome-of-care-measures.csv')
  
  ## Check that state and outcome are valid
  if (!any(data1['State']==state)) {
    stop('invalid state')
  } 
  
  #creating dataframe
  data1 <- subset(data1, State == state)
  
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
  
  #creating new dataframe
  hospital <- data1[["Hospital.Name"]]
  Rate <- data1[[outcome]]
  
  data1 <- data.frame(hospital, Rate)
  data1['Rate'] = as.numeric(unlist(data1['Rate']))
  data1 <- subset(data1, Rate != "Not Available")
  data1 <- arrange(data1, Rate, hospital)
  
  
  #converting num input into usable numeric value
  numx <- 0
  if (num == 'best') {
    numx <- 1
  } else if (num == 'worst') {
    numx <- nrow(data1)
  } else {
    numx <- num
  }
  
  answer <- data1$hospital[[numx]]
  answer
}

