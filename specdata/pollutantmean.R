pollutantmean <- function(directory, pollutant, id = 1:322) {
  dir1 <- paste('~/datasciencecoursera/specdata/', directory, sep= '')    #setting the directory
  wd1 <- setwd(dir1)                                  #to find csv files
  sum1 <- 0
  length1 <- 0
  xy <- list.files(pattern='.csv')
  for (i in id) {
    csvfile <- read.csv(xy[i]) 
    col <- csvfile[[pollutant]]                       #finding mean
    sum1 <- sum1 + sum(col[!(is.na(col))])
    length1 <- length1 + length(col[!(is.na(col))])
  }
  mean1 <- sum1/length1
  mean1
}


