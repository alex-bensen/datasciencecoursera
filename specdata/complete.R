complete <- function(directory, id) {
  dir1 <- paste('~/datasciencecoursera/specdata/', directory, sep= '')    #setting the directory
  wd1 <- setwd(dir1)  #to find csv files
  
  xy <- list.files(pattern='.csv')
  
  data1 <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(data1) <- c('id', 'nodes')
  
  for (i in id) {
    csvfile <- read.csv(xy[i])
    x <- !(is.na(csvfile['sulfate'] & csvfile['nitrate']))
    xlist <- x[x == TRUE] 
    xlen <- length(xlist)
    new <- c(i, xlen)
    data1[nrow(data1) + 1,] <- new
  }
  data1
}

