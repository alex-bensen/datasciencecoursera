corr <- function(directory, threshold) {
  dir1 <- paste('~/specdata/', directory, sep= '')    #setting the directory
  wd1 <- setwd(dir1)  
  
  xy <- list.files(pattern='.csv')
  
  data2 <- c()
  
  for (i in 1:length(xy)) {
    csvfile <- read.csv(xy[i])
    x <- !(is.na(csvfile['sulfate'] & csvfile['nitrate']))
    xlist <- x[x == TRUE] 
    xlen <- length(xlist)
    if (xlen > threshold) {
      df <- which(x == TRUE)
      xnit <- csvfile[df, 'nitrate']
      xsul <- csvfile[df, 'sulfate']
      xcorr <- cor(xnit, xsul)
      data2 <- c(data2, xcorr)
    }
  }
  data2
}

