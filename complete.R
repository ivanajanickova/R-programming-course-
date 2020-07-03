complete <- function(directory, id = 1 : 332) {
  setwd(paste0("C:/Users/Ivana/",directory))
  index <- 1
  nobs <- c(1 : length(id))
  for(i in id) {
    if(i < 10) {
      i_new <- paste0("00", i, ".csv")
      csv <- read.csv(i_new)
      nobs[index] = nrow(na.omit(csv))
      index <- index + 1
    }
    else if(i >=  10 && i < 100) {
      i_new <- paste0("0", i, ".csv")
      csv <- read.csv(i_new)
      nobs[index] = nrow(na.omit(csv))
      index <- index + 1
    }
    else {
      i_new <- paste0(i, ".csv")
      csv <- read.csv(i_new)
      nobs[index] = nrow(na.omit(csv))
      index <- index + 1
    }
  }
  return (data.frame(id, nobs))
}