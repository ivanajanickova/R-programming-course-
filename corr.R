corr <- function(directory, treshold = 0) {
  sulfate <- 0
  nitrate <- 0
  correlation <-0
  complete_cases <- complete(directory = "specdata")
  for(i in 1 : nrow(complete_cases)){
    if(complete_cases$nobs[i] > treshold) {
      if(complete_cases$id[i] < 10) {
        csv <- read.csv(paste0("00", complete_cases$id[i], ".csv"))
        no_na <- na.omit(csv)
        correlation<- c(correlation, cor(no_na$sulfate, no_na$nitrate))
      }
      else if(complete_cases$id[i] >= 10 && complete_cases$id[i] < 100) {
        csv <- read.csv(paste0("0", complete_cases$id[i], ".csv"))
        no_na <- na.omit(csv)
        correlation<- c(correlation, cor(no_na$sulfate, no_na$nitrate))
        
      }
      else {
        csv <- read.csv(paste0(complete_cases$id[i], ".csv"))
        no_na <- na.omit(csv)
        correlation<- c(correlation, cor(no_na$sulfate, no_na$nitrate))
        
      }
    }
  }
  return (correlation[2 : length(correlation)])
}