#This function woks with data from monitoring sufate and nitrate levels on 332 monitors
##It takes directory of a file and treshold value for complete cases a  monitor file (001.csv - 332.csv) has to have
###The function returns correlations between sulfate and nitrate levels for each monitor
corr <- function(directory, treshold = 0) {
  correlation <-0
  complete_cases <- complete(directory = "specdata")
  for(i in 1 : nrow(complete_cases)){
    if(complete_cases$nobs[i] > treshold) {  #chceck if the complete cases meet the treshold requirement
      if(complete_cases$id[i] < 10) {
        csv <- read.csv(paste0("00", complete_cases$id[i], ".csv"))  #loading the csv file 
        no_na <- na.omit(csv)  #getting rid of NA values
        correlation<- c(correlation, cor(no_na$sulfate, no_na$nitrate)) #adding the value to the correlation vector
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
  return (correlation[2 : length(correlation)]) #vector of correlations for monitors that meet treshold requuirement
}
