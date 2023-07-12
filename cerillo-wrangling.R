# Christina Guerrero
# christinagb@arizona.edu
message("Loading functions from cerillo-wrangling.R")
# Function for wrangling Cerillo data to input into Growthcurver
cerillo_wrangling<-function(d){
  
  #Uses the first row as row names and deletes the duplicate row
  colnames(d) <- as.character(d[1,])
  d<- d[-1,]
  # Remove Temperature Column
  d<-d[,-(1:2)] 
  d<-d[,-2]
  # Convert class to numeric
  d<-as.data.frame(sapply(d, as.numeric))
  
  # Rename column as time
  names(d)[1] <- "time"
  d$time <- as.numeric(unlist(d$time))
  
  # Convert Linux Time to Time Since Beginning of Experiment in Hours
  d$time<- d$time - d[1,1] #Subtracting the beginning time point from every value
  d$time<- d$time/60 # Converting to minute
  d$time<- d$time/60 # Converting to hours
  
  
  #Convert Columns from A1,A2,A3, to A1,B1, C1
  d<-melt(d, id=c("time")) 
  d<- d %>% separate(variable, into = c("text", "num"), 
                     sep = "(?<=[A-Za-z])(?=[0-9])" )
  d<-d[order(as.numeric(d$num)),]
  d<-d %>%unite('variable', text:num, sep = "")
  d<-pivot_wider(d, names_from= variable, values_from = value)
  return(d)
}
