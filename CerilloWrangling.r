# Christina Guerrero
# christinagb@arizona.edu
# 
# 
# Install required packages
install.packages("caret")
install.packages("tidyverse")
install.pcakages("readr")
install.packages("FSA")
install.packages("growthcurver")

#Load required libraries
library(reshape2)
library(tidyr)
library(stringr)
library(growthcurver)
library(caret)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(FSA)

# Set Working Directory
source("SummarizeGrowthByPlateCB.R") # This file should be in your working directory


#Interactively choose a file to import and skip the first 9 lines of the file
d<- read_csv(file.choose(), 
                                              col_names = FALSE, skip = 9)

outputPath <- '2023-04-05_Test'  #Change to your output file name

#Uses the first row as row names and deletes the duplicate row
colnames(d) <- as.character(d[1,])
d<- d[-1,]

# Remove the columns that we will not be using
d <-select(d,-c("Date and Time", "Duration (Hours)", "Duration (Minutes)", "Air Temp", "NA"))

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
#Add a header
colnames(d) <- as.character(d[1,])
d<- d[-1,]

# Remove Temperature Column
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

# Select the blank columns to run a quick check 
blanks<- d[seq(1, nrow(d), 3), ] #Take every nth timepoint to save time while plotting
blanks<-blanks[,-c(2:65)] 
blanks<-melt(blanks, id.vars = "time", variable.name = "well", value.name = "od")
# View the blanks file and select up to 3 wells that will be used for background correction
ggplot(blanks, aes(x=time, y=od)) + geom_point(aes(), alpha=0.5)  + facet_wrap(~well, ncol = 4, dir = c("v")) + theme_bw()


# View the blanks file and determine which wells will be used for background correction
# Manual select the well used to blank column
d$blank<-apply(d[, c("H10", "H11", "H12")], 1, mean) #creates a blank column from the means of blank wells


# Check one well to determine if the data needs to be trimmed
ggplot(d, aes(x = time, y = C5)) + geom_point(alpha=0.7) 

#Growth Curver

# Run simple growthcurver
 
gc_out<-SummarizeGrowthByPlateCB(d, t_trim = 0, bg_correct = "blank", plot_fit = TRUE, plot_file = paste(outputPath, "_gc.pdf", sep = ''))

# Save data to a csv
write.table(gc_out, file = paste(outputPath,"_gc_table.csv", sep = ""), sep = ",", row.names = FALSE)

#Notes