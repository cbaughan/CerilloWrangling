# Christina Guerrero
# christinagb@arizona.edu
# 
# Prepare data for growth curve analysis using growthcurver
# Install required packages
install.packages("caret")
install.packages("tidyverse")
install.packages("readr")
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
# Load source codes. These files should be placed in your working directory
source("SummarizeGrowthByPlateCB.R")
source("cerillo-wrangling.R")
  
#Interactively choose a file to import and skip the first 9 lines of the file
d<- read_csv(file.choose(), 
                                        col_names = TRUE, skip = 9)

outputPath <- 'yourfilename'  #Change to your output file name

# Use cerillo_wrangling function to format the dataframe 
d<-cerillo_wrangling(d)

# Select the blank columns to run a quick check 
blanks<- d[seq(1, nrow(d), 3), ] #Take every nth timepoint to save time while plotting
blanks<-blanks[,-c(2:65)] #Removes columns corresponding to wells A1-H8
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
