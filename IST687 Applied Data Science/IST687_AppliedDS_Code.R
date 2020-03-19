# Project Elia Kostyrka

# R Code - unexecuted

# set working directory (this will be the only part 
# of the code that is different for each of us)

setwd("C:/Users/C/Desktop/Project")

# call library to read data
library(openxlsx)
# save the first worksheet of Elia's spreadsheet and
# name the file "Project Data"
df <- read.xlsx("Project Data.xlsx")

# Step 1: Clean the data

# rename columns by row 2
colnames(df) <- df[2,]

# remove unnecessary columns
df2 <- df[-1:-3,-c(1, 2, 5, 6, 10, 12:14, 24, 25 )]

#remove columns for open-ended questions
TeamData <- df2[,-c(14,15,24:26)]

#changing variable classes
TeamData$District <- as.factor(TeamData$District)
TeamData$`Study Status` <- as.factor(TeamData$`Study Status`)
TeamData$`Post Eval Question 1` <- as.factor(TeamData$`Post Eval Question 1`)
TeamData$`Pretest Score` <- as.numeric(TeamData$`Pretest Score`)
TeamData$`Posttest Score Attempt 1 ` <- as.numeric(TeamData$`Posttest Score Attempt 1 `)

View(TeamData)

# Elia Q1. Which regions required the most remediation?
# Keep only necessary columns (Study Title, Pretest Score, Use Regional Office variables)
library("dplyr")
TeamDataek = select(TeamData, 4, 5, 24)

# Clean data, remove NAs and standardize Regional Office names
library("stringr")
TeamDataek<-na.omit(TeamDataek)
TeamDataek$`Pretest Score` <- as.numeric(TeamDataek$`Pretest Score`)
TeamDataek$'Use Regional Office '[TeamDataek$'Use Regional Office' == 'Denver/Cheyenne'] = 'Cheyenne/Denver'
TeamDataek$'Use Regional Office '[TeamDataek$'Use Regional Office' == 'Other (AMO)'] = 'AMO'
TeamDataek$'Use Regional Office '[TeamDataek$'Use Regional Office' == 'Other (VACO)'] = 'VACO'
TeamDataek$'Use Regional Office '[TeamDataek$'Use Regional Office' == 'Washington'] = 'Washington DC'
TeamDataek$`Use Regional Office ` <- as.factor(TeamDataek$`Use Regional Office `)

# Create categorical "Failed" column
TeamDataek$Failed <- ifelse(TeamDataek[,2]<100,"1","0")
View(TeamDataek)

# Calculate percentage of participants that failed (scored less than 100%) by Regional Office
TeamDataek$Failed <- as.numeric(TeamDataek$Failed)
Remediation<-aggregate(TeamDataek$'Failed' ~ TeamDataek$'Use Regional Office ', FUN = mean)

# Create new df and rename columns
TeamDataekQ1 <- data.frame(Remediation)
colnames(TeamDataekQ1) <- c("RO","PCTRemediation")
View(TeamDataekQ1)

# Create plot for Remediation
library("ggplot2")
TeamDataekQ1_bar <- ggplot(TeamDataekQ1,aes(x=reorder(RO,-PCTRemediation),y=PCTRemediation,fill=-PCTRemediation))
TeamDataekQ1_bar <- TeamDataekQ1_bar + geom_col()
TeamDataekQ1_bar <- TeamDataekQ1_bar + ggtitle("PCT Remediation by Regional Office")+ theme (axis.text.x=element_text(angle=90,hjust=1))
TeamDataekQ1_bar <- TeamDataekQ1_bar + theme (plot.title=element_text(hjust=0.5))+xlab("Regional Office")+ylab("Percent Remediation")
TeamDataekQ1_bar
