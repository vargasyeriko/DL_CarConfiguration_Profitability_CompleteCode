#Calling Packages
library(xlsx);library(rJava);library(ggplot2)
library(summarytools);library(dplyr);library(tidyr)
library(splitstackshape);library(plyr)

#import data set
wk <- read.csv("/Users/yerikovargas/Documents/FCA/SM.csv")

#names and levels 
names(wk)

#Types of variables and levels #str(wk)
#Histogram Net Margin #hist(wk$NetMargin)

#Expanding categories  (108 levels)
wk$cat <- gsub("\\*","", wk$Configuration)

gc <- cSplit_e(data = wk, split.col = "cat", sep = ",", mode = "binary",
               type = "character", fill = "0", drop = FALSE)  

#for >10,000
gc$d.nm <- ifelse(wk$NetMargin > 10000, "1", "0")

#for >12,000
#gc$d.nm <- ifelse(wk$NetMargin > 12000, "1", "0")

table(gc$d.nm)

## 75% of the sample size
smp_size <- floor(1.0* nrow(gc))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(gc)), size = smp_size)
train_GC <- gc#[train_ind, ]
test_GC <- gc[-train_ind, ]
names(train_GC)

#For building the model purposes********Atention:
                   test_GC <- gc

########################end---------------------------------------**********************************
########################end---------------------------------------**********************************

##More Analysis...
#expaning categories of Trim Level (11 levels)

gc <- cSplit_e(data = gc, split.col = "TrimLevel", sep = ",", mode = "binary",
               type = "character", fill = "0",  drop = FALSE )

#expaning categories of year model (6 levels)

gc <- cSplit_e( data = gc, split.col = "ModelYear", sep = ",", mode = "binary", 
                type = "character", fill = "0", drop = FALSE)

## preparin variable days to sell median = 87
gc$days <- ifelse(wk$DaysToSell > 120 ,2, ifelse(wk$DaysToSell >= 30, 1,0 ))
#table(gc$days)


#Expanding categories of Days to sell

gc <- cSplit_e( data = gc, split.col = "days", sep = ",", mode = "binary",
                type = "character", fill = "0", drop = TRUE)

#diachotomous variable for ******************************2*******************
gc$d.nm <- ifelse(wk$NetMargin > 15000, "1", "0")
table(gc$d.nm)

#diachotomous variable for ****************************3********************
#gc$d.nm <- ifelse(wk$NetMargin > 16000, 2, ifelse(wk$NetMargin >= 4000, 1,0 ))
#table(gc$d.nm)
#table(gc$d.nm, gc$TL)

#Prepare Data
## 75% of the sample size
smp_size <- floor(0.65 * nrow(gc))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(gc)), size = smp_size)

train_GC <- gc[train_ind, ]
test_GC <- gc[-train_ind, ]


########################
#random samples

#rnamesGC <- row.names(gc)

#set.seed(464)
#samprows.GC<- sample(rnamesGC, 3000, replace = FALSE)
#train_GC<- subset(gc, rnamesGC%in%samprows.GC)

#set.seed(465)
#samprows.GC1<- sample(rnamesGC, 3000, replace = FALSE)
#test_GC<- subset(gc, rnamesGC%in%samprows.GC1)


#export csv file
#write.table(gc, file = "/Users/yerikovargas/Documents/FCA/FCAGC.csv",
#            sep = ",", row.names=FALSE)


#Diagnostic of the main variable

#hist(wk$NetMargin) 
#mean(wk$NetMargin)

#graphs

#ggplot(data=wk, aes(x=Configuration, y=)) +  geom_bar(stat="identity")

#ggplot(data=wk, aes(x=State, y=Incentives)) +  geom_bar(stat="identity")
#ggplot(data=wk, aes(x=State, y=ModelYear)) +  geom_bar(stat="identity")

#ggplot(data=wk, aes(x=State, y=DaysToSell)) +  geom_bar(stat="identity")
#freq

table(gc$d.nm4, gc$TrimLevel)
table(gc$d.nm4, gc$PaintProcess)
table(gc$d.nm4, gc$EngineCode)

#table(wk$EngineCode, wk$State)
#table(wk$w, wk$TrimLevel)

#histograms
#hist(wk$PaintCode)