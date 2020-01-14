#Calling Packages
library(xlsx);library(rJava);library(ggplot2)
library(summarytools);library(dplyr);library(tidyr)
library(splitstackshape);library(plyr)

wk <- read.csv("/Users/yerikovargas/Documents/FCA/SM.csv")
wk$cat <- gsub("\\*","", wk$Configuration)
gcf <- cSplit_e(data = wk, split.col = "cat", sep = ",", mode = "binary",
                type = "character", fill = "0", drop = FALSE)  
#By category:

aa_cat <- aggregate(wk$NetMargin, 
                    by=list(cat=wk$cat), 
                    FUN=mean )

ab_cat <- aggregate(wk$cat, 
                    by=list(cat=wk$cat), 
                    FUN=function(x){NROW(x)})

a_cat <- merge(aa_cat, ab_cat, by = "cat")

names(a_cat)[2] <- "mean_NM"
names(a_cat)[3] <- "number_percat"


a_cat <- cSplit_e(data = aa_cat, split.col = "cat", sep = ",", mode = "binary",
                  type = "character", fill = "0", drop = FALSE) 