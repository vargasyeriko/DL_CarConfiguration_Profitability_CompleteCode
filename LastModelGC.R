#Calling Packages
library(xlsx);library(rJava);library(ggplot2)
library(summarytools);library(dplyr);library(tidyr)
library(splitstackshape);library(plyr)
#Building last 3 way model
#Estimates for model idach for 10, 14 and 17 k, modeling prob of being 0, under diach

estimates_10k <-c(  2.45024000, -0.74834774, -1.55764556, -3.04363254, -2.74478243,  0.70161783,  0.27940163, 
                   1.45574156,  0.09729978, 0.10723379,  1.86989528,  0.97747723, -1.51054246,  1.53469312,  1.88640632)

estimates_14k <-c(  1.3919200  , 5.6917148 , -1.1603098, -11.4125931, -10.9393060,   0.2404349 , -0.4298133  , 
                    0.6247939 , -0.2677228, 1.3836491  , 2.0253027,  -0.8215082  ,-1.1900737 ,  1.8078628,  -0.4387880)

estimates_17k <- c(0.9369000 , 12.4556707,  -1.9225705, -21.8554570, -19.7783483,  -5.5823461,
                   -2.3060188 ,-2.8401573,  -5.1216060,   5.5461249,   2.1709605,  -0.7423876,
                 -1.7715914,   3.0585617, -7.0231388)

#Data set
wk <- read.csv("/Users/yerikovargas/Documents/FCA/SM.csv")
wk$cat <- gsub("\\*","", wk$Configuration)
gcf <- cSplit_e(data = wk, split.col = "cat", sep = ",", mode = "binary",
               type = "character", fill = "0", drop = FALSE)  

#Diach for 10k
gcf$d_10 <- ifelse(wk$NetMargin > 10000, "1", "0")

#Diach for 14k
gcf$d_14 <- ifelse(wk$NetMargin > 14000, "1", "0")

#Diach for 17k
gcf$d_17 <- ifelse(wk$NetMargin > 17000, "1", "0")

#to numeric
gcf[] <- lapply(gcf, function(x) {
  if(is.factor(x)) as.double(as.character(x)) else x})

#Total var used
gcf$total_numvar <- rowSums(gcf[41:148])

include_list <- c("cat", "cat_ERC", "cat_-X9", "cat_DFL", "cat_DFT", "cat_XAN", 
                  "cat_GWA", "cat_CJ", "cat_RA4", "cat_UAQ", "cat_DL", "cat_RC3",
                  "cat_-XL","cat_RH1", "cat_YL","d_10", "d_14", "d_17", "total_numvar", "NetMargin")

a_gcf <- gcf[,include_list ]

a_gcf$partial_numvar <- rowSums(a_gcf[2:15])
a_gcf$ratio_numvar <- a_gcf$total_numvar/a_gcf$partial_numvar

#agregate all by conf
z_gcf <- aggregate(a_gcf$cat, 
                          by=list(cat=a_gcf$cat), 
                          FUN=function(x){NROW(x)})

agc <- merge(a_gcf, z_gcf, by ="cat")

#10k
agc$int_10k <- estimates_10k[1]
agc$ERC_10k <- agc$cat_ERC*estimates_10k[2]*agc$ratio_numvar
agc$X9_10k <- agc$`cat_-X9`*estimates_10k[3]*agc$ratio_numvar
agc$DFL_10k <- agc$cat_DFL*estimates_10k[4]*agc$ratio_numvar
agc$DFT_10k <- agc$cat_DFT*estimates_10k[5]*agc$ratio_numvar
agc$XAN_10k <- agc$cat_XAN*estimates_10k[6]*agc$ratio_numvar
agc$GWA_10k <- agc$cat_GWA*estimates_10k[7]*agc$ratio_numvar
agc$CJ_10k <- agc$cat_CJ*estimates_10k[8]*agc$ratio_numvar
agc$RA4_10k <- agc$cat_RA4*estimates_10k[9]*agc$ratio_numvar
agc$UAQ_10k <- agc$cat_UAQ*estimates_10k[10]*agc$ratio_numvar
agc$DL_10k <- agc$cat_DL*estimates_10k[11]*agc$ratio_numvar
agc$RC3_10k <- agc$cat_RC3*estimates_10k[12]*agc$ratio_numvar
agc$XL_10k <- agc$`cat_-XL`*estimates_10k[13]*agc$ratio_numvar
agc$RH1_10k <- agc$cat_RH1*estimates_10k[14]*agc$ratio_numvar
agc$YL_10k <- agc$cat_YL*estimates_10k[15]*agc$ratio_numvar

#Getting percentages of being 
agc$pred_10k <-  1/(1+exp(rowSums(agc[24:38]))) 

#Classification
agc$class_10k <- ifelse(agc$pred_10k > .5, "0", "1")

table(agc$class_10k, agc$d_10)

#14k
agc$int_14k <- estimates_14k[1]
agc$ERC_14k <- agc$cat_ERC*estimates_14k[2]*agc$ratio_numvar
agc$X9_14k <- agc$`cat_-X9`*estimates_14k[3]*agc$ratio_numvar
agc$DFL_14k <- agc$cat_DFL*estimates_14k[4]*agc$ratio_numvar
agc$DFT_14k <- agc$cat_DFT*estimates_14k[5]*agc$ratio_numvar
agc$XAN_14k <- agc$cat_XAN*estimates_14k[6]*agc$ratio_numvar
agc$GWA_14k <- agc$cat_GWA*estimates_14k[7]*agc$ratio_numvar
agc$CJ_14k <- agc$cat_CJ*estimates_14k[8]*agc$ratio_numvar
agc$RA4_14k <- agc$cat_RA4*estimates_14k[9]*agc$ratio_numvar
agc$UAQ_14k <- agc$cat_UAQ*estimates_14k[10]*agc$ratio_numvar
agc$DL_14k <- agc$cat_DL*estimates_14k[11]*agc$ratio_numvar
agc$RC3_14k <- agc$cat_RC3*estimates_14k[12]*agc$ratio_numvar
agc$XL_14k <- agc$`cat_-XL`*estimates_14k[13]*agc$ratio_numvar
agc$RH1_14k <- agc$cat_RH1*estimates_14k[14]*agc$ratio_numvar
agc$YL_14k <- agc$cat_YL*estimates_14k[15]*agc$ratio_numvar

#Getting percentages of being 
agc$pred_14k <-  1/(1+exp(rowSums(agc[41:55]))) 

#Classification
agc$class_14k <- ifelse(agc$pred_14k > .5, "0", "1")

table(agc$class_14k, agc$d_14)

#17k
agc$int_17k <- estimates_17k[1]
agc$ERC_17k <- agc$cat_ERC*estimates_17k[2]*agc$ratio_numvar
agc$X9_17k <- agc$`cat_-X9`*estimates_17k[3]*agc$ratio_numvar
agc$DFL_17k <- agc$cat_DFL*estimates_17k[4]*agc$ratio_numvar
agc$DFT_17k <- agc$cat_DFT*estimates_17k[5]*agc$ratio_numvar
agc$XAN_17k <- agc$cat_XAN*estimates_17k[6]*agc$ratio_numvar
agc$GWA_17k <- agc$cat_GWA*estimates_17k[7]*agc$ratio_numvar
agc$CJ_17k <- agc$cat_CJ*estimates_17k[8]*agc$ratio_numvar
agc$RA4_17k <- agc$cat_RA4*estimates_17k[9]*agc$ratio_numvar
agc$UAQ_17k <- agc$cat_UAQ*estimates_17k[10]*agc$ratio_numvar
agc$DL_17k <- agc$cat_DL*estimates_17k[11]*agc$ratio_numvar
agc$RC3_17k <- agc$cat_RC3*estimates_17k[12]*agc$ratio_numvar
agc$XL_17k <- agc$`cat_-XL`*estimates_17k[13]*agc$ratio_numvar
agc$RH1_17k <- agc$cat_RH1*estimates_17k[14]*agc$ratio_numvar
agc$YL_17k <- agc$cat_YL*estimates_17k[15]*agc$ratio_numvar

#Getting percentages of being 
agc$pred_17k <-  1/(1+exp(rowSums(agc[58:72]))) 

#Classification
agc$class_17k <- ifelse(agc$pred_17k > .5, "0", "1")
table(agc$class_17k, agc$d_17)

#Frame everything
include_list1 <- c("cat","d_10","d_14", "d_17", "NetMargin", "class_10k", "class_14k", "class_17k" ,"x"
                   , "pred_10k", "pred_14k", "pred_17k")

aaa <- agc[,include_list1 ]

aaa_new <- na.omit(aaa) 

aaa_new1 <-aggregate(aaa_new$NetMargin , by=list(cat=wk$cat), FUN= sum) 

#see how many rows are similiar 3X3

############
aaa$dd <- ifelse(aaa$d_10 == , "1", "0")




