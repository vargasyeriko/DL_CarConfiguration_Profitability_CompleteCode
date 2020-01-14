#*******************************Building alternative model 
#From the table of the classification count of individual codes,
#being correctly classified or not. get probability of under from each code
#Start triming data for model by eliminating the cases with probunder is 1.
#Get insights of differences that the same car is sold differently with same codes
#then add covariates

#Remove NAN
dfcodes <- Pcodes[complete.cases(Pcodes), ]

#modelcode =1 means probunder is not worth to view as of now, most of probunder =1
#were classified into grater than 17000
a_dfcodes<- dfcodes[!dfcodes$probunder == "1", ]

#filter codes where #codes > .8#totalcodes :attained with these 14 codes
#Exclude  KL since is makin fn to be zero
include_list <- c("cat_ERC", "cat_-X9", "cat_DFL", "cat_DFT", "cat_XAN", 
                  "cat_GWA", "cat_CJ", "cat_RA4", "cat_UAQ", "cat_DL", "cat_RC3",
                  "cat_-XL","cat_RH1", "cat_YL")

a_dfcodes <- dfcodes[include_list, ]
head(a_dfcodes)
rownames(a_dfcodes)

##Make predictions on all the possible combinations
####

dtatopredict <- aggregate(gc$Configuration, 
                    by=list(Configuration=gc$Configuration), 
                    FUN=function(x){NROW(x)})

dtatopredict$cat <- gsub("\\*","", dtatopredict$Configuration)
dtatopredict <- dtatopredict[, -c(1)]
dtatopredict <- dtatopredict[ , c(2,1)]

#Sum all the ones from coded diachotomous under and over 10000

number_ones <- aggregate(as.numeric(gc$d.nm), 
                          by=list(Configuration=gc$Configuration), 
                          FUN=sum)

number_ones$cat <- gsub("\\*","", number_ones$Configuration)

a_dta <-merge(dtatopredict, number_ones, by ="cat")

#Compute percentages of every category
names(a_dta)[2] <- "number_cars"
names(a_dta)[4] <- "over_counts"
a_dta$under_counts = a_dta$number_cars - a_dta$over_counts
a_dta$under_dta_percent <- a_dta$under_counts/a_dta$number_cars

#Prepare x values from all cong data set list
a_predta <- cSplit_e(data = dtatopredict, split.col = "cat", sep = ",", mode = "binary",
               type = "character", fill = "0", drop = FALSE)  

#Give predictions and class prediction 
a_predta <- a_predta[,-c(2)]
x_pred = data.matrix(a_predta[,-c(1)],  rownames.force = NA)
a_predta$probs_under <- predict(model, x_pred)

#predict classes
#a_predta$probs_classes <- predict_classes(model, x_pred)

a_predta[] <- lapply(a_predta, function(x) {
  if(is.factor(x)) as.double(as.character(x)) else x})
#Total num of variables in configuration
a_predta$total_numvar <- rowSums(a_predta[2:109])


#exclude codes that are not going to be used
include_list1 <- c("cat","cat_ERC", "cat_-X9", "cat_DFL", "cat_DFT", "cat_XAN", 
                   "cat_GWA", "cat_CJ", "cat_RA4", "cat_UAQ", "cat_DL", "cat_RC3",
                   "cat_-XL","cat_RH1", "cat_YL", "probs_under", "total_numvar")


a_predta <- a_predta[,include_list1 ]
names(a_predta)
#str(a_predta)

##Change to numeric
a_predta[] <- lapply(a_predta, function(x) {
  if(is.factor(x)) as.double(as.character(x)) else x
})

a_predta$numvar_used <- rowSums(a_predta[2:15])
#Eliminate rows for which the count of nvar used is zero

#a_predta  <- a_predta[a_predta$numvar_used != 0, ]

#Important dta set
a_general<- merge(a_dta[-c(3)], a_predta, by = "cat")


a_general$ERC<- ((a_general$cat_ERC)*(a_dfcodes$conditionalUnder[1])/a_general$total_numvar)*a_general$numvar_used
a_general$X9<- ((a_general$`cat_-X9`)*(a_dfcodes$conditionalUnder[2])/a_general$total_numvar)*a_general$numvar_used
a_general$DFL<- ((a_general$cat_DFL)*(a_dfcodes$conditionalUnder[3])/a_general$total_numvar)*a_general$numvar_used
a_general$DFT<- ((a_general$cat_DFT)*(a_dfcodes$conditionalUnder[4])/a_general$total_numvar)*a_general$numvar_used
a_general$XAN<- ((a_general$cat_XAN)*(a_dfcodes$conditionalUnder[5])/a_general$total_numvar)*a_general$numvar_used
a_general$GWA<- ((a_general$cat_GWA)*(a_dfcodes$conditionalUnder[6])/a_general$total_numvar)*a_general$numvar_used
a_general$CJ<- ((a_general$cat_CJ)*(a_dfcodes$conditionalUnder[7])/a_general$total_numvar)*a_general$numvar_used
a_general$RA4<- ((a_general$cat_RA4)*(a_dfcodes$conditionalUnder[8])/a_general$total_numvar)*a_general$numvar_used
a_general$UAQ<- ((a_general$cat_UAQ)*(a_dfcodes$conditionalUnder[9])/a_general$total_numvar)*a_general$numvar_used
a_general$DL<- ((a_general$cat_DL)*(a_dfcodes$conditionalUnder[10])/a_general$total_numvar)*a_general$numvar_used
a_general$RC3<- ((a_general$cat_RC3)*(a_dfcodes$conditionalUnder[11])/a_general$total_numvar)*a_general$numvar_used
a_general$XL<- ((a_general$`cat_-XL`)*(a_dfcodes$conditionalUnder[12])/a_general$total_numvar)*a_general$numvar_used
a_general$RH1<- ((a_general$cat_RH1)*(a_dfcodes$conditionalUnder[13])/a_general$total_numvar)*a_general$numvar_used
a_general$YL<- ((a_general$cat_YL)*(a_dfcodes$conditionalUnder[14])/a_general$total_numvar)*a_general$numvar_used

#Attacch classes prediction 
a_general$pred_classes <- predict_classes(model, x_pred)

#Merge results to the complete data set
newsetMUL <- merge( gc[,-c(41:148)], a_general, by ="cat")

#To fit a multivariate logistic
model_1 = glm(as.numeric(d.nm) ~ ERC+X9+DFL+DFT+XAN+GWA+CJ+RA4+UAQ+DL+RC3+XL+RH1+YL,
                  data=newsetMUL,
                  family = binomial(link="logit"),
                  na.action(na.omit))

summary(model_1)

#THIS TABLE IS FOR SAS
#write.table(newsetMUL[,-c(2:33)], file = "/Users/yerikovargas/Documents/FCA/this_mul.csv",sep = ",", row.names=FALSE)

########################end---------------------------------------**********************************
########################end---------------------------------------**********************************
