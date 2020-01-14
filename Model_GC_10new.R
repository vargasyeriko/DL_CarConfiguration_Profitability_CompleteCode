#Model 10k fo sure

#*******************************Building alternative model 
#From the table of the classification count of individual codes,
#being correctly classified or not. get probability of under from each code
#Start triming data for model by eliminating the cases with probunder is 1.
#Get insights of differences that the same car is sold differently with same codes
#then add covariates

#Remove NAN
dfcodes <- Pcodes[complete.cases(Pcodes), ]

#modelcode =1 means probunder is not worth to view as of now, most of probunder =1
#were classified into grater than 10000
a_dfcodes<- dfcodes[!dfcodes$probunder == "1", ]

#filter codes where #codes > .8#totalcodes :attained with these 15 codes
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


#Make new variables to predict

a_general$ERC<- ((a_general$cat_ERC))
a_general$X9<- ((a_general$`cat_-X9`))
a_general$DFL<- ((a_general$cat_DFL))
a_general$DFT<- ((a_general$cat_DFT))
a_general$XAN<- ((a_general$cat_XAN))
a_general$GWA<- ((a_general$cat_GWA))
a_general$CJ<- ((a_general$cat_CJ))
a_general$RA4<- ((a_general$cat_RA4))
a_general$UAQ<- ((a_general$cat_UAQ))
a_general$DL<- ((a_general$cat_DL))
a_general$RC3<- ((a_general$cat_RC3))
a_general$XL<- ((a_general$`cat_-XL`))
a_general$RH1<- ((a_general$cat_RH1))
a_general$YL<- ((a_general$cat_YL))

#Attacch classes prediction 
a_general$pred_classes <- predict_classes(model, x_pred)

#Merge results to the complete data set
a_newset <- merge( gc[,-c(41:148)], a_general, by ="cat")

#Fit the model
#To fit a multivariate logistic
model_1 = glm(as.numeric(d.nm) ~ ERC+X9+DFL+DFT+XAN+GWA+CJ+RA4+UAQ+DL+RC3+XL+RH1+YL,
              data=a_newset,
              family = binomial(link="logit"),
              na.action(na.omit))

summary(model_1)

##good

#estimates
estimates_10k <- c(   3.59666,-1.37495,-1.68954,-1.71684,-1.63197,0.51654    ,0.01684, 0.60774,-0.20056 
     ,-0.07153, 1.24651 , 0.50098    ,-1.62194    ,1.03608   , 0.96042)

#Ratio of var used
a_newset$ration_var <- a_newset$numvar_used/a_newset$total_numvar

#New Predictors :parameters*newsetcodes(which are already multiplied by ratio and cond prob)
a_newset$interModel <- estimates_10k[1]
a_newset$cat_ERCa <- a_newset$ERC*estimates_10k[2]
a_newset$cat_X9a <- a_newset$X9*estimates_10k[3]
a_newset$cat_DFLa <- a_newset$DFL*estimates_10k[4]
a_newset$cat_DFTa <- a_newset$DFT*estimates_10k[5]
a_newset$cat_XANa <- a_newset$XAN*estimates_10k[6]
a_newset$cat_GWAa <- a_newset$GWA*estimates_10k[7]
a_newset$cat_CJa <- a_newset$CJ*estimates_10k[8]
a_newset$cat_RA4a <- a_newset$RA4*estimates_10k[9]
a_newset$cat_UAQa <- a_newset$UAQ*estimates_10k[10]
a_newset$cat_DLa <- a_newset$DL*estimates_10k[11]
a_newset$cat_RC3a <- a_newset$RC3*estimates_10k[12]
a_newset$cat_XLa <- a_newset$XL*estimates_10k[13]
a_newset$cat_RH1a <- a_newset$RH1*estimates_10k[14]
a_newset$cat_YLa <- a_newset$YL*estimates_10k[15]

#Predict from model 
#change name of var here to clean up
a_newset$percent_under_model <- 1/(1+exp(rowSums(a_newset[79:93]))) 

#Diach by model
a_newset$diachmodel <- ifelse(a_newset$percent_under_model > .5, "U", "O")

#Diach by data
a_newset$diachdta <- ifelse(a_newset$NetMargin > 10000, "1", "0")

#using the reduced model, predicitng with the entire data set it was able to achieve .705025
table(a_newset$diachmodel,a_newset$diachdta)

#Using the neural netwok model it was able to achieve 0.7427293
table(a_newset$pred_classes, a_newset$d.nm)




