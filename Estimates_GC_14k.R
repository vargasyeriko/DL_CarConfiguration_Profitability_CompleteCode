#fit model to see the prediction with nm > 14000

estimModel_14 <- c(  1.39192   ,
                           195.08273   ,
                             -44.78060  , 
                           -351.08287   ,
                           -429.63285  ,
                              3.38101   , 
                            -26.10605  ,
                              31.28019  , 
                             -7.22068    ,
                             46.47626  ,
                              20.77810  , 
                           -102.19194    ,
                             -39.37073   ,
                             14.99909    ,
                           -6.82876    )

#This times the ratio var used over var included
estimates_14k<- c(1*estimModel_14[1] , a_dfcodes$conditionalUnder[1]*estimModel_14[2], a_dfcodes$conditionalUnder[2]*estimModel_14[3],
                  a_dfcodes$conditionalUnder[3]*estimModel_14[4], a_dfcodes$conditionalUnder[4]*estimModel_14[5],
                  a_dfcodes$conditionalUnder[5]*estimModel_14[6], a_dfcodes$conditionalUnder[6]*estimModel_14[7],
                  a_dfcodes$conditionalUnder[7]*estimModel_14[8], a_dfcodes$conditionalUnder[8]*estimModel_14[9],
                  a_dfcodes$conditionalUnder[9]*estimModel_14[10], a_dfcodes$conditionalUnder[10]*estimModel_14[11],
                  a_dfcodes$conditionalUnder[11]*estimModel_14[12], a_dfcodes$conditionalUnder[12]*estimModel_14[13],
                  a_dfcodes$conditionalUnder[13]*estimModel_14[14] , a_dfcodes$conditionalUnder[14]*estimModel_14[15])

#Save estimates 10k

#estimates_10k <-c(  1.3919200  , 5.6917148 , -1.1603098, -11.4125931, -10.9393060,   0.2404349 , -0.4298133  , 
               #       0.6247939 , -0.2677228, 1.3836491  , 2.0253027,  -0.8215082  ,-1.1900737 ,  1.8078628,  -0.4387880)

#Note: before building last model including three, try every one if they work 

#Change everything down below
#Ratio of var used
newsetMUL$ration_var <- newsetMUL$numvar_used/newsetMUL$total_numvar

#New Predictors
newsetMUL$interModel <- estimModel[1]
newsetMUL$cat_ERCa <- newsetMUL$ERC*estimModel[2]
newsetMUL$cat_X9a <- newsetMUL$X9*estimModel[3]
newsetMUL$cat_DFLa <- newsetMUL$DFL*estimModel[4]
newsetMUL$cat_DFTa <- newsetMUL$DFT*estimModel[5]
newsetMUL$cat_XANa <- newsetMUL$XAN*estimModel[6]
newsetMUL$cat_GWAa <- newsetMUL$GWA*estimModel[7]
newsetMUL$cat_RCAa <- newsetMUL$RCA*estimModel[8]
newsetMUL$cat_RA4a <- newsetMUL$RA4*estimModel[9]
newsetMUL$cat_UAQa <- newsetMUL$UAQ*estimModel[10]
newsetMUL$cat_GLa <- newsetMUL$GL*estimModel[11]
newsetMUL$cat_RC3a <- newsetMUL$RC3*estimModel[12]
newsetMUL$cat_XLa <- newsetMUL$XL*estimModel[13]
newsetMUL$cat_HLa <- newsetMUL$HL*estimModel[14]
newsetMUL$cat_YLa <- newsetMUL$YL*estimModel[15]

#Predict from model
newsetMUL$percent_under_model <- 1/(1+exp(rowSums(newsetMUL[79:93]))) 

#Diach by model
newsetMUL$diachmodel <- ifelse(newsetMUL$percent_under_model > .5, "0", "1")

#Diach by data
newsetMUL$diachdta <- ifelse(newsetMUL$NetMargin > 14000, "1", "0")

#using the reduced model, predicitng with the entire data set it was able to achieve 
table(newsetMUL$diachmodel,newsetMUL$diachdta)

#Using the neural netwok model it was able to achieve
table(newsetMUL$pred_classes, newsetMUL$d.nm)

#^*^Conclusion: reduced model does very good, only thing is that from the model one has to multiply by cond prob
#those are the params and to predict the only thing you have to multiply for are the weights or var used

#Purpose is to do three models where one can see in a more accurate way where would the car be located 

#model prob of under cut off point: carx --> model10k = .8  model14k = .7 model16k = .45 

#Last comment to add is that for models greater than 10k or less it will def be more accurate,
#**************Should I try to choose same variables among models, change some or how?

########################end---------------------------------------**********************************
########################end---------------------------------------**********************************




##Pred from model
trythis <- gc[c(41:148)]

x_p = data.matrix(trythis,  rownames.force = NA)
trythis$probs_under <- predict(model, x_p)

#predict classes
trythis$probs_classes <- predict_classes(model, x_p)
trythis$diach <- gc$d.nm
table(trythis$probs_classes, trythis$diach)

er$w <- er$numvar_used/er$total_numvar

er$int <- (er$numvar_used/er$numvar_used)*2.0862
er$cat_ERC.y <- (er$cat_ERC.y*erc)

er$sumestimates <- rowSums(a_predta[2:109])

