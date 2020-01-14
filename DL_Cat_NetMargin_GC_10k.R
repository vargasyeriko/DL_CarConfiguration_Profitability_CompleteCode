library(keras);library(tensorflow)
library(tidyverse);library(reticulate);library(reshape)

#*************************Fitting a Deep Learning Model***************************************
#Training Process

#define entries as data.X and data.Y is the diacotomous if NM > 18000
data.X <- train_GC[ -c(1:40,149:150) ]
data.Y <- train_GC[-c(1:148)]

#Make both data frames to numeric matrices
x_data = data.matrix(data.X,  rownames.force = NA)
y_data = data.matrix(data.Y, rownames.force = NA)

head(x_data)
head(y_data)

#when you do the next one, two columns; 1st represents ydata =0 and 2nd represents ydata=1. 
#if order of new matrix is "0" "1" = ydata = 1, & "1" "0" = ydata = 0.'

#+++++++++++++++++++++++++
#use_condaenv(py36,conda="/Users/yerikovargas/anaconda3/envs/py36",required = FALSE)
#use_condaenv(condaenv = "py36", conda = "/Users/yerikovargas/anaconda3/envs/py36")
#+++++++++++++++++++++++++'

y_data_oneh=to_categorical(y_data, num_classes =2)
head(y_data_oneh)

#Define model Architecture
model = keras_model_sequential() %>%   
  layer_dense(units = 24, activation = "relu", input_shape = ncol(x_data)) %>%
  layer_dropout(0.1) %>%
  layer_dense(units = ncol(y_data_oneh), activation = "sigmoid")

#Define model Learning process
#Now it's time to define the loss and optimizer functions, and the metric to optimize.
#sgd= optimizer_sgd(lr=0.09  )
compile(model, loss = "binary_crossentropy", optimizer = optimizer_adam(lr=.001,), 
        metrics = "binary_accuracy")

#Initialize learning processs specify number of iterations, batch size etc. 
history = fit(model,  x_data, y_data_oneh, epochs = 10, batch_size = 64, validation_split = 0.2)

#plot the metrics by epoch
plot(history)

#Validating with unseen data, took it from another random sample defined as test_GC
x_test <- test_GC[ -c(1:40,149:150) ]
x_data_test = data.matrix(x_test,  rownames.force = NA)
head(x_data_test)
dim(x_data_test)

#predict using those entrances form test_GC, function to predict classes
y_data_pred=predict_classes(model, x_data_test)
glimpse(y_data_pred)

#Please note that the dimension is 3000 rows and 2 columns. 
#predict_classes automatically does the one-hot decoding. thus...
y_data_pred_oneh=predict(model, x_data_test)
dim(y_data_pred_oneh)
head(y_data_pred_oneh)

#Evaluating the model (Training vs. Test)
#Creating the "real" y target to compare against the predicted:
y_test <- test_GC[-c(1:148)]
head(y_test)

y_data_real = data.matrix(y_test,  rownames.force = NA)
y_data_real_oneh=to_categorical(y_data_real)
head(y_data_real_oneh)

#Accuracy based on data set that was trained with
evaluate(model, x_data, y_data_oneh, verbose = 0)

#Accuracy based on 10% of the data, sat approx 25 000 cars for evaluating accuracy & loss
evaluate(model, x_data_test, y_data_real_oneh, verbose = 0)


#+++++++++++++++++++++++++++++++**********************+++++++++++++++++++++++++++++++++++
#library(reticulate)
#use_condaenv(py36,conda="auto",required = FALSE)
#++++++++++++++++++++++++++++++++**********************+++++++++++++++++++++++++++++++++++

##Results Representation are the table of missclasification
#if predicted column is the same keep.

predxtst <- y_data_pred
actuallvls <- y_test
df <- data.frame(predxtst, actuallvls)
head(df)

#To analize bring predictors from xtest back to data set test_GC
#then if prediction matches the actual label set a "good" column for data
#all 1s representing entries that were correctly classified.
test_GC$pred <-df$predxtst
test_GC$good = ifelse(df$predxtst == df$d.nm, 1, 0)

table(test_GC$good)

#now split correct classified entries from test data and missclassified into two DS and chopp
#off test_GC such that just 108 classificaion are included there
test_GC_0 <- filter(test_GC[ -c(1:40)], good == 0)
test_GC_1 <- filter(test_GC[ -c(1:40)], good == 1)

#Dimension of data
dim(test_GC_0)
dim(test_GC_1)

#cases where prediction and label are the same for testing data
##Change entries from factors to actual numbers to sum later on
test_GC_1[] <- lapply(test_GC_1, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(test_GC_1, class)
##Cases where prediction and actual labels are different.
test_GC_0[] <- lapply(test_GC_0, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(test_GC_0, class)

#from test_GC_1 we can see all the right classifications and we can see in terms of
#configurations which are prompting the different intervals of net margin

result_T <- aggregate(test_GC_1[-c(109:112)] , 
                              by=list(diach=test_GC_1$d.nm), 
                              FUN=sum )
result_F <- aggregate(test_GC_0[-c(109:112)] , 
                      by=list(diach=test_GC_0$d.nm), 
                      FUN=sum )

#dt2 <- merge(result_F,result_T, by = "diach")
#Reshape data
result_T <- melt(result_T, id=c("diach"))
result_F <- melt(result_F, id =c("diach"))
#new data set with 4 columns representing  0 1 (correct classification) & 0 1 (missclasification)
df1 <- spread(result_T, diach, value  )
df2 <- spread(result_F, diach, value )
dd = cbind(df1, df2)


names(dd)[2] <- "under_correct"
names(dd)[3] <- "over_correct"
names(dd)[5] <- "under_missc"
names(dd)[6] <- "over_missc"
names(dd)

Pcodes <- dd[c(2:6)]

row.names(Pcodes) <- Pcodes$variable

##first
Pcodes$denum <-Pcodes$under_correct +Pcodes$over_correct +Pcodes$under_missc +Pcodes$over_missc
Pcodes$num <-Pcodes$under_correct +Pcodes$over_correct
#"is not accuracy now is going to be prob of under"
Pcodes$probunder <- Pcodes$under_correct/(Pcodes$under_correct +Pcodes$under_missc )
Pcodes$probover <- Pcodes$over_correct/(Pcodes$over_correct + Pcodes$over_missc)
Pcodes$falsepositive <- Pcodes$under_missc/Pcodes$under_correct
#Conditional Under given Over
Pcodes$conditionalUnder <- Pcodes$falsepositive*Pcodes$probunder/Pcodes$probover
names(Pcodes)

#export csv file; prob codes missclassified and classified with Accuracies 

#write.table(Pcodes, file = "/Users/yerikovargas/Documents/FCA/Pcodes_10k.csv",
#            sep = ",", row.names=FALSE)

########################end---------------------------------------**********************************
########################end---------------------------------------**********************************

