#all the files and some words (and their normalized)
## frequencies have been placed in one .csv file.
#Also, the rows containing joint authorship files called HM and Jay's files have been removed
#to have an unbiased sample.
###############################################################################################
setwd("C:/Users/gagd2/Desktop/Syracuse/IST_707/Assignments/fedcsvfile")
## Next, load in the .csv
filename="fedPapers85Updated.csv"

##The following will show you that you read in 5 documents
require(tm)

textdataframe <- read.csv(filename, header = TRUE, sep = ",", encoding = "UTF-8")
(textdataframe[1:30])
str(textdataframe)
dim(textdataframe)
(colnames(textdataframe))


library(plyr)
library(dplyr)
#Filter Madison's and Hamilton's files into separate variables to separate as training and test datasets
trainingHamilton = which(textdataframe$author== 'Hamilton')
trainingMadison = which(textdataframe$author == 'Madison')
train1 = textdataframe[trainingHamilton,]
train2 = textdataframe[trainingMadison,]

#remove the first column which is unnecessary
NoColTwoDF_train1 <- train1[-c(1)]
NoColTwoDF_train2 <- train2[-c(1)]
# calculate the frequency of words and sort it by frequency for Hamilton's files
word.freq <- sort(numcolwise(sum)(train_updated), decreasing = T)
(word.freq)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1,
          random.order = F)
# calculate the frequency of words and sort it by frequency for Madison's files
word.freq <- sort(numcolwise(sum)(test_updated), decreasing = T)
(word.freq)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1,
          random.order = F)

#Create training and testing sets where first 10 documents of Hamilton is stored for training and the next 5 for testing
train1_subset = NoColTwoDF_train1[1:10,]
test1_subset = NoColTwoDF_train1[11:15,]
#train_1 = sample_n(train_1, 10)

#Create training and testing sets where first 10 documents of Madison is stored for training and the next 5 for testing

#train2 = sample_n(train_2, 10)
train2_subset = NoColTwoDF_train2[1:10,]
test2_subset = NoColTwoDF_train2[11:15,]

train_updated = rbind(train1_subset, train2_subset)
View(train_updated)

test_updated = rbind(test1_subset, test2_subset)
View(test_updated)



library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
## Create the decision tree using rpart
fit <- rpart(train_updated$author ~ ., data = train_updated, method="class", control=rpart.control(minsplit=2,cp=0))
summary(fit)
plot(fit)
text(fit)
#predict the test dataset using the model created
predicted= predict(fit, test_updated, type="class")

#plot the decision tree
fancyRpartPlot(fit)

#confusion matrix to find correct and incorrect predictions
table(Authorship=predicted, true=test_updated$author)


## Save the Decision Tree as a jpg image
jpeg("DecisionTree_Authorship.jpg")

#creating another decision tree with selected words
fit2 <- rpart(train_updated$author ~ by+on+at+may+there+any+have, data = train_updated, method="class", 
              control=rpart.control(minsplit=2,cp=0))
summary(fit2)
#predict the test dataset using the model created
predicted2= predict(fit2, test_updated, type="class")
#plot the decision tree
fancyRpartPlot(fit2)

#confusion matrix to find correct and incorrect predictions
table(Authorship=predicted2, true=test_updated$author)
## Save the Decision Tree as a jpg image
jpeg("DecisionTree_Authorship_prediction2.jpg")

#creating a third model with different set of selected words
fit3 <- rpart(train_updated$author ~ the+of+to+and+be+a, data = train_updated, method="class", 
              control=rpart.control(minsplit=2,cp=0))
summary(fit3)
predicted3= predict(fit3, test_updated, type="class")
fancyRpartPlot(fit3)
table(Authorship=predicted3, true=test_updated$author)

## Save the Decision Tree as a jpg image
jpeg("DecisionTree_Authorship_prediction3.jpg")

#Now let's apply the decision tree model created on the original test set that contains the disputed papers

testingdispt = which(textdataframe$author == 'dispt')
test = textdataframe[testingdispt,]
test <- test[-c(1)]
View(test)

## Create the decision tree using rpart
fit_final <- rpart(train_updated$author ~ ., data = train_updated, method="class")
summary(fit_final)
predicted_final= predict(fit_final, test, type="class")

fancyRpartPlot(fit_final)

table(Authorship=predicted_final, true=test$author)


## Save the Decision Tree as a jpg image
jpeg("DecisionTree_Authorship_final.jpg")


library(CORElearn)
Method.CORElearn <- CORElearn::attrEval(train_updated$author ~ ., data=train_updated,  estimator = "InfGain")
(Method.CORElearn)

Method.CORElearn3 <- CORElearn::attrEval(train_updated$author ~ ., data=train_updated,  estimator = "Gini")
(Method.CORElearn3) 

Method.CORElearn2 <- CORElearn::attrEval(train_updated$author ~ ., data=train_updated,  estimator = "GainRatio")
(Method.CORElearn2)


