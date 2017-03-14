#create separate testProbs for AUC
#Turn off the scientific notation
options(scipen=999)

#run through with simulated data
library(lattice)
library(AppliedPredictiveModeling)
library(randomForest)
library(popbio)
library(ggplot2)
library(MASS)
library(caret)
library(pscl)
library(ROCR)
library(ggmap)
library(e1071)
library(pROC)

#setwd("D:/Penn/2015-2016/Fall/590-GIS/Homework/HW11_Final/Data/For regression")
#con <- readRDS("bus_license.Rds")

#---------------------Train the data on 2013 year---------------------

#run through with food inspection 2013 in Chicago
#name the data frame 'food'

#how many restaurants passed the food inspection?
sum(food13$Pass1) / nrow(food13)#75.6%

#lets create the data we need
#first an xy layer
food13xy <- food13[,c(9,8)]#Logitude and latitude
food13data <- food13[,c(19,14,18,6,15,13,16,17,12,9,8,7)]

#logistic regression
#start with this model
foodModel13 <- glm(Pass1 ~ ., family="binomial"(link="logit"), data = food13data)

summary(foodModel13)

pR2(foodModel13)

## Predict the test set probabilities using the data (ie. there is no test set here)
ClassProbs13 <- predict(foodModel13, food13data, type="response")
#put these into a data frame
testProbs13 <- data.frame(Class = food13data$Pass1,Probs = ClassProbs13)
head(testProbs13)

#plot the distrubtion of predictied probabilities for each binary output.
adminPropsPlot1 <- ggplot(testProbs13, aes(x = Probs, fill=Class)) + geom_density(binwidth = 0.02) +
  facet_grid(Class ~ .) + xlab("Probability") + geom_vline(xintercept = .5)
adminPropsPlot1


#lets create a cutoff threshhold
testProbs13$predClass  = ifelse(testProbs13$Probs > .5 ,1,0)
head(testProbs13)

#how about a confusion matrix - using a command from caret
#by default this uses a .5 threshold
confusionMatrix(testProbs13$Class,testProbs13$predClass)

#RememberL
#  Predicted = 0, Observed = 0    ---> True Negative      
#  Predicted = 1, Observed = 1    ---> True Positive      
#  Predicted = 1, Observed = 0    ---> False Positive     
#  Predicted = 0, Observed = 1    ---> False Negative     

#how about an ROC Curve
pred13 <- prediction( testProbs13$Probs, testProbs13$Class)
perf13 <- performance(pred13,"tpr","fpr")
plot(perf13)
abline(a=0, b= 1)

#lets calculate another goodness of fit metric - the Area Under the Curve or 'AUC'
auc(testProbs13$Class, testProbs13$Probs) 
#this number runs between 0 and 1 - 1 obviously, being highest


#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
#Perform stepwise regression - this as a tool for doing feature selection
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

backwards13 = step(foodModel13)

summary(backwards13)

pR2(backwards13)

#let's just clear the slate
remove(ClassProbs13, testProbs13)

#Predict the test set probabilities
ClassProbs13 <- predict(backwards13, food13data, type="response")
#put these into a data frame
testProbs13 <- data.frame(Class = food13data$Pass1,Probs = ClassProbs13)
head(testProbs13)


#plot the distrubtion of predictied probabilities for each binary output.
adminPropsPlot13 <- ggplot(testProbs13, aes(x = Probs, fill=Class)) + geom_density(binwidth = 0.02) +
  facet_grid(Class ~ .) + xlab("Probability")
adminPropsPlot13

#lets create a threshhold
testProbs13$predClass  = ifelse(testProbs13$Probs > .5 ,1,0)
head(testProbs13)

#how about a confusion matrix - using a command from caret
#by default this uses a .5 threshold
confusionMatrix(testProbs13$Class ,testProbs13$predClass)

#RememberL
#  Predicted = 0, Observed = 0    ---> True Negative      
#  Predicted = 1, Observed = 1    ---> True Positive      
#  Predicted = 1, Observed = 0    ---> False Positive     
#  Predicted = 0, Observed = 1    ---> False Negative     

#how about an ROC Curve
pred13 <- prediction( testProbs13$Probs, testProbs13$Class)
perf13 <- performance(pred13,"tpr","fpr")
plot(perf13)
abline(a=0, b= 1)

#lets calculate 'AUC'
auc(testProbs13$Class, testProbs13$Probs) 


#---------------------Test the data on 2014 year---------------------

#run through with food inspection 2014 in Chicago
#name the data frame 'food'

#how many restaurants passed the food inspection?
sum(food14$Pass) / nrow(food14)

#lets create the data we need
#first an xy layer
food14xy <- food14[,c(7,6)]#Logitude and latitude
food14data <- food14[,c(8,10,12,14:18,7,6)]

#logistic regression

#start with this model
foodModel14 <- glm(Pass ~ ., family="binomial"(link="logit"), data = food14data)

summary(foodModel14)

pR2(foodModel14)#overfitting

## Predict the test set probabilities using the data (ie. there is no test set here)
ClassProbs14 <- predict(foodModel14, food14data, type="response")
#put these into a data frame
testProbs14 <- data.frame(Class = food14data$Pass,Probs = ClassProbs14)
head(testProbs14)

#plot the distrubtion of predictied probabilities for each binary output.
adminPropsPlot14 <- ggplot(testProbs14, aes(x = Probs, fill=Class)) + geom_density(binwidth = 0.02) +
  facet_grid(Class ~ .) + xlab("Probability") + geom_vline(xintercept = .5)
adminPropsPlot14


#lets create a cutoff threshhold
testProbs14$predClass  = ifelse(testProbs14$Probs > .5 ,1,0)
head(testProbs14)

#how about a confusion matrix - using a command from caret
#by default this uses a .5 threshold
confusionMatrix(testProbs14$Class,testProbs14$predClass)

#RememberL
#  Predicted = 0, Observed = 0    ---> True Negative      
#  Predicted = 1, Observed = 1    ---> True Positive      
#  Predicted = 1, Observed = 0    ---> False Positive     
#  Predicted = 0, Observed = 1    ---> False Negative     

#how about an ROC Curve
pred14 <- prediction( testProbs14$Probs, testProbs14$Class)
perf14 <- performance(pred14,"tpr","fpr")
plot(perf14)
abline(a=0, b= 1)

#lets calculate another goodness of fit metric - the Area Under the Curve or 'AUC'
auc(testProbs14$Class, testProbs14$Probs) 
#this number runs between 0 and 1 - 1 obviously, being highest


# lets map these predicted probabilities - so here we're not so much interested in the classification.
#we're interested in understanding the probability the a given block will have some new construction occur.

#First we'll tack on the predicted probabilities from our last model to the original data frame
food14$probs <- testProbs14$Probs 
#next we'll use the Google API to get a base map centered around Philadelphia
baseMap <- get_map(location = c(lon = -87.6847, lat = 41.8369), source = "stamen", zoom = 12, maptype= 'toner')
ggmap(baseMap)

#Next use the longitude and latitude coordinates in the hed data frame to make a map
ggmap(baseMap) + geom_point(aes(x=Longitude, y=Latitude, color=probs, size = .1), data = food14) + 
  scale_color_gradient(low="yellow", high="blue") 

InterpolatedMap <- ggmap(baseMap) %+% food14 + 
  aes(x = Longitude,
      y = Latitude,
      z = probs) +
  stat_summary2d(fun = mean, 
                 binwidth = c(.002, .002),
                 alpha = .5) + 
  scale_fill_gradientn(name = "Probability",
                       colours = rainbow(10),
                       space = "Lab") 

InterpolatedMap

write.csv(food14, file = "D:/Penn/2015-2016/Fall/590-GIS/Homework/HW11_Final/Data/For regression/food2014probs.csv")
