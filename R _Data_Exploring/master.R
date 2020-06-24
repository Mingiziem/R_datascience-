
# Using the data from the spam email data file and using logistic regresion, create a predictive model to know if an email is spam or not. Use all the variables. (What are the significant variables and which is their order of importance?)

# Step 0: Importing libraries:

library(caTools)
library(questionr)
library(car)
library(corrplot)

# Step 1: import the data and read the documentation 

spam <- read.csv("/cloud/project/spam7.csv")

# Step 2: Exploratory analysis

summary(spam)
sum(is.na(spam))
str(spam)

# Step 3: Rename "crl.tot"

names(spam)[names(spam)=="crl.tot"]<-"lencap"

# Step 4: Splitting the data into train / test

split <- sample.split(spam, SplitRatio = 0.8)
train<- subset(spam, split == "TRUE")
test<-subset(spam, split == "FALSE")

# Step 5: Training the model. yesno is the dependant variable and the others are the independant. We need to recode yesno, redo the train / test and then run the model. 

spam$yesno <-recode(spam$yesno, "'y'=1; 'n'=0") 
split <- sample.split(spam, SplitRatio = 0.8)
train<- subset(spam, split == "TRUE")
test<-subset(spam, split == "FALSE")

mymodel <- glm(yesno ~ lencap+dollar+bang+money+n000+make, data = train, family = "binomial")
summary(mymodel)

# Comment: In this case we can see the following order based on the results. Dollar would be the strongest variable followed by n000, bang, money and crl.tot. The one with less significance would be make variable.


# Step 6: Running the test data through the model

res <- predict(mymodel, test, type = 'response')

# Step 7: Creating the confusion matrix to validate the model
confmatrix <- table(Actual_value=test$yesno, Predicted_value = res > 0.5)
confmatrix

# Step 8: Calculating the accuracy of our model

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)


# Bonus challenge: We've seen the make column was not significant. Would the accuracy be higher isf we drop it?

#Dropping make from the dataset
spam2 <- spam[,-7]

#Creating test/train from the new dataset, old can not be used
split2 <- sample.split(spam2, SplitRatio = 0.8)
train2<- subset(spam2, split == "TRUE")
test2<-subset(spam2, split == "FALSE")

#New model without make, using the new train partition
mymodel2 <- glm(yesno ~ lencap+dollar+bang+money+n000, data = train2, family = "binomial")
summary(mymodel2)
#New prediction with the new test data
res2 <- predict(mymodel2, test2, type = 'response')
#New confidence matrix 
confmatrix2 <- table(Actual_value=test2$yesno, Predicted_value = res2 > 0.5)
confmatrix2

# Calculating the accuracy of our (new) model

(confmatrix2[[1,1]] + confmatrix2[[2,2]]) / sum(confmatrix2)
