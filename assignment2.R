# load the tm packages
library(tm)
library(glmnet)
library(entropy)
library(rpart)
library(rpart.plot)
source("mnb.r")
source("utility.r")
source("logistic_regression.r")
source("classification_tree.r")



train.dtm <- read_training_data()
labels <- read_training_data1()
test.dtm <- read_testing_data(train.dtm)
x <- rep (1, 80)
y <- rep (0,80)
true_label<- c(x,y)

#run naive_bayes
naive_bayes(train.dtm,labels,test.dtm,true_label)


#logistic regression
logistic_regression(train.dtm,labels,test.dtm,true_label)

#classification tree
classification_tree(train.dtm,labels,test.dtm,true_label)