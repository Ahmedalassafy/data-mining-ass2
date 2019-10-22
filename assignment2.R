# load the tm packages
library(tm)
library(glmnet)
library(entropy)
library(rpart)
library(rpart.plot)
source("mnb.r")
source("utility.r")



train.dtm <- read_training_data()
labels <- read_training_data1()
test.dtm <- read_testing_data(train.dtm)

#run naive_bayes
naive_bayes(train.dtm,labels,test.dtm)


############################# logistic regression with lasso penalty#########

reviews.glmnet <- cv.glmnet(as.matrix(train.dtm),labels, family="binomial",type.measure="class")

reviews.glmnet

plot(reviews.glmnet)

coef(reviews.glmnet,s="lambda.1se")


# make predictions on the test set
reviews.logreg.pred <- predict(reviews.glmnet, newx=as.matrix(test.dtm),s="lambda.1se",type="class")
reviews.logreg.pred
# show confusion matrix
n <- rep (1, 80)
m <- rep (0,80)
true_label_2<- c(n,m)

confusion_matrix_2 <- table(reviews.logreg.pred,true_label_2)


print("confusion matrix")
print(confusion_matrix_2)

print("accuracy")
print((confusion_matrix_2[1,1]+confusion_matrix_2[2,2])/160)

print("precision")
print(confusion_matrix_2[1,1]/(confusion_matrix_2[1,1]+confusion_matrix_2[1,2]))

print("recall")
print(confusion_matrix_2[1,1]/(confusion_matrix_2[1,1]+confusion_matrix_2[2,1]))


########################################classification tree#########

data.predictors <- as.matrix(train.dtm)

training.data <- cbind.data.frame(data.predictors, labels)

reviews.rpart <- rpart(labels~. ,data = training.data,cp=0, method = "class")

rpart.plot(reviews.rpart, type = 4,extra = 101)


# simple tree for plotting
reviews.rpart.pruned <- prune(reviews.rpart,cp=1.37e-02)

rpart.plot(reviews.rpart.pruned)

# tree with lowest cv error
reviews.rpart.pruned <- prune(reviews.rpart,cp=0.001)
# make predictions on the test set

out_df <- data.frame(as.matrix(test.dtm), check.names = FALSE, stringsAsFactors = FALSE)

reviews.rpart.pred <- predict(reviews.rpart.pruned, newdata = out_df,type="class")

#show confusion matrix
table(reviews.rpart.pred,true_label)



