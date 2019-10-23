source("utility.r")

logistic_regression <- function(train.dtm,labels,test.dtm,true_label){
  
  #########Application of Logistic Regression to hotel Reviews
  
  # logistic regression with lasso penalty#########
  reviews.glmnet <- cv.glmnet(as.matrix(train.dtm),labels, family="binomial",type.measure="class")
  plot(reviews.glmnet)
  
  # coef(reviews.glmnet,s="lambda.1se")
  
  
  print("Logisitc Reggression")
  
  # make predictions on the test set
  reviews.logreg.pred <- predict(reviews.glmnet, newx=as.matrix(test.dtm),s="lambda.1se",type="class")
  
  # show confusion matrix
  
  print_confusion(reviews.logreg.pred,true_label)
  
}