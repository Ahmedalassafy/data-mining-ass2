source("utility.r")


classification_tree <- function(train.dtm,labels,test.dtm,true_label){
  
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
  
  print("Classification Tree")
  print_confusion(reviews.rpart.pred,true_label)
  
}
  