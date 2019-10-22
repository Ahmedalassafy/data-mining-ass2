source("utility.r")
train.mnb <- function (train.dtm,labels)
{
  dtm <- train.dtm
  call <- match.call()
  V <- ncol(dtm)
  N <- nrow(dtm)
  prior <- table(labels)/N
  print(prior)
  labelnames <- names(prior)
  nclass <- length(prior)
  cond.probs <- matrix(nrow=V,ncol=nclass)
  dimnames(cond.probs)[[1]] <- dimnames(dtm)[[2]]
  dimnames(cond.probs)[[2]] <- labelnames
  index <- list(length=nclass)
  for(j in 1:nclass){
    index[[j]] <- c(1:N)[labels == labelnames[j]]
  }
  
  for(i in 1:V){
    for(j in 1:nclass){
      cond.probs[i,j] <- (sum(dtm[index[[j]],i])+1)/(sum(dtm[index[[j]],])+V)
    }
  }
  list(call=call,prior=prior,cond.probs=cond.probs)
}



predict.mnb <-
  function (model,dtm)
  {
    classlabels <- dimnames(model$cond.probs)[[2]]
    logprobs <- dtm %*% log(model$cond.probs)
    N <- nrow(dtm)
    nclass <- ncol(model$cond.probs)
    logprobs <- logprobs+matrix(nrow=N,ncol=nclass,log(model$prior),byrow=T)
    classlabels[max.col(logprobs)]
  }



naive_bayes<-function(train.dtm,labels,test.dtm){
  
  # NAIVE BAYES
  reviews.mnb<- train.mnb(train.dtm, labels)
  reviews.mnb.pred <-  predict.mnb(reviews.mnb,as.matrix(test.dtm))
  
  x <- rep (1, 80)
  y <- rep (0,80)
  true_label<- c(x,y)
  
  print("Naive Bayes")
  
  print_confusion(reviews.mnb.pred,true_label)
  
  
  ############################# Computing Mutual Information
  ######################### Using the top-50 features
  
  
  # compute mutual information of each term with class label
  train.mi <- apply(as.matrix(train.dtm),2, function(x,y){mi.plugin(table(x,y)/length(y))},labels)
  
  # sort the indices from high to low mutual information
  train.mi.order <- order(train.mi,decreasing=T)
  
  # train on the 50 best features
  revs.mnb.top50 <- train.mnb(as.matrix(train.dtm)[,train.mi.order[1:50]], labels)
  
  # predict on the test set
  revs.mnb.top50.pred <- predict.mnb(revs.mnb.top50, as.matrix(test.dtm)[,train.mi.order[1:50]])
  
  print("Naive Bayes with 50 features")
  
  print_confusion(revs.mnb.top50.pred,true_label)
  
  #END NAIVE BAYES
  
}

