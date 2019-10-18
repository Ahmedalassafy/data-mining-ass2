# load the tm package
library(tm)
# Read the training data

reviews.neg.deceptive.path <- "data/negative_polarity/deceptive_from_MTurk/fold1"
reviews.neg.deceptive <- VCorpus(DirSource(reviews.neg.deceptive.path,
                                           encoding="UTF-8"))

reviews.neg.truthful.path <- "data/negative_polarity/truthful_from_Web/fold1"
reviews.neg.truthful <- VCorpus(DirSource(reviews.neg.truthful.path,
                                          encoding="UTF-8"))

# Join negative truthful and deceptive reviews into a single Corpus
reviews.all <- c(reviews.neg.truthful,reviews.neg.deceptive)

# create label vector (1=positive, 0=negative)
labels <- c(rep(1,320),rep(0,320))

# Remove punctuation marks (comma’s, etc.)
reviews.all <- tm_map(reviews.all,removePunctuation)
# Make all letters lower case
reviews.all <- tm_map(reviews.all,content_transformer(tolower))
# Remove stopwords
reviews.all <- tm_map(reviews.all, removeWords, stopwords("english"))
# Remove numbers
reviews.all <- tm_map(reviews.all,removeNumbers)
# Remove excess whitespace / eliminate extra white-spaces
reviews.all <- tm_map(reviews.all,stripWhitespace)


# A document-term matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents. 
train.dtm <- DocumentTermMatrix(reviews.all)

# if we apply dim (train.dtm) we get 640 rows and 6900 features, which means too much so we have applied removeSparseTerms (sparsity refers to the threshold of relative document frequency. and Relative document frequency here means a proportion)/ If we choose 0.5 it will let us to view only the texts that are appearing in 50% of times in the entire element. This is done by calculating after all such per-processing.
train.dtm <- removeSparseTerms(train.dtm,0.95)

# if we apply dim (train.dtm) we get now 640 rows and 307 features
# to view a small part of the document-term matrix by using inspect(train.dtm[100:110,80:85])


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





test.neg.deceptive.path <- "data/negative_polarity/deceptive_from_MTurk/fold5"
test.neg.deceptive <- VCorpus(DirSource(test.neg.deceptive.path,
                                        encoding="UTF-8"))

test.neg.truthful.path <- "data/negative_polarity/truthful_from_Web/fold5"
test.neg.truthful <- VCorpus(DirSource(test.neg.truthful.path,
                                       encoding="UTF-8"))

# Join negative truthful and deceptive reviews into a single Corpus
test.all <- c(test.neg.truthful,test.neg.deceptive)


# Remove punctuation marks (comma’s, etc.)
test.all <- tm_map(test.all,removePunctuation)
# Make all letters lower case
test.all <- tm_map(test.all,content_transformer(tolower))
# Remove stopwords
test.all <- tm_map(test.all, removeWords, stopwords("english"))
# Remove numbers
test.all <- tm_map(test.all,removeNumbers)
# Remove excess whitespace / eliminate extra white-spaces
test.all <- tm_map(test.all,stripWhitespace)

# A document-term matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents. 
test.dtm <- DocumentTermMatrix(test.all,list(dictionary=dimnames(train.dtm)[[2]]))




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


# To Run: 
 reviews.mnb<- train.mnb(train.dtm, labels)
 reviews.mnb.pred <-  predict.mnb(reviews.mnb,as.matrix(test.dtm))
 vec ->c
 
 x <- rep(1, 80)
 y <- rep (0,80)
 true_label<- c(x,y)
 confusion_matrix <-table(reviews.mnb.pred,true_label)

 print(confusion_matrix)
 
 print((confusion_matrix[1,1]+confusion_matrix[2,2])/160)
