library(RTextTools)
library(tm)

grasscity <- read.csv("cleaned.csv",stringsAsFactors = F)
grasscity$forum <- 'grasscity'

cannabis <- read.csv("cleaned2.csv",stringsAsFactors = F)
cannabis$forum <- 'cannabis'

marijuana <-read.csv("cleaned3.csv",stringsAsFactors = F)
marijuana$forum <-'marijuana'
marijuana<- marijuana[-3]

posts <- rbind(grasscity,cannabis,marijuana)

set.seed(5126)
train.idx<-sample(1:nrow(posts),round(0.1*nrow(posts)))
train<-posts[train.idx,]
test<-posts[-train.idx,]

#Check train samples are evenly distributed throughout the 3 forums
table(train$forum)

#write.csv(train,"train.csv",row.names = F)
#manual labeling of train data follow by reading in the manually labeled data for analysis

train<-read.csv("train.csv",stringsAsFactors = F)

# Create_matrix creates an object of class DocumentTermMatrix
dtMatrix <- create_matrix(train["processed"], 
                          weighting=weightTfIdf, removeSparseTerms=.99)

container <- create_container(dtMatrix, factor(train$support), trainSize=1:nrow(train),virgin=FALSE)

#cross validation on 6 algorithms

SVM <- cross_validate(container, 5, "SVM", cost = 1)
SLDA <- cross_validate(container, 5, "SLDA")
BAGGING <- cross_validate(container, 5, "BAGGING")
BOOSTING <- cross_validate(container, 5, "BOOSTING", maxitboost = 500)
RF <- cross_validate(container, 5, "RF", ntree = 500)
TREE <- cross_validate(container, 5, "TREE")

# > SVM <- cross_validate(container, 5, "SVM", cost = 1)
# Fold 1 Out of Sample Accuracy = 0.6644737
# Fold 2 Out of Sample Accuracy = 0.6964286
# Fold 3 Out of Sample Accuracy = 0.6331361
# Fold 4 Out of Sample Accuracy = 0.6946108
# Fold 5 Out of Sample Accuracy = 0.6466667
# > SLDA <- cross_validate(container, 5, "SLDA")
# Fold 1 Out of Sample Accuracy = 0.8855422
# Fold 2 Out of Sample Accuracy = 0.7988166
# Fold 3 Out of Sample Accuracy = 0.8678161
# Fold 4 Out of Sample Accuracy = 0.7837838
# Fold 5 Out of Sample Accuracy = 0.7651007
# > BAGGING <- cross_validate(container, 5, "BAGGING")
# Fold 1 Out of Sample Accuracy = 0.8979592
# Fold 2 Out of Sample Accuracy = 0.9152542
# Fold 3 Out of Sample Accuracy = 0.9354839
# Fold 4 Out of Sample Accuracy = 0.8867925
# Fold 5 Out of Sample Accuracy = 0.9345238
# > BOOSTING <- cross_validate(container, 5, "BOOSTING", maxitboost = 500) #0.9576
# Fold 1 Out of Sample Accuracy = 0.9532164
# Fold 2 Out of Sample Accuracy = 0.9496855
# Fold 3 Out of Sample Accuracy = 0.9452055
# Fold 4 Out of Sample Accuracy = 0.9876543
# Fold 5 Out of Sample Accuracy = 0.952381
# > RF <- cross_validate(container, 5, "RF", ntree = 500) #0.9522
# Fold 1 Out of Sample Accuracy = 0.945122
# Fold 2 Out of Sample Accuracy = 0.9754601
# Fold 3 Out of Sample Accuracy = 0.9548387
# Fold 4 Out of Sample Accuracy = 0.9122807
# Fold 5 Out of Sample Accuracy = 0.9738562
# > TREE <- cross_validate(container, 5, "TREE")
# Fold 1 Out of Sample Accuracy = 0.8169935
# Fold 2 Out of Sample Accuracy = 0.8402778
# Fold 3 Out of Sample Accuracy = 0.7831325
# Fold 4 Out of Sample Accuracy = 0.8229167
# Fold 5 Out of Sample Accuracy = 0.7417219

#LogitBoost is the best algorithm with cross-validated accuracy of 95.8%

#predict labels for the test posts
all<-rbind(train['processed'],test['processed'])
dtMatrix.test <- create_matrix(all, weighting=weightTfIdf, removeSparseTerms=.99)
container.test <- create_container(dtMatrix.test, factor(train$support), trainSize=1:nrow(train), testSize=nrow(train)+1:nrow(test),virgin = FALSE)
best.model <- train_model(container.test,"BOOSTING", maxitboost = 500)

test.pred<-classify_model(container.test, best.model)
test$support<-test.pred[,1]
table(test$support)

#write.csv(test,"test.csv",row.names = F)

#aggregate and summarize results

allposts<-rbind(train,test)

grasscity.supp<-sum(allposts$forum=='grasscity'& allposts$support ==1)
cannabis.supp<-sum(allposts$forum=='cannabis'& allposts$support ==1)
marijuana.supp<-sum(allposts$forum=='marijuana'& allposts$support ==1)

grasscity.pct<-sum(allposts$forum=='grasscity')
cannabis.pct<-sum(allposts$forum=='cannabis')
marijuana.pct<-sum(allposts$forum=='marijuana')


summary<- data.frame(forum = c("grasscity.com","cannabis.com","marijuana.com"),
                     Proportion = c(grasscity.pct,cannabis.pct,marijuana.pct),
                     Support = c(grasscity.supp,cannabis.supp,marijuana.supp))
                     
#write.csv(summary,"summary.csv",row.names = F)
