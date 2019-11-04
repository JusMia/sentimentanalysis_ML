set.seed(10)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial10mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial10<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(10=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(10=10,y=y)) +
  geom_point(data=points,aes(10=10, y=y, color=my.df$sentiment)) +
  geom_te10t(data=points,aes(10=10, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear10<- cbind (TNR,TPR,PPV,ACC)
svm190linear10mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear10, svm190radial10)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(10=fit[,130], y=fit[,130])
points <- data.frame(10=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(10=10,y=y)) +
  geom_point(data=points,aes(10=10, y=y, color=my.df$sentiment)) +
  geom_te10t(data=points,aes(10=10, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19010mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19010 <- cbind (TNR,TPR,PPV,ACC)

mat19010<-cbind (svm190radial10mat,svm190linear10mat, nn19010mat,knn19010mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19010mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19010 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat10 <- cbind(svm190radial10mat, svm190linear10mat, nn19010mat, knn19010mat)
result.conf10 <- rbind(svm190linear10,svm190radial10,nn19010,knn19010)

set.seed(20)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial20mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial20<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(20=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(20=20,y=y)) +
  geom_point(data=points,aes(20=20, y=y, color=my.df$sentiment)) +
  geom_te20t(data=points,aes(20=20, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear20<- cbind (TNR,TPR,PPV,ACC)
svm190linear20mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear20, svm190radial20)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(20=fit[,130], y=fit[,130])
points <- data.frame(20=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(20=20,y=y)) +
  geom_point(data=points,aes(20=20, y=y, color=my.df$sentiment)) +
  geom_te20t(data=points,aes(20=20, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19020mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19020 <- cbind (TNR,TPR,PPV,ACC)

mat19020<-cbind (svm190radial20mat,svm190linear20mat, nn19020mat,knn19020mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19020mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19020 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat20 <- cbind(svm190radial20mat, svm190linear20mat, nn19020mat, knn19020mat)
result.conf20 <- rbind(svm190linear20,svm190radial20,nn19020,knn19020)

set.seed(30)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial30mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial30<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(30=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(30=30,y=y)) +
  geom_point(data=points,aes(30=30, y=y, color=my.df$sentiment)) +
  geom_te30t(data=points,aes(30=30, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear30<- cbind (TNR,TPR,PPV,ACC)
svm190linear30mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear30, svm190radial30)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(30=fit[,130], y=fit[,130])
points <- data.frame(30=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(30=30,y=y)) +
  geom_point(data=points,aes(30=30, y=y, color=my.df$sentiment)) +
  geom_te30t(data=points,aes(30=30, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19030mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19030 <- cbind (TNR,TPR,PPV,ACC)

mat19030<-cbind (svm190radial30mat,svm190linear30mat, nn19030mat,knn19030mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19030mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19030 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat30 <- cbind(svm190radial30mat, svm190linear30mat, nn19030mat, knn19030mat)
result.conf30 <- rbind(svm190linear30,svm190radial30,nn19030,knn19030)



set.seed(40)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial40mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial40<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(40=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(40=40,y=y)) +
  geom_point(data=points,aes(40=40, y=y, color=my.df$sentiment)) +
  geom_te40t(data=points,aes(40=40, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear40<- cbind (TNR,TPR,PPV,ACC)
svm190linear40mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear40, svm190radial40)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(40=fit[,130], y=fit[,130])
points <- data.frame(40=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(40=40,y=y)) +
  geom_point(data=points,aes(40=40, y=y, color=my.df$sentiment)) +
  geom_te40t(data=points,aes(40=40, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19040mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19040 <- cbind (TNR,TPR,PPV,ACC)

mat19040<-cbind (svm190radial40mat,svm190linear40mat, nn19040mat,knn19040mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19040mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19040 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat40 <- cbind(svm190radial40mat, svm190linear40mat, nn19040mat, knn19040mat)
result.conf40 <- rbind(svm190linear40,svm190radial40,nn19040,knn19040)


set.seed(50)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial50mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial50<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(50=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(50=50,y=y)) +
  geom_point(data=points,aes(50=50, y=y, color=my.df$sentiment)) +
  geom_te50t(data=points,aes(50=50, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear50<- cbind (TNR,TPR,PPV,ACC)
svm190linear50mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear50, svm190radial50)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(50=fit[,130], y=fit[,130])
points <- data.frame(50=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(50=50,y=y)) +
  geom_point(data=points,aes(50=50, y=y, color=my.df$sentiment)) +
  geom_te50t(data=points,aes(50=50, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19050mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19050 <- cbind (TNR,TPR,PPV,ACC)

mat19050<-cbind (svm190radial50mat,svm190linear50mat, nn19050mat,knn19050mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19050mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19050 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat50 <- cbind(svm190radial50mat, svm190linear50mat, nn19050mat, knn19050mat)
result.conf50 <- rbind(svm190linear50,svm190radial50,nn19050,knn19050)

set.seed(60)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial60mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial60<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(60=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(60=60,y=y)) +
  geom_point(data=points,aes(60=60, y=y, color=my.df$sentiment)) +
  geom_te60t(data=points,aes(60=60, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear60<- cbind (TNR,TPR,PPV,ACC)
svm190linear60mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear60, svm190radial60)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(60=fit[,130], y=fit[,130])
points <- data.frame(60=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(60=60,y=y)) +
  geom_point(data=points,aes(60=60, y=y, color=my.df$sentiment)) +
  geom_te60t(data=points,aes(60=60, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19060mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19060 <- cbind (TNR,TPR,PPV,ACC)

mat19060<-cbind (svm190radial60mat,svm190linear60mat, nn19060mat,knn19060mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19060mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19060 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat60 <- cbind(svm190radial60mat, svm190linear60mat, nn19060mat, knn19060mat)
result.conf60 <- rbind(svm190linear60,svm190radial60,nn19060,knn19060)


set.seed(70)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial70mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial70<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(70=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(70=70,y=y)) +
  geom_point(data=points,aes(70=70, y=y, color=my.df$sentiment)) +
  geom_te70t(data=points,aes(70=70, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear70<- cbind (TNR,TPR,PPV,ACC)
svm190linear70mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear70, svm190radial70)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(70=fit[,130], y=fit[,130])
points <- data.frame(70=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(70=70,y=y)) +
  geom_point(data=points,aes(70=70, y=y, color=my.df$sentiment)) +
  geom_te70t(data=points,aes(70=70, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19070mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19070 <- cbind (TNR,TPR,PPV,ACC)

mat19070<-cbind (svm190radial70mat,svm190linear70mat, nn19070mat,knn19070mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19070mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19070 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat70 <- cbind(svm190radial70mat, svm190linear70mat, nn19070mat, knn19070mat)
result.conf70 <- rbind(svm190linear70,svm190radial70,nn19070,knn19070)

set.seed(80)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial80mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial80<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(80=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(80=80,y=y)) +
  geom_point(data=points,aes(80=80, y=y, color=my.df$sentiment)) +
  geom_te80t(data=points,aes(80=80, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear80<- cbind (TNR,TPR,PPV,ACC)
svm190linear80mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear80, svm190radial80)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(80=fit[,130], y=fit[,130])
points <- data.frame(80=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(80=80,y=y)) +
  geom_point(data=points,aes(80=80, y=y, color=my.df$sentiment)) +
  geom_te80t(data=points,aes(80=80, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19080mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19080 <- cbind (TNR,TPR,PPV,ACC)

mat19080<-cbind (svm190radial80mat,svm190linear80mat, nn19080mat,knn19080mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19080mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19080 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat80 <- cbind(svm190radial80mat, svm190linear80mat, nn19080mat, knn19080mat)
result.conf80 <- rbind(svm190linear80,svm190radial80,nn19080,knn19080)

set.seed(90)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial90mat<- rbind(tables.res,
                          rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial90<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(90=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(90=90,y=y)) +
  geom_point(data=points,aes(90=90, y=y, color=my.df$sentiment)) +
  geom_te90t(data=points,aes(90=90, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear90<- cbind (TNR,TPR,PPV,ACC)
svm190linear90mat<- rbind(tables.res,
                          rr)
svm190 <- rbind(svm190linear90, svm190radial90)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(90=fit[,130], y=fit[,130])
points <- data.frame(90=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(90=90,y=y)) +
  geom_point(data=points,aes(90=90, y=y, color=my.df$sentiment)) +
  geom_te90t(data=points,aes(90=90, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn19090mat<- rbind(my.results.nn,
                   rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn19090 <- cbind (TNR,TPR,PPV,ACC)

mat19090<-cbind (svm190radial90mat,svm190linear90mat, nn19090mat,knn19090mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn19090mat<- rbind(tables.res2,
                    rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn19090 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat90 <- cbind(svm190radial90mat, svm190linear90mat, nn19090mat, knn19090mat)
result.conf90 <- rbind(svm190linear90,svm190radial90,nn19090,knn19090)


set.seed(100)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)

library(e1071)
my.svm <- svm(sentiment~., train, kernel='radial', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radial100mat<- rbind(tables.res,
                           rr)
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)



svm190radial100<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(100=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(100=100,y=y)) +
  geom_point(data=points,aes(100=100, y=y, color=my.df$sentiment)) +
  geom_te100t(data=points,aes(100=100, y=y, label=row.names(my.df)))
dev.copy(png,'radial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='linear', scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])

tables.res
rr
TP <- tables.res[2,2]
TN <- tables.res[1,1]
FP <- tables.res[2,1]
FN <- tables.res[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)

svm190linear100<- cbind (TNR,TPR,PPV,ACC)
svm190linear100mat<- rbind(tables.res,
                           rr)
svm190 <- rbind(svm190linear100, svm190radial100)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(100=fit[,130], y=fit[,130])
points <- data.frame(100=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(100=100,y=y)) +
  geom_point(data=points,aes(100=100, y=y, color=my.df$sentiment)) +
  geom_te100t(data=points,aes(100=100, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
#set.seed(10)
#sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

library(neuralnet)

datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainNN = datatrain.test[sample.id , ]
testNN = datatrain.test[-sample.id , ]
#colnames(trainNN) 

nn <- neuralnet(sentiment~.,data=trainNN,hidden=1,linear.output=FALSE)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testNN = predict(nn, testNN)
typeof(nn)
my.results <- list()
for (a in 1:length(testNN$sentiment))
{
  sentiment <- 2
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 1
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
rr1

nn190100mat<- rbind(my.results.nn,
                    rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn190100 <- cbind (TNR,TPR,PPV,ACC)

mat190100<-cbind (svm190radial100mat,svm190linear100mat, nn190100mat,knn190100mat )

tosave.nn<-cbind(my.results.nn,rr1)
write.csv(nn190, file = "NeuralNetwork.csv")
plot(nn, rep = 'best')
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =c(1,2) , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =2 , highlight = FALSE,
#        type = "p", col = "black")
# gwplot(nn, rep = "best",
#        selected.covariate = sentiment, selected.response =1 , highlight = FALSE,
#        type = "p", col = "black")

save.image("~/III/dataPCA2500nn.RData")
##########
# KNN- neighbour
#install.packages("DMwR")
library(DMwR)

#set.seed(10)
data.test.knn <- data.test
data.test.knn$sv <- rep(0, length(data.test.knn$sentiment))
data.test.knn$sv[data.test.knn$sentiment == 'Pos'] = 1
data.test.knn$sv[data.test.knn$sentiment == 'Neg'] = 0 
data.test.knn$sentiment <- data.test.knn$sv
data.test.knn$sv <- NULL
colnames(data.test.knn)
summary(data.test.knn$sentiment)

#colnames(data.test.knn$sentiment)
#sample.id <- sample(1:nrow(data.test.knn),round(0.75*nrow(data.test.knn)))
train2 <- as.data.frame(data.test.knn[sample.id,])
test2 <- as.data.frame(data.test.knn[-sample.id,])

library(class) 
##### number of observation
NROW(train2)
knn.61  <- kNN(sentiment ~ .,train2,test2,norm=FALSE,k=61)
result.knn61 <- table(test2 [,'sentiment'],knn.61)
knn.61.norm  <- kNN(sentiment ~ .,train2,test2,norm=TRUE,k=61)
result.knn.norm<-table(test2 [,'sentiment'],knn.61.norm)
result.knn <-c(result.knn61,result.knn.norm)

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn190100mat<- rbind(tables.res2,
                     rr2)
cbind(tables.res2,
      rr2)
TP <- tables.res2[2,2]
TN <- tables.res2[1,1]
FP <- tables.res2[2,1]
FN <- tables.res2[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


knn190100 <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.mat100 <- cbind(svm190radial100mat, svm190linear100mat, nn190100mat, knn190100mat)
result.conf100 <- rbind(svm190linear100,svm190radial100,nn190100,knn190100)

result.conf <-
cbind(result.conf10,result.conf20,result.conf30,result.conf40,result.conf50,result.conf60,result.conf70,result.conf80,result.conf90,result.conf100)
colnames(result.conf)
result.mat <-
  rbind(result.mat10,result.mat20,result.mat30,result.mat40,result.mat50,result.mat60,result.mat70,result.mat80,result.mat90,result.mat100)
