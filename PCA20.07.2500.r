# load required libraries
library(tm)
library(ggplot2)
library(lsa)
library(devtools)
library(httr)
library(dplyr)
library(text2vec)
library(tidytext)
library(tokenizers)
library(mallet)

wczytaj.tekst <- function(path.to.file)
{
  tekst.do.analizy = readLines(path.to.file, n = 1, warn = FALSE)
  stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
  tekst.do.analizy <- stringr::str_replace_all(tekst.do.analizy, stopwords_regex, '')
  #tekst.do.analizy <- tokenize_words(tekst.do.analizy)
  #tekst.do.analizy <- unlist(tekst.do.analizy)
  return (tekst.do.analizy)
}

#path1 <- 'c:\\projects\\word2vec\\data\\aclImdb\\test\\neg\\'
path1 <- 'C:\\Users\\justi\\Documents\\III\\aclImdb\\train\\neg\\'
files <- list.files(path1)
txt <- list()
for (a in 1:2500)
{
  txt[[a]] <- wczytaj.tekst(paste(path1, files[a], sep = ''))
}
sentimentN <- rep('Neg', length(txt))
#path1 <- 'c:\\projects\\word2vec\\data\\aclImdb\\test\\pos\\'


path1 <- 'C:\\Users\\justi\\Documents\\III\\aclImdb\\train\\pos\\'
files <- list.files(path1)
txt2 <- list()
for (a in 1:2500)
{
  txt2[[a]] <- wczytaj.tekst(paste(path1, files[a], sep = ''))
  
}
sentimentP <- rep('Pos', length(txt2))


my.df2 <- data.frame(text = c(unlist(txt),unlist(txt2)), sentiment = c(sentimentN, sentimentP), stringsAsFactors = FALSE)

# prepare corpus
corpus <- Corpus(VectorSource(my.df2$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument, language = "english")
#corpus # check corpus

# 2. MDS with raw term-document matrix
# compute distance matrix
library(slam)
td.mat <- TermDocumentMatrix(corpus)

x<-rbind(td.mat)

##################################
z<- slam::row_sums(td.mat) 
z<-as.data.frame(z) 
m <- as.matrix(td.mat)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),total=v)
tf <- d$total/length(d$word)

idf<- list()
for (a in 1:length(d$word))
{
  idf[a]<- log((length(corpus)/d$total[a]))
}
idf <- unlist(idf)
tf_idf <- tf*idf

#Zipf’s Law
d <- data.frame(word = names(v),total=v,df = z, tf = tf, idf = idf, tf_idf = tf_idf)
freq_by_rank <- group_by(d) %>% mutate(rank = row_number(), 
                                       `word_frequency` = total/length(d$word))
freq_by_rank %>% 
  ggplot(aes(rank, `word_frequency`, color = "blue")) + 
  geom_abline(intercept = 0, slope = -1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

dev.copy(png,'wordbyrank.png')
dev.off()

rank_subset <- freq_by_rank %>% 
  filter(rank < 5199,
         rank > 10)
lm(log10(word_frequency) ~ log10(rank), data = rank_subset)

words.to.remain <- rank_subset

wczytaj.tekst2 <- function(path.to.file, words.to.remain)
{
  
  tekst.do.analizy = readLines(path.to.file, n = 1, warn = FALSE)
  stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
  tekst.do.analizy <- stringr::str_replace_all(tekst.do.analizy, stopwords_regex, '')
  tekst.do.analizy <- tokenize_words(tekst.do.analizy)
  tekst.do.analizy <- unlist(tekst.do.analizy)
  tekst.do.analizy <- paste(tekst.do.analizy[tekst.do.analizy %in% words.to.remain$word], collapse=" ")
  return (tekst.do.analizy)
}

###################################################


path1 <- 'C:\\Users\\justi\\Documents\\III\\aclImdb\\train\\neg\\'
files <- list.files(path1)
txt <- list()
for (a in 1:2500)
{
  txt[[a]] <- wczytaj.tekst2(paste(path1, files[a], sep = ''), words.to.remain)
}

sentimentN <- rep('Neg', length(txt))

path1 <- 'C:\\Users\\justi\\Documents\\III\\aclImdb\\train\\pos\\'
files <- list.files(path1)
txt2 <- list()
for (a in 1:2500)
{
  txt2[[a]] <- wczytaj.tekst2(paste(path1, files[a], sep = ''), words.to.remain)
}
sentimentP <- rep('Pos', length(txt2))




my.df <- data.frame(text = c(unlist(txt),unlist(txt2)), sentiment = c(sentimentN, sentimentP), stringsAsFactors = FALSE)

# prepare corpus
corpus <- Corpus(VectorSource(my.df$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)


# 2. MDS with raw term-document matrix
# compute distance matrix

td.mat <- TermDocumentMatrix(corpus)
m <- as.matrix(td.mat)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


#length(dist.mat)
library(wordcloud2)

wordcloud2(d[1:200,],figPath = NULL, fontWeight = 'bold', color = 'random-dark', rotateRatio = 0.4, shape = 'circle',size=0.3)


dev.copy(png,'wordcloud.png')
dev.off()
#head(d, 100)
dist.mat <- dist(t(as.matrix(td.mat)))

save.image("~/III/dataPCA2500423.RData")
# MDS
# (rr - recognition rate)

setwd('C:\\Users\\justi\\Documents\\III\\Last\\')
j <- c()
results <- c(1:10)
results <- as.data.frame(results)
fit <- cmdscale(dist.mat, eig=TRUE, k=190)
data.test <- data.frame(fit$points, sentiment = my.df$sentiment)

set.seed(10)

sample.id <- sample(1:nrow(data.test),round(0.75*nrow(data.test)))

train <- data.test[sample.id,]
test <- data.test[-sample.id,]
summary(train$sentiment)
summary(test$sentiment)


library(e1071)
my.svm <- svm(sentiment~., train, kernel='sigmoid', gamma= 1/190, scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radialxmat<- rbind(tables.res,
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



svm190radialx<-cbind (TNR,TPR,PPV,ACC)
#write.csv(svm190radial, file = "svm190radial.csv")


points <- data.frame(x=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(x=x,y=y)) +
  geom_point(data=points,aes(x=x, y=y, color=my.df$sentiment)) +
  geom_text(data=points,aes(x=x, y=y, label=row.names(my.df)))
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

svm190linearx<- cbind (TNR,TPR,PPV,ACC)
svm190linearxmat<- rbind(tables.res,
                         rr)
svm190 <- rbind(svm190linearx, svm190radialx)

#write.csv(svm190, file = "svml190.csv")


####### True Positive, True Negative, Recall, (PPV) precision

#points <- data.frame(x=fit[,130], y=fit[,130])
points <- data.frame(x=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(x=x,y=y)) +
  geom_point(data=points,aes(x=x, y=y, color=my.df$sentiment)) +
  geom_text(data=points,aes(x=x, y=y, label=row.names(my.df)))
dev.copy(png,'linear190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='polynomial',gamma = 1/190, degree = 1, scale = FALSE)
my.predict <- predict(my.svm, test)

tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190radialxmat<- rbind(tables.res,
                         rr)
rr

points <- data.frame(x=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(x=x,y=y)) +
  geom_point(data=points,aes(x=x, y=y, color=my.df$sentiment)) +
  geom_text(data=points,aes(x=x, y=y, label=row.names(my.df)))
dev.copy(png,'polynomial190.png')
dev.off()

my.svm <- svm(sentiment~., train, kernel='sigmoid',gamma = 1/190, scale = FALSE)
my.predict <- predict(my.svm, test)
write.csv(my.predict,file = "svm.csv")
tables.res <- table(test$sentiment, my.predict)
sentiment<- test$sentiment
rr <- (tables.res[1,1] + tables.res[2,2]) / sum(tables.res)

tables.res[1,] <- tables.res[1,]  / sum(tables.res[1,])
tables.res[2,] <- tables.res[2,]  / sum(tables.res[2,])


svm190sigmoidxmat<- rbind(tables.res,
                          rr)
rr

points <- data.frame(x=fit$points[,190], y=fit$points[,190])
ggplot(points, aes(x=x,y=y)) +
  geom_point(data=points,aes(x=x, y=y, color=my.df$sentiment)) +
  geom_text(data=points,aes(x=x, y=y, label=row.names(my.df)))
dev.copy(png,'sigmoid190.png')
dev.off()
save.image("~/III/dataPCA2500svm.RData")
#####################
# seed for 10,20,30,40,50,60,70,80,90,100
set.seed(10)
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
  sentiment <- 1
  if (predict_testNN[a,1] > predict_testNN[a,2])
  {
    sentiment <- 0
  }
  my.results[[a]] <- sentiment
}
my.results <- unlist(my.results)
sentiment.help <- as.numeric(testNN$sentiment)
table(my.results, sentiment.help)
my.results.nn <- table(my.results, sentiment.help)
rr1 <- (my.results.nn[1,1] + my.results.nn[2,2]) / sum(my.results.nn)
rr1
my.results.nn[1,] <- my.results.nn[1,]  / sum(my.results.nn[1,])
my.results.nn[2,] <- my.results.nn[2,]  / sum(my.results.nn[2,])
my.results.nn
write.csv(my.results, file="ann.csv")

nn190xmat<- rbind(my.results.nn,
                            rr1)

TP <- my.results.nn[2,2]
TN <- my.results.nn[1,1]
FP <- my.results.nn[2,1]
FN <- my.results.nn[1,2]
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


nn190x <- cbind (TNR,TPR,PPV,ACC)

#write.csv(nn190, file = "NeuralNetwork.csv")
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

set.seed(10)
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
result.knn <-rbind(result.knn61,result.knn.norm)
write.csv(knn.61.norm, file = "knn.61.norm.csv")

tables.res2<-table(test2 [,'sentiment'],knn.61.norm)
rr2 <- (tables.res2[1,1] + tables.res2[2,2]) / sum(tables.res2)

tables.res2[1,] <- tables.res2[1,]  / sum(tables.res2[1,])
tables.res2[2,] <- tables.res2[2,]  / sum(tables.res2[2,])
tables.res2

knn190xmat<- rbind(tables.res2,
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


knn190x <- cbind (TNR,TPR,PPV,ACC)
#test<-rbind(svm190linear,svm190radial)

#write.csv(knn190, file = "KNN61v10.csv")
result.matx <- cbind(svm190radialxmat, svm190linearxmat, nn190xmat, knn190xmat)
result.confx <- rbind(svm190linearx,svm190radialx,nn190x,knn190x)

#tosave.knn<-cbind(tables.res2,rr2)
save.image("~/III/dataPCA25004knn.RData")


#####random forest

library(randomForest)

set.seed(10)
datatrain.test <- data.test
datatrain.test$sentiment <- as.factor(datatrain.test$sentiment)

trainRF = datatrain.test[sample.id , ]
testRF = datatrain.test[-sample.id , ]
#colnames(trainNN) 

rr3 <- randomForest(sentiment~.,data=trainRF)

#save.image("~/III/dataPCA2500423nn.RData")
predict_testRF = as.data.frame(predict(rr3, testRF))
typeof(rr3)
write.csv(predict_testRF, file = "predict_testRF.csv")


RF190xmat<-rr3$confusion
as.data.frame(RF190xmat)

TP <- 1346
TN <- 1422
FP <- 462
FN <- 520
TPR <- TP / sum(TP,FN)
TNR <- TN  / sum(TN,FP)
PPV <- TP / sum(TP,FP)
ACC <- sum(TP,TN)/sum(TP,FN,TN,FP)


RF190x <- cbind (TNR,TPR,PPV,ACC)

##############HOE
HOE<-read.csv(file="results.csv", header = TRUE)

for (a in 1:nrow(HOE))
{
  HOE$Vote[a]<-0
  if (sum(HOE[a,]) >1 )
  {
    HOE$Vote[a]<-1
  }
}
sum(HOE$SVM)
sum(HOE$ANN)
sum(HOE$kNN)

first <- sum(HOE$Vote)
null <-  nrow(HOE) - first

first

nn190xmat
knn190xmat
svm190sigmoidxmat

tables.res.HOE<-table(test2 [,'sentiment'],HOE$Vote)
rrHOE <- (tables.res.HOE[1,1] + tables.res.HOE[2,2]) / sum(tables.res.HOE)

TP.HOE <- tables.res.HOE[2,2]
TN.HOE <- tables.res.HOE[1,1]
FP.HOE <- tables.res.HOE[2,1]
FN.HOE <- tables.res.HOE[1,2]
TPR.HOE<- TP.HOE / sum(TP.HOE,FN.HOE)
TNR.HOE <- TN.HOE  / sum(TN.HOE,FP.HOE)
PPV.HOE <- TP.HOE / sum(TP.HOE,FP.HOE)
ACC.HOE <- sum(TP.HOE,TN.HOE)/sum(TP.HOE,FN.HOE,TN.HOE,FP.HOE)
result.HOE <- cbind (TNR.HOE,TPR.HOE,PPV.HOE,ACC.HOE)
library(philentropy)
# 3. MDS with LSA
#dtm.mat.lsa <- lw_bintf(dtm.mat) * gw_idf(dtm.mat) # weighting
td.mat<-as.matrix(td.mat)
typeof(td.mat)
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
#lsaSpace <- lsa(dtm.mat.lsa)
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
#dist.mat.lsa # check distance mantrix



# MDS
fit <- cmdscale(dist.mat.lsa, eig=FALSE, k=190)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) +
  geom_point(data=points,aes(x=x, y=y, color=my.df$sentiment)) +
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(my.df)))
dev.copy(png,'rdist.mat.lsa.190.png')
dev.off()

library(scatterplot3d)
#fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=130)
colors <- rep(c("blue", "green", "red"), each=20)
my.df$color <- my.df$sentiment
my.df$color[my.df$color == 'Pos'] <- 'blue'
my.df$color[my.df$color == 'Neg'] <- 'red'
scatterplot3d(fit[, 190], fit[, 190], fit[, 190], color=my.df$color, pch=16,
              main="Semantic Space Scaled to 3D", xlab="x", ylab="y", zlab="z", type="h")
dev.copy(png,'rdist.mat.lsa.130.3d.png')
dev.off()
plot(x=fit[,190], y=fit[, 190], col=my.df$color, xlab="x", ylab="y")
dev.copy(png,'rdist.mat.lsa.190.png')
dev.off()


save.image("~/III/dataPCA2500all.RData")

