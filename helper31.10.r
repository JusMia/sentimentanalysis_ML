################ matrix for all
result.mat <-
  cbind(result.mat10,result.mat20,result.mat30,result.mat40,result.mat50,result.mat60,result.mat70,result.mat80,result.mat90,result.mat100)

##############conf mat for all


result.conf <-
  cbind(result.conf10,result.conf20,result.conf30,result.conf40,result.conf50,result.conf60,result.conf70,result.conf80,result.conf90,result.conf100)


############

svm190radialres <-rbind(svm190radial10,svm190radial20,svm190radial30,svm190radial40,svm190radial50,svm190radial60,svm190radial70,svm190radial80,svm190radial90,svm190radial100)

a1<-col_means(svm190radialres)

svm190radialmat <- cbind(svm190radial10mat,svm190radial20mat,svm190radial30mat,svm190radial40mat,svm190radial50mat,svm190radial60mat,svm190radial70mat,svm190radial80mat,svm190radial90mat,svm190radial100mat)
mean(unique(svm190radialmat[3,]))

############

svm190linearres <-rbind(svm190linear10,svm190linear20,svm190linear30,svm190linear40,svm190linear50,svm190linear60,svm190linear70,svm190linear80,svm190linear90,svm190linear100)
a2<-col_means(svm190linearres)
svm190linearmat <- cbind(svm190linear10mat,svm190linear20mat,svm190linear30mat,svm190linear40mat,svm190linear50mat,svm190linear60mat,svm190linear70mat,svm190linear80mat,svm190linear90mat,svm190linear100mat)
mean(unique(svm190linearmat[3,]))
############

nn190res <-rbind(nn19010,nn19020,nn19030,nn19040,nn19050,nn19060,nn19070,nn19080,nn19090,nn190100)
mean(nn190res[,4])
a3<-col_means(nn190res)
nn190mat <- cbind(nn19010mat,nn19020mat,nn19030mat,nn19040mat,nn19050mat,nn19060mat,nn19070mat,nn19080mat,nn19090mat,nn190100mat)
mean(unique(nn190mat[3,]))

############

kNN190res <-rbind(knn19010,knn19020,knn19030,knn19040,knn19050,knn19060,knn19070,knn19080,knn19090,knn190100)
mean(kNN190res[,4])
a4<-col_means(kNN190res)
knn190mat <- cbind(knn19010mat,knn19020mat,knn19030mat,knn19040mat,knn19050mat,knn19060mat,knn19070mat,knn19080mat,knn19090mat,knn190100mat)
mean(unique(knn190mat[3,]))
w<- rbind(a1,a2,a3,a4)
rownames(w) <- c("SVM Radial", "SVM Linear", "NN", "kNN")
al<-rbind(svm190linearres, svm190radialres, nn190res,kNN190res)
write.csv(al, file = "al190.csv")
write.csv(w, file = "al190mean.csv")

