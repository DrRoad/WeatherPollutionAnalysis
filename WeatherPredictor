
AirPollOrig<-read.csv(file.choose())
AirPollData<-AirPollOrig
PollData<-AirPollData[-c(507:527),-c(1,2)]
AirPollData[c(507:527),]
tail(PollData)
Poll.dist <- get_dist(PollData, stand = TRUE, method = "pearson")
fviz_dist(Poll.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

fviz_nbclust(PollData, kmeans, method = "silhouette")

cl <- kmeans(PollData,3)
cl$cluster
plot(PollData$SO2, PollData$NOx,col=cl$cluster)
points(cl$centers, pch=16)
set.seed(125)
km.res <- kmeans(my_data, 3, nstart = 25)
fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
#PollDataSo2<-as.matrix(AirPollData[,-c(1,2,4,5)])
#PollDataNox<-as.matrix(AirPollData[,-c(1:3,5)])
#PollDataRSPM<-as.matrix(AirPollData[,-c(1:4)])
#fviz_nbclust(PollDataSo2, kmeans, method = "gap_stat")
fviz_nbclust(scale(PollData[,-c(4)]), kmeans,method="silhouette")
set.seed(125)
km.res <- kmeans(scale(PollData[,c(-4)]), 4, nstart = 25)
km.res
km.res$cluster
fviz_cluster(km.res, data = scale(PollData),
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
install.packages("NbClust")
library("NbClust")
nb <- NbClust(scale(PollData[,-c(4)]), distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)
#library("flexclust")
PollNewData<-scale(AirPollData[c(507:527),-c(1,2)])
#predict(km.res,PollNewData)

summary(factor(km.res$cluster))
PollData$Class<-NULL
PollData<-as.data.frame(scale(PollData))
PollData$C<-km.res$cluster

model1<-multinom(PollData$C~., PollData)
summary(model1)
predict_Values<-predict(model1,PollNewData)
str(predict_Values)
summary(predict_Values)

install.packages("gmodels")
library("class")
AirPoll_test_pred <- knn(train = PollData[,-c(4)], test = PollNewData,cl = PollData$C, k=4)
summary(AirPoll_test_pred)
