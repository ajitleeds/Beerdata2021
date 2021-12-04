#Import Dataset
data<-read.csv("BrewdogNew.csv", header=TRUE, stringsAsFactors = T)


#Load the required packages to analyze and impute missing data

#VIM Package
install.packages("VIM")
library("VIM")

#MICE Package
install.packages("mice")
library(mice)

#DPLYR Package
install.packages("dplyr")
library("dplyr")

#Corrgram Package
install.packages("corrgram")
library("corrgram")

#Understanding Data
head(data)
summary(data)
sum(complete.cases(data))
sum(!complete.cases(data))

pmiss<-function(x){sum(is.na(x))/length(x)*100}
apply(data, 2, pmiss)
apply(data,1,pmiss)

md.pattern(data)
aggr_plot<- aggr(data, col=c('grey', 'pink'), number=TRUE, prop=FALSE)
marginplot(data)
corrgram(data)

misdata<- data
misdata$missing <- as.numeric(!complete.cases(data))
corrgram(misdata)

#IMPUTATION

#Simple Imputation
si<-data
si$ABV[is.na(si$ABV)]<- mean(si$ABV, na.rm=TRUE)
si$EBC[is.na(si$EBC)]<- mean(si$EBC, na.rm=TRUE)

#comparing original dataset vs imputed dataset
summary(data)
summary(si)


#Multiple Imputation
tempdata<- mice(data, m=20, maxit=20, seed=1000)
summary(tempdata)
tempdata$imp$ABV
tempdata$imp$EBC
tempdata$method

completedata<-complete(tempdata)
completedata
densityplot(tempdata)
stripplot(tempdata, pch=20, cex=1.0)

#CLUSTERING

#Load the required packages for clustering

install.packages("fastcluster")
library("fastcluster")
install.packages("NbClust")
library("NbClust")
install.packages("cluster")
library("cluster")

beerc<- hclust(dist(completedata[2:8]), "ward.D2")
plot(beerc, labels = completedata$Name, hang=-1)
rect.hclust(beerc,2, border="red")
install.packages("factoextra")
library("factoextra")
fviz_dend(beerc, cex=0.8, lwd=0.8)



res<-NbClust(completedata[2:8], min.nc=2, max.nc=15, method="ward.D2")
km <-kmeans(completedata[2:8], 3)
km
beercluster<-data.frame(completedata, km$cluster)
beercluster[order(beercluster[10]),]

View(completedata)
dismatrix <- dist(completedata[2:9])

dm<-daisy(completedata[2:9])
clust<-agnes(dm, diss = TRUE, method="ward")
clust
plot(clust, main='Dendrogram', labels=completedata$Name)
beerclust<- data.frame(completedata, cutree(clust, k=3))
beerclust[c(1:9,3)][order(beercluster[3]),]

