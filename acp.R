library(shiny)
library(readxl)
library(qcr)
library(ggplot2)
library(ggdendro)
library(cluster)
runApp(App)
data<-read_excel("ACP.xlsx",sheet = "Taller",na = "")
d<-data[,2:5]
covarianza<-cov(d)
correlaciones<-cor(d)
#componentes principales
pca<- prcomp(d)
pca
str(pca)
summary(pca)
#histograma de valores propios
plot(pca)
pca2<-pca$x[,1]
str(pca2)
names(data)
#Se ajusta un GLM
glm1<- glm(data[,5]~data[,3] + pca2, family=gaussian)
summary(glm1)
anova(glm1)
par(mfcol=c(2,2))
plot(glm1)

#Conglomerados
par(mfcol=c(1,1))
plot(d[,2],d[,3],xlim = c(0,max(d[,2])),ylim = c(0,max(d[,3])),type = "p",col="blue")
cj<-hclust(dist(d), method = "ward.D") 
row.names(data)<-data[,1]
plot(cj,main="DENDOGRAMA",labels=row.names(data),hang=-1,ylab = "Distancia",xlab="Concesionario",sub = "") 
ggdendrogram(cj,labels =row.names(data),hang=-1 )



if(require(cluster)){
  model <- agnes(d, metric = "ward.D", stand = TRUE)
  dg <- as.dendrogram(model)
  ggdendrogram(dg)
  
  model <- diana(d, metric = "ward.D", stand = TRUE)
  dg <- as.dendrogram(model)
  ggdendrogram(dg)
}

##################################################################
