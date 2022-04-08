library(readr)
library(dplyr)
library(ggplot2)
library(cluster)
par(mfrow=c(1,1))

# Les noms des villes sont trop longs. Pour faciliter la lecture, j'ai remplacé les noms par les chiffres. "ref" est pour voir quelle ville correspond à quel chiffre.

#------------------"donnees"----------------------------------
# load dataset in relative path
setwd("C:/Users/eziod/Pictures/S8/Tronc commun/ADD/Project/") # change it to your path
a <- read_csv("./meteo/data.csv")

# pass the first column into meta data
name<-unlist(a[,1])

a<-a[,2:length(a)] # length(a) returns the number of columns
ref<-data.frame(name)



#----------------------dendrogram----------------------
# without scale
hc<-hclust(a%>%dist,method="ward.D") 
plot(hc)
#with scale
hc<-hclust(a%>%scale%>%dist,method="ward.D") 
plot(hc)

#------------------les patates----------------------
# 2 patates
cl2<-cutree(hc,2)
clusplot(a%>%dist,cl2,diss=T,shade=T,color=T,labels=2,main="CHA/ACP")
# 4 patates
cl2<-cutree(hc,4)
clusplot(a%>%dist,cl2,diss=T,shade=T,color=T,labels=2,main="CHA/ACP")


#----------------with rectangles------------------
nbr_rectangle=4
plot(hc)
rect.hclust(hc,k=nbr_rectangle,border='red')





