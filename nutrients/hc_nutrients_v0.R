library(readr)
library(dplyr)
library(ggplot2)
library(cluster)
par(mfrow=c(1,1))



#------------------"donnees"----------------------------------
# load dataset in relative path
setwd("D:/2018-2023/S8/Tronc commun/ADD/ADD_projet/nutrient") # change it to your path
a <- read_csv("nndb_flat.csv")
# # pass the first column into meta data
# name<-unlist(a[,1])
# a<-a[,2:length(a)] # length(a) returns the number of columns
# ref<-data.frame(name)

a<-a[,8:(a%>%length)] # drop the non-numeric columns
a<-a[,1:((a%>%length)-15)] # drop highly correlated columns




#----------------------dendrogram----------------------
# without scale
hc<-hclust(a%>%dist,method="ward.D") 
plot(hc)
#with scale
hc<-hclust(a%>%scale%>%dist,method="ward.D") 
plot(hc)

#------------------les patates----------------------
# warning!! ne fais pas run cette section, ça va peter ta RAM
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
