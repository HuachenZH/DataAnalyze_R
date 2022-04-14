library(readr)
library(dplyr)
library(ggplot2)
library(cluster)
par(mfrow=c(1,1))



#------------------dataset----------------------------------
# load dataset in relative path
a<-mtcars

#----------------------dendrogram----------------------
# without scale
hc<-hclust(a%>%dist,method="ward.D")
plot(hc)
#with scale
hc<-hclust(a%>%scale%>%dist,method="ward.D") 
plot(hc)

#----------------with rectangles------------------
nbr_rectangle=4
plot(hc)
rect.hclust(hc,k=nbr_rectangle,border='red')


#-------------les patates------------------
# 2 patates
cl2<-cutree(hc,2)
clusplot(a%>%dist,cl2,diss=T,shade=T,color=T,labels=2,main="CHA/ACP")
# 4 patates
cl2<-cutree(hc,4)
clusplot(a%>%dist,cl2,diss=T,shade=T,color=T,labels=2,main="CHA/ACP")









