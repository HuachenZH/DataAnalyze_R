
library(FactoMineR)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)

#--------------------prepare dataset-----------------
setwd("D:/2018-2023/S8/Tronc commun/ADD/ADD_projet/nutrient") # change it to your path
d <- read_csv("nndb_flat.csv") # import dataset
foodGroup<-as.matrix(d[,2])
d<-d[,8:(d%>%length)] # drop the non-numeric columns
d<-d[,1:((d%>%length)-15)] # drop highly correlated columns

#-------------------PCA()----------------
PCA(as.matrix(d))


# -----------------prcomp()----------------
# https://www.datacamp.com/community/tutorials/pca-analysis-r
d.pca<-prcomp(d)

# # to install ggbiplot
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot) # ggbiplot: Biplot for Principal Components using ggplot2
ggbiplot(d.pca)
# PC1 and PC2:
ggbiplot(d.pca, groups=foodGroup, ellipse=TRUE) # ellips=True...Error: object True not found
# pC3 and PC4:
ggbiplot(d.pca, choices=c(3,4))


#------------------PCA fait à la maison------------------
# define standard deviation of population
sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}

# "X"
# standardize --> code must can be optimized
# mean vector
meanvec<-cbind(1:(d%>%length))%>%as.numeric
for (i in 1:(d%>%length)){
  meanvec[i]<-d[,i]%>%unlist%>%mean
}
# standard deviation vector    
sdpvec<-meanvec #sdp for standard deviation of population
for (i in 1:(d%>%length)){
  sdpvec[i]<-d[,i]%>%unlist%>%sd.p
}
# standardize
ds<-d # ds for dataframe.standardized
for (i in 1:(d%>%length)){
  ds[,i]<-(ds[,i]-rep(1,d%>%nrow)*meanvec[i])/sdpvec[i] # nrow returns row number
}

# "R"
# correlation matrix
dscor<-cor(ds)


# "LAM et P"
# eigen
eigen<-eigen(dscor) # pas compris pq mais c'est correct
evn<-eigen$vectors # matrix P, evn for eigen vector normalized
    #plot the first two columns energy and protein, with original data
    plot(x=ds$Energy_kcal, y=ds$Protein_g, xlim=c(-5,5),ylim=c(-5,5),xlab='Energy_kcal',ylab='Protein_g',main='standardized')
    abline(h=0,v=0)
    
    #plot the first two columns, after projected to eigenvector
    C<-as.matrix(ds)%*%evn  # %*% is for matrix multiplication
    plot(x=C[,1], y=C[,2], xlim=c(-5,5),ylim=c(-5,5),xlab='Energy_kcal',ylab='Protein_g',main='standardized+projected to eigenvector')
    abline(h=0,v=0)
    
# not sure if the matrix of eigen vector is already normalized
nvec<-rep(0,evn%>%ncol)
for (i in 1:(evn%>%ncol)){
  nvec[i]<-evn[,i]^2 %>% sum %>% sqrt # get norm of each column
}
for (i in 1:(evn%>%ncol)){
  evn[,i]<-evn[,i]/nvec[i] # normalize the matrix
}

# "C"
# C=A*P ou X*P
# A ou X matrice centree reduite, P matrice de vect propre reduite
C<-as.matrix(ds)%*%evn  # %*% is for matrix multiplication



# "eboulis"
lam<-eigen$values
inertie<-lam/sum(lam)
inertiecum<-rep(1,lam%>%length)*inertie[1]
for(i in 2:(lam%>%length)){
  inertiecum[i]<-inertiecum[i-1]+inertie[i]
}
eboulis<-data.frame(lam,inertie,inertiecum)
# ggplot
xval=(1:(lam%>%length))%>%as.character
tmp<-xval
for(i in 1:(xval%>%length)){
  xval[i]<-paste("lam",tmp[i])
}
remove(tmp)

ggplot(eboulis)+
  geom_col(aes(x=xval,y=inertiecum*3),size=1,color="darkblue",fill="white")+
  geom_line(aes(x=xval,y=lam,group=1))+geom_point(aes(x=xval,y=lam))+
  scale_y_continuous(sec.axis = sec_axis(~./3,name="bar,percentage"))
labs(x="Eigen value",y="Value of eigen value")
# https://biostats.w.uib.no/overlaying-a-line-plot-and-a-column-plot/

# "Projection"
# projection in the principal plan, C1 and C2, see part "C", matrix C
C1C2<-C[,1:2]%>%data.frame
ggplot(C1C2,aes(x=C1C2[,1],y=C1C2[,2],label=rownames(C1C2)))+
  geom_point()+
  geom_text_repel()+
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65")

# circle of correlations
# https://www.gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/
# function to create a circle
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)

# correlation
dscor<-as.data.frame(cor(d, C))

# data frame with arrows coordinates
arrows = data.frame(x1 = rep(0,nrow(dscor)), y1 = rep(0,nrow(dscor)), x2 = dscor[,1], y2 = dscor[,2])

# geom_path will do open circles
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = dscor, aes(x = dscor[,1], y = dscor[,2], label = rownames(dscor))) + 
  geom_hline(yintercept = 0, colour = "gray65") + 
  geom_vline(xintercept = 0, colour = "gray65") + 
  xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + 
  labs(x = "pc1 aixs", y = "pc2 axis") + 
  ggtitle("Circle of correlations")




#---------------test----------




