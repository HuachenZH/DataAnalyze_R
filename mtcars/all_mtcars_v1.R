# cf https://caoyang.tech/zh/post/pricinple-component-analysis/
#--------------PCA analysis--------------
library(dplyr)
mtcars # let's take a look at the dataset. It will be outprinted in the console.

mtcars_pca <- prcomp(select(mtcars, -c("vs", "am")),center = TRUE, scale = TRUE) 
# the column vs and am are not very interesting for the analyse, so we will drop them.
# select: select variables byby name. Negative values to drop variables.
# if the first expression is negative, select() will automatically start with all variables
# prcomp is the function of R for principal component analysis. With the parameter center=TRUE and scale=TRUE, the dateset will be standardized (centré réduit) automatically.
# If you have any doubt of prcomp, I suggest you to go to the official documentation. 
# cf: https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/select

summary(mtcars_pca) 
# with summary, we can see the proportion of variance and the cumulative proportion

#-------------PCA: visualize the result--------------
library(ggbiplot)
# ggbiplot est dédié pour pca, si on regarde dans sa documentation, il ne prend que l'objet de prcomp ou princomp comme parametre

ggbiplot(mtcars_pca,var.axes = FALSE) # no arrows, only points
ggbiplot(mtcars_pca, circle = TRUE) # with arrows
ggbiplot(mtcars_pca, labels=rownames(mtcars), circle = TRUE,alpha=1) # with label names


# regroup the cars with their countries
mtcars_country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(mtcars_pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars_country)
# with ellipse=TRUE, there will be ellipses on the graph, grouped by mtcars_country

# the third and forth principal components
ggbiplot(mtcars_pca,ellipse=TRUE,choices=c(3,4), alpha=1,  labels=rownames(mtcars), groups=mtcars_country)
# With the parameter choices=c(3,4), it will show the third and the forth principal components, PC3 and PC4.
# For those amoung you who are not familiar with R, c() is how we combine numbers (or strings) into vector.


#--------------------hc-------------------
library(cluster)
# without scale
hc<-hclust(mtcars%>%dist,method="ward.D")
plot(hc)
#with scale
hc<-hclust(mtcars%>%scale%>%dist,method="ward.D") 
plot(hc)

#----------------hc: with rectangles------------------
nbr_rectangle=4
plot(hc)
rect.hclust(hc,k=nbr_rectangle,border='red')



