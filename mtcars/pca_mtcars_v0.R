# cf https://caoyang.tech/zh/post/pricinple-component-analysis/
#--------------PCA analysis--------------
library(ggbiplot)
mtcars_pca <- prcomp(select(mtcars, -c("vs", "am")),center = TRUE, scale = TRUE) 
# select: select variables byby name. Negative values to drop variables.
# if the first expression is negative, select() will automatically start with all variables
# cf: https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/select

summary(mtcars_pca) 
# with summary, we can see the proportion of variance and the cumulative proportion

#-------------visualize the result--------------
library(ggbiplot)
# ggbiplot est dédié pour pca, si on regarde dans sa documentation, il ne prend que l'objet de prcomp ou princomp comme parametre

ggbiplot(mtcars_pca,var.axes = FALSE) # no arrows, only points
ggbiplot(mtcars_pca, circle = TRUE) # with arrows
ggbiplot(mtcars_pca, labels=rownames(mtcars), circle = TRUE,alpha=1) # with label names


# regroup the cars with their countries
mtcars_country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(mtcars_pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars_country)


# the third and forth principal components
ggbiplot(mtcars_pca,ellipse=TRUE,choices=c(3,4), alpha=1,  labels=rownames(mtcars), groups=mtcars_country)

