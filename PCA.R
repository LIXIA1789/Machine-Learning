# View data
data("mtcars")
head(mtcars)
dim(mtcars)

# 1.Calculate the principal components
res <- prcomp(mtcars, scale = TRUE)
names(res)
res$rotation <- -1*res$rotation
res$rotation 
summary(res$rotation,loadings=TRUE)

## scores
res$x <- -1*res$x
head(res$x)

# 2.Draw the biplot
biplot(res, scale = 0)

## display states with highest murder rates in original data set
head(mtcars[order(-mtcars$carb),])
max(mtcars$carb)
which.max(mtcars$carb)

# 3.Calculate proportion of variance
var_explained = res$sdev^2 / sum(res$sdev^2)

# 4.Make scree plot
library(ggplot2)
qplot(c(1:11), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# 5.Get new data
new.data<-as.data.frame(predict(res)[,1:3])
head(new.data)

# 6.visualize PCA
library("ggplot2")
mtcars$cyl <- as.factor(mtcars$cyl)
ggplot(faithful,aes(waiting, eruptions)) + geom_point() + stat_ellipse()
p <- ggplot(faithful,aes(waiting, eruptions,color=eruptions > 3)) + geom_point()
p + stat_ellipse()
p + stat_ellipse(type="norm")

library(pca3d)
data("mtcars")
pca <- prcomp(mtcars, scale = TRUE)
## 2D
pca2d(pca,bg="black",)
## 3D
pca3d(pca)
## 3D+
pca3d(pca,fancy=TRUE, bg= "black",axes.color= "cyan",new=TRUE)
## 3D++
pca3d(pca, fancy=FALSE, bg= "black", axes.color= "blue", new=TRUE,show.centroids=TRUE)
## 3D+++
pca3d(pca, fancy=FALSE, bg= "black", axes.color= "yellow", new=TRUE,show.shadows=TRUE)
pca3d(pca, fancy=FALSE, bg= "white", axes.color= "green", new=TRUE,show.ellipses=TRUE)



