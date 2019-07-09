install.packages("readr")
install.packages("corrplot")
install.packages("klaR")
install.packages("pander")
install.packages("GGally")
install.packages("devtools")
install.packages("psych")
install.packages("ggfortify")
library("readr")
library(MASS) 
library(ggplot2) 
library(dplyr) 
library(klaR)
library(pander) 
library(corrplot) 
library(car) 
library(GGally)
library(psych)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggfortify)

#INTRODUCCIÓN DE DATOS
data <- read.table(file.choose(), sep = "" , header = F)
names (data) = c("Ar", "Pe", "Co", "LofK", "WofK", "AsC", "LofKG", "Ty")
data

#Histogramas de cada variable:
for (j in 1:7) { 
  hist(data[,j], xlab=colnames(data)[j], 
       main=paste("Histograma de",colnames(data[j])), 
       col="cyan", labels = TRUE) 
}
#CORRELACIÓN
corrplot(cor(data), method = "color")

round(cor(data), 2)

pairs.panels(data[1:7],
             gap = 0,
             bg = c("blue", "green",
                    "red")[data$Ty],pch = 21)
mtext("Tipos de semillas: blue = 1; green = 2; red = 3", 1, line=3.7,cex=.8)


#PCA
pca <- prcomp(data)
summary(pca)

screeplot(pca)
autoplot(pca, loadings = TRUE, loadings.label = TRUE)

data.pca$rotation[,1] #PC1
data.pca$rotation[,2] #PC2

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              groups = factor(data.Ty), ellipse = TRUE,
              circle = TRUE, cex=1)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

#LDA

#FUNCIÓN PARA PINTAR LDA
ggplotLDA <- function(data){
  if (!is.null(Terms <- data$terms)) {
    datas <- model.frame(data)
    X <- model.matrix(delete.response(Terms), datas)
    f <- model.response(datas)
    xint <- match("(Intercept)", colnames(X), nomatch = 0L)
    if (xint > 0L)
      X <- X[, -xint, drop = FALSE]
  }
  means <- colMeans(data$means)
  X <- scale(X, center = means, scale = FALSE) %*% data$scaling
  a <- as.data.frame(cbind(X,labels=as.character(f)))
  a <- data.frame(X,labels=as.character(f))
  return(a)
}
#Graph <- ggplotLDA(ldaobject)
#ggplot(Graph, aes(LD1,LD2, color=labels))+
#geom_point() +
#stat_ellipse(aes(x=LD1, y=LD2, fill = labels), alpha = 0.2, geom = "polygon")

#CASO 1
mod=lda(Ty~., data=data)
mod

Graph <- ggplotLDA(mod)
ggplot(Graph, aes(LD1,LD2, color=labels))+
geom_point() +
tat_ellipse(aes(x=LD1, y=LD2, fill = labels), alpha = 0.2, geom = "polygon")

p <- predict(mod)
freq <- table(p$class, data$Ty)
freq

#A veces da problemas al pintar los histogramas y hay
#que redefinir el objeto LDA

ldahist(data=mod.values$x[,1],g=training$Ty) 
ldahist(data=mod.lda.values$x[,2],g=training$Ty) 


#CASO 2

set.seed(555)
ind <- sample(2, nrow(data),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- data[ind==1,]
testing <- data[ind==2,]

mod2 <- lda(Ty~., data=training)
mod2

Graph2 <- ggplotLDAPrep(mod2)
ggplot(Graph2, aes(LD1,LD2, color=labels)) + geom_point() +
stat_ellipse(aes(x=LD1, y=LD2, fill = labels), alpha = 0.2, geom = "polygon")

ldahist(data=mod2.values$x[,1],g=training$Ty) #first ld function
ldahist(data=mod2.values$x[,2],g=training$Ty) #second ld function

p2 <- predict(mod2)
freq2 <- table(p2$class, training$Ty)
freq2

p3 <- predict(mod2, testing)
freq3 <- table(p3$class, testing$Ty)
freq3

#CASO 3

mod3=qda(Ty~., data=data)
mod3

p4 <- predict(mod3)
freq4 <- table(p3$class, data$Ty)
freq4


