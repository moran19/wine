install.packages("ggplot2")
install.packages("tidyverse")
install.packages("GGally")
install.packages("ggcorrplot")
install.packages("dplyr")
library (ggplot2)
library(tidyverse)
library(GGally)
library(ggcorrplot)
library(dplyr)

setwd("D:/Data Mining")
df = read.csv("winequality-red.csv", header = TRUE)
df["quality"] = lapply(df["quality"] , factor)

wine_tib = as_tibble((df))

#Cleaning
df = df %>% unique() %>% drop_na()
wine = wine_tib %>% unique() %>% drop_na()

#Preliminary exploration
ggpairs(wine_tib, aes(color=quality))

summary(wine_tib)

wine %>% group_by(quality) %>% summarise_all(mean)
wine %>% group_by(quality) %>% summarise_all(median)

#Histogram
h=ggplot(wine, aes(fixed.acidity, fill=quality)) + geom_histogram(bins = 100)
h1=ggplot(wine, aes(volatile.acidity, fill=quality)) + geom_histogram(bins = 100)
h2=ggplot(wine, aes(citric.acid, fill=quality)) + geom_histogram(bins = 100)
h3=ggplot(wine, aes(residual.sugar, fill=quality)) + geom_histogram(bins = 100)
h4=ggplot(wine, aes(chlorides, fill=quality)) + geom_histogram(bins = 100)
h5=ggplot(wine, aes(free.sulfur.dioxide, fill=quality)) + geom_histogram(bins = 100)
h6=ggplot(wine, aes(total.sulfur.dioxide, fill=quality)) + geom_histogram(bins = 100)
h7=ggplot(wine, aes(density, fill=quality)) + geom_histogram(bins = 100)
h8=ggplot(wine, aes(pH, fill=quality)) + geom_histogram(bins = 100)
h9=ggplot(wine, aes(sulphates, fill=quality)) + geom_histogram(bins = 100)
h10=ggplot(wine, aes(alcohol, fill=quality)) + geom_histogram(bins = 100)
grid.arrange(h, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, ncol=4)

#Boxplot
b=ggplot(wine, aes(quality, fixed.acidity, fill=quality)) + geom_boxplot()
b1=ggplot(wine, aes(quality, volatile.acidity, fill=quality)) + geom_boxplot()
b2=ggplot(wine, aes(quality, citric.acid, fill=quality)) + geom_boxplot()
b3=ggplot(wine, aes(quality, residual.sugar, fill=quality)) + geom_boxplot()
b4=ggplot(wine, aes(quality, chlorides, fill=quality)) + geom_boxplot()
b5=ggplot(wine, aes(quality, free.sulfur.dioxide, fill=quality)) + geom_boxplot()
b6=ggplot(wine, aes(quality, total.sulfur.dioxide, fill=quality)) + geom_boxplot()
b7=ggplot(wine, aes(quality, density, fill=quality)) + geom_boxplot()
b8=ggplot(wine, aes(quality, pH, fill=quality)) + geom_boxplot()
b9=ggplot(wine, aes(quality, sulphates, fill=quality)) + geom_boxplot()
b10=ggplot(wine, aes(quality, alcohol, fill=quality)) + geom_boxplot()
grid.arrange(b, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, ncol=4)

#Class Imbalance - Should adjust by grouping quality into 2 groups
ggplot(wine, aes(y = quality)) + geom_bar()


#Clean and capped outliers at 1% and 99%
df99 = read.csv("winequality-red-99.csv", header = TRUE)
df99 = df99[c(2:13)]
df99["quality"] <- lapply(df99["quality"] , factor)

#Boxplot 1% and 99% cap
ggplot(df99, aes(quality, fixed.acidity, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, volatile.acidity, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, citric.acid, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, residual.sugar, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, chlorides, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, free.sulfur.dioxide, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, total.sulfur.dioxide, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, density, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, pH, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, sulphates, fill=quality)) + geom_boxplot()
ggplot(df99, aes(quality, alcohol, fill=quality)) + geom_boxplot()


#Parallel coordinates plot
install.packages("seriation")
library(seriation)
ord <- seriate(as.dist(1-cor(df99 [,1:11])), method="BBURCG") 
get_order(ord)
ggparcoord(as_tibble(df99), columns = get_order(ord), groupColumn = 12)

#principal components
pc = df99 %>% select(-quality) %>% as.matrix() %>% prcomp()
plot(pc)
str(pc)
ggplot(as_tibble(pc$x), aes(x = PC1, y = PC2)) + geom_point()

#Correlation
install.packages("Hmisc")
install.packages("corrplot")
library(Hmisc)
library(corrplot)

df99$quality=as.numeric(df99$quality)
cor_matrix = df99 %>% as.matrix %>%scale()%>% cor()
cor_matrix
ggcorrplot(cor_matrix)
p.mat<-cor_pmat(cor_matrix)
ggcorrplot(cor_matrix,type='lower',outline.color="white",colors=c("CornflowerBlue","white","Salmon"),
           lab = T,lab_size = 3,tl.cex = 9, p.mat = p.mat,sig.level = 0.3,pch = 4,pch.cex = 7)

p_values = rcorr(as.matrix(df99))
p_values


#Data matrix visualization
ggplot(df99 %>% mutate(id = row_number()) %>% pivot_longer(cols = 1:11), 
       aes(x = name, y = id, fill = value)) + geom_tile() + scale_fill_viridis_c()

#Group quality column into high/low
#https://stackoverflow.com/questions/12979456/categorize-numeric-variable-into-group-bins-breaks
df99 = read.csv("winequality-red-99.csv", header = TRUE)
df99 = df99[c(2:13)]
df99 <- df99 %>% mutate(quality = case_when(quality >= 6 ~ "high", quality <= 5 ~ "low")) 
df99["quality"] <- lapply(df99["quality"] , factor)
df99 %>% count(quality)

df99 %>% group_by(quality) %>% summarise_all(min)
df99 %>% group_by(quality) %>% summarise_all(max)
df99 %>% group_by(quality) %>% summarise_all(median)

#Standardization
scale_numeric <- function(x)x %>% mutate_if(is.numeric,function(y) as.vector(scale(y)))
df99s <- df99 %>% scale_numeric()
summary(df99)


#Decision tree
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("party")
library(caret)
library(rpart)
library(rpart.plot)
library(party)

tree_default = df99 %>% rpart(quality ~ ., data = .)
tree_default
rpart.plot(tree_default, extra = 2)


set.seed(12345)

#Spliting the data into training set and test set
inTrain <- createDataPartition(y = df99s$quality, p = .7, list = FALSE)
df99_train <- df99s %>% slice(inTrain)
df99_test <- df99s %>% slice(-inTrain)

df99_train %>% count(quality)
df99_test %>% count(quality)

train_index <- createFolds(df99_train$quality, k = 10)


#Conditional Inference Tree (Decision Tree)
ctreeFit <- df99_train %>% train(quality ~ .,
                                method = "ctree",
                                data = .,
                                tuneLength = 5,
                                trControl = trainControl(method = "cv", 
                                indexOut = train_index))
ctreeFit
plot(ctreeFit$finalModel)

#C 4.5 Decision Tree - Not working. need to try in class
install.packages("RWeka")
library(RWeka)
C45Fit <- df99_train %>% train(quality ~ .,
                              method = "J48",
                              data = .,
                              tuneLength = 5,
                              trControl = trainControl(method = "cv", indexOut = train_index))
C45Fit
C45Fit$finalModel

#K-Nearest Neighbors
knnFit <- df99_train %>% train(quality ~ .,
                              method = "knn",
                              data = .,
                              preProcess = "scale",
                              tuneLength = 5,
                              tuneGrid=data.frame(k = 1:10),
                              trControl = trainControl(method = "cv", indexOut = train_index))
knnFit
knnFit$finalModel

#Naïve Bayes Classifiers
install.packages("klaR")
library(klaR)
NBayesFit <- df99_train %>% train(quality ~ .,
                                 method = "nb",
                                 data = .,
                                 tuneLength = 5,
                                 trControl = trainControl(method = "cv", indexOut = train_index))
NBayesFit

#Linear Support Vector Machines
svmFit <- df99_train %>% train(quality ~.,
                              method = "svmLinear",
                              data = .,
                              tuneLength = 5,
                              trControl = trainControl(method = "cv", indexOut = train_index))
svmFit
svmFit$finalModel


#Comparison
resamps <- resamples(list(
  ctree = ctreeFit,
  #C45 = C45Fit,
  KNN = knnFit,
  NBayes = NBayesFit,
  SVM = svmFit
  #rf = rfFit
  ))

resamps
summary(resamps)

library(lattice)
bwplot(resamps, layout = c(3, 1))


#Applying the best model to the test data

pr <- predict(knnFit, df99_test)
pr
confusionMatrix(pr, reference = df99_test$quality)


#KNN
install.packages("factoextra")
library(factoextra)
km<-kmeans(df99s[,-12],centers = 4,nstart = 10)
#km


#names(cdf99s)

#ggplot(cdf99s,aes(x= alcohol,y= -volatile.acidity,color=cluster))+geom_point(cex=2)+geom_point(data = centroids, aes(x = alcohol, y = -volatile.acidity, color = cluster), pch = 3, size = 10)





#Remove Outlines - Results weren't as good as python capping at 99%
quartiles <- quantile(wine$fixed.acidity, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$fixed.acidity)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$fixed.acidity > lower & wine$fixed.acidity < upper)

quartiles <- quantile(wine$volatile.acidity, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$volatile.acidity)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$volatile.acidity > lower & wine$volatile.acidity < upper)

quartiles <- quantile(wine$citric.acid, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$citric.acid)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$citric.acid > lower & wine$citric.acid < upper)

quartiles <- quantile(wine$residual.sugar, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$residual.sugar)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$residual.sugar > lower & wine$residual.sugar < upper)

quartiles <- quantile(wine$chlorides, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$chlorides)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$chlorides > lower & wine$chlorides < upper)

quartiles <- quantile(wine$free.sulfur.dioxide, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$free.sulfur.dioxide)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$free.sulfur.dioxide > lower & wine$free.sulfur.dioxide < upper)

quartiles <- quantile(wine$total.sulfur.dioxide, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$total.sulfur.dioxide)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$total.sulfur.dioxide > lower & wine$total.sulfur.dioxide < upper)

quartiles <- quantile(wine$density, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$density)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$density > lower & wine$density < upper)

quartiles <- quantile(wine$pH, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$pH)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$pH > lower & wine$pH < upper)

quartiles <- quantile(wine$alcohol, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wine$alcohol)
lower = quartiles[1] - 1.5*IQR
upper = quartiles[2] + 1.5*IQR
wine = subset(wine, wine$alcohol > lower & wine$alcohol < upper)

#Cap outliers at 1% and 99% - doesn't work as well as Python code
# https://stackoverflow.com/questions/13339685/how-to-replace-outliers-with-the-5th-and-95th-percentile-values-in-r
df99 = read.csv("winequality-red.csv", header = TRUE)
df99["quality"] <- lapply(df99["quality"] , factor)
df99 = df99 %>% unique()

qn = quantile(df99$fixed.acidity, c(0.99), na.rm = TRUE)
df99 = within(df99, {fixed.acidity = ifelse(fixed.acidity > qn[1], qn[1], fixed.acidity)})

qn = quantile(df99$volatile.acidity, c(0.99), na.rm = TRUE)
df99 = within(df99, {volatile.acidity = ifelse(volatile.acidity > qn[1], qn[1], volatile.acidity)})

qn = quantile(df99$citric.acid, c(0.01, 0.99), na.rm = TRUE)
df99 = within(df99, {citric.acid = ifelse(citric.acid < qn[1], qn[1], citric.acid)
citric.acid = ifelse(citric.acid > qn[2], qn[2], citric.acid)})

qn = quantile(df99$residual.sugar, c(0.01, 0.99), na.rm = TRUE)
df99 = within(df99, {residual.sugar = ifelse(residual.sugar < qn[1], qn[1], residual.sugar)
residual.sugar = ifelse(residual.sugar > qn[2], qn[2], residual.sugar)})

qn = quantile(df99$chlorides, c(0.01, 0.99), na.rm = TRUE)
df99 = within(df99, {chlorides = ifelse(chlorides < qn[1], qn[1], chlorides)
chlorides = ifelse(chlorides > qn[2], qn[2], chlorides)})

qn = quantile(df99$free.sulfur.dioxide, c(0.99), na.rm = TRUE)
df99 = within(df99, {free.sulfur.dioxide = ifelse(free.sulfur.dioxide > qn[1], qn[1], free.sulfur.dioxide)})

qn = quantile(df99$total.sulfur.dioxide, c(0.99), na.rm = TRUE)
df99 = within(df99, {total.sulfur.dioxide = ifelse(total.sulfur.dioxide > qn[1], qn[1], total.sulfur.dioxide)})

qn = quantile(df99$density, c(0.01, 0.99), na.rm = TRUE)
df99 = within(df99, {density = ifelse(density < qn[1], qn[1], density)
density = ifelse(density > qn[2], qn[2], density)})

qn = quantile(df99$pH, c(0.01, 0.99), na.rm = TRUE)
df99 = within(df99, {pH = ifelse(pH < qn[1], qn[1], pH)
pH = ifelse(pH > qn[2], qn[2], pH)})

qn = quantile(df99$sulphates, c(0.01, 0.99), na.rm = TRUE)
df99 = within(df99, {sulphates = ifelse(sulphates < qn[1], qn[1], sulphates)
sulphates = ifelse(sulphates > qn[2], qn[2], sulphates)})

qn = quantile(df99$alcohol, c(0.01, 0.99), na.rm = TRUE)
df99 = within(df99, {alcohol = ifelse(alcohol < qn[1], qn[1], alcohol)
alcohol = ifelse(alcohol > qn[2], qn[2], alcohol)})

#Random Forest
install.packages("randomForest")
library(randomForest)
rfFit <- df99_train %>% train(quality ~.,
                              method = "rf",
                              data = .,
                              tuneLength = 5,
                              trControl = trainControl(method = "cv", indexOut = train_index))
rfFit
rfFit$finalModel

#Full tree - too complected, not used
tree_full <- df99 %>% rpart(quality ~., data = ., control = rpart.control(minsplit = 20, cp = 0)) 
rpart.plot(tree_full, extra = 2)
tree_full

#Adding a cost matrix
cost <- matrix(c(
  0,   1,
  100, 0
), byrow = TRUE, nrow = 2)

fit = df99_train %>% train(quality ~ ., data = .,
                           method = "rpart",
                           parms = list(loss = cost),
                           trControl = trainControl(method = "cv"))
fit

confusionMatrix(data = predict(fit, df99_test), ref = df99_test$quality, positive = "low")

#Knn without corelated variables - less accurate results
df99 = read.csv("winequality-red-99.csv", header = TRUE)
df99 = df99[c(2, 3, 5, 6, 7, 11, 12, 13)]
df99 = df99[c(3, 10, 12, 13)]
df99[3, 12, 13]



#FOR REPORT USE ONLY
#Histogram
#For combining graphics for report: https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
#For color: https://www.datacamp.com/tutorial/make-histogram-ggplot2?utm_source=google&utm_medium=paid_search&utm_campaignid=19589720821&utm_adgroupid=157156375351&utm_device=c&utm_keyword=&utm_matchtype=&utm_network=g&utm_adpostion=&utm_creative=680291483658&utm_targetid=aud-438305022466:dsa-2218886984060&utm_loc_interest_ms=&utm_loc_physical_ms=9069537&utm_content=&utm_campaign=230119_1-sea~dsa~tofu_2-b2c_3-row-p1_4-prc_5-na_6-na_7-le_8-pdsh-go_9-na_10-na_11-na-bfcm23&gad_source=1&gclid=EAIaIQobChMIu8_HgvTIggMV3sZMAh204AAjEAAYASAAEgKP2vD_BwE
install.packages("gridExtra")
library(gridExtra)


p = ggplot(wine, aes(fixed.acidity, fill=quality)) + geom_histogram(bins = 100)
p1 = ggplot(wine, aes(volatile.acidity, fill=quality)) + geom_histogram(bins = 100)
p2 = ggplot(wine, aes(citric.acid, fill=quality)) + geom_histogram(bins = 100)
p3 = ggplot(wine, aes(residual.sugar, fill=quality)) + geom_histogram(bins = 100)
p4 = ggplot(wine, aes(chlorides, fill=quality)) + geom_histogram(bins = 100)
p5 = ggplot(wine, aes(free.sulfur.dioxide, fill=quality)) + geom_histogram(bins = 100)
p6 = ggplot(wine, aes(total.sulfur.dioxide, fill=quality)) + geom_histogram(bins = 100)
p7 = ggplot(wine, aes(density, fill=quality)) + geom_histogram(bins = 100)
p8 = ggplot(wine, aes(pH, fill=quality)) + geom_histogram(bins = 100)
p9 = ggplot(wine, aes(sulphates, fill=quality)) + geom_histogram(bins = 100)
p10 = ggplot(wine, aes(alcohol, fill=quality)) + geom_histogram(bins = 100)

#Class Imbalance - Should adjust by grouping quality into 2 groups
p11 = ggplot(wine, aes(y = quality, fill=quality)) + geom_bar()
grid.arrange(p, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, ncol=4)

#Boxplot

b = ggplot(wine, aes(quality, fixed.acidity, fill=quality)) + geom_boxplot()
b1 = ggplot(wine, aes(quality, volatile.acidity, fill=quality)) + geom_boxplot()
b2 = ggplot(wine, aes(quality, citric.acid, fill=quality)) + geom_boxplot()
b3 = ggplot(wine, aes(quality, residual.sugar, fill=quality)) + geom_boxplot()
b4 = ggplot(wine, aes(quality, chlorides, fill=quality)) + geom_boxplot()
b5 = ggplot(wine, aes(quality, free.sulfur.dioxide, fill=quality)) + geom_boxplot()
b6 = ggplot(wine, aes(quality, total.sulfur.dioxide, fill=quality)) + geom_boxplot()
b7 = ggplot(wine, aes(quality, density, fill=quality)) + geom_boxplot()
b8 = ggplot(wine, aes(quality, pH, fill=quality)) + geom_boxplot()
b9 = ggplot(wine, aes(quality, sulphates, fill=quality)) + geom_boxplot()
b10 = ggplot(wine, aes(quality, alcohol, fill=quality)) + geom_boxplot()
grid.arrange(b, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, ncol=3)




install.packages("mlbench")
install.packages("e1071")
install.packages("caret")
install.packages("rpart")
install.packages('seriation')
install.packages('cluster')
install.packages('factoextra')
install.packages('fpc')

library(ggplot2)
library(lattice)
library(tidyverse)
library(mlbench)
library(e1071)
library(caret)
library(rpart)
library(GGally)
library(seriation)
library(cluster)
library(factoextra)
library(fpc)

winequality<-read.csv('D:/Data Mining/winequality-red-99 nox.csv')
winequality$quality<-ifelse(winequality$quality>=6,'high','low')
#winequality<-as_tibble(winequality)

#winequality%>%group_by(quality)%>% summarise_all()


######Pairwise comparison of data
set.seed(2000)
ggpairs(winequality,aes(color=quality))

######parallel coordinates plot
ord<-seriate(as.dist(1-cor(winequality[1:11])),method='BBURCG')
get_order(ord)
ggparcoord(winequality,columns = get_order(ord),groupColumn = 12)
ggparcoord(winequality,columns = 1:11,groupColumn = 12)###no order

######tutorial6 pictures
ggplot(winequality,aes(x= alcohol,y= -volatile.acidity,color=quality))+geom_point(cex=5)

#######scale
scale_numeric<-function(x)x%>%mutate_if(is.numeric,function(y) as.vector(scale(y)))
winequality_scaled<-winequality%>%scale_numeric()
ggplot(winequality_scaled,aes(x= alcohol,y= -volatile.acidity,color=quality))+geom_point(cex=5)


#######k—means
km<-kmeans(winequality_scaled[,-12],centers = 2,nstart = 10)
km
ggplot(winequality_scaled,aes(x= alcohol,y= -volatile.acidity,color=km$cluster))+geom_point(cex=5)


winequality_scaled_2<-winequality_scaled[c(2,11,12)]######just 2 predictions
km2<-kmeans(winequality_scaled_2[,-3],centers = 2,nstart = 10)

winequality_scaled_12<-winequality_scaled
winequality_scaled_12$quality<-ifelse(winequality_scaled_12$quality=='high',2,1)
accuary2<-sum(winequality_scaled_12$quality==km$cluster)/length(winequality_scaled_12$quality)
accuary2

ggplot(winequality_scaled_2,aes(x=alcohol ,y= -volatile.acidity,color=km2$cluster))+geom_point(cex=5)
######centroids
centroids<-as_tibble(km$centers,rownames="cluster")
centroids
winequality_clustered<-winequality_scaled%>%add_column(cluster=factor(km$cluster))
winequality_clustered


ggplot(winequality_clustered,aes(x= alcohol,y= -volatile.acidity,color=cluster))+geom_point(cex=2)+geom_point(data = centroids, aes(x = alcohol, y = -volatile.acidity, color = cluster), pch = 3, size = 10)
########use the factoextra package for visualization
fviz_cluster(km,data = data.frame(winequality_scaled$alcohol,-winequality_scaled$volatile.acidity),centroids=TRUE,repel = TRUE,ellipse.type = 'norm')

fviz_cluster(km2,data = data.frame(winequality_scaled_2$alcohol,-winequality_scaled_2$volatile.acidity),centroids=TRUE,repel = TRUE,ellipse.type = 'norm')######just 2 predictions

####### Hierarchical Clustering 
d<-dist(winequality_scaled[,-12])
hc<-hclust(d,method = 'complete')
plot(hc)

d2<-dist(winequality_scaled_2[,-12])######just 2 predictions
hc2<-hclust(d2,method = 'complete')
plot(hc2)
######cl picture
fviz_dend(hc2,k=2)

clusters<-cutree(hc,k=2)
cluster_complete<-winequality_scaled%>%add_column(cluster=factor(clusters))
cluster_complete
ggplot(cluster_complete, aes(alcohol, -volatile.acidity, color = cluster)) + geom_point(cex=5) 

clusters2<-cutree(hc2,k=2)######just 2 predictions
cluster_complete2<-winequality_scaled_2%>%add_column(cluster2=factor(clusters2))
cluster_complete2
ggplot(cluster_complete2, aes(alcohol, -volatile.acidity, color = cluster2)) + geom_point(cex=5) 


accuary3<-sum(winequality_scaled_12$quality==cluster_complete2$cluster2)/length(winequality_scaled_12$quality)
accuary3
#########fviz_cluster
fviz_cluster(list(data = winequality_scaled[1:11], cluster = cutree(hc, k = 2)), geom = "point") 

########internal cluster validation
cluster.stats(d,km$cluster)

######silhouette plot(bars are missing)
plot(silhouette(km$cluster,d))

#####ggplot visualization using factorxtra
fviz_silhouette(silhouette(km$cluster,d))

#########

#########Obtaining results using the knn method
trainlist<-createDataPartition(winequality$quality,p=0.7,list=F)
trainset<-winequality[trainlist,]
testset<-winequality[-trainlist,]

train_index <- createFolds(trainset$quality, k =10)

knnFit <- trainset %>% train(quality ~ .,
                             method = "knn",
                             data = .,
                             preProcess = "scale",
                             tuneLength = 5,
                             tuneGrid=data.frame(k = 1),
                             trControl = trainControl(method = "cv", indexOut = train_index))
knnFit

predicted<-predict(knnFit,testset)
accuary<-sum(testset$quality==predicted)/length(testset$quality)
accuary

ggplot(testset,aes(x= alcohol,y= -volatile.acidity,color=predicted))+geom_point(cex=5)
ggplot(testset,aes(x= alcohol,y= -volatile.acidity,color=testset$quality))+geom_point(cex=5)

