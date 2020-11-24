##Airline Dataset clustering 

# Hierarchical Clustering 
dim(Airlines)
names(Airlines)
names(Airlines)[names(Airlines) == 'Award?'] <- 'Award' ## changing name make name simpler 
Airlines <- Airlines[,2:12]
str(Airlines)
Airlines <- as.data.frame(Airlines)
normalized_airline <- scale(Airlines)
View(normalized_airline)
airline_d <- dist(normalized_airline,method = 'euclidean')
summary(airline_d)
airline_fit <- hclust(airline_d,method = 'complete')
plot(airline_fit)
library(factoextra)
?fviz_nbclust
fviz_nbclust(normalized_airline,hcut,method = 'wss')+labs(subtitle = 'Airline Data')
fviz_nbclust(normalized_airline, FUNcluster = hcut, method = c("wss"), 
             diss = NULL, k.max = 25, nboot = 100,
             verbose = interactive(), barfill = "steelblue",
             barcolor = "steelblue", linecolor = "steelblue",
             print.summary = TRUE)
airline_group <- cutree(airline_fit,k=6)
rect.hclust(airline_fit,k=6,border = 'red')
airline_memebership <- as.matrix(airline_group)
Airlines_group <- cbind(Airlines,airline_memebership)
View(Airlines)
airline_fit
## Kmean clustering 
plot(Airlines)
airline_km <- kmeans(normalized_airline,6)
library(animation)
kmeans.ani(normalized_airline,6)
airline_km$cluster
testData <- aggregate(normalized_airline,list(airline_km$cluster),FUN="mean")
View(testData)
library(kselection)
k <- kselection(normalized_airline,parallel = T)
k

## Clustering on Crime data 
crime_data1 <- crime_data[,2:6]
str(crime_data)
normalized_data <- scale(crime_data1)
View(normalized_data)
d <- dist(normalized_data,method = 'euclidean')
fit <- hclust(d,method = 'complete')
plot(fit,hang = -1)
fviz_nbclust(normalized_data,hcut,method = 'wss')+labs(subtitle = 'Eblow Curve') ## suggesting 10 cluster 
k1 <- kselection(normalized_data,parallel = T)
k1  ## suggesting 2 cluster
crime_group <- cutree(fit,k = 5)
rect.hclust(fit,k=5,border = 'red')
crime_group <- as.matrix(crime_group)
crime_data <- cbind(crime_data,crime_group)
head(crime_data)
View(aggregate(crime_data1,by=list(crime_group),FUN = 'mean'))