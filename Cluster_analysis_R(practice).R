## Cluster Analysis

library(cluster) # conduct the cluster analysis
library(compareGroups) # build descriptive statistics table
library(HDclassif) # contains the dataset
library(NbClust) # cluster validity measure
library(sparcl) #colored dendogram

data(wine) # loading the data
str(wine) # checking the structure

names(wine) <- c("Class","Alcohol","MalicAcid","Ash","Alk_ash","magnesium","T_phenols","Flavanoids","Non_flav","Proantho","C_Intensity","Hue","OD280_315","Proline")

names(wine)

df <- as.data.frame(scale(wine[,-1])) # creating dataframe eliminating the class
str(df)
table(wine$Class)

###### Hierarchical Clustering ####
# numComplete this will give us the plot where we can see that Number of clster is best here is 3.
numComplete <- NbClust(df,distance = "euclidean",min.nc = 2,max.nc = 6, method = "complete",index = "all")

numComplete$Best.nc
# with three clusters as recommended selection, we will now compute or distance matrix and build heirarchical object

dis <- dist(df, method = "euclidean")
# we will now use this matrix as input for our hclust() actual clustering
hc <- hclust(dis,method = "complete")
# ploting the Heirarchical cluster object
plot(hc, main = "Complete Linkage", hang = -1)

comp3 <- cutree(hc,3)
ColorDendrogram(hc, y= comp3, main= "complete", branchlength = 50)

table(comp3)
table(comp3,wine$Class) # rows are clusters, columns are cultivers

###### Ward's Linkage ######

numbward <- NbClust(df, diss = NULL, distance = "euclidean",min.nc = 2,max.nc = 6,method = "ward.D2", index="all")

hcward <- hclust(dis,method = "ward.D2")
plot(hcward, main = "Complete Linkage", hang = -1)

ward3 <- cutree(hcward,3)
table(ward3,wine$Class)

aggregate(wine[,-1],list(ward3),mean)
aggregate(wine[,-1],list(comp3),mean)

##### K Means Clustering #####
numKmeans <- NbClust(df,min.nc = 2,max.nc = 15, method = "kmeans")
set.seed(1234)
km <- kmeans(df,3,nstart = 25)
table(km$cluster)

km$centers
boxplot(wine$Alcohol ~ km$cluster, data = wine, main= "Alchohole content - Kmeans")
boxplot(wine$Alcohol ~ ward3, data = wine, main= "Alchohole content - Kmeans")

table(km$cluster, wine$Class)

### Gower and Pam ####
wine$Alcohol <- as.factor(ifelse(df$Alcohol>0, "High","Low"))
disMatrix <- daisy(wine[,-1], metric = "gower")
set.seed(123)
pamFit <- pam(disMatrix, k=3)
table(pamFit$clustering)
table(pamFit$clustering,wine$Class)

wine$cluster <- pamFit$clustering
group <- compareGroups(cluster ~ ., data= wine)
