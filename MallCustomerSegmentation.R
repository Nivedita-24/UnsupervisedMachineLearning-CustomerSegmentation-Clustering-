## PROJECT SUMMARY AND TASK DETAIL
#----------------------------------------------------------------------------------------------
## GOAL : 
# The goal of this analysis is to identify customer segments using K-Means Clustering, in order to support marketing team in planning the marketing strategies.
#----------------------------------------------------------------------------------------------

## R Packages
library(ggplot2)
library(plyr)
library(plotrix)
library(caret)
library(purrr)

## Reading the data
customersData <- read.csv("Mall_Customers.csv")
head(customersData)
tail(customersData)
dim(customersData)

# Structure and Summary of the data
str(customersData)
summary(customersData)

#----------------------------------------------------------------------------------------------
## DATA VISUALIZATION

# Analysis of customers based on Gender
# Barplot
ggplot(customersData, aes(Gender)) + geom_bar(fill=c("pink","lightblue")) 

# Piechart
gender<-table(customersData$Gender)
pct = round(gender/sum(gender)*100)
lbs = paste(c("Female","Male")," ",pct,"%",sep = " ")

pie3D(gender,labels = lbs, main ="pie Chart Depicting Ratio of Female and Male", col= c("red","orange"))

# It can be observed that 56% of the customer in the dataset are females and rest 44% are males.

# Analysis of Age of customers.
#Histogram
ggplot(MallCustomers, aes(x=Age)) + 
  geom_histogram(binwidth=5,aes(label=..count..),color="white", fill="cadetblue4")+
  stat_bin(binwidth= 5, geom="text", aes(label=..count..) , 
           vjust = -1) +
  scale_x_continuous(breaks = round(seq(min(MallCustomers$Age), max(MallCustomers$Age), by = 5),1))

#Boxplot
ggplot(MallCustomers, aes(y=Age)) + 
  geom_boxplot(fill="red3")+
  scale_y_continuous(breaks = round(seq(min(MallCustomers$Age), max(MallCustomers$Age), by = 5),1))

# We can conclude that the maximum no. of customers are between 28-33 Years. Minimum age of the customer is 18 and the maximum is 70 years.

## Analysis of Annual Income of customers
#Histogram
ggplot(MallCustomers, aes(x=Annual.Income..k..)) + 
  geom_histogram(binwidth=10,aes(label=..count..),color="white", fill="orange")+
  stat_bin(binwidth= 10, geom="text", aes(label=..count..) , 
           vjust = -1) + 
  scale_x_continuous(breaks = round(seq(min(MallCustomers$Annual.Income..k..), max(MallCustomers$Annual.Income..k..), by = 10),1))

#Densityplot
ggplot(MallCustomers, aes(x=Annual.Income..k..)) + 
  geom_density(fill="olivedrab1") + 
  scale_x_continuous(breaks = round(seq(min(MallCustomers$Annual.Income..k..), max(MallCustomers$Annual.Income..k..), by = 10),1))


## It can be concluded that minimum annual income is 15 and maximum is 137. The people earning an of 55-65 have the highest frequency count in the dataset.
## From the density plot we can see that the annual income of customers has a normal distribution.

## Analysing spending score of customers

#Histogram
ggplot(MallCustomers, aes(x=Spending.Score..1.100.)) + 
  geom_histogram(binwidth=10,aes(label=..count..),color="white", fill="cadetblue4")+
  stat_bin(binwidth= 10, geom="text", aes(label=..count..) , 
           vjust = -1) +
  scale_x_continuous(breaks = round(seq(min(MallCustomers$Spending.Score..1.100.), max(MallCustomers$Spending.Score..1.100.), by = 10),1))

#Boxplot
ggplot(MallCustomers, aes(y=Spending.Score..1.100.)) + 
  geom_boxplot(fill="lightblue")+ coord_flip()+
  scale_x_continuous(breaks = round(seq(min(MallCustomers$Spending.Score..1.100.), max(MallCustomers$Spending.Score..1.100.), by = 10),1))

# Customers has a minimum spending score 1 and maximum 99, the customers count is maximum between 45 to 55 spending score.

#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# DATA PREPROCESSING

# set row names to the customerID
row.names(customersData) <- customersData[,1]
summary(customersData)

customersData <- customersData[,-1]
summary(customersData)
row.names(customersData)

# normalizing the variables

customersData.norm <- preProcess(customersData[,-1], method=c("center", "scale"))
customersData.norm.df <- predict(customersData.norm, customersData[,-1])

summary(customersData.norm.df)
#----------------------------------------------------------------------------------------------
# K-MEANS CLUSTERING ALGORITHM

# While using the K-means clustering algorithm, the first step is to indicate the number of clusters (k).
## Elbow Method:

set.seed(100)
iss <- function(k){
  kmeans(customersData.norm.df,k,iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
}

k.values <-1:10

iss_values <- map_dbl(k.values,iss)
plot(k.values,iss_values,type = "b",pch =19, frame= FALSE, xlab = "Number of Clusters K", ylab ="Total Intra Clusters Sum of Squares", col ="#1287A5")

# we conclude that 6 is the appropriate number of clusters since it seems to be appearing at the bend in the elbow plot.
# Adding more clusters beyond 6 brings less improvement to cluster homogeneity.

# K-MEANS MODEL
set.seed(100)
customerskm <- kmeans(customersData.norm.df, 6)

#----------------------------------------------------------------------------------------------
## EVALUATING MODEL PERFORMANCE

## Cluster Size
customerskm$size
# Here, we see the six clusters we requested. The smallest cluster has 21 customers (11 percent) while the largest cluster has 45 customers (23 percent). 
# The gap between the number of customers in the largest and smallest clusters is quite small.

## Cluster Centroids
# For a more in-depth look at the clusters, we can examine the coordinates of the cluster centroids.
customerskm$centers

# The rows of the output (labeled 1 to 6) refer to the six clusters, while the numbers across each row indicate the cluster’s average value for the interest listed at the top of the column.
# As the values are z-score standardized, positive values are above the overall mean level for all the customers and negative values are below the overall mean.
# By examining whether the clusters fall above or below the mean level for each interest category, we can begin to notice patterns that distinguish the clusters from each other.

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(customerskm$centers), max(customerskm$centers)), xlim = c(0, 5))

# label x-axes
axis(1, at = c(1:3), labels = names(customersData.norm.df))

# plot centroids
for (i in c(1:6)){
  lines(customerskm$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                           "red", "dark grey"))}

# name clusters
text(x = 0.5, y = customerskm$centers[, 1], labels = paste("Cluster", c(1:6)))

dist(customerskm$centers)

customerskm
## we conclude the useful information being –

# cluster – This is a vector of several integers that denote the cluster which has an allocation of each point.
# totss – This represents the total sum of squares.
# centers – Matrix comprising of several cluster centers
# withinss – This is a vector representing the intra-cluster sum of squares having one component per cluster.
# tot.withinss – This denotes the total intra-cluster sum of squares.
# betweenss – This is the sum of between-cluster squares.
# size – The total number of points that each cluster holds.

customerskm$withinss
sum(customerskm$withinss)
mean(customerskm$withinss)

#It can be inferred that; we have 77.7% sum of squares between the clusters, which is very high. Around
# 22% of sum of squares are actually within the clusters.

#----------------------------------------------------------------------------------------------
## VISUALIZING CLUSTERS
set.seed(1)
ggplot(customersData, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(customerskm$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

#----------------------------------------------------------------------------------------------
## CONCLUSION
## Cluster 1 : This cluster represents the customer_data having a low annual income as well as a low annual spend. 
## Cluster 2 : This cluster represents the customer_data having a high annual income as well as a high annual spend. 
## cluster 3 : This cluster represents the customer_data having a high annual income as well as a low annual spend.
## Cluster 4 : This cluster represents the customer_data having a low annual income as well as a high annual spend.
## Cluster 5&6 : This cluster represents the customer_data having a medium annual income as well as a medium annual spend. 

