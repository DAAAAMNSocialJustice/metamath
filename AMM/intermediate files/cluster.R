#Trying kmeans
#install.packages(c("tidyverse", "cluster", "factoextra", "fclust"))

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(fclust) #fuzzy clustering

#read in df from intermediate data files
df <- read.csv("intermediate files/combined_df.csv")

#adds elite category based on rank
df$elite <- df$Rescaled.Rank < 40  #(recall rank indexes at 0)

# clustering analysis just based on % women and funding
# scale data to standard normal for clustering
gf_df<-cbind.data.frame(scale(df$Percentage.of.Women), scale(df$AwardedAmount))


### K Means clustering
k2 <- kmeans(gf_df, centers = 2, nstart = 25)
str(k2)

#Result: identifies one cluster that has varied percentages of women, 
#but a boundary on the amount of money the institutions are awarded
#And another cluster that has access to funding, but a narrow range
# of representation of women

#visualize
fviz_cluster(k2, data = gf_df)

#how well did the clustering analysis identify "elite" institutions
cluster_df <- cbind.data.frame(k2$cluster, df$elite)
agg_df <- table(cluster_df)

#note that the stochastic nature of the clustering means 
#that the table aggregates differently each time
#(1 and 2)
elite_true <- 31/40
elite_false <- 9/40
nonelite_true <- 112/121
nonelite_false <- 9/121

#Result - even though the initial prestige rankings were not based on women, 
#nor funding levels, they are very closely predicted by the two variables
#make some comments on the ones it did not identify correctly

##  Check how many clusters this should have by plotting sum of squares
set.seed(123)

# function to compute total within-cluster sum of square 
fviz_nbclust(gf_df, kmeans, method = "wss")
# sillhouette method instead
fviz_nbclust(gf_df, kmeans, method = "silhouette")
#Gap stat suggested cluster of 1, but actually 3 was a better statistic
#Result: Majority suggest that the "bend" in the knee happens for 3 clusters

k3 <- kmeans(gf_df, centers = 3, nstart = 25)
str(k3)
#visualize
fviz_cluster(k3, data = gf_df)

#how well did the clustering analysis identify "elite" institutions
cluster3_df <- cbind.data.frame(k3$cluster, df$elite)
agg3_df <- table(cluster3_df)


#Cluster 2:
#everything in the green cluster on the lower right is 100% 
#"correctly" categorized as non-elite. 
#Cluster 1:
#About 75% of everything in red was on the "elite" list. 
#So there are 9 schools that were "non-elite" categories that 
#were in the red category. Another way of looking at that is of 
#the "elites" - about 67.5% were put into the red cluster. 
#Cluster 3
#In the blue cluster, 32 are non-elite and 13 are elite.  So it is 
#about 29% elite vs about 25% in the whole sample.  That feels like
#it could be assigned by random chance.


### Fuzzy clustering - good for when there are some points that you don't 
#know which cluster they belong to
#fc2cm<-Fclust(X=gf_df,k=2, type="standard",noise=TRUE)
#cluster2f_df <- cbind.data.frame(fc2cm$clus, df$elite)
#agg2f_df <- table(cluster2f_df)
#nothing really useful any more than kmeans