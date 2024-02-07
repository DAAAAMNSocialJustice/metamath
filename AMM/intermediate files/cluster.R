#Trying kmeans
#install.packages(c("tidyverse", "cluster", "factoextra", "fclust"))

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(fclust) #fuzzy clustering
library(ggrepel) #for non overlapping labels

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
k3plot <-fviz_cluster(k3, data = gf_df) +
  labs(y = "Total NSF funding (scaled)", x = "Proportion of Women (scaled)", title = "") + 
  theme_light()


# Tutorial: https://remiller1450.github.io/s230s19/clustering_tutorial.html
# example from documention of fviz_cluster
#  p <- ggpubr::ggline(df, x = "clusters", y = "gap", group = 1, color = linecolor)+
#ggplot2::geom_errorbar(aes_string(ymin="ymin", ymax="ymax"), width=.2, color = linecolor)+
#  geom_vline(xintercept = k, linetype=2, color = linecolor)+
#  labs(y = "Gap statistic (k)", x = "Number of clusters k",
#       title = "Optimal number of clusters")

# trying to fix legend
# tutorial https://stackoverflow.com/questions/53572037/adjusting-output-in-fviz-cluster
# p2 + geom_text(data=p2$data, aes(x=x, y=y, label=name, colour=cluster),vjust=-1, show.legend = F)

k3leg <- fviz_cluster(k3, data = gf_df, geom = c("point")) +
  scale_color_brewer('Cluster', palette='Set1') + 
  scale_fill_brewer('Cluster', palette='Set1') +
  scale_shape_manual('Cluster', values=c(22,23,24)) + 
  ggtitle(label='') +
  geom_text_repel(data=k3$data, aes(x=x, y=y, label=name, colour=cluster), show.legend = F, force_pull = 3, size = 2.8, min.segment.length = Inf) +
  theme_light() + theme(legend.position = c(0.8, 0.7)) +
  labs(y = "Total NSF funding (scaled)", x = "Proportion of Women (scaled)", title = "")

k3leg

#how well did the clustering analysis identify "elite" institutions
cluster3_df <- cbind.data.frame(k3$cluster, df$elite)
agg3_df <- table(cluster3_df)

#Lower left cluster
# 32.5% of all elites are in this cluster and 25% in the whole sample.  That feels like
# 66.1 % of all non elites are in this cluster

#Lower right cluster:
#everything in the cluster on the lower right is 100% 
#categorized as non-elite. 

#Upper left cluster:
#About 75% of everything in red was on the "elite" list. 
#So there are 9 schools that were "non-elite" categories that 
#were in the red category. Another way of looking at that is of 
#the "elites" - about 67.5% were put into the red cluster. 




### Fuzzy clustering - good for when there are some points that you don't 
#know which cluster they belong to
#fc2cm<-Fclust(X=gf_df,k=2, type="standard",noise=TRUE)
#cluster2f_df <- cbind.data.frame(fc2cm$clus, df$elite)
#agg2f_df <- table(cluster2f_df)
#nothing really useful any more than kmeans