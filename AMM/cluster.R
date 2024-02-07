
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


#Only needed to put elite number labels on graph
library(ggrepel) #for non overlapping labels

#read in df from intermediate data files
df <- read.csv("intermediate files/combined_df.csv")

#adds elite category based on rank
df$elite <- df$Rescaled.Rank < 40  #(recall rank indexes at 0)

# clustering analysis just based on % women and funding
# scale data to standard normal for clustering
gf_df<-cbind.data.frame(scale(df$Percentage.of.Women), scale(df$AwardedAmount))


### K Means clustering
##  Check how many clusters this should have by plotting sum of squares
set.seed(123)

# function to compute total within-cluster sum of square 
fviz_nbclust(gf_df, kmeans, method = "wss")
# sillhouette method instead
fviz_nbclust(gf_df, kmeans, method = "silhouette")
#Gap stat suggested cluster of 1, but actually 3 was a better statistic
#Result: Majority suggest that the "bend" in the knee happens for 3 clusters

k3 <- kmeans(gf_df, centers = 3, nstart = 25)
k3leg <- fviz_cluster(k3, data = gf_df, geom = c("point")) +
  scale_color_brewer('Cluster', palette='Set1') + 
  scale_fill_brewer('Cluster', palette='Set1') +
  scale_shape_manual('Cluster', values=c(22,23,24)) + 
  ggtitle(label='') +
#  geom_text_repel(data=k3$data, aes(x=x, y=y, label=name, colour=cluster), show.legend = F, force_pull = 3, size = 2.8, min.segment.length = Inf) +
  theme_light() + theme(legend.position = c(0.8, 0.7)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "NSF DMS Average Annual Funding (scaled)", x = "Percentage of Women (scaled)", title = "")

ggsave("figures/cluster3.png", k3leg)

#how well did the clustering analysis identify "elite" institutions
cluster3_df <- cbind.data.frame(k3$cluster, df$elite)
names(cluster3_df) <- c("Cluster", "Elite")
agg3_df <- table(cluster3_df)
print(agg3_df)
chisq.test(agg3_df)


#Lower left cluster
#32.5% of all elites are in this cluster and 25% in the whole sample.  
#66.1 % of all non elites are in this cluster

#Lower right cluster:
#everything in the cluster on the lower right is 100% 
#categorized as non-elite. 

#Upper left cluster:
#About 75% of everything in red was on the "elite" list. 
#So there are 9 schools that were "non-elite" categories that 
#were in the red category. Another way of looking at that is of 
#the "elites" - about 67.5% were put into the red cluster. 

