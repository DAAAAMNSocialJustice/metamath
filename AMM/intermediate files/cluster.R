#Trying kmeans
#install.packages("tidyverse", "cluster", "factoextra")

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#read in df from intermediate data files
df <- read.csv("intermediate files/combined_df.csv")

#adds elite category based on rank
df$elite <- df$Rescaled.Rank < 40  #(recall rank indexes at 0)

# clustering analysis just based on %women and funding
gf_df<-select(df, Percentage.of.Women, AwardedAmount)
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
elite_true <- 27/40
elite_false <- 13/40
nonelite_true <- 112/121
nonelite_false <- 9/121

#Result - even though the initial prestige rankings were not based on women, 
#nor funding levels, they are very closely predicted by the two variables
#make some comments on the ones it did not identify correctly