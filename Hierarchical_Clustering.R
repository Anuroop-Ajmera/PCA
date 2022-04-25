library(dplyr) # for glimpse etc.
#library(tidyr) # for drop_na etc.
library(funModeling) # for df_status etc.

universities_df <- read.csv("./Universities.csv")

head(universities_df)

#str(universities_df)

glimpse(universities_df)

# health status of universities data to check variables with NA values
df_status(universities_df)

#######
#Q1
#######

##a)

universities_complete_df <- universities_df[complete.cases(universities_df), ]
glimpse(universities_complete_df)
df_status(universities_complete_df)

# We can see that NA values are removed from Universities data and result 
# is saved in universities_complete_df dataframe

##b) 

#Let us normalize the data first
universities_complete_norm_df <- sapply(universities_complete_df[,-(1:3)], scale)
head(universities_complete_norm_df)


# cluster analysis
universities_complete_norm_df_dist <- dist(universities_complete_norm_df, method = "euclidean")
#universities_complete_norm_df_dist
#str(universities_complete_norm_df_dist)
hclust1 <- hclust(universities_complete_norm_df_dist, method = "complete")
plot(hclust1, hang = -1, ann = FALSE)
member <- cutree(hclust1, k = 4) # cut 4 clusters
#head(member)
#str(member)
table(member)
#head(universities_complete_norm_df)

##Conclusion: 
# 4 clusters seem reasonable for describing this data. 
# Only 1 observation each in cluster 3 and cluster 4 showing that those may be outliers


##c)
# Let us compare the cluster centroids to characterize the different clusters,
##and try to give  each cluster a label
centroids <- aggregate( . ~ member, data = universities_complete_df[,-(1:3)], FUN = mean)
centroids


##d)
table(universities_complete_df$State,member)
table(universities_complete_df$Public..1...Private..2.,member)
#table(universities_complete_df$College.Name,member)


##e)
#Few external factors that looks important are-
#University rankings --> higher rankings can help in better explanation
#Jobs placement services --> better services can help in better explanation


##f)
universities_df
#tufts_dist_df <- universities_df %>% filter(State == "AK")
tufts_df <- universities_df %>% filter(College.Name == "Tufts University")
tufts_df
#tufts_dist_df$member <- 0
tufts_num_df <- tufts_df[,-(1:3)]
tufts_num_df
tufts_member_df <- cbind(member=0,tufts_num_df)
tufts_member_df
tufts_centroids_df <- rbind(tufts_member_df,centroids)
tufts_centroids_df

tufts_centroids_df_dist <- dist(tufts_centroids_df, method = "euclidean")
tufts_centroids_df_dist

#1         2         3         4
#2 15219.723                              
#3 27141.517 26027.078                    
#4 15418.884  4886.475 29986.232          
#5 24401.459 27769.716 21434.574 30956.635

# By looking at the above, we can see that observation of Tufts University
# is closest to cluster 1 (i.e. 2nd record in above as 1st record is for Tufts)

# Let's impute PT.undergrad attribute for Tufts record from 1st centroid as this is the only missing value
#tufts_centroids_df$X..PT.undergrad

cluster1_PT.undergrad <- tufts_centroids_df %>% filter(member == 1) %>% select(X..PT.undergrad)
cluster1_PT.undergrad

#universities_df$College.Name == "Tufts University"

#universities_df$X..PT.undergrad[universities_df$College.Name == "Tufts University"]

universities_df$X..PT.undergrad[universities_df$College.Name == "Tufts University"] <- cluster1_PT.undergrad

universities_df %>% filter(College.Name == "Tufts University") %>% select(X..PT.undergrad)
universities_df %>% filter(College.Name == "Tufts University")
