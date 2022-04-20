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

#######
#Q2
#######
toyota_corolla_df = read.csv("ToyotaCorolla.csv")

glimpse(toyota_corolla_df)

df_status(toyota_corolla_df)

##a)

#catagorical_vars <- (unlist(lapply(toyota_corolla_df, is.character)))

#names(which(sapply(toyota_corolla_df, class) == "factor"))

#catagorical_vars %>% filter(FALSE)


catagorical_vars <- sapply(toyota_corolla_df, is.character)
catagorical_vars
categorical_df <- toyota_corolla_df[,catagorical_vars]
head(categorical_df)

#Model, Fuel_Type and Color are categorical variables

##b) 

#A dummy variable, also called as indicator variable is like an artificial attribute created with two 
#or more distinct categories or levels.
#A variable having N categories can be transformed into N or N-1 dummy variables where each dummy variable
#will represent if a certain category is present or not. 
#e.g. Stock recommendation: Strong buy, Strong sell, Moderate buy, Moderate Sell, Hold.
#If we create dummy variables for stock recommendation, we will get 5 dummy variables: 
# stock_recom_strong_buy (if the stock recommendation is Strong buy then stock_recom_strong_buy=1, otherwise stock_recom_strong_buy=0),
# stock_recom_strong_sell (if the stock recommendation is Strong sell then stock_recom_strong_sell=1, otherwise stock_recom_strong_sell=0),
# stock_recom_moderate_buy (if the stock recommendation is Moderate buy then stock_recom_moderate_buy=1, otherwise stock_recom_moderate_buy=0),
# stock_recom_moderate_sell (if the stock recommendation is Moderate sell then stock_recom_moderate_sell=1, otherwise stock_recom_moderate_sell=0),
# stock_recom_hold (if the stock recommendation is Hold then stock_recom_hold=1, otherwise stock_recom_hold=0).
# We can create only N-1 dummy variables for N categorical variables. These dummy variables can have
# Yes/No or 1/0 for each of N-1 respective category and the last category will have all N (or 0).

##c)

#N or N-1 dummy binary variables are required to capture the information in a categorical variable with N categories.
#In some situations like linear regression, use of all N dummy variables will cause failure because the nth variable 
#contains redundant information and can be expressed as a linear combination of the others.
# Ideally N-1 variables should be used as they contain all the available information about the variable 
# from which they were derived.
# eg. say Salary attribute has 3 values High, medium, low. 2 dummy variable can be defined as Salary_high (with value 1 or 0),
# Salary_medium (with value 1 or 0), observations with 0 in salary_high and salary_medium are having Salary value as Low.

##d) 
#install.packages("dummies")
library(dummies)

#toyota_corolla_dummy_df <- dummy.data.frame(toyota_corolla_df,sep='.')
#names(toyota_corolla_dummy_df)

# above command is creating dummy variable for Model also (in addition to Fuel_Type and Color) 
# which has a lot of different values so let's select Fuel_Type and COlor only for 
# creating dummy variables
toyota_corolla_dummy_df <- dummy.data.frame(toyota_corolla_df, names = c("Fuel_Type","Color") , sep = ".")
names(toyota_corolla_dummy_df)
head(toyota_corolla_dummy_df)

# Let's describe the values in dummy variables for 1st record
# Fuel_Type.Diesel = 1 whereas Fuel_Type.CNG = 0 and Fuel_Type.Petrol = 0 which means this
# Toyota Corolla version is a Diesel vehicle.
# Color.Blue = 1 whereas the value is 0 for all other dummy variables like 
# Color.Beige, Color.Black, Color.Green, Color.Grey, Color.Red, Color.Silver, Color.Violet, 
# Color.White and Color.Yellow which means this car is of Blue color.

##e)

toyota_corolla_df_subset = subset(toyota_corolla_df, select = -c(Id,Model, Fuel_Type, Color,Met_Color,
                                                                 Automatic, Cylinders, Gears, Mfr_Guarantee,
                                                                 Dealer_Guarantee, ABS, Airbag_1,
                                                                 Airbag_2, Aircond, Automatic_aircond,
                                                                 Boardcomputer, CD_Player,Central_Lock,
                                                                 Powered_Windows, Power_Steering, Radio,
                                                                 Mistlamps, Sport_Model, Backseat_Divider,
                                                                 Metallic_Rim, Radio_cassette, Parking_Assistant,
                                                                 Tow_Bar)) 
head(toyota_corolla_df_subset)

toyota_corolla_complete_df <- toyota_corolla_df_subset[complete.cases(toyota_corolla_df_subset), ]
toyota_corolla_complete_df
glimpse(toyota_corolla_complete_df)

#is.nan(toyota_corolla_complete_df$Price)

cor(toyota_corolla_complete_df)
## Let's visualize the correlation matrix created above
library(corrplot)
cor.vis <- round(cor(toyota_corolla_complete_df),2)

library(ggcorrplot)
ggcorrplot(cor.vis,hc.order = TRUE,
           type = "upper",
           lab = TRUE,
           lab_size = 2)

#lab_size is used to control size of coefficients in the plot

### Conclusion ###
# Looking at the visualization we can identify the following:

#There is a moderately positive correlation between Age..month and KM (0.51), which is intuitive as 
# it's not always true that an old car would have run for more KMs than a comparatively new car
#There is a very high negative correlation between Age..month and Price (-0.88), which is intuitive
#There is a very high negative correlation between Age..month and Mfg_Year (-0.96), which is intuitive

#There is a highly negative correlation between KM and Price (-0.57), which is intuitive
#There is a highly positive correlation between Price and Mfg_Year (0.89), which is intuitive
#There is a moderately positive correlation between Price and Weight (0.58), which is intuitive as 
# it's not always true that an heavy cars would be more expensive

#There is a moderately positive correlation between Weight and Quarterly_Tax (0.63), which is intuitive as
# in general, higher the weight, higher the tax


#######
#Q3
#######

#a)

# As a starting configuration is a subset of 3 starting points out of 6 from S,
# total number of starting configurations would be C(6,3)

choose(6,3)
# Result is 20

#b)

library(gtools)

combinations(6, 3, letters[1:6], set=TRUE, repeats.allowed=FALSE)

a <- c(0,0)
b <- c(8,0)
c <- c(16,0) 
d <- c(0,6)
e <- c(8,6) 
f <- c(16,6)

x<-c(0,8,16,0,8,6)
y<-c(0,0,0,6,6,6)
df <- data.frame(x,y)



my_cent1 <- matrix(c(0,8,16,0,0,0), 3, 2)
my_cent2 <- matrix(c(0,8,0,0,0,6), 3, 2)
my_cent3 <- matrix(c(0,8,8,0,0,6), 3, 2)
my_cent4 <- matrix(c(0,8,16,0,0,6), 3, 2)
my_cent5 <- matrix(c(0,8,0,0,0,6), 3, 2)
my_cent6 <- matrix(c(0,16,8,0,0,6), 3, 2)
my_cent7 <- matrix(c(0,16,16,0,0,6), 3, 2)
my_cent8 <- matrix(c(0,0,8,0,6,6), 3, 2)
my_cent9 <- matrix(c(0,0,16,0,6,6), 3, 2)
my_cent10 <- matrix(c(0,8,16,0,6,6), 3, 2)
my_cent11<- matrix(c(8,16,0,0,6,6), 3, 2)
my_cent12<- matrix(c(8,16,8,0,0,6), 3, 2)
my_cent13<- matrix(c(8,16,16,0,0,6), 3, 2)
my_cent14<- matrix(c(8,0,8,0,0,6), 3, 2)
my_cent15<- matrix(c(8,0,16,0,6,6), 3, 2)
my_cent16<- matrix(c(8,8,16,0,6,6), 3, 2)
my_cent17<- matrix(c(16,0,8,0,6,6), 3, 2)
my_cent18<- matrix(c(16,0,16,0,6,6), 3, 2)
my_cent19<- matrix(c(16,8,16,0,6,6), 3, 2)
my_cent20<- matrix(c(0,8,16,6,6,6), 3, 2)


my_centre <- list(my_cent1,my_cent2,my_cent3,my_cent4,my_cent5,my_cent6,my_cent7,my_cent8,
                my_cent9,my_cent10,my_cent11,my_cent12,my_cent14,my_cent15,my_cent16,my_cent17,my_cent20)

max_iter <- 10
my_df <- df
for (outer in 1:16){
  
  for (inner in 1:max_iter){
    tryCatch({
      #dfCluster <- kmeans(df,centers = my_centres'i', iter.max = max_iter)
      tryCatch({dfCluster <- kmeans(df,centers = my_centre[[outer]], iter.max = max_iter)},
               warning = function(e) "Caught error")
      
      done <- TRUE
    }, 
    warning=function(w) {done <- FALSE})
    plot(df[,1], df[,2], col=dfCluster$cluster,pch=19,cex=2, main=paste("iter",inner))
    points(dfCluster$centers,col=1:5,pch=3,cex=3,lwd=3)
    if(done) break
  }
  #print("inner loop ran ",inner," times"
  print(paste0("inner loop ran ",inner," times"))
  centroid <- dfCluster$cluster
  my_df[ , paste0("centroid", outer)]<- centroid
}

my_df

#> my_df
#x y centroid1 centroid2 centroid3 centroid4 centroid5 centroid6 centroid7 centroid8 centroid9
#1  0 0         1         1         1         1         1         1         1         1         1
#2  8 0         2         3         3         2         3         3         3         2         1
#3 16 0         3         2         2         3         2         2         2         3         3
#4  0 6         1         1         1         1         1         1         1         1         2
#5  8 6         2         3         3         2         3         3         3         2         2
#6  6 6         2         3         3         2         3         3         3         2         2
#centroid10 centroid11 centroid12 centroid13 centroid14 centroid15 centroid16
#1          1          3          1          2          2          1          2
#2          2          1          1          3          1          1          3
#3          3          2          2          1          3          3          1
#4          1          3          3          2          2          2          2
#5          2          1          3          3          1          2          3
#6          2          1          3          3          1          2          3


# Above iteration shows that we have got stable cluster when we took starting points as 
# Cluster partition is: {a,d}, {b,e,f} and {c}
# Cluster partition is: {a,b}, {d,e,f} and {c}
# 16 centroids were taken out of 20 because other 4 were resulting in empty clusters

##c)
# 13 starting configurations are leading to stable partition {a,d}, {b,e,f} and {c}
# 3 starting configurations are leading to stable partition {a,b}, {d,e,f} and {c}


##d)
#It took maximum of 1 iteration for each configuration to reach stable partition.

