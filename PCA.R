library(dplyr) # for glimpse etc.
#library(tidyr) # for drop_na etc.
library(funModeling) # for df_status etc.


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
