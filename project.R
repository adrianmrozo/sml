#library(ISLR)
#library(MASS)
#library(ggplot2)
library(mice)
library(tidyverse)
print(getwd())

setwd("/Users/a.kotatis/Desktop/Studium/Master/1. Semester/Supervised Machine Learning/Final Project")

dforiginal <- read.csv("training.csv")
df <- dforiginal
#dropping all the variables/columns that which we discussed in the group that will not be used.
drop <- c("GDENAMK", "GDENR", "address", "appartments", "area_useable", "basement", "bath", "bath_tube", "bright", "building_plot", "cabletv", "ceiling", "cheminee", "date", "descr", "dishwasher", "dryer", "furnished", "garden_m2", "gardenshed", "heating_air", "heating_earth", "heating_electro", "heating_far", "heating_gas", "heating_oil", "heating_pellets", "lat", "laundry", "lon", "manlift", "middle_house", "minergie", "new_building", "oldbuilding", "oven", "pets", "playground", "pool", "public_transport", "quarter_general", "quarter_specific", "quiet", "shared_flat", "shopping", "shower", "size_land", "sunny", "terrace", "toilets", "topstorage", "veranda", "water", "wheelchair", "year", "dist_to_lake", "dist_to_main_stat", "geb_wohnnutz_total")
df = df[,!(names(df) %in% drop)]

#Setting in the three columns NA to 0, which we agreed in the group: balcony, elevator, kids_friendly
df$balcony[is.na(df$balcony)] <- 0
df$elevator[is.na(df$elevator)] <- 0
df$kids_friendly[is.na(df$kids_friendly)] <- 0
df$parking_indoor[is.na(df$parking_indoor)] <- 0
df$parking_outside[is.na(df$parking_outside)] <- 0
df$raised_groundfloor[is.na(df$raised_groundfloor)] <- 0

#Overview of the remaining NA values
NAperc <- df.ou1 %>% summarize_all(funs(sum(is.na(.))/length(.)))

summary(df)

#creating sq2rent to look for outliers
df$sq2rent <- df$rent_full/df$area
summary(df$sq2rent)
#it would be logical for a rent to not be higher than 200 per sq2 or lower than 15
df <- subset(df, sq2rent > 15 & sq2rent<200)
#we can again delete this column
df <- df[,-43]
summary(df)
#area and room still have some crazy values, lets delete some more outliers
summary(df$area) #250 & 5
boxplot(df$area)
summary(df$rooms)#10 & 1
boxplot(df$rooms)
df <- subset(df, rooms > 0 & rooms<11) 
df <- subset(df, area > 5 & area < 250)
summary(df)

#for the missing values we will impute them
df.out <- mice(df, method="rf", m = 1)  # perform mice imputation, based on random forests. takes some time but its worth :)) 
df.out <- complete(df.out)  # generate the completed data.
df.out <- df.out%>%select(-wgh_avg_sonnenklasse_per_egid.1)
anyNA(df.out)


#Prediction models needing to set seed beforehand.
set.seed(123)
model1 <- train(rent_full~., data=df.out, trControl=trainControl(method = "cv", number = 5), method="leapBackward")
print(model1)

model2 <- train(rent_full~., data=df.out, trControl=trainControl(method = "cv", number = 5), method="leapForward")
print(model2)

model3 <- train(rent_full~., data=df.out, trControl=trainControl(method = "cv", number = 5), method="bstTree")
print(model3)

model4 <- train(rent_full~., data=df.out, trControl=trainControl(method = "cv", number = 5), method="rpart")
print(model4)


#not yet testes (takes also way too long)
library(randomForest)
model5 <- randomForest(rent_full~., df.out)
print(model5)



