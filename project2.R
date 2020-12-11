#library(ISLR)
#library(MASS)
#library(ggplot2)

library(caret)
library(tidyverse)
print(getwd())

setwd("/Users/a.kotatis/Desktop/Studium/Master/1. Semester/Supervised Machine Learning/Final Project")

dforiginal <- read.csv("training.csv")
X_test <- read.csv("X_test.csv")
df <- dforiginal
#dropping all the variables/columns that which we discussed in the group that will not be used.
drop <- c("GDENAMK", "GDENR", "address", "appartments", "area_useable", "basement", "bath", "bath_tube", "bright", "building_plot", "cabletv", "ceiling", "cheminee", "date", "descr", "dishwasher", "dryer", "furnished", "garden_m2", "gardenshed", "heating_air", "heating_earth", "heating_electro", "heating_far", "heating_gas", "heating_oil", "heating_pellets", "lat", "laundry", "lon", "manlift", "middle_house", "minergie", "new_building", "oldbuilding", "oven", "pets", "playground", "pool", "public_transport", "quarter_general", "quarter_specific", "quiet", "shared_flat", "shopping", "shower", "size_land", "sunny", "terrace", "toilets", "topstorage", "veranda", "water", "wheelchair", "year", "dist_to_lake", "dist_to_main_stat", "geb_wohnnutz_total")
df = df[,!(names(df) %in% drop)]
df_nas_dropped <- df[, which(colMeans(!is.na(df)) > 0.8)]
for(i in 1:ncol(df_nas_dropped)){
  df_nas_dropped[is.na(df_nas_dropped[,i]), i] <- mean(df_nas_dropped[,i], na.rm = TRUE)
}
NAperc <- df_nas_dropped %>% summarize_all(funs(sum(is.na(.))/length(.)))


#Setting in the three columns NA to 0, which we agreed in the group: balcony, elevator, kids_friendly


#Overview of the remaining NA values


summary(df_nas_dropped)

#creating sq2rent to look for outliers
df_nas_dropped$sq2rent <- df_nas_dropped$rent_full/df_nas_dropped$area
summary(df_nas_dropped$sq2rent)
#it would be logical for a rent to not be higher than 200 per sq2 or lower than 15
df_nas_dropped <- subset(df_nas_dropped, sq2rent > 15 & sq2rent<200)
#we can again delete this column
df_nas_dropped <- df_nas_dropped[,-43]
summary(df_nas_dropped)
#lets delete some more outliers
summary(df_nas_dropped$area) 
boxplot(df_nas_dropped$area)
summary(df_nas_dropped$rooms)
boxplot(df_nas_dropped$rooms)
summary(df_nas_dropped$rent_full)
df_nas_dropped <- subset(df_nas_dropped, rooms > 0 & rooms<11) 
df_nas_dropped <- subset(df_nas_dropped, rent_full > 200 & rent_full < 6000)
summary(df_nas_dropped)
df_nas_dropped <- df_nas_dropped[,-35]

anyNA(df_nas_dropped)


#Prediction models needing to set seed beforehand.
set.seed(123)
model1 <- train(rent_full~., data=df_nas_dropped, trControl=trainControl(method = "cv", number = 5), method="leapBackward")
print(model1)

model2 <- train(rent_full~., data=df_nas_dropped, trControl=trainControl(method = "cv", number = 5), method="leapForward")
print(model2)

model3 <- train(rent_full~., data=df_nas_dropped, trControl=trainControl(method = "cv", number = 5), method="bstTree")
print(model3)

model4 <- train(rent_full~., data=df_nas_dropped, trControl=trainControl(method = "cv", number = 5), method="rpart")
print(model4)


ourprediction <- predict(model1, newdata=X_test)
ourprediction <- as.data.frame(ourprediction)
ourprediction$ID <- seq.int(index(ourprediction))
ourprediction <- ourprediction[,c(2,1)]
ourprediction <- ourprediction%>%rename(rent=ourprediction)
write.csv(ourprediction, "Y_test.csv", row.names = FALSE)



#not yet testes (takes also way too long)
library(randomForest)
model5 <- randomForest(rent_full~., df_nas_dropped)
print(model5)
