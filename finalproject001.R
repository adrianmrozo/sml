library(ISLR)
library(MASS)
library(ggplot2)

print(getwd())
library("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))

#or
#getwd()
#and paste in manually in here:
#setwd("PASTEHERE")

list.files()
list.files(pattern=".csv")

df <- read.csv("training.csv")


#Lets create a very obvious additional column, that is not yet part of the dataset, price per squaremeter

df$pricepersquaremeter <- df$rent_full/df$area


#Lets check if there are any rows without any rent prices:

checkifthereareemptyvaluesforrent <- df[is.na(df$rent_full), ]

#If there would have been rows with empty rents, we would need to delete all the rows where we don't have no information on the rent (rent_full empty/NA)
#As these rows would have been useless for us. But its all good.




attach(df) #so we don't have to write df$ for our columns anymore, create any needed columns before this line.

names(df) #show all column names
str(df) #quick summary, to check classes for example
summary(df) #more detailed summary with min, median, mean, max, NAs of all the columns

#Let's take a look at our most important number, that we have to predict in this project.

summary(rent_full) # detailed summary/overview of rent_full with min, median, mean, max, NAs of
hist(rent_full) #histogram
hist(rent_full, breaks = 50) #more granular histogram
hist(rent_full, breaks = 100) #more granular histogram

#Let's take a look at the most obvious important factor of the rent, the space
plot(rent_full, area,
     xlab = "Rent paid in CHF",
     ylab = "Floor space of the apartment",
     main = "Rent & floorspace",
     pch = ".",
     col="darkblue"
     )

#Rent vs. rooms
plot(rent_full, rooms,
     xlab = "Rent paid in CHF",
     ylab = "Amount of rooms in the apartment",
     main = "Rent & rooms",
     pch = ".",
     col="darkblue"
     )

#Lets make the outliers a bit better visible
plot(rent_full, rooms,
     xlab = "Rent paid in CHF",
     ylab = "Amount of rooms in the apartment",
     main = "Rent & rooms",
     pch = 20,
     col="darkblue"
)

#Rent vs. Microrating
plot(rent_full, Micro_rating,
     xlab = "Rent paid in CHF",
     ylab = "Calculated rating of the location",
     main = "title",
     pch = ".",
     col="darkblue"
     )

#Rent vs. Microrating
plot(pricepersquaremeter, Micro_rating,
     xlab = "Rent paid per squaremeter",
     ylab = "Calculated rating of the location",
     main = "title",
     pch = ".",
     col="darkblue"
)


#It seems some people entered wrong listings (as some prices per squaremeter are crazy


summary(pricepersquaremeter) # detailed summary/overview of rent_full with min, median, mean, max, NAs of
hist(pricepersquaremeter) #histogram
hist(pricepersquaremeter, breaks = 50) #more granular histogram
hist(pricepersquaremeter, breaks = 100) #more granular histogram

#Rent vs. Microrating
plot(pricepersquaremeter, area,
     xlab = "Rent paid per squaremeter",
     ylab = "Calculated rating of the location",
     main = "title",
     pch = ".",
     col="darkblue"
)


#Rent vs.Microrating
plot(pricepersquaremeter, Micro_rating,
     xlab = "Rent paid per squaremeter",
     ylab = "Calculated rating of the location",
     xlim = c(0, 100),
     main = "title",
     pch = ".",
     col="darkblue"
)

#Rent vs. Microrating
plot(pricepersquaremeter, Micro_rating,
     xlab = "Rent paid per squaremeter",
     ylab = "Calculated rating of the location",
     xlim = c(0, 2),
     main = "Wrong listings? Too cheap.",
     pch = 20,
     col="darkblue"
)

#Rent vs. Microrating
plot(pricepersquaremeter, Micro_rating,
     xlab = "Rent paid per squaremeter",
     ylab = "Calculated rating of the location",
     xlim = c(1000, 3000),
     main = "Wrong listings? Too expensive.",
     pch = 20,
     col="darkblue"
)


#Let's a look at the correlations, between some columns:

pairs(~rent_full + rooms + area + Micro_rating, data=df, pch = ".", col="darkblue",)


#Let's split up into training and testing sets (lets use an 80/20 approach, as we have enough data)

train.size = dim(df)[1] * 0.8
train = sample(1:dim(df)[1], train.size)
test = -train
df.train = df[train, ]
df.test = df[test, ]

trainset <- df.train
testset <- df.test


#FROM HERE ON, THERE ARE JUST SOME UNCLEAN CODE OUT OF THE DISCUSSED EXERCISES
#EVERYTHING BELOW THIS LINE to be deleted


plot(KTKZ, rooms)


plot(rent_full, rooms)
lm.fit=lm(rent_full~rooms)
summary(lm.fit)

predict(lm.fit, data.frame(rooms=c(4.5)), interval="confidence")
predict(lm.fit, data.frame(rooms=c(4.5)), interval="prediction")


abline(lm.fit)
abline(lm.fit,col="red")

plot(lm.fit)



#show the following plots:
#definitely residuals vs. fitted
#q vs q
#if it would follow a strict normal distribution of the residuals, how would it look lik
#it is important how the residuals are behaving, to see what the model does 
#well and what it does not well
#the other 2 are less important

lm.fit = lm(rent_full~. -bath, data=df)




#Creating a new variable
Elite = rep("No", nrow(df)) #rep function
Elite[df$Top10perc>50]="Yes"
Elite = as.factor(Elite)
df = data.frame(df,Elite) #Put them together

summary(df$Elite) #there are 78 elite universities

#Histogram
par(mfrow=c(2,2))
hist(df$Books, col = 2, breaks = 50, xlab = "Books", ylab = "Count")
hist(df$PhD, col = 3, breaks = 50, xlab = "PhD", ylab = "Count")
hist(df$Grad.Rate, col = 4, breaks = 50, xlab = "Grad Rate", ylab = "Count")
hist(df$perc.alumni, col = 6, breaks = 50, xlab = "% alumni who donate", ylab = "Count")


#Exercises 2.4.9
View(Auto)
str(Auto)

attach(Auto)

summary(Auto$mpg)
range(Auto$mpg)

sapply(Auto[ ,(1:8)], mean)
sapply(Auto[ ,(1:8)], sd)

subset1 = subset(Auto[-(10:85),])
sapply(subset1[ ,(1:8)], mean)

pairs(Auto[1:6]) 

class(Auto$cylinders)
Auto$cylinders<-as.factor(Auto$cylinders)

par(mfrow=c(1,1))
plot(Auto$cylinders, Auto$mpg, xlab = "Cylinders", ylab ="Mileage", varwidth = T, col = c(2:6))
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab ="Mileage", col = "black", pch=19)
plot(Auto$weight, Auto$mpg, xlab = "Weight", ylab ="Mileage", col = "blue", pch=19)
plot(Auto$displacement, Auto$mpg, xlab = "Displacement", ylab ="Mileage", col = "blue", pch=19)

cor(Auto$weight, Auto$horsepower) 
cor(Auto$displacement, Auto$horsepower) 
cor(Auto$displacement, Auto$weight) 

Auto$year = as.factor(Auto$year)
plot(Auto$year, Auto$mpg, varwidth=T, xlab = "Year", ylab ="Mileage", col = "blue", pch=19)


View(Auto)      
?Auto           

attach(Auto) 

plot(mpg,horsepower)
lm.fit=lm(mpg~horsepower)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")

plot(horsepower,mpg)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

par(mfrow=c(2,2))
plot(lm.fit)

#Exercise 3.7.9
pairs(Auto)
cor(subset(Auto, select=-name))

lm.fit = lm(mpg~.-name, data=Auto)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

lm.fit1 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit1)
plot(lm.fit1)

lm.fit2 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)

     