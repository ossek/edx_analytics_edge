#read int the data set
usda = read.csv("USDA.csv")
# let's look at which row has highest sodium
usda[which.max(usda$Sodium),]
# get a subset where sodium is over 10000
highSodium = subset(usda,usda$Sodium > 10000)
highSodium$Description
# caviar is known to have high sodium
usda[match("caviar",usda$Description),]
usda$sodium[match("CAVIAR",usda$Description),]
# what's the standard deviation of sodium?
sd(usda$Sodium,na.rm=TRUE)
#let's plot protein against total fat
plot(usda$Protein,usda$TotalFat)
#give it some labels
plot(usda$Protein,usda$TotalFat,xlab="protein",ylab="total fat, fatty")
#Make a histogram.  By default the subdivisions ar too large, so we will limit to the range 0,100
# and then we have to define subdivisions, with respect to our original range.  In the original, Vitamin C max
# was 2400.  So we break out by ....(The video said 2000 but also referenced the original range.  I think
# 2000 and 2400 give the same result?
hist(usda$VitaminC,xlab= "vittamin c (mg)", main = "vitamin c levels hist",xlim=c(0,100),breaks = 2000)
#box plot
boxplot(usda$Sugar,main = "sugar levels" ,ylab = "sugar (g)")

# Now we'll make additional variables
HighSodium = usda$Sodium > mean(usda$Sodium,na.rm = TRUE)
# What if we want bool flags (0,1) instead?
HighSodium = as.numeric(usda$Sodium > mean(usda$Sodium,na.rm = TRUE))
# add it to data frame instead
usda$HighSodium = as.numeric(usda$Sodium > mean(usda$Sodium,na.rm = TRUE))
#protein, fat, and
usda$HighProtein = as.numeric(usda$Protein > mean(usda$Protein,na.rm = TRUE))
usda$HighFat = as.numeric(usda$TotalFat > mean(usda$TotalFat,na.rm = TRUE))
usda$HighCarb = as.numeric(usda$Carbohydrate > mean(usda$Carbohydrate,na.rm = TRUE))
# compare values, where rows belong to first input, and columns to second input
table(usda$HighSodium,usda$HighFat)
# compute average iron,for each of high and low protein
tapply(usda$Iron,usda$HighProtein,mean,na.rm = TRUE)
# max level of vitamin c in foods with high and low carbs
tapply(usda$VitaminC,usda$HighCarb,max,na.rm = TRUE)
# is it true that foods with high carb also have high vitamin c?
# another perspective:
tapply(usda$VitaminC,usda$HighCarb,summary,na.rm = TRUE)