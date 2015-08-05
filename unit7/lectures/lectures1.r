#video 7-1-4
who = read.csv("./WHO (1).csv")
str(who)
plot(who$GNI, who$FertilityRate)

install.packages("ggplot2")
library(ggplot2)

#data, aesthetic mapping of variables, geometric object
scatterplot = ggplot(who,aes(x = GNI, y = FertilityRate))
#add gemetry
scatterplot + geom_point()
#or
scatterplot + geom_line()
#
scatterplot + geom_point(color = "blue", size = 3, shape = 15)

fertilitygniplot = scatterplot + geom_point(color = "darkred", size = 3, shape = 8) + ggtitle("fertility rate vs. gross national income")
pdf("fertilitygniplot.pdf")
print(fertilitygniplot)
#turn off viewer
dev.off()
#turn on viewer
dev.new()
?dev.off

#video 7-1-5
#color by region
ggplot(who,aes(x = GNI, y = FertilityRate,color = Region))  + geom_point()
#color by life expectancy
ggplot(who,aes(x = GNI, y = FertilityRate,color = LifeExpectancy))  + geom_point()

#this doesn't look like a linear relationship
ggplot(who,aes(x = FertilityRate,y= Under15)) + geom_point()
#maybe log will look like a linear relationship
ggplot(who,aes(x = log(FertilityRate),y= Under15)) + geom_point()

model = lm(data=who,Under15 ~ log(FertilityRate))
summary(model)
#now let's add a regression line drawn by our model:
ggplot(who,aes(x = log(FertilityRate),y= Under15)) + geom_point() + stat_smooth(method = "lm")
#notice that by default ggplot draws a 95% confidence interval around it
ggplot(who,aes(x = log(FertilityRate),y= Under15)) + geom_point() + stat_smooth(method = "lm",level = .99)
#or no conf intervale
ggplot(who,aes(x = log(FertilityRate),y= Under15)) + geom_point() + stat_smooth(method = "lm",se = FALSE,color = "orange")

#QQ
ggplot(who, aes(x = FertilityRate, y = Under15,color = Region)) + geom_point() + scale_color_brewer(palette="Dark2")
