mvt = read.csv("mvtWeek1.csv")

subset(mvt,mvt$Arrest == TRUE)

#arrests in an alley
mvtAlley = subset(mvt,mvt$LocationDescription == "ALLEY")

#lets convert strings to R's date type
DateConvert = as.Date(strptime(mvt$Date,"%m/%d/%Y %H:%M"))
mvt$Date = DateConvert

#add month to the df
mvt$Month = months(DateConvert)
#and how about day of the week
mvt$Weekday = weekdays(DateConvert)

#mvt (motor vehicle thefts) by month
table(mvt$Month)

#mvt by weekday
table(mvt$Weekday)

#let's show mvt by month only for mvt where arrest was made
arrests = subset(mvt,mvt$Arrest == TRUE,na.rm = TRUE)
table(arrests$Month)

#see if we can visualize a trend
hist(mvt$Date, breaks=100)

#visualize mvt where arrests were made
boxplot(mvt$Date ~ mvt$Arrest)

#we can use this to get mvt where arrests were made as a proportion of total arrests in a 
#given year
table(mvt$Year,mvt$Arrest)

#let's look for the top 5 locations where arrests were made
sort(table(mvt$LocationDescription))

#how about a subset of just these top five locations
Top5 = subset(mvt,mvt$LocationDescription == "STREET" | mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | mvt$LocationDescription == "ALLEY" | mvt$LocationDescription == "GAS STATION" | mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")

#the $LocationDescription in Top5 still represents the same factor as in mvt, so if we do 
table(Top5$LocationDescription)
# we get a lot of unnecessary output.  To limit the factor in Top5 to only the Levels
# that would have > 0 observations (the ones we did not filter out)
Top5$LocationDescription = factor(Top5$LocationDescription)
# ^ Basically I think this means: look at the levels in Top5$LocationDescription, make levels out of the stuff seen there

#we can use this to find arrest rates at certain locations
table(Top5$LocationDescription,Top5$Arrest)

#and this to find on what day of the week the most arrest happen at gas stations
gasStations = subset(Top5,Top5$LocationDescription == "GAS STATION")
table(gasStations$Weekday)
