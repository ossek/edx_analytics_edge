#video 7-2-3
# heat maps are a good way to viz 2-d table data with 3 attributes

mvt = read.csv("./mvt.csv",stringsAsFactors = FALSE)
str(mvt)
#condvert date strings to R date type
mvt$Date = strptime(mvt$Date,format = "%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

str(mvt)

#let's make a line plot of tot number of crimes each day of week
table(mvt$Weekday)
weekdayCounts = as.data.frame(table(mvt$Weekday))
str(weekdayCounts)
library(ggplot2)
#second aes call groups data into 1 line
ggplot(weekdayCounts,aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))
#default sort for days was alphabetical. so we need to specify ordering on the weekday factor
weekdayCounts$Var1 = factor(weekdayCounts$Var1,ordered = TRUE,
                            levels = c("Sunday","Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(weekdayCounts,aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))
#relabel
ggplot(weekdayCounts,aes(x = Var1, y = Freq)) + geom_line(aes(group = 1)) + xlab("Day of the week") + ylab("motor vehicle thefts")

#QQ
ggplot(weekdayCounts,aes(x = Var1, y = Freq)) + geom_line(aes(group = 1),alpha = 0.3) + xlab("Day of the week") + ylab("motor vehicle thefts")

#7-2-4
table(mvt$Weekday,mvt$Hour)
dayHourCounts = as.data.frame(table(mvt$Weekday,mvt$Hour))
str(dayHourCounts)
#factor to numeric
dayHourCounts$Hour = as.numeric(as.character(dayHourCounts$Var2))
ggplot(dayHourCounts,aes(x =Hour,y = Freq)) + geom_line(aes(group=Var1))
#colors for days of the week
ggplot(dayHourCounts,aes(x =Hour,y = Freq)) + geom_line(aes(group=Var1,color = Var1),size = 2)
#now let's do heatmap
#fix the order of the days
dayHourCounts$Var1 = factor(dayHourCounts$Var1,ordered = TRUE,
       levels = c("Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
ggplot(dayHourCounts,aes(x =Hour,y = Var1)) + geom_tile(aes(fill = Freq))

#for some reason this can't be split across lines it seems
ggplot(dayHourCounts,aes(x =Hour,y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name= "Total mv thefts",low="white", high="red")+ theme(axis.title.y = element_blank())

#7-2-5
#plotting crime on map of chicago
install.packages("maps")
library(maps)
install.packages("ggmap")
library(ggmap)
chicago = get_map(location="chicago",zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data=mvt[1:100,],aes(x=Longitude,y=Latitude))

#the map would be too full if we plotted everything, so we'll do some grouping
#  by rounding the lat long digits to make our locations more general
latLongCounts = as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(latLongCounts)
#convert factors to numerics
latLongCounts$Long = as.numeric(as.character(latLongCounts$Var1))
latLongCounts$Lat = as.numeric(as.character(latLongCounts$Var2))

#plot making size and color of points dependent on total mvt at the location
ggmap(chicago) + geom_point(data=latLongCounts,aes(x=Long,y=Lat,color = Freq, size = Freq)) + scale_color_gradient(low="yellow",high="red")

#more like a traditional heatmap 
ggmap(chicago) + geom_tile(data = latLongCounts,aes(x = Long, y = Lat,alpha = Freq), fill = "red")

#QQ
#remove for lat long where counts are 0
latLongCounts2 = subset(latLongCounts,latLongCounts$Freq > 0)
ggmap(chicago) + geom_tile(data = latLongCounts2,aes(x = Long, y = Lat,alpha = Freq), fill = "red")

#how many removed"
nrow(latLongCounts) - nrow(latLongCounts2)

#7-2-6
#heatmap on usa
murders = read.csv("./murders.csv")
str(murders)
statesMap = map_data("state")
#we have a df describing how to draw the map
str(statesMap)
#let's use some polygons to do that
ggplot(statesMap,aes(x = long,y = lat,group = group)) + geom_polygon(fill = "white", color = "black")

#add region to muders
murders$region = tolower(murders$State)

#merge based on shared idendtifier(region)
murderMap = merge(statesMap,murders, by = "region")
str(murderMap)
ggplot(murderMap,aes(x = long,y = lat,group = group,fill = Murders)) + geom_polygon( color = "black") + scale_fill_gradient(low = "black", high= "red",guide = "legend")

#do california and texas only have the most murders because they have most population?
ggplot(murderMap,aes(x = long,y = lat,group = group,fill = Population)) + geom_polygon( color = "black") + scale_fill_gradient(low = "black", high= "red",guide = "legend")

#number of murders per 100000 population
murderMap$MurderRate = murderMap$Murders / murderMap$Population *100000
ggplot(murderMap,aes(x = long,y = lat,group = group,fill = MurderRate)) + geom_polygon( color = "black") + scale_fill_gradient(low = "black", high= "red",guide = "legend")

#limit to only murder rates below 10 (removing an outlier we magically knew about that was washington d.c. or something)
ggplot(murderMap,aes(x = long,y = lat,group = group,fill = MurderRate)) + geom_polygon( color = "black") + scale_fill_gradient(low = "black", high= "red",guide = "legend",limits = c(0,10))

#QQ
ggplot(murderMap,aes(x = long,y = lat,group = group,fill = GunOwnership)) + geom_polygon( color = "black") + scale_fill_gradient(low = "black", high= "red",guide = "legend")
