cps = read.csv("./CPSData.csv")
summary(cps)
str(cps)
str(
table(cps$Industry)

#fewest and most interviewees
sta = table(cps$State)
sort(sta)

#proportion who are us citizens
table(cps$Citizenship)
(116639 + 7073)/(116639 + 7073 + 7590 )

#which races of hispanic people have > 250 respondents
hispanic = subset(cps,cps$Hispanic == TRUE, na.rm = TRUE)
table(hispanic$Race)

# how many didn't say whether they were married, by region or other factors
table(cps$Region, is.na(cps$Married))
table(cps$Sex, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))
table(cps$Citizenship, is.na(cps$Married))

#people living in non metro areas
metroRespondDf = as.data.frame.matrix(table(cps$State, is.na(cps$MetroAreaCode)))
colnames(metroRespondDf) <- c('response','noresponse')
subset(metroRespondDf,metroRespondDf$response == 0)
subset(metroRespondDf,metroRespondDf$noresponse == 0)

#largest proportion of responders living in non metro area
metroRespondByRegion = as.data.frame.matrix(table(cps$Region, is.na(cps$MetroAreaCode)))
colnames(metroRespondByRegion) <- c('response','noresponse')
metroRespondByRegion$total = metroRespondByRegion$noresponse + metroRespondByRegion$response
metroRespondByRegion$proportionInNonMetro = metroRespondByRegion$noresponse / metroRespondByRegion$total

#proportions living in non-metro, by state (since mean treats TRUE as 1 and False as 0)
proportionInNonMetro = as.data.frame(tapply(is.na(cps$MetroAreaCode),cps$State,mean))
colnames(proportionInNonMetro) <- c('proportion')
#or
cps$LiveInMetro = is.na(cps$MetroAreaCode)
tapply(cps$LiveInMetro,cps$State,mean)
#order them so we can see closest to 30%
proportionInNonMetro[with(proportionInNonMetro,order(proportion)),]

#read the stuff into dictionaries:
MetroAreaMap = read.csv("./MetroAreaCodes.csv")
CountryMap = read.csv("./CountryCodes.csv")

#merge in our code text from the maps to the numeric codes in cps
#data frames to be merged are referred to as 'x' and 'y' for this function
#   all.x=TRUE makes this like a left outer join on Code from the MetroAreaMap
cps = merge(cps, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(cps)
#count of missing metro from new variable 'MetroArea'
summary(cps)

#highest of 4 different given metro areas
nrow(subset(cps,cps$MetroArea == 'Atlanta-Sandy Springs-Marietta, GA'))
nrow(subset(cps,cps$MetroArea == 'Baltimore-Towson, MD'))
nrow(subset(cps,cps$MetroArea == 'Boston-Cambridge-Quincy, MA-NH'))
nrow(subset(cps,cps$MetroArea == 'San Francisco-Oakland-Fremont, CA'))

#metro with highest proportion of hispanic responders
sort(tapply(cps$Hispanic,cps$MetroArea,mean))
