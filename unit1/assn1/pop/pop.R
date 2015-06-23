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

#Number of metro areas where at least 20% of interviewees are asian
percentAsianByMetro = as.data.frame(sort(tapply(cps$Race == "Asian",cps$MetroArea,mean)))
colnames(percentAsianByMetro) <- c('percent')
nrow(subset(percentAsianByMetro,percentAsianByMetro$percent >= .2))

#metro area with smallest percent having high school diploma
sort(tapply(cps$Education == "No high school diploma", cps$MetroArea, mean,na.rm=TRUE))

#merge in country of birth info
str(cps)
cps = merge(cps, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

#most common country of birth?
sort(table(cps$Country))

#percent of people from New York-Northern New Jersey-Long Island, NY-NJ-PA
## that have a non-us country of birth (excluding missing country of birth)
nymetro = subset(cps,cps$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA' & !is.na(cps$Country))
nymetro$BornInUs = nymetro$Country == 'United States'
1- mean(nymetro$BornInUs)

# metro area with largest number of responders having birth country in India
sort(tapply(cps$Country == 'India',cps$MetroArea,sum,na.rm = TRUE))
# metro area with largest number of responders having birth country in Brazil
sort(tapply(cps$Country == 'Brazil',cps$MetroArea,sum,na.rm = TRUE))
# metro area with largest number of responders having birth country in Somalia
sort(tapply(cps$Country == 'Somalia',cps$MetroArea,sum,na.rm = TRUE))

85.3 - (41+0.59*77)
98.6 - (41+.59*85)
94 - (41+.59*95.5)
((5.46 )/(63.2))*(-0.499)
-.0431*101.8
19.94+4.388
