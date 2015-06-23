convertDate = function(df)
{
  df$Date = as.Date(df$Date, "%m/%d/%y")
  df
}

IBM = convertDate(read.csv("./IBMStock.csv"))
Boeing = convertDate(read.csv("./BoeingStock.csv"))
GE = convertDate(read.csv("./GEStock.csv"))
CocaCola = convertDate(read.csv("./CocaColaStock.csv"))
ProctorGamble = convertDate(read.csv("./ProcterGambleStock.csv"))

# num observations
c(str(IBM),str(Boeing),str(GE),str(CocaCola),str(ProctorGamble))

#find out what the minimum date is
#put frames into a list
dataFrameList = list(IBM,Boeing,GE,CocaCola,ProctorGamble)
# then get min over the list
lapply(dataFrameList,function(frame){min(frame$Date)})

mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)
sd(ProctorGamble$StockPrice)

plot(x = CocaCola$Date, y = CocaCola$StockPrice,col="red")
lines(ProctorGamble$Date,ProctorGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-03-01")), lwd=1)

#plot from 1995 to 2005 (indexes of those years given to us)
#  we limit the y to 210 
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
#add the rest of the companies, assuming that we already have done the first
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="blue")
lines(ProctorGamble$Date[301:432], ProctorGamble$StockPrice[301:432], type="l", col="green")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="black")
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="purple")
abline(v=as.Date(c("2000-03-01")), lwd=1)
#look for sharp declines here
abline(v=as.Date(c("1997-09-01")), lwd=1,col = "red")
abline(v=as.Date(c("1997-11-01")), lwd=1,col = "red")

#use tapply to get monthly means
tapply(IBM$StockPrice,months(IBM$Date),mean)
mean(IBM$StockPrice)
# remembering that order was : dataFrameList = list(IBM,Boeing,GE,CocaCola,ProctorGamble)
lapply(dataFrameList,function(dataframe){tapply(dataframe$StockPrice,months(dataframe$Date),mean)})