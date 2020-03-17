# we will try to separate the faucet data which occured after toilet use in part 1. Assuming a faucet use occured within 5 minutes after a toilet use
# we can change the asumption at line 56
# Part 2 will have just the faucet Data 
# created by Mahmudur Rahman Aveek
##########################################################################################################
# Part 1
library (RODBC)
library(dplyr)
channel <- odbcConnectAccess2007("D:\\R\\R\\WRF - Residential End Uses of Water 2016.accdb") # connect with your file, you have to write down the file path
# this part is not required (required when we want to subset the data based on the city)
# "the REU2016_Survey-End Use Sample" file is located in the MS Access Database of 
# "WRF-Residential End Uses of Water 2016 Database". It can be imported
# using the RODBC package of R by mannually exporting the "REU2016_Survey-End Use" Sample table 
# from Ms Access using sqlFetch command
# endUseSample <- data.frame(sqlFetch(channel, "REU2016_Survey - End Use Sample")), 

# or if already been exported to an Excel file, use the following
# endUseSample <- read_excel('REU2016_Survey - End Use Sample.xlsx')

#individual event data
mainMeterEvent3 <- data.frame(sqlFetch(channel, "REU2016_Main_Meter_Events", as.is=TRUE))

# Changing the cases to upper case, as some of the keycode are mistakenly were mistakenly written in lowercase
# This step will help identify the location (e.g., city) of the household for further analysis as we can call the location
# using the keycode(id) of the household
mainMeterEvent <- as.data.frame(sapply(mainMeterEvent3,toupper))

# Sub-setting data for faucet and toilet use
mainMeterEvent.FT<- subset(mainMeterEvent, mainMeterEvent[,2] %in% c("TOILET", "FAUCET"), drop=TRUE)

# now we will findout the toilet uses and the relevant faucet uses
# first we will separate the rows which have faucet use followed by a toilet use by identifying the index values of the rows
ind2 <- which(mainMeterEvent.FT$SumAs=="FAUCET" & dplyr::lag(mainMeterEvent.FT$SumAs)=="TOILET")
ind1 <- which(mainMeterEvent.FT$SumAs=="FAUCET" & dplyr::lag(mainMeterEvent.FT$SumAs)=="TOILET")-1

# now we are creating a new dataframe that will have the toilet use followed by a faucet use, as the index numbers are called separately, we need to re-order those
newDF2<-mainMeterEvent.FT[c(ind1,ind2),]

newDF2$index <- as.numeric(row.names(newDF2))

newDF3<-newDF2[order(newDF2$index),]

# now we need to check if the faucet event occured right after the toilet event by checking the time stamp 


newDF3$TT<- as.POSIXct(newDF3$StartTime, format = c("%Y-%m-%d %H:%M:%OS")) # creating a new column which can be used to calculate the time difference

newDF4<-newDF3 %>%
  arrange(TT,index) %>%
  group_by(Keycode) %>%
  mutate(diff = TT - lag(TT),
         diff_secs = as.numeric(diff, units = 'secs'))
newDF4$diff_min <- newDF4$diff_secs/60

# assuming the faucet event will occur within 5 min after the toilet event 
newDF4$faucetAfterToilet<- with(newDF4, ifelse(newDF4$diff_min=="NA","NA",ifelse(newDF4$diff_min<=2,1,0)))

# now separating the faucet data

newDF5<- subset(newDF4, SumAs=="FAUCET")
newDF6<- subset(newDF5, faucetAfterToilet==1)

#changing the duration values which is a factor class to a numeric class
newDF6$Duration2<-as.numeric((as.character(newDF6$Duration)))



#creating custom range to create histogram
faucet.cut <- cut(newDF6$Duration2, c(-Inf,5,19.99,30,50,500,Inf))
x<-data.frame(table(faucet.cut))

# plot(table(faucet.cut),xaxt='n',ylab="frequency")
# axis(1, at=1:6, labels = c('<5sec', '5-19 sec', '19-30 sec', '30-50 sec',
#                            '50-500 sec', '>500 sec'))
# box(bty='L')

# histogram for faucet use after toilet events
xax <- barplot(table(faucet.cut), xaxt='n', main='Actual faucet use duration \n after each toilet event\n (Source: REUS 2016)', ylab='frequency', xlab='duration in seconds')
axis(1, at=xax, labels = c('<5sec', '5-20 sec', '20-30 sec', '30-50 sec',
                           '50-500 sec', '>500 sec'))
box(bty='L')

# 
# hist(newDF6$Duration2,  xlim=c(0,100), 
#      las=1, 
#      breaks=100, 
#      prob = FALSE)


FaucetAfterToilet <- subset(newDF6, select= c(Keycode, SumAs,  TT ,Duration2, Peak, Volume, Mode))
setwd("D:\\R\\R\\log_normal_tests")
write.csv(FaucetAfterToilet,"FaucetUseAfterToiletUse.csv")
write.csv(x,"FaucetUseAfterToiletUse_histogramData.csv")

##############################################################################################
# linking the data with the survey data to get more insight about the users

faucetAvgDuration<-aggregate(FaucetAfterToilet[, 4],list(FaucetAfterToilet$Keycode),mean)
faucetAvgDuration.cut <- cut(faucetAvgDuration$Duration2, c(-Inf,5,19.99,30,50,Inf))
xx<- data.frame(table(faucetAvgDuration.cut))
write.csv(xx,"FaucetUseAfterToiletUse_histogramData_HHwise.csv")

#creating histogram for the average household use data for the faucet use which occured after toilet use
xax <- barplot(table(faucetAvgDuration.cut), xaxt='n', main='Avg. Faucet use duration per HH \n after toilet events\n (Source: REUS 2016)', ylab='frequency', xlab='duration in seconds')
axis(1, at=xax, labels = c('<5sec', '5-20 sec', '20-30 sec', '30-50 sec',
                           '>50 sec'))
box(bty='L')

# ranking the house hold depending on their faucet use duration
faucetAvgUse.rank <- faucetAvgDuration[order(faucetAvgDuration$Duration2), ]
faucetAvgUse.rank$rank <- 1:nrow(faucetAvgUse.rank)



# from Ms Access using sqlFetch command getting the location data from "REU2016_Survey - End Use Sample" table
endUseSample <- data.frame(sqlFetch(channel, "REU2016_Survey - End Use Sample"))
# getting the name of the cities using the Keycode
endUseSample.city <- data.frame(endUseSample$Keycode,endUseSample$ServiceCity)
names(endUseSample.city)[1]<-"Keycode"
names(endUseSample.city)[2]<-"ServiceCity"

faucetUse<-merge(faucetAvgUse.rank,endUseSample.city, by.x="Group.1", by.y="Keycode")
faucetAvgCityWise<-aggregate(faucetUse[, 2],list(faucetUse$ServiceCity),mean)
faucetAvgCityWise.rank <-faucetAvgCityWise[order(faucetAvgCityWise$x),]
faucetAvgCityWise.rank$rank <-1:nrow(faucetAvgCityWise.rank)

faucetAvgCityWise.rank$state <- c("CO","CO","WA","GA","ON, Canada","CO", "CO",
                                  "CO","CO","ON, Canada","FL", "CO", "ON, Canada",
                                  "WA", "GA", "GA","WA","AZ","ON, Canada",
                                  "GA","GA","WA","TX","GA","GA","GA","GA","GA",
                                  "CO","FL")
faucetAvgStateWise <-aggregate(faucetAvgCityWise.rank[,2], list(faucetAvgCityWise.rank$state),mean)

############################################################################################################
# part 2: separating overall faucet use data
############################################################################################################
# subsetting the Faucet data
mainMeterEvent.F<- subset(mainMeterEvent, mainMeterEvent[,2] %in% c("FAUCET"), drop=TRUE)
# Changing the duration column from factor to numeric
mainMeterEvent.F$Duration<-as.numeric((as.character(mainMeterEvent.F$Duration)))

# histogram for overall faucet use
faucet.cut <- cut(mainMeterEvent.F$Duration, c(-Inf,5,19,30,50,500,Inf))
View(table(faucet.cut))

# histogram for faucet use after toilet events
xax <- barplot(table(faucet.cut), xaxt='n', main='Faucet use duration (REUS 2016 dataset)', ylab='frequency', xlab='duration in seconds')
axis(1, at=xax, labels = c('<5sec', '5-19 sec', '19-30 sec', '30-50 sec',
                           '50-500 sec', '>500 sec'))
box(bty='L')




write.csv(mainMeterEvent.F,"FaucetUse.csv")
