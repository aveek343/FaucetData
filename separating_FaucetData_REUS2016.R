# we will try to separate the faucet data which occured after toilet use in part 1. Assuming a faucet use occured within 5 minutes after a toilet use
# we can change the asumption at line 56
# Part 2 will have just the faucet Data 
# created by Mahmudur Rahman Aveek
##########################################################################################################
# Part 1
library (RODBC)
library(dplyr)
channel <- odbcConnectAccess2007("D:\\R\\R\\WRF - Residential End Uses of Water 2016.accdb") # connect with your file, you have to write down the file path
# this part is not required when we want to subset the data based on the city
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
  arrange(index, TT) %>%
  group_by(Keycode) %>%
  mutate(diff = TT - lag(TT),
         diff_secs = as.numeric(diff, units = 'secs'))
newDF4$diff_min <- newDF4$diff_secs/60

# assuming the faucet event will occur within 5 min after the toilet event 
newDF4$faucetAfterToilet<- with(newDF4, ifelse(newDF4$diff_min=="NA","NA",ifelse(newDF4$diff_min<=5,1,0)))

# now separating the faucet data

newDF5<- subset(newDF4, SumAs=="FAUCET")
newDF6<- subset(newDF5, faucetAfterToilet==1)

#changing the duration values which is a factor class to a numeric class
newDF6$Duration2<-as.numeric((as.character(newDF6$Duration)))



#class(newDF6$Duration2)
hist(newDF6$Duration2,  xlim=c(0,100), 
     las=1, 
     breaks=100, 
     prob = FALSE)

FaucetAfterToilet <- subset(newDF6, select= c(Keycode, SumAs,  TT ,Duration2, Peak, Volume, Mode))
setwd("D:\\R\\R\\log_normal_tests")
write.csv(FaucetAfterToilet,"FaucetUseAfterToiletUse.csv")


############################################################################################################
# part 2: separating the faucet use data
############################################################################################################
# subsetting the Faucet data
mainMeterEvent.F<- subset(mainMeterEvent, mainMeterEvent[,2] %in% c("FAUCET"), drop=TRUE)
# Changing the duration column from factor to numeric
mainMeterEvent.F$Duration<-as.numeric((as.character(mainMeterEvent.F$Duration)))
write.csv(mainMeterEvent.F,"FaucetUse.csv")
