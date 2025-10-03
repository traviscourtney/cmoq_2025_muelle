#######Muelle Sensor Deployment######
#
#
#
#
#######Load Libraries#######
library(tidyverse)

#######Import Data##########

ctd_offshore<- read_csv("data/CTD_diver_muelle_offshore_class_2025.CSV", 
         col_types = cols(`Date/time` = col_datetime(format = "%Y/%m/%d %H:%M:%S")), 
         skip = 61)
summary(ctd_offshore)

miniDOT_inshore <- read_csv("data/miniDOT_muelle_inshore_class_2025.TXT", 
                   col_types = cols(`UTC_Date_&_Time` = col_datetime(format = "%Y -%m-%d %H:%M:%S"), 
                                    `Bolivia Time` = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                    Temperature = col_number(),
                                    `Dissolved Oxygen` = col_number(),
                                    `Dissolved Oxygen Saturation` = col_number()),  
                   skip = 6) %>% slice(-1)
View(miniDOT_inshore)
summary(miniDOT_inshore)
miniDOT_offshore <- read_csv("data/miniDOT_muelle_offshore_class_2025.TXT", 
                            col_types = cols(`UTC_Date_&_Time` = col_datetime(format = "%Y -%m-%d %H:%M:%S"), 
                                             `Bolivia Time` = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                             Temperature = col_number(),
                                             `Dissolved Oxygen` = col_number(),
                                             `Dissolved Oxygen Saturation` = col_number()), 
                            skip = 6) %>% slice(-1)
View(miniDOT_offshore)
summary(miniDOT_offshore)

miniPAR_offshore <- read_csv("data/miniPAR_muelle_offshore_class_2025.TXT", 
                             col_types = cols(`UTC_Date_&_Time` = col_datetime(format = "%Y -%m-%d %H:%M:%S"), 
                                              `Bolivia Time` = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                              Temperature = col_number(),
                                              PAR = col_number()),
                             skip = 3) %>% slice(-1)
View(miniPAR_offshore)
summary(miniPAR_offshore)

calibration_data = read_csv("data/calibration_data.csv")
View(calibration_data)
summary(calibration_data)
######Let's plot our data########

summary(miniDOT_inshore)

DO_plot=
  ggplot()+
  geom_line(data=miniDOT_inshore, aes(x=`Bolivia Time`,y=`Dissolved Oxygen`,color="Inshore"))+
  geom_line(data=miniDOT_offshore, aes(x=`Bolivia Time`,y=`Dissolved Oxygen`,color="Offshore"))+
  scale_color_manual(name="Station",
                     breaks=c("Inshore","Offshore"),
                     values=c("Inshore"="#bae4bc","Offshore"="#0868ac"))

#looks like there are signs of biofouling so let's look at our calibration data to see

summary(calibration_data)

calibration_data_ts=
  calibration_data %>% 
  unite("date_time",`Date`:Time_AST_24HR,sep=" ") %>% 
  mutate("Bolivia Time"=as.POSIXct(date_time,format="%m/%d/%Y %H:%M:%S"))

summary(calibration_data_ts)

calibration_data_ts_inshore=calibration_data_ts %>% 
  filter(Site=="Inshore")
ggplot()+
  geom_line(data=miniDOT_inshore, aes(x=`Bolivia Time`,y=`Dissolved Oxygen`,color="Inshore"))+
  #geom_line(data=miniDOT_offshore, aes(x=`Bolivia Time`,y=`Dissolved Oxygen`,color="Offshore"))+
  geom_point(data=calibration_data_ts_inshore,aes(x=`Bolivia Time`,y=DO_mgL),color="red")+
  scale_color_manual(name="Station",
                     breaks=c("Inshore","Offshore"),
                     values=c("Inshore"="#bae4bc","Offshore"="#0868ac"))

#no obvious signs of biofouling for the inshore sensor relative to our calibration measurements

calibration_data_ts_offshore=calibration_data_ts %>% 
  filter(Site=="Offshore")
ggplot()+
  #geom_line(data=miniDOT_inshore, aes(x=`Bolivia Time`,y=`Dissolved Oxygen`,color="Inshore"))+
  geom_line(data=miniDOT_offshore, aes(x=`Bolivia Time`,y=`Dissolved Oxygen`,color="Offshore"))+
  geom_point(data=calibration_data_ts_inshore,aes(x=`Bolivia Time`,y=DO_mgL),color="red")+
  scale_color_manual(name="Station",
                     breaks=c("Inshore","Offshore"),
                     values=c("Inshore"="#bae4bc","Offshore"="#0868ac"))

#no obvious signs of biofouling for the offshore sensor relative to our calibration measurements

#make a time series plot of the offshore temperature data (miniDOT, miniPAR, CTD, calibration)

ggplot()+
  geom_line(data=miniDOT_offshore, aes(x=`Bolivia Time`,y=`Dissolved Oxygen`,color="SensorName"))+
  geom_point(data=calibration_data_ts_inshore,aes(x=`Bolivia Time`,y=DO_mgL),color="red")+
  scale_color_manual(name="Sensor",
                     breaks=c("CTD Diver","miniDOT","miniPAR","Hydrolab"),
                     values=c("Inshore"="#bae4bc","Offshore"="#0868ac"))#manually set the colors for each sensor 

#next week we will use TEOS-10 to calculate seawater salinity and density

#the following we will do interpolations to assess covariation between all the things