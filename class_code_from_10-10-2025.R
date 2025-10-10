#######Muelle Sensor Deployment######
#
#
#
#
#######Load Libraries#######
library(tidyverse)

#######Import Data##########
ctd_offshore <- read_csv("data/CTD_diver_muelle_offshore_class_2025_edit.csv", 
                         col_types = cols(`Date/time` = col_datetime(format = "%m/%d/%Y %H:%M")), 
                         skip = 63)
summary(ctd_offshore)
colnames(ctd_offshore)

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

colnames(ctd_offshore)
ggplot()+
  geom_line(data=ctd_offshore, aes(x=`Date/time`,y=`Temperature[C]`))
ctd_offshore$`Date/time`

ctd_offshore <- read_csv("data/CTD_diver_muelle_offshore_class_2025_edit.csv", 
                         col_types = cols(`Date/time` = 
                                            col_datetime(format = "%m/%d/%Y %H:%M")), 
                         skip = 63)

colnames(ctd_offshore)

library(gsw)

ggplot()+
  geom_line(data=ctd_offshore, aes(x=`Date/time`,y=`2:Spec.cond.[ms/cm]`))

?gsw_SP_from_C
#requires in situ temperature (degrees C), sea pressure (dbar), conductivity (mS/cm)

#we are missing pressure, but we can probs calculate from depth
?gsw_p_from_z

#requires z (m) below the surface and latitude (17.97)

View(ctd_offshore)

ggplot()+
  geom_line(data=ctd_offshore, aes(x=`Date/time`,y=`Pressure[cmH2O]`))

#let's calculate mean atmospheric pressure to subtract from our CTD pressure data
mean_pressure_atm = ctd_offshore %>% 
  filter(`Pressure[cmH2O]`<1100) %>% 
  summarize(mean(`Pressure[cmH2O]`))
mean_pressure_atm

pressure_atm = 	1034.602

#side note: we can filter our CTD data based on these same parameters
ggplot()+
  geom_line(data=filter(ctd_offshore,`Pressure[cmH2O]`>1100), aes(x=`Date/time`,y=`Temperature[C]`))

ggplot()+
  geom_line(data=filter(ctd_offshore,`2:Spec.cond.[ms/cm]`>1), aes(x=`Date/time`,y=`Temperature[C]`))

#we are trying to calculate salinity from our ctd data using the depth, pressure, conductivity, etc in GSW

ctd_offshore_salinity=
  ctd_offshore %>% 
  mutate(pressure_dbar=gsw_p_from_z(z=-(`Pressure[cmH2O]`-pressure_atm)/100,latitude=17.97)) %>% 
  mutate(salinity=gsw_SP_from_C(`2:Spec.cond.[ms/cm]`,`Temperature[C]`,pressure_dbar))

View(ctd_offshore_salinity)

ggplot()+
  geom_line(data=ctd_offshore_salinity, aes(x=`Date/time`,y=`salinity`,color="CTD Diver"))+
  geom_point(data=calibration_data_ts_offshore,aes(x=`Bolivia Time`,y=Salinity_psu,color="Hydrolab"))+
  scale_color_manual(name="Sensor",breaks=c("CTD_Diver","Hydrolab"),values=c("CTD_Diver"="blue","Hydrolab"="red"))+
  theme_classic()

#this salinity data is real bad, let's correct it using what we have
#let's try to correct our salinity using the first calibration sample

#measured by hydrolab 8/22 @ 14:50 35.13
#calculated from measurement by CTD 8/22 @ 14:45 28.56765

ctd_offshore_salinity_corrected = ctd_offshore_salinity %>% 
  mutate(salinity_corrected=(35.13/28.56765)*salinity) %>% 
  drop_na()

ggplot()+
  geom_line(data=ctd_offshore_salinity_corrected, aes(x=`Date/time`,y=`salinity_corrected`,color="CTD Diver"))+
  geom_point(data=calibration_data_ts_offshore,aes(x=`Bolivia Time`,y=Salinity_psu,color="Hydrolab"))+
  scale_color_manual(name="Sensor",breaks=c("CTD_Diver","Hydrolab"),values=c("CTD_Diver"="blue","Hydrolab"="red"))+
  theme_classic()

#we have all of our timeseries data, but what if we wanted to see how they co-vary

#first, let's convert all of these to timeseries objects

library(xts)

#convert the data to time series objects
miniPAR_offshore_ts = xts(miniPAR_offshore$PAR,
                          order.by=miniPAR_offshore$`Bolivia Time`)
miniDOT_offshore_ts = xts(miniDOT_offshore$`Dissolved Oxygen`,
                          order.by=miniDOT_offshore$`Bolivia Time`)
miniDOT_inshore_ts = xts(miniDOT_inshore$`Dissolved Oxygen`,
                         order.by=miniDOT_inshore$`Bolivia Time`)
ctd_offshore_temperature_ts = 
  xts(ctd_offshore_salinity_corrected$`Temperature[C]`,
      order.by=ctd_offshore_salinity_corrected$`Date/time`)
ctd_offshore_salinity_ts = 
  xts(ctd_offshore_salinity_corrected$salinity_corrected,
      order.by=ctd_offshore_salinity_corrected$`Date/time`)
ctd_offshore_pressure_ts = 
  xts(ctd_offshore_salinity_corrected$pressure_dbar,
      order.by=ctd_offshore_salinity_corrected$`Date/time`)

#we want to interpolate these to a common datetime, so we need to code that in

common_datetime = seq.POSIXt(
  from=min(filter(ctd_offshore,`Pressure[cmH2O]`>1100)$`Date/time`),
  to=max(filter(ctd_offshore,`Pressure[cmH2O]`>1100)$`Date/time`),
  by="1 mins")

library(imputeTS)

#interpolate each timeseries to the common datetime

miniPAR_offshore_ts_i=
  na_interpolation(merge(miniPAR_offshore_ts,xts(,common_datetime)))

#repeat this interpolation for all of our time series and merge them together

miniDOT_offshore_ts_i=
  na_interpolation(merge(miniDOT_offshore_ts,xts(,common_datetime)))
miniDOT_inshore_ts_i=
  na_interpolation(merge(miniDOT_inshore_ts,xts(,common_datetime)))

#and then we can merge all of our data together
miniDOT_inshore_offshore=merge(miniDOT_offshore_ts_i,miniDOT_inshore_ts_i)
miniDOT_inshore_offshore_miniPAR=merge(miniPAR_offshore_ts_i,miniDOT_inshore_offshore)

#put all data frames into list
ts_list <- list(miniDOT_offshore_ts_i,
                miniDOT_inshore_ts_i,
                miniPAR_offshore_ts_i,
                ctd_offshore_temperature_ts,
                ctd_offshore_salinity_ts,
                ctd_offshore_pressure_ts)

#merge all data frames in list
ts_all=Reduce(function(x, y) merge(x, y, all=TRUE), ts_list)
