#######Muelle Sensor Deployment######
# This code imports all 2025 muelle sensor deployment data, interpolates it to a common time period with 1 minute sampling interval, and exports it as a single CSV file for subsequent analyses.
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

#next week we will use TEOS-10 to calculate seawater salinity and density

library(gsw)

?gsw

?gsw_SP_from_C
#requires conductivity, in-situ temperature, and sea pressure dbar
# we have conductivity and temperature, but must calculate pressure from depth
?gsw_p_from_z
#requires z (- is below surface in meters) and latitude

mean_pressure_atm =ctd_offshore %>% 
  filter(`Pressure[cmH2O]`<1100) %>% 
  summarize(mean(`Pressure[cmH2O]`))

pressure_atm = 	1034.602 #cm based on the deployment prior to entering water

ctd_offshore_salinity=ctd_offshore %>% 
  mutate(pressure=gsw_p_from_z(z=-(`Pressure[cmH2O]`-pressure_atm)/100,latitude=17.97)) %>% 
  mutate(salinity=gsw_SP_from_C(`2:Spec.cond.[ms/cm]`,`Temperature[C]`,pressure)) %>% 
  drop_na()

ggplot()+
  geom_line(data=ctd_offshore_salinity, aes(x=`Date/time`,y=`salinity`,color="CTD Diver"))+
  geom_point(data=calibration_data_ts_offshore,aes(x=`Bolivia Time`,y=Salinity_psu,color="Hydrolab"))+
  scale_color_manual(name="Sensor",
                     breaks=c("CTD Diver","Hydrolab"),
                     values=c("CTD Diver"="#bae4bc","Hydrolab"="#0868ac"))+
  theme_classic()

ctd_offshore_salinity_corrected=ctd_offshore_salinity %>% 
  mutate(salinity_corrected=(35.1/28.56765)*salinity)

ggplot()+
  geom_line(data=ctd_offshore_salinity_corrected, aes(x=`Date/time`,y=`salinity_corrected`,color="CTD Diver"))+
  geom_point(data=calibration_data_ts_offshore,aes(x=`Bolivia Time`,y=Salinity_psu,color="Hydrolab"))+
  scale_color_manual(name="Sensor",
                     breaks=c("CTD Diver","Hydrolab"),
                     values=c("CTD Diver"="#bae4bc","Hydrolab"="#0868ac"))+
  theme_classic()

#before we continue to co-varying parameters, let's take a look at our available NOAA data

#download data here: https://tidesandcurrents.noaa.gov/stationhome.html?id=9759110

#the following we will do interpolations to assess covariation between all the things

library(xts)

colnames(miniPAR_offshore)

# Convert the data to time series objects
miniPAR_offshore_ts <- xts(as.numeric(miniPAR_offshore$PAR), 
                           order.by = miniPAR_offshore$`Bolivia Time`)
miniDOT_offshore_ts <- xts(as.numeric(miniDOT_offshore$`Dissolved Oxygen`), 
                  order.by = miniDOT_offshore$`Bolivia Time`)
ctd_offshore_salinity_corrected_T_ts <- xts(as.numeric(ctd_offshore_salinity_corrected$`Temperature[C]`), 
                           order.by = ctd_offshore_salinity_corrected$`Date/time`)


#create a common datetime to interpolate all data but let's also filter this for when the sensors were in the water
common_datetime <- 
  seq.POSIXt(from = min(filter(ctd_offshore,`Pressure[cmH2O]`>1100)$`Date/time`),
             to = max(filter(ctd_offshore,`Pressure[cmH2O]`>1100)$`Date/time`),
             by = "1 mins")


library(imputeTS)
#interpolate each miniDOT sensor to the common datetime
miniPAR_offshore_ts_i = na_interpolation(merge(miniPAR_offshore_ts, xts(,common_datetime)))
ctd_offshore_salinity_corrected_T_ts_i = na_interpolation(merge(ctd_offshore_salinity_corrected_T_ts, xts(,common_datetime)))

miniPAR_CTD=merge(miniPAR_offshore_ts_i,ctd_offshore_salinity_corrected_T_ts_i)
View(miniPAR_CTD)

#repeat for all timeseries, now they are all interpolated to the same time so we can directly assess covariation between our timeseries data

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
miniDOT_inshore_temp_ts = xts(miniDOT_inshore$Temperature,
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

miniDOT_inshore_temp_ts_i=miniDOT_inshore_temp_ts_i=
  na_interpolation(merge(miniDOT_inshore_temp_ts,xts(,common_datetime)))

ctd_offshore_temperature_ts_i=
  na_interpolation(merge(ctd_offshore_temperature_ts,
                         xts(,common_datetime)))

ctd_offshore_salinity_ts_i=
  na_interpolation(merge(ctd_offshore_salinity_ts,
                         xts(,common_datetime)))

ctd_offshore_pressure_ts_i=
  na_interpolation(merge(ctd_offshore_pressure_ts,
                         xts(,common_datetime)))


#put all data frames into list
ts_list <- list(miniDOT_offshore_ts_i,
                miniPAR_offshore_ts_i,
                ctd_offshore_temperature_ts_i,
                ctd_offshore_salinity_ts_i,
                ctd_offshore_pressure_ts_i,
                miniDOT_inshore_ts_i,
                miniDOT_inshore_temp_ts_i)

#merge all data frames in list
ts_all=Reduce(function(x, y) merge(x, y, all=TRUE), ts_list)

#Homework: download windspeed data in local standard time from NOAA tides and currents (https://tidesandcurrents.noaa.gov/stationhome.html?id=9759110), import to R, interpolate to the common date/time, merge with ts_all, and upload as a CSV to moodle

NOAA_muelle_winds_1 <- read_csv("data/CO-OPS_9759110_met_8-15-2025_to_9-15-2025.csv")
NOAA_muelle_winds_2 <- read_csv("data/CO-OPS_9759110_met_9-16-2025_to_9-18-2025.csv")

NOAA_muelle_winds = rbind(NOAA_muelle_winds_1,NOAA_muelle_winds_2) %>% 
  unite("date_time",`Date`:`Time (LST)`,sep=" ") %>% 
  mutate("Bolivia Time"=as.POSIXct(date_time,format="%Y-%m-%d %H:%M:%S"))

NOAA_muelle_winds_ts = xts(as.numeric(NOAA_muelle_winds$`Wind Speed (m/s)`),
                           order.by=NOAA_muelle_winds$`Bolivia Time`)

NOAA_muelle_winds_ts_i=
  na_interpolation(merge(NOAA_muelle_winds_ts,
                         xts(,common_datetime)))

ts_all_NOAA_winds = cbind(ts_all,NOAA_muelle_winds_ts_i)

ts_all_NOAA_winds_cropped=
  ts_all_NOAA_winds["2025-08-15 13:00:00 UTC/2025-09-19 11:15:00 UTC"]

write_csv(data.frame(date_time=time(ts_all_NOAA_winds_cropped),as.matrix(ts_all_NOAA_winds_cropped)),"CMOQ_2025_muelle_data.csv")
