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
                miniDOT_inshore_ts_i,
                miniPAR_offshore_ts_i,
                ctd_offshore_temperature_ts_i,
                ctd_offshore_salinity_ts_i,
                ctd_offshore_pressure_ts_i)

#merge all data frames in list
ts_all=Reduce(function(x, y) merge(x, y, all=TRUE), ts_list)
