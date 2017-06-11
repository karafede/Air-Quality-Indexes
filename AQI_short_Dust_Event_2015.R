
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)

################################################################################
## AIR QUALITY INDEXES ##-------------------------------------------------------
################################################################################

AQ_data <- read_csv("D:/AQI/AQ_data_all_clean_new_DUST_event.csv")
# AQ_data <-read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_all_clean.csv")

str(AQ_data)

AQ_data$Max_CO_8hr <- as.numeric(AQ_data$Max_CO_8hr )

# load function for AQI calcualtions

# source("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/aqi_fun.R")
source("D:/AQI//aqi_fun.R")

###########
### O3 ####
########### each hour (Max 8-hr average) ppb

O3_data <- as.vector(AQ_data$Max_O3_8hr)

# calculate Air Quality index for O3
aqi_O3 <- lapply(O3_data, aqi_O3_fun)
aqi_O3 <- as.numeric(aqi_O3)

aqi_O3 <- as.data.frame(aqi_O3)



#############
### CO ####
############# every 1-hr (Max 8-hr average) ppm

CO_data <- as.vector(AQ_data$Max_CO_8hr)

# calculate Air Quality index for CO
aqi_CO <- lapply(CO_data, aqi_CO_fun)

aqi_CO <- as.numeric(aqi_CO)
aqi_CO <- as.data.frame(aqi_CO)



###########  
# PM2.5 ###
###########


PM25_data <- as.vector(AQ_data$PM25_24hr)
PM25_data <- as.numeric(PM25_data)

# calculate Air Quality index for PM2.5
aqi_PM25 <- lapply(PM25_data, aqi_PM25_fun)

aqi_PM25 <- as.numeric(aqi_PM25)
aqi_PM25 <- as.data.frame(aqi_PM25)
  
  
##########
## PM10 ##
##########
  
PM10_data <- as.vector(AQ_data$PM10_24hr)

# calculate Air Quality index for PM10
aqi_PM10 <- lapply(PM10_data, aqi_PM10_fun)

aqi_PM10 <- as.numeric(aqi_PM10)
aqi_PM10 <- as.data.frame(aqi_PM10)
  
 
#############
### SO2 ####
############# every 1-hr ppb
  
SO2_data <- as.vector(AQ_data$SO2_1hr)

# calculate Air Quality index for PM10
aqi_SO2 <- lapply(SO2_data, aqi_SO2_fun)

aqi_SO2 <- as.numeric(aqi_SO2)
aqi_SO2 <- as.data.frame(aqi_SO2)
    
    
#############
### NO2 ####
############ every 1-hr ppb

NO2_data <- as.vector(AQ_data$NO2_1hr)

# calculate Air Quality index for PM10
aqi_NO2 <- lapply(NO2_data, aqi_NO2_fun)

aqi_NO2 <- as.numeric(aqi_NO2)
aqi_NO2 <- as.data.frame(aqi_NO2)


AQ_data_AQI <- cbind(AQ_data, aqi_O3, aqi_CO, aqi_PM25, aqi_PM10, aqi_SO2, aqi_NO2)
# write_csv(AQ_data_AQI, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_AQI_new.csv")

write_csv(AQ_data_AQI, "D:/AQI/AQ_data_AQI_new_Dust_Event.csv")

##########################################################################
##########################################################################

# find maximum AQI in each row

library(dplyr)
library(leaflet)
library(readr)
library(lubridate)

AQ_data_AQI <- read_csv("D:/AQI/AQ_data_AQI_new_Dust_Event.csv")



all_AQI <- AQ_data_AQI %>%
  dplyr::select(aqi_O3,
                aqi_CO,
                aqi_PM25,
                aqi_PM10,
                aqi_SO2,
                aqi_NO2)

str(all_AQI)
  
all_AQI$aqi_PM25 <- as.numeric(all_AQI$aqi_PM25)
all_AQI$aqi_CO <- as.numeric(all_AQI$aqi_CO)


# Boxplot of 
boxplot(all_AQI) 

# all_AQI <- all_AQI[2,]


# function to find the Pollutant with the maximum AQI

max_fe <- function(fed){
  if(all(is.na(fed))){
    daw<- NA
  }else{
    daw<- max(fed, na.rm = TRUE)
  }
  return(daw)
}


# function to find the Pollutant with the count for Pollutant with max AQI

max_pollu <- function(fed){
  if(all(is.na(fed))){
    daw<- NA
    o3 = 0
    co = 0
    pm25 = 0
    pm10 = 0
    so2 = 0
    no2 = 0
    count_line<- c(daw,o3,co,pm25,pm10,so2,no2)
  }else{
    chk_1<- max(fed, na.rm = TRUE)
    federico<-fed==chk_1
    o3 = 0
    co = 0
    pm25 = 0
    pm10 = 0
    so2 = 0
    no2 = 0
    indi_max<-which(federico)
    if(indi_max==1){
      daw<-"O3"
      o3<-1
    }
    if(indi_max==2){
      daw<-"CO"
      co<-1
    }
    if(indi_max==3){
      daw<-"PM25"
      pm25<-1
    }
    if(indi_max==4){
      daw<-"PM10"
      pm10<-1
    }
    if(indi_max==5){
      daw<-"SO2"
      so2<-1
    }
    if(indi_max==6){
      daw<-"NO2"
      no2<-1
    }
  }
  count_line<- (c(daw,o3,co,pm25,pm10,so2,no2))
  
  return(count_line)
}

# fed<-all_AQI[20,1:6]

# max_pollu(all_AQI[20,])

# attach the maximum
all_AQI$max_AQI <- apply(all_AQI[,1:6], 1, max_fe)

new_data<-data.frame()
new_data <- apply(all_AQI[,1:6], 1, max_pollu)
new_data <- t(new_data)
new_data <- as.data.frame(new_data)

# all_AQI$max_Pollutant <- apply(all_AQI[,1:6], 1, max_pollu)
# all_AQI$max_AQI <- apply(all_AQI, 1, max, na.rm = TRUE)

AQ_data_AQI <- cbind(AQ_data_AQI, all_AQI[,7],new_data)
# rename columns
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V1'] <- 'max_Pollutant'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V2'] <- 'count_O3'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V3'] <- 'count_CO'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V4'] <- 'count_PM25'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V5'] <- 'count_PM10'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V6'] <- 'count_SO2'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V7'] <- 'count_NO2'


# write_csv(AQ_data_AQI, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_AQI_counts_new.csv")
write_csv(AQ_data_AQI, "D:/AQI/AQ_data_AQI_counts_new_Dust_Event.csv")



####################################################################################

library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

### read data
AQ_data_AQI<- read_csv("D:/AQI/AQ_data_AQI_counts_new_Dust_Event.csv")


AQ_data_AQI_max <- AQ_data_AQI %>%
  group_by(Site,
           Date,
           Hour) %>%
  summarize(max_AQI = max(max_AQI))

AQ_data_AQI_Pollutant <- AQ_data_AQI_max %>%
  left_join(AQ_data_AQI, by = c("Site", "Date", "Hour", "max_AQI"))

# remove lines wtih NA in the max_AQI column
AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant[!is.na(AQ_data_AQI_Pollutant$max_AQI),]

AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant %>%
  select(Site,
         Date,
         Hour,
         max_AQI,
         max_Pollutant)



# create a time field
AQ_data_AQI_Pollutant$DATETIME <- paste0(AQ_data_AQI_Pollutant$Date, " ", AQ_data_AQI_Pollutant$Hour, ":00")

AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant %>%
  mutate(DateTime = ymd_hm(DATETIME))

AQ_data_AQI_Pollutant <- na.omit(AQ_data_AQI_Pollutant)


#########################################################
# make plots for all the monitoring stations (facets) ###
#########################################################


jpeg('D:/AQI/AQI_DUST_EVENT_2015_hours.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_AQI_Pollutant, aes(DateTime, max_AQI)) + 
  theme_bw() +
  geom_point(aes(y = max_AQI, col = "max_AQI"), alpha=1, col="red", size =1) +
  facet_wrap( ~ Site) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 14)) + 
  ylab(expression(paste("AQI"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour = "black")) +
  ylim(0, 300)  
plot


par(oldpar)
dev.off()


##### calculate average AQI at national level during the days of the dust event #####

UAE_AQI_DUST <- AQ_data_AQI_Pollutant %>%
  group_by(DateTime) %>%
  summarise(mean_AQI = mean(max_AQI))


min <- as.POSIXct("2015-03-29 01:00:00") 
max <- as.POSIXct("2015-04-05 13:00:00") 



jpeg('D:/AQI/UAE_MEAN_AQI_DUST_EVENT_2015_hours.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(UAE_AQI_DUST, aes(DateTime, mean_AQI)) + 
  theme_bw() +
  geom_line(aes(y = mean_AQI, col = "mean_AQI"), alpha=1, col="black", size =2) +
  theme(legend.position="none") + 
  ylab(expression(paste("mean AQI"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=30, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=30),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=30, colour = "black")) +
  ylim(0, 170)  +
  xlim(min, max) +
  
  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 2) +
#  geom_text(aes(x = min  , y = 55, label = "Good"), size = 6) +
  
  
  geom_hline(yintercept = 100, col="#ffff00", lty=1, size=2) +
#  geom_text(aes(x = min , y = 105, label = "Moderate"), size = 6) +
  
  geom_hline(yintercept = 150, col="#e59400", lty=1, size=2) +
 # geom_text(aes(x = as.POSIXct("2015-03-30 15:00:00") , y = 155, label = "Unhealthy for Sensitive Groups"), size = 6) 

geom_hline(yintercept = 200, col="#ff0000", lty=1, size=2) 
 #  geom_text(aes(x = 0.7 , y = 220, label = "Unhealthy"), size = 3) +

plot


par(oldpar)
dev.off()


############################################################################
######## box plot ##########################################################

jpeg('D:/AQI/UAE_BOXPLOT_AQI_DUST_EVENT_2015_hours.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_AQI_Pollutant, aes(Date, max_AQI)) +
  geom_boxplot(aes(group = cut_width(Date, 0.25)),fatten = 1.5, lwd=1, outlier.size = 4) + 
  theme_bw() +
  guides(fill=FALSE) +   # no legend
  theme(legend.position="none") + 
  ylab(expression(paste("AQI"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=40, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=40),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=40, colour = "black")) +
  ylim(0, 500)  +

  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 2) +
#  geom_text(aes(x = min  , y = 55, label = "Good"), size = 6) +
  
  
  geom_hline(yintercept = 100, col="#ffff00", lty=1, size=2) +
#  geom_text(aes(x = min , y = 105, label = "Moderate"), size = 6) +
  
  geom_hline(yintercept = 150, col="#e59400", lty=1, size=2)  +
#  geom_text(aes(x = min , y = 155, label = "Unhealthy for Sensitive Groups"), size = 6) +
  
  geom_hline(yintercept = 200, col="#ff0000", lty=1, size=2) +
#  geom_text(aes(x = 0.7 , y = 220, label = "Unhealthy"), size = 3) +
  
  geom_hline(yintercept = 300, col="#800080", lty=1, size=2) +
  #  geom_text(aes(x = 0.7 , y = 220, label = "Very Unhealthy"), size = 3) +
  
  geom_hline(yintercept = 500, col="#800000", lty=1, size=2) +
  #  geom_text(aes(x = 0.7 , y = 220, label = "Hazardous), size = 3) +
  
  # remove grids
 # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # remove background
  
  theme(legend.position="none")

plot


par(oldpar)
dev.off()

summary(AQ_data_AQI_Pollutant)




###############################################################################


jpeg('D:/AQI/UAE_Pollutant_BOXPLOT_AQI_DUST_EVENT_2015_hours.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_AQI_Pollutant, aes(max_Pollutant, max_AQI, fill = max_Pollutant)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(Date ~ .) +
#  guides(fill=FALSE) +   
  ylim(0, 500) +
  theme( strip.text = element_text(size = 14)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  ylab(expression("dominat AQI by pollutant")) +
  xlab(expression("dominant pollutant")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour="black")) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=14),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=14, colour="black")) +
  ggtitle("Air Quality Indexes (UAE 29-03-2015 - 04-04-2015)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) 

plot

par(oldpar)
dev.off()


  
  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 1) +
 # geom_text(aes(x = 0.7 , y = 70, label = "Good"), size = 3) +
  
  
  geom_hline(yintercept = 100, col="#ffff00", lty=1, size=1) +
#  geom_text(aes(x = 0.7 , y = 120, label = "Moderate"), size = 3) +
  
  geom_hline(yintercept = 150, col="#e59400", lty=1, size=1) +
 # geom_text(aes(x = 1.15 , y = 170, label = "Unhealthy for Sensitive Groups"), size = 3) +
  
  geom_hline(yintercept = 200, col="#ff0000", lty=1, size=1) +
#  geom_text(aes(x = 0.7 , y = 220, label = "Unhealthy"), size = 3) +
  
  geom_hline(yintercept = 200, col="#ff0000", lty=1, size=1) +
#  geom_text(aes(x = 0.7 , y = 220, label = "Unhealthy"), size = 3) +
  
  geom_hline(yintercept = 300, col="#800080", lty=1, size=2) +
  #  geom_text(aes(x = 0.7 , y = 220, label = "Very Unhealthy"), size = 3) +
  
  geom_hline(yintercept = 500, col="#800000", lty=1, size=2) +
  #  geom_text(aes(x = 0.7 , y = 220, label = "Hazardous), size = 3) +
  
  # remove grids
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  # remove background
  
#  theme(legend.position="none")

plot







