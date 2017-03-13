
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)

################################################################################
## AIR QUALITY INDEXES ##-------------------------------------------------------
################################################################################


AQ_data <-read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_all_clean.csv")

# load function for AQI calcualtions
source("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/aqi_fun.R")

###########
### O3 ####
########### each hour (8-hr average) ppb

O3_data <- as.vector(AQ_data$O3_8hr)

# calculate Air Quality index for O3
aqi_O3 <- lapply(O3_data, aqi_O3_fun)
aqi_O3 <- as.numeric(aqi_O3)

aqi_O3 <- as.data.frame(aqi_O3)


#############
### CO ####
############# every 1-hr (8-hr average) ppm

CO_data <- as.vector(AQ_data$CO_8hr)

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
write_csv(AQ_data_AQI, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_AQI.csv")

##########################################################################
##########################################################################

# find maximum AQI in each row

library(dplyr)
library(leaflet)
library(readr)
library(lubridate)

AQ_data_AQI <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_AQI.csv")


AQ_data_AQI <- AQ_data_AQI[6,]

all_AQI <- AQ_data_AQI %>%
  dplyr::select(aqi_O3,
                aqi_CO,
                aqi_PM25,
                aqi_PM10,
                aqi_SO2,
                aqi_NO2)
  
all_AQI$aqi_PM25 <- as.numeric(all_AQI$aqi_PM25)

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


write_csv(AQ_data_AQI, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_AQI_counts.csv")

####################################################################################

library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

### read data
AQ_data_AQI<- read_csv("D:/AQI/AQ_data_AQI_counts.csv")
AQ_data_AQI <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_AQI_counts.csv")
AQ_data_AQI <- AQ_data_AQI %>%
  select(- max_Pollutant_1,
         - max_AQI_1)



## get the months of observations
AQ_data_AQI$month <- factor(format(AQ_data_AQI$Date, format = "%b"), levels = month.abb)

## Define seasons
AQ_data_AQI$season <- character(length = nrow(AQ_data_AQI))
AQ_data_AQI$season[AQ_data_AQI$month %in% month.abb[c(1:2)]] <- "winter"
AQ_data_AQI$season[AQ_data_AQI$month %in% month.abb[c(12)]] <- "winter"
AQ_data_AQI$season[AQ_data_AQI$month %in% month.abb[c(3:5)]] <- "spring"
AQ_data_AQI$season[AQ_data_AQI$month %in% month.abb[c(6:8)]] <- "summer"
AQ_data_AQI$season[AQ_data_AQI$month %in% month.abb[c(9:11)]] <- "fall"
AQ_data_AQI$season <- factor(AQ_data_AQI$season, levels = c("winter","spring","summer","fall"))


# remove lines wtih NA in the max_Pollutant column
AQ_data_AQI <- AQ_data_AQI[!is.na(AQ_data_AQI$max_Pollutant),]

# remove stations with high CO (probably something wrong there)
AQ_data_AQI <- AQ_data_AQI %>%
  filter(!Site==c("Bain Al Jesrain")) %>%
  filter(!Site==c("Zabeel")) %>%
  filter(!Site==c("Safa")) %>%
  filter(!Site==c("Karama")) %>%
  filter(!Site==c("JEBEL ALI VILLAGE"))

# ADD YEAR AS VARIABLE ##---------------------------------------------------
AQ_data_AQI <- AQ_data_AQI %>%
  mutate(year = year(Date))

#### plot summary of AQI ----------------------------------------------------

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Summary_AQI_UAE_2013_2016.jpg',    
     quality = 100, bg = "white", res = 200, width = 9, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_AQI, aes(max_Pollutant, max_AQI, fill = max_Pollutant)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(season ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 250) +
  theme( strip.text = element_text(size = 18)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  ylab(expression("dominat AQI by pollutant")) +
  xlab(expression("dominant pollutant")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour="black")) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=16),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=16, colour="black")) +
  ggtitle("Air Quality Indexes (UAE 2013-2016)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) +
  
  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 1) +
  geom_text(aes(x = 0.7 , y = 70, label = "Good"), size = 3) +


 geom_hline(yintercept = 100, col="#ffff00", lty=1, size=1) +
 geom_text(aes(x = 0.7 , y = 120, label = "Moderate"), size = 3) +

 geom_hline(yintercept = 150, col="#e59400", lty=1, size=1) +
 geom_text(aes(x = 1.15 , y = 170, label = "Unhealthy for Sensitive Groups"), size = 3) +

 geom_hline(yintercept = 200, col="#ff0000", lty=1, size=1) +
 geom_text(aes(x = 0.7 , y = 220, label = "Unhealthy"), size = 3) +
  
  # remove grids
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # remove background
  
  theme(legend.position="none")
  
plot

par(oldpar)
dev.off()


###################################################################################

### Annual AQIs-------------------------------------------------------------------

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQI_annual.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


AQ_data_AQI$year <- as.factor(AQ_data_AQI$year)


plot <- ggplot(AQ_data_AQI, aes(year, max_AQI, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 150) +
  guides(fill=FALSE) +   # no legend
  ylab(expression("Air Quality Index (AQI)")) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  ggtitle("annual distribution of Air Quality Index") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 1) +
  geom_text(aes(x = 0.7 , y = 54, label = "Good"), size = 8) +
  
  
  geom_hline(yintercept = 100, col="#ffff00", lty=1, size=1) +
  geom_text(aes(x = 0.7 , y = 104, label = "Moderate"), size = 7)
  
plot


par(oldpar)
dev.off()










# geom_hline(yintercept = 300, col="#800080", lty=1, size=1) +
# geom_text(aes(x = 7 , y = 310, label = "Unhealthy for Very Vnhealthy", color = "#800080")) +
# 
