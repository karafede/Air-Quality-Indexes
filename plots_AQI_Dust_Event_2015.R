
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(leaflet)

### read data
AQ_data_AQI<- read_csv("D:/AQI/AQ_data_AQI_counts_new.csv")


# filter data on the days of the dust storm event on 2 April 2015
AQ_data_AQI <- AQ_data_AQI %>%
    filter(Date >= "2015-03-29" & Date <= "2015-04-04")  


# create a dataframe with max_AQI and its max or dominant pollutant

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



# create a time field...?
AQ_data_AQI_Pollutant$DATETIME <- paste0(AQ_data_AQI_Pollutant$Date, " ", AQ_data_AQI_Pollutant$Hour, ":00")

AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant %>%
  mutate(DateTime = ymd_hm(DATETIME))

AQ_data_AQI_Pollutant <- na.omit(AQ_data_AQI_Pollutant)


# AQ_data_AQI <- AQ_data_AQI %>%
# filter(Site %in% c("Khalifa City A", "Hamdan Street",
#                    "Liwa Oasis", "Khadeja Primary School",
#                    "Bida Zayed", "Mussafah"))

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
max <- as.POSIXct("2015-04-04 23:00:00") 



jpeg('D:/AQI/UAE_MEAN_AQI_DUST_EVENT_2015_hours.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(UAE_AQI_DUST, aes(DateTime, mean_AQI)) + 
  theme_bw() +
  geom_line(aes(y = mean_AQI, col = "mean_AQI"), alpha=1, col="black", size =2) +
  theme(legend.position="none") + 
  ylab(expression(paste("AQI"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=28, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=28),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=28, colour = "black")) +
  ylim(0, 170)  +
  xlim(min, max) +
  
  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 2) +
  geom_text(aes(x = min  , y = 55, label = "Good"), size = 6) +
  
  
  geom_hline(yintercept = 100, col="#ffff00", lty=1, size=2) +
  geom_text(aes(x = min , y = 105, label = "Moderate"), size = 6) +
  
  geom_hline(yintercept = 150, col="#e59400", lty=1, size=2) +
  geom_text(aes(x = as.POSIXct("2015-03-30 15:00:00") , y = 155, label = "Unhealthy for Sensitive Groups"), size = 6) 
plot


par(oldpar)
dev.off()


############################################################################
######## box plot ##########################################################

jpeg('D:/AQI/UAE_BOXPLOT_AQI_DUST_EVENT_2015_hours.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

min <- as.Date("2015-03-29") 
max <- as.Date("2015-04-04") 


plot <- ggplot(AQ_data_AQI_Pollutant, aes(Date, max_AQI)) +
  geom_boxplot(aes(group = cut_width(Date, 0.25))) + 
  theme_bw() +
  guides(fill=FALSE) +   # no legend
  theme(legend.position="none") + 
  ylab(expression(paste("AQI"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=28, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=28),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=28, colour = "black")) +
  ylim(0, 300)  +
  xlim(min, max) +
  
  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 2) +
  geom_text(aes(x = min  , y = 55, label = "Good"), size = 6) +
  
  
  geom_hline(yintercept = 100, col="#ffff00", lty=1, size=2) +
  geom_text(aes(x = min , y = 105, label = "Moderate"), size = 6) +
  
  geom_hline(yintercept = 150, col="#e59400", lty=1, size=2) +
  geom_text(aes(x = min , y = 155, label = "Unhealthy for Sensitive Groups"), size = 6) +


# remove grids
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # remove background
  
  theme(legend.position="none")

plot




par(oldpar)
dev.off()


###############################################################################


jpeg('D:/AQI/UAE_Pollutant_BOXPLOT_AQI_DUST_EVENT_2015_hours.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_AQI_Pollutant, aes(max_Pollutant, max_AQI, fill = max_Pollutant)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(Date ~ .) +
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







  