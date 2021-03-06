
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)

# load data

# Ozone data (8-hr)--------------------------------------------------------------------
dir_O3 <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3"
# dir_O3 <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3"


EAD_O3_2013 <- read.csv(paste0(dir_O3, "/","database_EAD_2013_O3_daily.csv"))
EAD_O3_2014 <- read.csv(paste0(dir_O3, "/","database_EAD_2014_O3_daily.csv"))
EAD_O3_2015 <- read.csv(paste0(dir_O3, "/","database_EAD_2015_O3_daily.csv"))
EAD_O3_2016 <- read.csv(paste0(dir_O3, "/","database_EAD_2016_O3_daily.csv"))

DM_O3_2013 <- read.csv(paste0(dir_O3, "/","database_DM_2013_O3_daily.csv"))
DM_O3_2014 <- read.csv(paste0(dir_O3, "/","database_DM_2014_O3_daily.csv"))
DM_O3_2015 <- read.csv(paste0(dir_O3, "/","database_DM_2015_O3_daily.csv"))
DM_O3_2016 <- read.csv(paste0(dir_O3, "/","database_DM_2016_O3_daily.csv"))

NCMS_O3_2013 <- read.csv(paste0(dir_O3, "/","database_NCMS_2013_O3_daily.csv"))
NCMS_O3_2014 <- read.csv(paste0(dir_O3, "/","database_NCMS_2014_O3_daily.csv"))
NCMS_O3_2015 <- read.csv(paste0(dir_O3, "/","database_NCMS_2015_O3_daily.csv"))
NCMS_O3_2016 <- read.csv(paste0(dir_O3, "/","database_NCMS_2016_O3_daily.csv"))

# bind data together
UAE_O3 <- rbind(EAD_O3_2013, EAD_O3_2014, EAD_O3_2015, EAD_O3_2016,
                DM_O3_2013, DM_O3_2014, DM_O3_2015, DM_O3_2016,
                NCMS_O3_2013, NCMS_O3_2014, NCMS_O3_2015, NCMS_O3_2016)

# UAE_O3 <- UAE_O3 %>%
#   mutate(DateTime = ymd_hms(Date))

# # add date field only
# UAE_O3 <- UAE_O3 %>%
#   mutate(Date = date(DateTime))

# conversion from ug/m3 to ppb (WHO conversion factor)
UAE_O3$MAX_8hour <- (UAE_O3$MAX_8hour) /1.96 

UAE_O3$Site <- as.character(UAE_O3$Site)

str(UAE_O3)


# change site names....
UAE_O3$Site  <- ifelse(grepl("ALAinIslamicIns", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Ain Islamic Ins", UAE_O3$Site)

UAE_O3$Site  <- ifelse(grepl("ALAinStreet", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Ain Street", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("AlMafraq", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Mafraq", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("AlQua0x27a", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Qua'a", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("AlRuwais", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Ruwais", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("AlTawia", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Tawia", UAE_O3$Site)
UAE_O3$Site <- ifelse(grepl("Bain Aljesrain", UAE_O3$Site, ignore.case = TRUE), 
                   "Bain Al Jesrain", UAE_O3$Site)
UAE_O3$Site <- ifelse(grepl("BainAljesrain", UAE_O3$Site, ignore.case = TRUE), 
                   "Bain Al Jesrain", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("baniyasSchool", UAE_O3$Site, ignore.case = TRUE), 
                    "Baniyas School", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("BidaZayed", UAE_O3$Site, ignore.case = TRUE), 
                    "Bida Zayed", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("E11Road", UAE_O3$Site, ignore.case = TRUE), 
                    "E11 Road", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("GayathiSchool", UAE_O3$Site, ignore.case = TRUE), 
                    "Gayathi School", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("Habshan", UAE_O3$Site, ignore.case = TRUE), 
                    "Habshan", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("HamdanStreet", UAE_O3$Site, ignore.case = TRUE), 
                    "Hamdan Street",UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("KhadejaPrimarySchool", UAE_O3$Site, ignore.case = TRUE), 
                    "Khadeja Primary School", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("KhalifaCityA", UAE_O3$Site, ignore.case = TRUE), 
                    "Khalifa City A", UAE_O3$Site)
UAE_O3$Site <- ifelse(grepl("KhalifaHighSchool", UAE_O3$Site, ignore.case = TRUE), 
                   "Khalifa High School", UAE_O3$Site)
UAE_O3$Site <- ifelse(grepl("LiwaOasis", UAE_O3$Site, ignore.case = TRUE), 
                   "Liwa Oasis", UAE_O3$Site)
UAE_O3$Site <- ifelse(grepl("Mussafah", UAE_O3$Site, ignore.case = TRUE),
                   "Mussafah", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("Sweihan", UAE_O3$Site, ignore.case = TRUE), 
                    "Sweihan", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("Zakher", UAE_O3$Site, ignore.case = TRUE), 
                    "Zakher", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("JEBELALIVILLAGE", UAE_O3$Site, ignore.case = TRUE), 
                    "JEBEL ALI VILLAGE", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("EMIRATESHILLS", UAE_O3$Site, ignore.case = TRUE), 
                    "EMIRATES HILLS", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("DUBAIAIRPORT", UAE_O3$Site, ignore.case = TRUE), 
                    "DUBAI AIR PORT", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("DUBAI AIRPORT", UAE_O3$Site, ignore.case = TRUE), 
                    "DUBAI AIR PORT", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("JEBELALIPORT", UAE_O3$Site, ignore.case = TRUE), 
                    "JEBEL ALI PORT", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", UAE_O3$Site, ignore.case = TRUE), 
                    "SHK. ZAYED ROAD", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", UAE_O3$Site, ignore.case = TRUE), 
                    "SHK. MOHD. BIN ZAYED ROAD", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("AlHamriyah", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Hamriyah", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("ELdErLyHouse", UAE_O3$Site, ignore.case = TRUE), 
                    "Elderly House", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("AlJeer", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Jeer", UAE_O3$Site)
UAE_O3$Site  <- ifelse(grepl("AlQasimiyah", UAE_O3$Site, ignore.case = TRUE), 
                    "Al Qasimiyah ", UAE_O3$Site)



# load stations infos
EAD <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_EAD_info.csv")
DM <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_DM_info.csv")
NCMS <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_NCMS_info.csv")
station_info <- rbind(EAD, DM, NCMS)

UAE_O3$Pollutant <- "O3"

# attach infos to ozone data
UAE_O3 <- UAE_O3 %>%
  left_join(station_info, c("Site", "Pollutant"))


names(UAE_O3)[names(UAE_O3) == 'Site.Type'] <- 'Site_Type'
# names(UAE_O3)[names(UAE_O3) == 'Value'] <- 'Max_O3_8hr'
UAE_O3$Site <- as.character(UAE_O3$Site)
names(UAE_O3)[names(UAE_O3) == 'Pollutant'] <- 'Pollutant_O3'
names(UAE_O3)[names(UAE_O3) == 'MAX_8hour'] <- 'Max_O3_8hr'


# str(UAE_O3)

UAE_O3 <- UAE_O3 %>%
  select(Date,
         Site,
         Pollutant_O3,
         Site_Type,
         Latitude,
         Longitude,
         Max_O3_8hr)

# create a field for the date & hour
UAE_O3 <- UAE_O3 %>%
  mutate(Date = date(Date))

write_csv(UAE_O3, "D:/AQI/UAE_O3_8h_Max.csv")


# CO data (8-hr)--------------------------------------------------------------------
dir_CO <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_CO"
# dir_CO <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_CO"


EAD_CO_2013 <- read.csv(paste0(dir_CO, "/","database_EAD_2013_CO_daily.csv"))
EAD_CO_2014 <- read.csv(paste0(dir_CO, "/","database_EAD_2014_CO_daily.csv"))
EAD_CO_2015 <- read.csv(paste0(dir_CO, "/","database_EAD_2015_CO_daily.csv"))
EAD_CO_2016 <- read.csv(paste0(dir_CO, "/","database_EAD_2016_CO_daily.csv"))

DM_CO_2013 <- read.csv(paste0(dir_CO, "/","database_DM_2013_CO_daily.csv"))
DM_CO_2014 <- read.csv(paste0(dir_CO, "/","database_DM_2014_CO_daily.csv"))
DM_CO_2015 <- read.csv(paste0(dir_CO, "/","database_DM_2015_CO_daily.csv"))
DM_CO_2016 <- read.csv(paste0(dir_CO, "/","database_DM_2016_CO_daily.csv"))

NCMS_CO_2013 <- read.csv(paste0(dir_CO, "/","database_NCMS_2013_CO_daily.csv"))
NCMS_CO_2014 <- read.csv(paste0(dir_CO, "/","database_NCMS_2014_CO_daily.csv"))
NCMS_CO_2015 <- read.csv(paste0(dir_CO, "/","database_NCMS_2015_CO_daily.csv"))
NCMS_CO_2016 <- read.csv(paste0(dir_CO, "/","database_NCMS_2016_CO_daily.csv"))

# bind data together
UAE_CO <- rbind(EAD_CO_2013, EAD_CO_2014, EAD_CO_2015, EAD_CO_2016,
                DM_CO_2013, DM_CO_2014, DM_CO_2015, DM_CO_2016,
                NCMS_CO_2013, NCMS_CO_2014, NCMS_CO_2015, NCMS_CO_2016)

# UAE_CO <- UAE_CO %>%
#   mutate(DateTime = ymd_hms(Date))


# conversion from mg/m3 to ppm (WHO conversion factor)
UAE_CO$MAX_8hour <- (UAE_CO$MAX_8hour) /1.15 


UAE_CO$Site <- as.character(UAE_CO$Site)
str(UAE_CO)



# change site names....
UAE_CO$Site  <- ifelse(grepl("ALAinIslamicIns", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Ain Islamic Ins", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("ALAinStreet", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Ain Street", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("AlMafraq", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Mafraq", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("AlQua0x27a", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Qua'a", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("AlRuwais", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Ruwais", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("AlTawia", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Tawia", UAE_CO$Site)
UAE_CO$Site <- ifelse(grepl("Bain Aljesrain", UAE_CO$Site, ignore.case = TRUE), 
                      "Bain Al Jesrain", UAE_CO$Site)
UAE_CO$Site <- ifelse(grepl("BainAljesrain", UAE_CO$Site, ignore.case = TRUE), 
                      "Bain Al Jesrain", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("baniyasSchool", UAE_CO$Site, ignore.case = TRUE), 
                       "Baniyas School", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("BidaZayed", UAE_CO$Site, ignore.case = TRUE), 
                       "Bida Zayed", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("E11Road", UAE_CO$Site, ignore.case = TRUE), 
                       "E11 Road", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("GayathiSchool", UAE_CO$Site, ignore.case = TRUE), 
                       "Gayathi School", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("Habshan", UAE_CO$Site, ignore.case = TRUE), 
                       "Habshan", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("HamdanStreet", UAE_CO$Site, ignore.case = TRUE), 
                       "Hamdan Street",UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("KhadejaPrimarySchool", UAE_CO$Site, ignore.case = TRUE), 
                       "Khadeja Primary School", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("KhalifaCityA", UAE_CO$Site, ignore.case = TRUE), 
                       "Khalifa City A", UAE_CO$Site)
UAE_CO$Site <- ifelse(grepl("KhalifaHighSchool", UAE_CO$Site, ignore.case = TRUE), 
                      "Khalifa High School", UAE_CO$Site)
UAE_CO$Site <- ifelse(grepl("LiwaOasis", UAE_CO$Site, ignore.case = TRUE), 
                      "Liwa Oasis", UAE_CO$Site)
UAE_CO$Site <- ifelse(grepl("Mussafah", UAE_CO$Site, ignore.case = TRUE),
                      "Mussafah", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("Sweihan", UAE_CO$Site, ignore.case = TRUE), 
                       "Sweihan", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("Zakher", UAE_CO$Site, ignore.case = TRUE), 
                       "Zakher", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("JEBELALIVILLAGE", UAE_CO$Site, ignore.case = TRUE), 
                       "JEBEL ALI VILLAGE", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("EMIRATESHILLS", UAE_CO$Site, ignore.case = TRUE), 
                       "EMIRATES HILLS", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("DUBAIAIRPORT", UAE_CO$Site, ignore.case = TRUE), 
                       "DUBAI AIR PORT", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("DUBAI AIRPORT", UAE_CO$Site, ignore.case = TRUE), 
                       "DUBAI AIR PORT", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("JEBELALIPORT", UAE_CO$Site, ignore.case = TRUE), 
                       "JEBEL ALI PORT", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", UAE_CO$Site, ignore.case = TRUE), 
                       "SHK. ZAYED ROAD", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", UAE_CO$Site, ignore.case = TRUE), 
                       "SHK. MOHD. BIN ZAYED ROAD", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("AlHamriyah", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Hamriyah", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("ELdErLyHouse", UAE_CO$Site, ignore.case = TRUE), 
                       "Elderly House", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("AlJeer", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Jeer", UAE_CO$Site)
UAE_CO$Site  <- ifelse(grepl("AlQasimiyah", UAE_CO$Site, ignore.case = TRUE), 
                       "Al Qasimiyah ", UAE_CO$Site)



# load stations infos
EAD <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_EAD_info.csv") 
DM <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_DM_info.csv") 
NCMS <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/Stations_NCMS_info.csv") 
station_info <- rbind(EAD, DM, NCMS)

UAE_CO$Pollutant <- "CO"

# station_info <- station_info %>%
#   filter(Pollutant =="CO")

# attach infos to ozone data
UAE_CO <- UAE_CO %>%
  left_join(station_info, c("Site", "Pollutant"))


names(UAE_CO)[names(UAE_CO) == 'Site.Type'] <- 'Site_Type'
# names(UAE_O3)[names(UAE_O3) == 'Value'] <- 'Max_O3_8hr'
UAE_CO$Site <- as.character(UAE_CO$Site)
names(UAE_CO)[names(UAE_CO) == 'Pollutant'] <- 'Pollutant_CO'
names(UAE_CO)[names(UAE_CO) == 'MAX_8hour'] <- 'Max_CO_8hr'



UAE_CO <- UAE_CO %>%
  select(Date,
         Site,
         Pollutant_CO,
         Site_Type,
         Latitude,
         Longitude,
         Max_CO_8hr)


# create a field for the date & hour
UAE_CO <- UAE_CO %>%
  mutate(Date = date(Date))

write_csv(UAE_CO, "D:/AQI/UAE_CO_8h_Max.csv")


##################################################################################
# AQ data (24-hr daily averages, filtered from outliers 4-box plots)-------------

dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/Daily_mean"
# dir_AQ <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/daily_filtered_4_box"
# dir_AQ <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/daily moved/Daily_mean"

EAD_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_EAD_2013_daily_mean.csv"))
EAD_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_EAD_2014_daily_mean.csv"))
EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD_2015_daily_mean.csv"))
EAD_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_EAD_2016_daily_mean.csv"))

DM_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_DM_2013_daily_mean.csv"))
DM_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_DM_2014_daily_mean.csv"))
DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_2015_daily_mean.csv"))
DM_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_DM_2016_daily_mean.csv"))

NCMS_AQ_2013 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2013_daily_mean.csv"))
NCMS_AQ_2014 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2014_daily_mean.csv"))
NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2015_daily_mean.csv"))
NCMS_AQ_2016 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2016_daily_mean.csv"))

# bind data together
UAE_AQ <- rbind(EAD_AQ_2013, EAD_AQ_2014, EAD_AQ_2015, EAD_AQ_2016,
                DM_AQ_2013, DM_AQ_2014, DM_AQ_2015, DM_AQ_2016,
                NCMS_AQ_2013, NCMS_AQ_2014, NCMS_AQ_2015, NCMS_AQ_2016)

UAE_AQ$Site <- as.character(UAE_AQ$Site)
UAE_AQ$Site_Type <- as.character(UAE_AQ$Site_Type)
UAE_AQ$Pollutant <- as.character(UAE_AQ$Pollutant)
 str(UAE_AQ)


# change site names....
UAE_AQ$Site  <- ifelse(grepl("ALAinIslamicIns", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Ain Islamic Ins", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("ALAinStreet", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Ain Street", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlMafraq", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Mafraq", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlQua0x27a", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Qua'a", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlRuwais", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Ruwais", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlTawia", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Tawia", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("Bain Aljesrain", UAE_AQ$Site, ignore.case = TRUE), 
                      "Bain Al Jesrain", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("BainAljesrain", UAE_AQ$Site, ignore.case = TRUE), 
                      "Bain Al Jesrain", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("baniyasSchool", UAE_AQ$Site, ignore.case = TRUE), 
                       "Baniyas School", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("BidaZayed", UAE_AQ$Site, ignore.case = TRUE), 
                       "Bida Zayed", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("E11Road", UAE_AQ$Site, ignore.case = TRUE), 
                       "E11 Road", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("GayathiSchool", UAE_AQ$Site, ignore.case = TRUE), 
                       "Gayathi School", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("Habshan", UAE_AQ$Site, ignore.case = TRUE), 
                       "Habshan", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("HamdanStreet", UAE_AQ$Site, ignore.case = TRUE), 
                       "Hamdan Street",UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("KhadejaPrimarySchool", UAE_AQ$Site, ignore.case = TRUE), 
                       "Khadeja Primary School", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("KhalifaCityA", UAE_AQ$Site, ignore.case = TRUE), 
                       "Khalifa City A", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("KhalifaHighSchool", UAE_AQ$Site, ignore.case = TRUE), 
                      "Khalifa High School", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("LiwaOasis", UAE_AQ$Site, ignore.case = TRUE), 
                      "Liwa Oasis", UAE_AQ$Site)
UAE_AQ$Site <- ifelse(grepl("Mussafah", UAE_AQ$Site, ignore.case = TRUE),
                      "Mussafah", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("Sweihan", UAE_AQ$Site, ignore.case = TRUE), 
                       "Sweihan", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("Zakher", UAE_AQ$Site, ignore.case = TRUE), 
                       "Zakher", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("JEBELALIVILLAGE", UAE_AQ$Site, ignore.case = TRUE), 
                       "JEBEL ALI VILLAGE", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("EMIRATESHILLS", UAE_AQ$Site, ignore.case = TRUE), 
                       "EMIRATES HILLS", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("DUBAIAIRPORT", UAE_AQ$Site, ignore.case = TRUE), 
                       "DUBAI AIR PORT", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("DUBAI AIRPORT", UAE_AQ$Site, ignore.case = TRUE), 
                       "DUBAI AIR PORT", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("JEBELALIPORT", UAE_AQ$Site, ignore.case = TRUE), 
                       "JEBEL ALI PORT", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", UAE_AQ$Site, ignore.case = TRUE), 
                       "SHK. ZAYED ROAD", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", UAE_AQ$Site, ignore.case = TRUE), 
                       "SHK. MOHD. BIN ZAYED ROAD", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlHamriyah", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Hamriyah", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("ELdErLyHouse", UAE_AQ$Site, ignore.case = TRUE), 
                       "Elderly House", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlJeer", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Jeer", UAE_AQ$Site)
UAE_AQ$Site  <- ifelse(grepl("AlQasimiyah", UAE_AQ$Site, ignore.case = TRUE), 
                       "Al Qasimiyah ", UAE_AQ$Site)


# names(UAE_AQ)[names(UAE_AQ) == 'Daily_mean'] <- 'Value'
# UAE_AQ$Date <- as.POSIXct(as.Date(UAE_AQ$Date, "%Y-%m-%d")) 
UAE_AQ$Date <- as.Date(UAE_AQ$Date)
str(UAE_AQ)

UAE_AQ <- UAE_AQ %>%
  select(Date,
         Site,
         Pollutant,
         Site_Type,
         Latitude,
         Longitude,
         Daily_mean)

str(UAE_AQ)

# select only PM25
UAE_PM25 <- UAE_AQ %>%
  filter(Pollutant == c("PM2.5"))

names(UAE_PM25)[names(UAE_PM25) == 'Pollutant'] <- 'Pollutant_PM25'

str(UAE_PM25)

# select only PM10
UAE_PM10 <- UAE_AQ %>%
  filter(Pollutant == c("PM10"))

names(UAE_PM10)[names(UAE_PM10) == 'Pollutant'] <- 'Pollutant_PM10'

names(UAE_PM25)[names(UAE_PM25) == 'Daily_mean'] <- 'PM25_24hr'
names(UAE_PM10)[names(UAE_PM10) == 'Daily_mean'] <- 'PM10_24hr'


str(UAE_PM25)



# SO2 and NO2 (1-hr)---------------------------------------------------------------------
# for EAD use filtered data (4 boxplot)
dir_SO2_NO2 <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates" 
# dir_SO2_NO2 <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box" 
# dir_SO2_NO2 <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates" 


EAD_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_2013_hourly.csv"))
EAD_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_2014_hourly.csv"))
EAD_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_2015_hourly.csv"))
EAD_SO2_NO2_2015$DateTime <- EAD_SO2_NO2_2015$DateTime +3   # seconds
EAD_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_EAD_2016_hourly.csv"))
EAD_SO2_NO2_2016$DateTime <- EAD_SO2_NO2_2016$DateTime +3  # seconds

DM_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_2013_hourly.csv"))
DM_SO2_NO2_2013$DateTime <- DM_SO2_NO2_2013$DateTime +3    # seconds
DM_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_2014_hourly.csv"))
DM_SO2_NO2_2014$DateTime <- DM_SO2_NO2_2014$DateTime +3    # seconds
DM_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_2015_hourly.csv"))
DM_SO2_NO2_2015$DateTime <- DM_SO2_NO2_2015$DateTime +3    # seconds
DM_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_DM_2016_hourly.csv"))
DM_SO2_NO2_2016$DateTime <- DM_SO2_NO2_2016$DateTime +3    # seconds

NCMS_SO2_NO2_2013 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_2013_hourly.csv"))
NCMS_SO2_NO2_2014 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_2014_hourly.csv"))
NCMS_SO2_NO2_2015 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_2015_hourly.csv"))
NCMS_SO2_NO2_2016 <- read_csv(paste0(dir_SO2_NO2, "/","database_NCMS_2016_hourly.csv"))


SO2_NO2_all <- rbind(EAD_SO2_NO2_2013, EAD_SO2_NO2_2014, EAD_SO2_NO2_2015, EAD_SO2_NO2_2016,
                     DM_SO2_NO2_2013, DM_SO2_NO2_2014, DM_SO2_NO2_2015, DM_SO2_NO2_2016,
                     NCMS_SO2_NO2_2013, NCMS_SO2_NO2_2014, NCMS_SO2_NO2_2015, NCMS_SO2_NO2_2016)

SO2_NO2_all <- SO2_NO2_all %>%
  select(DateTime,
         Site,
         Pollutant,
         Site_Type,
         Latitude,
         Longitude,
         Value) 

str(SO2_NO2_all)

# change site names....
SO2_NO2_all$Site  <- ifelse(grepl("ALAinIslamicIns", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Ain Islamic Ins", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("ALAinStreet", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Ain Street", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("AlMafraq", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Mafraq", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("AlQua0x27a", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Qua'a", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("AlRuwais", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Ruwais", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("AlTawia", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Tawia", SO2_NO2_all$Site)
SO2_NO2_all$Site <- ifelse(grepl("Bain Aljesrain", SO2_NO2_all$Site, ignore.case = TRUE), 
                      "Bain Al Jesrain", SO2_NO2_all$Site)
SO2_NO2_all$Site <- ifelse(grepl("BainAljesrain", SO2_NO2_all$Site, ignore.case = TRUE), 
                      "Bain Al Jesrain", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("baniyasSchool", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Baniyas School", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("BidaZayed", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Bida Zayed", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("E11Road", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "E11 Road", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("GayathiSchool", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Gayathi School", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("Habshan", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Habshan", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("HamdanStreet", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Hamdan Street",SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("KhadejaPrimarySchool", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Khadeja Primary School", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("KhalifaCityA", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Khalifa City A", SO2_NO2_all$Site)
SO2_NO2_all$Site <- ifelse(grepl("KhalifaHighSchool", SO2_NO2_all$Site, ignore.case = TRUE), 
                      "Khalifa High School", SO2_NO2_all$Site)
SO2_NO2_all$Site <- ifelse(grepl("LiwaOasis", SO2_NO2_all$Site, ignore.case = TRUE), 
                      "Liwa Oasis", SO2_NO2_all$Site)
SO2_NO2_all$Site <- ifelse(grepl("Mussafah", SO2_NO2_all$Site, ignore.case = TRUE),
                      "Mussafah", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("Sweihan", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Sweihan", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("Zakher", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Zakher", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("JEBELALIVILLAGE", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "JEBEL ALI VILLAGE", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("EMIRATESHILLS", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "EMIRATES HILLS", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("DUBAIAIRPORT", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "DUBAI AIR PORT", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("DUBAI AIRPORT", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "DUBAI AIR PORT", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("JEBELALIPORT", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "JEBEL ALI PORT", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "SHK. ZAYED ROAD", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "SHK. MOHD. BIN ZAYED ROAD", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("AlHamriyah", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Hamriyah", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("ELdErLyHouse", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Elderly House", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("AlJeer", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Jeer", SO2_NO2_all$Site)
SO2_NO2_all$Site  <- ifelse(grepl("AlQasimiyah", SO2_NO2_all$Site, ignore.case = TRUE), 
                       "Al Qasimiyah ", SO2_NO2_all$Site)

# select SO2
UAE_SO2 <- SO2_NO2_all %>%
  filter(Pollutant == "SO2") 

# filter our negative values
UAE_SO2 <- UAE_SO2 %>%
  filter(Value >= 0 | is.na(Value))


# create a field for the date & hour
UAE_SO2 <- UAE_SO2 %>%
  mutate(Date = date(DateTime),
         Hour = hour(DateTime))



# select NO2
UAE_NO2 <- SO2_NO2_all %>%
  filter(Pollutant == "NO2") 

# filter our negative values
UAE_NO2 <- UAE_NO2 %>%
  filter(Value >= 0 | is.na(Value))

# create a field for the date & hour
UAE_NO2 <- UAE_NO2 %>%
  mutate(Date = date(DateTime),
         Hour = hour(DateTime))


# names(SO2_NO2_all)[names(SO2_NO2_all) == 'DateTime'] <- 'Date'



# conversion from ug/m3 to ppb (WHO conversion factor)
# SO2 
UAE_SO2$Value <- UAE_SO2$Value /2.62
names(UAE_SO2)[names(UAE_SO2) == 'Pollutant'] <- 'Pollutant_SO2'

# NO2 
UAE_NO2$Value <- UAE_NO2$Value /1.88
names(UAE_NO2)[names(UAE_NO2) == 'Pollutant'] <- 'Pollutant_NO2'

names(UAE_SO2)[names(UAE_SO2) == 'Value'] <- 'SO2_1hr'
names(UAE_NO2)[names(UAE_NO2) == 'Value'] <- 'NO2_1hr'

# # filter out ozone data (daily average)
# UAE_AQ <- UAE_AQ %>%
#   filter(Pollutant != "O3")

########################################
### Bind all Data Together #############
########################################

######################## join O3 and CO #########
AAA <- UAE_O3 %>%
  left_join(UAE_CO, by = c("Date", "Site", "Latitude", "Longitude", "Site_Type"), all=TRUE)

########################

######################## join O3, CO, SO2 #######
BBB <- AAA %>%
  left_join(UAE_SO2, c("Date", "Site", "Latitude", "Longitude", "Site_Type"), all=TRUE)

# save intermediate files because there is not memory

 write_csv(BBB, "D:/AQI/BBB.csv")
 write_csv(UAE_NO2, "D:/AQI/UAE_NO2.csv")

 ########################################################################
 ########################################################################
 ########################################################################
 ########################################################################
 # reload data......
 
 
 setwd("D:/AQI")
 BBB <- read_csv("D:/AQI/BBB.csv")
 UAE_NO2 <- read_csv("D:/AQI/UAE_NO2.csv")
 
 
 # filter data on the days of the dust storm event on 2 April 2015
 BBB <- BBB %>%
   filter(Date >= "2015-03-29" & Date <= "2015-04-04") 
 
 UAE_NO2 <- UAE_NO2 %>%
   filter(Date >= "2015-03-29" & Date <= "2015-04-04") 
 
 UAE_PM25 <- UAE_PM25 %>%
   filter(Date >= "2015-03-29" & Date <= "2015-04-04") 
 
 UAE_PM10 <- UAE_PM10 %>%
   filter(Date >= "2015-03-29" & Date <= "2015-04-04") 

 CCC <- BBB %>%
   left_join(UAE_NO2, c("Date",  "Site", "Latitude", "Longitude", "Site_Type", "Hour"),  all=TRUE)


########################

AQ_data <- CCC %>%
  left_join(UAE_PM25, c("Date", "Site", "Latitude", "Longitude", "Site_Type"), all=TRUE)

AQ_data <- AQ_data %>%
  left_join(UAE_PM10, c("Date", "Site", "Latitude", "Longitude", "Site_Type"),all=TRUE)

str(AQ_data)

 write_csv(AQ_data, "D:/AQI/AQ_data_all_DUST_event.csv")
# write_csv(AQ_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_all.csv")

################################################################################
## AIR QUALITY INDEXES ##-------------------------------------------------------
################################################################################

AQ_data <- read_csv("D:/AQI/AQ_data_all_DUST_event.csv")
# AQ-data <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_all_no_outliers.csv")

 # remove lines wtih NA in the max_AQI column
 AQ_data <- AQ_data[!is.na(AQ_data$Hour),]
 
 str(AQ_data)

 AQ_data_clean <- AQ_data %>%
   select(-DateTime.x,
          -DateTime.y)
 
 

str(AQ_data_clean)

AQ_data_clean$PM25_24hr <- as.numeric(AQ_data_clean$PM25_24hr)

 write_csv(AQ_data_clean, "D:/AQI/AQ_data_all_clean_DUST_event.csv" )
# write_csv(AQ_data_clean, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_all_clean.csv" )


#####################################################################################
#### correct existing data with Max daily 8h O3 and CO values #######################
 
 
 AQ_data_all_clean <- read_csv("D:/AQI/AQ_data_all_clean_DUST_event.csv" )
 
 O3_max <- read_csv("D:/AQI/UAE_O3_8h_Max.csv")
 CO_max <- read_csv("D:/AQI/UAE_CO_8h_Max.csv")
 
 # filter data on the days of the dust storm event on 2 April 2015
 O3_max <- O3_max %>%
   filter(Date >= "2015-03-29" & Date <= "2015-04-04") 
 
 CO_max <- CO_max %>%
   filter(Date >= "2015-03-29" & Date <= "2015-04-04") 
 
 
 
 AQ_data_all_clean <- AQ_data_all_clean %>%
   select(- Pollutant_O3,
          - Max_O3_8hr,
          - Pollutant_CO,
          - Max_CO_8hr)
 
 
 AQ_data_all_clean <- AQ_data_all_clean %>%
   left_join(O3_max, c("Date", "Site", "Latitude", "Longitude", "Site_Type"), all=TRUE)
 
 AQ_data_all_clean <- AQ_data_all_clean %>%
   left_join(CO_max, c("Date", "Site", "Latitude", "Longitude", "Site_Type"), all=TRUE)
 
 write_csv(AQ_data_all_clean, "D:/AQI/AQ_data_all_clean_new_DUST_event.csv")
 
 
#############################################################################
#############################################################################
 
 
 
 
 
 
