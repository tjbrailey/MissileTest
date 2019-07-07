### Missile Test Temporal Study ###
          ### POLI 170A ###

# Setup
setwd(paste0(getwd(), '/data/'))
wd<-setwd(getwd())

# Install required packages
library(magrittr)
library(ggplot2)

# Install NK xlsx doc
files <- list.files(wd, "north_korea_missile_test_database.xlsx")
files <- files[]

read_excel_allsheets <- function(filename) { 
  sheets <- readxl::excel_sheets(filename) 
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X)) 
  names(x) <- sheets 
  x 
} 

out <- lapply(files, read_excel_allsheets)
basename(files)

nk_missile_tests <- out[[1]]$`Missile Tests`
nk_data_summary <- out[[1]]$`Data Summary`
nk_missile_summary <- out[[1]]$`Missile Summary`
nk_facilities <- out[[1]]$`Facilities`

# Install IRN xlsx doc
files <- list.files(wd, "iran_missile_launch_database.xlsx")
files <- files[]

out <- lapply(files, read_excel_allsheets)
basename(files)

irn_missile_tests <- out[[1]]$`Iran Database`
irn_data_summary <- out[[1]]$`Data Summary`
irn_missile_summary <- out[[1]]$`Missile Summary`

rm(out, files, read_excel_allsheets)

# Install IRQ data
irq_missile_tests <- rio::import("iraq_missile_launch_database.csv")
colnames(irq_missile_tests) <- as.character(irq_missile_tests[1,])
irq_missile_tests <- irq_missile_tests[-1,]

# Clean data
nk_missile_tests$Country <- "North Korea"
nk_missile_tests <- nk_missile_tests %>%
  dplyr::rename(EventID = F1,
                MissileFamily = `Missile Type`,
                FacilityLatitude = `Facility Latitude`,
                Confirmation = `Confirmation Status`,
                FacilityLongitude = `Facility Longitude`,
                TestOutcome = `Test Outcome`,
                DateEntered = `Date Entered/Updated`, 
                FacilityName = `Facility Name`,
                LandingLocation = `Landing Location`,
                Source = `Source(s)`,
                MissileName = `Missile Name`,
                DistanceTravelled = `Distance Travelled`,
                AdditionalInformation = `Additional Information`,
                OtherName = `Other Name`,
                FacilityLocation = `Facility Location`,
                LaunchAgency = `Launch Agency/Authority`,
                LaunchTimeUTC = `Launch Time (UTC)`)
nk_missile_tests$Confirmation[nk_missile_tests$Confirmation == "Confirmed"] <- TRUE
nk_missile_tests$Confirmation[nk_missile_tests$Confirmation == "Unconfirmed"] <- FALSE
nk_missile_tests$Confirmation <- as.logical(nk_missile_tests$Confirmation)
nk_missile_tests$Apogee <- gsub("[a-zA-Z/, ]", "", nk_missile_tests$Apogee)
nk_missile_tests$Apogee <- as.numeric(nk_missile_tests$Apogee)
nk_missile_tests$DistanceTravelled <- gsub("[a-zA-Z/, ]", "", nk_missile_tests$DistanceTravelled)
nk_missile_tests$DistanceTravelled <- as.numeric(nk_missile_tests$DistanceTravelled)
nk_missile_tests$Date <- as.character(nk_missile_tests$Date)

irn_missile_tests$Country <- "Iran"
irn_missile_tests <- irn_missile_tests %>% 
  dplyr::rename(Date = DateOccurred)
irn_missile_tests$Date <- as.character(irn_missile_tests$Date)

irq_missile_tests$Country <- "Iraq"
irq_missile_tests <- irq_missile_tests %>%
  dplyr::rename(MissileName = "",
                AdditionalInformation = Status,
                MaxRange = `Maximum Range (km)`,
                PayloadKG = `Payload (kg)`)
irq_missile_tests$Date[irq_missile_tests$Date == "05-Dec-89"] <- "1989-10-05"
irq_missile_tests$Date[irq_missile_tests$Date == "Jun-00"] <- "2000-06-01"
irq_missile_tests$Date[irq_missile_tests$Date == "May-93"] <- "1993-05-01"
# Join data
join <- dplyr::full_join(nk_missile_tests, irn_missile_tests)
join <- dplyr::full_join(join, irq_missile_tests)
#save(tb_data, file = paste0(here::here(), '/data/tb_data.csv'))
