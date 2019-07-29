### Missile Test Temporal Study ###
        ### POLI 170A ###

rm(list = ls())

# Setup
setwd(paste0(here::here(), '/data/'))
wd <- setwd(paste0(here::here(), '/data/'))


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

# Install PAK data
pak_missile_tests <- rio::import("pakistan_missile_launch_database.csv")

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
join <- dplyr::full_join(join, pak_missile_tests)

missile_dat_final <- join
rm(join)

# Clean
missile_dat_final$Date <- as.character(missile_dat_final$Date)
missile_dat_final$DateEntered <- as.character(missile_dat_final$DateEntered)
missile_dat_final$LaunchTimeUTC <- as.character(missile_dat_final$LaunchTimeUTC)

missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Scud-B"] <- "SRBM" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Al Hussein"] <- "SRBM" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Al Abbas"] <- "SRBM" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Condor II/ BADR-2000"] <- "MRBM" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "FK120/ Sakr 200"] <- "SRBM" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Fahad (Al Fahd)"] <- "SRBM" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Al Abid"] <- "SLV" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Tammuz I"] <- "SLV" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Al Samoud"] <- "TBM" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "Al Ababil"] <- "SRBM" 
missile_dat_final$MissileFamily[missile_dat_final$MissileName == "J-1"] <- "SRBM"

  # Create new variables to be manually completed
missile_dat_final$EventUNSCResolution <- NA
missile_dat_final$EventHOSVisit <- NA
missile_dat_final$EventHOSTravel <- NA
missile_dat_final$EventNotes <- NA
missile_dat_final$EventSource <- NA
missile_dat_final$TestCount <- NA
missile_dat_final$Crisis <- NA

missile_dat_final$TestDummy <- 1

  # Set Y to country/year/month
missile_dat_final$Date <- stringr::str_sub(missile_dat_final$Date, end = -4)
missile_dat_final$Month <- stringr::str_sub(missile_dat_final$Date, start = 6)
missile_dat_final$Month <- as.numeric(missile_dat_final$Month)

missile_dat_final$Date <- stringr::str_sub(missile_dat_final$Date, end = 4)
missile_dat_final <- dplyr::rename(missile_dat_final, Year = Date)
missile_dat_final$Year = as.numeric(missile_dat_final$Year) 

missile_dat_final <- missile_dat_final %>%
  dplyr::select(-EventID) %>%
  dplyr::select(Country, 
                Year, 
                Month, 
                TestDummy, 
                TestCount, 
                dplyr::everything())

    # Get unique missile occurances 
missile_dat_final <- missile_dat_final %>%
  dplyr::distinct(Year, Month, Country, .keep_all = TRUE) %>% 
  tidyr::drop_na(Year)

    # Complete years and months 
missile_dat_final <- dplyr::arrange(missile_dat_final, Country) %>%
  dplyr::group_by(Country)

missile_dat_final <- missile_dat_final %>%
  tidyr::complete(Country, Year = 1984:2019, 
                  fill = list(incidents = 0)) %>%
  tidyr::complete(Year, Month = 1:12,
                  fill = list(incidents = 0))

    # Fill in variables
missile_dat_final <- missile_dat_final %>%
  dplyr::mutate(TestDummy = ifelse(is.na(TestDummy), 0, TestDummy))

# Check 
visdat::vis_dat(missile_dat_final)
Amelia::missmap(missile_dat_final)

# Save joined data
readr::write_csv(missile_dat_final, 'C:/Users/Tom Brailey/Dropbox/github_private/MissileTest/data/missile_dat_final.csv')



# Manaul data entry will occur at this point 
# Data will be re-uploaded into R as missile_dat_final_manual_edits



# Load manually edited data
missile_dat_final_manual_edits <- rio::import("missile_dat_final_manual_edits.csv")

# Visualize data
visdat::vis_dat(missile_dat_final_manual_edits)
Amelia::missmap(missile_dat_final_manual_edits)

# Clean
missile_dat_final_manual_edits$Year <- as.numeric(missile_dat_final_manual_edits$Year)
missile_dat_final_manual_edits$Country <- as.factor(missile_dat_final_manual_edits$Country)
missile_dat_final_manual_edits <- dplyr::as_tibble(missile_dat_final_manual_edits)

# Create lag variables for event variables
missile_dat_final_manual_edits <- plyr::ddply(missile_dat_final_manual_edits, 
                                              plyr::.(Country), transform, 
                                              EventUNSCResolutionLag1 = c(NA, EventUNSCResolution[-length(EventUNSCResolution)])
                                              )

missile_dat_final_manual_edits <- plyr::ddply(missile_dat_final_manual_edits, 
                                              plyr::.(Country), transform, 
                                              EventUNSCResolutionLag2 = c(NA, EventUNSCResolutionLag1[-length(EventUNSCResolutionLag1)])
                                              )

# Drop columns we don't need for the analysis and organize
missile_dat_final_manual_edits <- missile_dat_final_manual_edits %>% 
  dplyr::select(Country,
                Year,
                Month,
                TestDummy,
                TestCount,
                MissileName,
                MissileFamily,
                Crisis,
                EventUNSCResolution,
                EventUNSCResolutionLag1,
                EventUNSCResolutionLag2, 
                EventHOSTravel,
                EventHOSVisit,
                EventNotes,
                EventSource,
                dplyr::everything(),
                -DateEntered)

# Get rid of NA vals (for the purpose of the logit model)
missile_dat_final_manual_edits <- missile_dat_final_manual_edits %>%
  dplyr::mutate(EventUNSCResolution = ifelse(is.na(EventUNSCResolution), 0, EventUNSCResolution),
                EventUNSCResolutionLag1 = ifelse(is.na(EventUNSCResolutionLag1), 0, EventUNSCResolutionLag1),
                EventUNSCResolutionLag2 = ifelse(is.na(EventUNSCResolutionLag2), 0, EventUNSCResolutionLag2),
                Crisis = ifelse(is.na(Crisis), 0, Crisis),
                EventHOSTravel = ifelse(is.na(EventHOSTravel), 0, EventHOSTravel),
                EventHOSVisit = ifelse(is.na(EventHOSVisit), 0, EventHOSVisit),
                TestCount = ifelse(is.na(TestCount), 0, TestCount))



# Set WD to save all visualizations and tables
setwd(paste0(here::here(), '/vis/'))

# Data analysis for TestDummy
  # Logit modelling
logit1 <- Zelig::zelig(TestDummy ~
                       Year +
                       Country +
                       EventUNSCResolution + 
                       EventUNSCResolutionLag1 + 
                       EventUNSCResolutionLag2 + 
                       EventHOSTravel + 
                       EventHOSVisit +
                       Crisis, 
                     data = missile_dat_final_manual_edits,
                     model = "logit")

#stargazer::stargazer(Zelig::from_zelig_model(logit1), 
#                     type = "text",
#                     title = "Logistic Regression Output (Core Hypothesis)",
#                     out = "logit1.tex") # LaTeX
#dev.off()

logit1_evs <- Zelig::setx(logit1)
logit1_sim <- Zelig::sim(logit1, x = logit1_evs)

logit1_evs_yr <- Zelig::setx(logit1, Year = c(1984:2019))
logit1_sim_yr <- Zelig::sim(logit1, x = logit1_evs_yr)

logit1_evs_unsc <- Zelig::setx(logit1, EventUNSCResolution = c(0:1))
logit1_sim_unsc <- Zelig::sim(logit1, x = logit1_evs_unsc)

logit1_evs_hosv <- Zelig::setx(logit1, EventHOSVisit = c(0:1))
logit1_sim_hosv <- Zelig::sim(logit1, x = logit1_evs_hosv)

logit1_evs_host <- Zelig::setx(logit1, EventHOSTravel = c(0:1))
logit1_sim_host <- Zelig::sim(logit1, x = logit1_evs_host)

logit1_evs_crs <- Zelig::setx(logit1, Crisis = c(0:1))
logit1_sim_crs <- Zelig::sim(logit1, x = logit1_evs_crs)

logit_plot1 <- Zelig::plot(logit1_sim)

jpeg(filename = "logit_all.jpg")
par(mfrow = c(3,2))
logit_plot2 <- Zelig::plot(logit1_sim_yr)
logit_plot3 <- Zelig::plot(logit1_sim_unsc)
logit_plot4 <- Zelig::plot(logit1_sim_hosv)
logit_plot5 <- Zelig::plot(logit1_sim_host)
logit_plot6 <- Zelig::plot(logit1_sim_crs)
dev.off()

jpeg(filename = "logit_by_year.jpg")
Zelig::plot(logit1_sim_yr)
dev.off()

jpeg(filename = "logit_summary.jpg")
Zelig::plot(logit1_sim) 
dev.off()

  # Logit 2 modelling (ensure that I get the same results as in Zelig)
logit2 <- glm(TestDummy ~ Year +
                  Country +
                  EventUNSCResolution + 
                  EventUNSCResolutionLag1 + 
                  EventUNSCResolutionLag2 + 
                  EventHOSTravel + 
                  EventHOSVisit +
                  Crisis, 
                  data=missile_dat_final_manual_edits, 
                family=binomial(link="logit"))
logit2_predicted <- plogis(predict(logit2, missile_dat_final_manual_edits))  # predicted scores

Zelig::summary(logit2)

jpeg(filename = "logit2_coef.jpg")
dotwhisker::dwplot(logit2)
dev.off()

#stargazer::stargazer(logit2) # LaTeX

jpeg(filename = "logit_resid.jpg")
par(mfrow = c(2,2))
Zelig::plot(logit2)
dev.off()

# Data Analysis for TestCount
jpeg(filename = "data_hist.jpg")
hist(missile_dat_final_manual_edits$TestCount) # A simple histogram shows that the data contain a lot of 0s
dev.off()

  # OLS
lm1 <- lm(TestCount ~ Year +
            Country +
            EventUNSCResolution + 
            EventUNSCResolutionLag1 + 
            EventUNSCResolutionLag2 + 
            EventHOSTravel + 
            EventHOSVisit +
            Crisis,
          data = missile_dat_final_manual_edits)
summary(lm1)

lm2 <- lm(TestCount ~ Country +
            EventUNSCResolution + 
            EventUNSCResolutionLag1 + 
            EventUNSCResolutionLag2 + 
            EventHOSTravel + 
            EventHOSVisit,
          data = missile_dat_final_manual_edits)
summary(lm2)

lm3 <- lm(TestCount ~ Country +
            EventUNSCResolution + 
            EventUNSCResolutionLag1 + 
            EventUNSCResolutionLag2 + 
            EventHOSTravel + 
            EventHOSVisit +
            Crisis:Year,
          data = missile_dat_final_manual_edits)
summary(lm3)

lm4 <- lm(TestCount ~ Country +
            EventUNSCResolution + 
            EventUNSCResolutionLag1 + 
            EventUNSCResolutionLag2 + 
            EventHOSTravel + 
            EventHOSVisit +
            Crisis*Year,
          data = missile_dat_final_manual_edits)
summary(lm4)

#stargazer::stargazer(lm1, lm2, lm3, lm4,
#                     type = "text",
#                     title = "Linear Model Outputs",
#                     out = "lm.tex") # LaTeX
    
    # Check for heteroscedasticity
jpeg(filename = "het_checks.jpg")
par(mfrow = c(2,2))
plot(lm1)
dev.off()

    # Robust standard errors
lm1_rob <- estimatr::lm_robust(TestCount ~ Year + 
                 Country +
                 EventUNSCResolution + 
                 EventUNSCResolutionLag1 + 
                 EventUNSCResolutionLag2 + 
                 EventHOSTravel + 
                 EventHOSVisit +
                 Crisis,
                 data = missile_dat_final_manual_edits)
summary(lm1_rob) 

#lm1_rob %>% # LaTeX
#  estimatr::tidy() %>%
#  xtable::xtable() %>%
#  xtable::print.xtable(file = "lm1_rob.tex")

  # Poisson modelling
poisson1 <- Zelig::zelig(TestCount ~ Year + 
                           Country +
                           EventUNSCResolution + 
                           EventUNSCResolutionLag1 + 
                           EventUNSCResolutionLag2 + 
                           EventHOSTravel + 
                           EventHOSVisit +
                           Crisis,
                          data = missile_dat_final_manual_edits,
                          model = "poisson")
Zelig::summary(poisson1)

#stargazer::stargazer(Zelig::from_zelig_model(poisson1),
#                     type = "text",
#                     title = "Poisson Distribution",
#                     out = "poisson1.tex") # LaTeX

  # Negative binomial
negbin1 <- Zelig::zelig(TestCount ~ Year + 
                          Country +
                          EventUNSCResolution + 
                          EventUNSCResolutionLag1 + 
                          EventUNSCResolutionLag2 + 
                          EventHOSTravel + 
                          EventHOSVisit +
                          Crisis,
                        data = missile_dat_final_manual_edits,
                        model = "negbin")
Zelig::summary(negbin1)

negbin1_evs <- Zelig::setx(negbin1)
negbin1_evs_yr <- Zelig::setx(negbin1, Year = c(1984:2019))
negbin1_sim <- Zelig::sim(negbin1, negbin1_evs)
negbin1_sim_yr <- Zelig::sim(negbin1, negbin1_evs_yr)

jpeg(filename = "negbin_summary.jpeg")
Zelig::plot(negbin1_sim)
dev.off()

jpeg(filename = "negbin_summary_yr.jpeg")
Zelig::plot(negbin1_sim_yr)
dev.off()

#stargazer::stargazer(Zelig::from_zelig_model(negbin1),
#                     type = "text",
#                     title = "Negative Binomial Model for Count DV",
#                     out = "negbin1.tex") # LaTeX



# Next steps: Convert data to time-series
