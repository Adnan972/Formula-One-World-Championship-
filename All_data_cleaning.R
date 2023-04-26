# Initial Setup ####################
# Start fresh by clearing the environment and the console
rm(list = ls()) #clear environment
cat("\f") #clear console
#-------------------------#
setwd("C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Formula1_1950To2022")
getwd()
#----------------------------#
# Install required libraries #
#----------------------------#
# Package names
packages <- c("TSA", "tseries", "lmtest", "FSAdata", "forecast", "urca", "rugarch", 
              "fGarch", "tswge", "imputeTS", "ggplot2", "readxl", "readr", "dplyr", 
              "tidyr", "psych", "stringr", "lubridate", "knitr", "hms", "lutz")#,"shiny", "coda", "rjags", "runjags")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) { install.packages(packages[!installed_packages]) }
#-------------------------#
# Load libraries          #
#-------------------------#
invisible(lapply(packages, library, character.only = TRUE))
#-------------------------#
rm(list = ls()) #clear environment
cat("\f") #clear console
# Setup complete
###########################################
#=========================
# Import data - Constructors
#=========================
constructor_results <- read.csv("constructor_results.csv")
constructor_standings <- read.csv("constructor_standings.csv")
constructors <- read.csv("constructors.csv")
# country data
setwd("C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Thomas and James/02 James")
country_info <- read.csv("Nationalities.csv")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#=========================
# constructor results
#=========================
constructor_results_clean <- constructor_results
head(constructor_results_clean)
describe(constructor_results_clean)
# check for duplicates/unique IDs
height <- nrow(constructor_results_clean)
length(unique(constructor_results_clean$constructorResultsId)) == height
#expand "status" so it is clear - E is "excluded", other values are NA
constructor_results_clean["status"][constructor_results_clean["status"] == "\\N"] <- NA
constructor_results_clean["status"][constructor_results_clean["status"] == "D"] <- "Excluded"
#save cleaned data
head(constructor_results_clean)
write.csv(constructor_results_clean, 
          file="C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/constructor_results.csv", 
          row.names = FALSE)
#
#=========================
# constructor standings
#=========================
constructor_standings_clean <- constructor_standings
head(constructor_standings_clean)
describe(constructor_standings_clean)
# check for duplicates/unique IDs
height <- nrow(constructor_standings_clean)
length(unique(constructor_standings_clean$constructorStandingsId)) == height
#expand "status" so it is clear
constructor_standings_clean["positionText"][constructor_standings_clean["positionText"] == "E"] <- "Excluded"
#rename columns for clarity
constructor_standings_clean <- constructor_standings_clean %>%
  rename("cumulativeSeasonPoints" = "points") %>%
  rename("cumulativeSeasonWins" = "wins")
#save cleaned data
head(constructor_standings_clean)
write.csv(constructor_standings_clean, 
          file="C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/constructor_standings.csv", 
          row.names = FALSE)
#
#=========================
# constructors
#=========================
constructors_clean <- constructors
head(constructors_clean)
describe(constructors_clean)
# check for duplicates/unique IDs
height <- nrow(constructors_clean)
length(unique(constructors_clean$constructorId)) == height
# drop "constructorRef" and "url" columns
constructors_clean <- select(constructors_clean, !c("constructorRef", "url"))
# add "country" to constructor data.
constructors_clean <- left_join(constructors_clean, 
                                country_info, 
                                by=c("nationality"="Nationality")) %>% 
  select(!c("ID", "Country.Code", "Person"))
#save cleaned data
head(constructors_clean)
write.csv(constructors_clean, 
          file = "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/constructors.csv", 
          row.names = FALSE)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#=========================
# Import data - Thomas J
#=========================
setwd("C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Formula1_1950To2022")
#=========================
driver_standings <- read.csv("driver_standings.csv")
drivers <- read.csv("drivers.csv")
qualifying <- read.csv("qualifying.csv")
results <- read.csv("results.csv")
#--------------------------------
#
#=========================
# Driver standings
#=========================
drop <- c("positionText")
driver_standings_new <- driver_standings[,!(names(driver_standings) %in% drop)]
# check for duplicates/unique IDs
height <- nrow(driver_standings_new)
length(unique(driver_standings_new$driverStandingsId)) == height
#rename wins to "cumulative wins"
driver_standings_new <- rename(driver_standings_new, cumulativeWins=wins)
#save cleaned data
write.csv(driver_standings_new, 
          file = "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/driver_standings.csv", 
          row.names = FALSE)
#
#=========================
# Drivers
#=========================
drop <- c("url", "driverRef")
drivers_new <- drivers[,!(names(drivers) %in% drop)]
# check for duplicates/unique IDs
height <- nrow(drivers_new)
length(unique(drivers_new$driverId)) == height
#replace "//N" values with NA
drivers_new["number"][drivers_new["number"] == "\\N"] <- NA
# drop the code column and recreate the column using driver surnames
drivers_new_2 <- drivers_new
drop <- c("code")
drivers_new_2 <- drivers_new[,!(names(drivers_new) %in% drop)]
code <- substr(drivers_new_2$surname, 1, 3)
code <- toupper(code)
drivers_new_2$code <- c(code)
drivers_new <- drivers_new_2
drivers_new
#save cleaned data
write.csv(drivers_new, 
          file="C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/drivers.csv", 
          row.names=FALSE)
#
#=========================
# Qualifying
#=========================
qualifying_new <- qualifying
describe(qualifying_new)
str(qualifying_new) #options(digits = 6)
# check for duplicates/unique IDs
height <- nrow(qualifying_new)
length(unique(qualifying_new$qualifyId)) == height
# replace "\\N" values with NA
qualifying_new[qualifying_new == "\\N"] <- NA
#separate qualifying rounds for ease of editing
q1 <- qualifying_new %>% select(c(qualifyId, q1))
q2 <- qualifying_new %>% select(c(qualifyId, q2))
q3 <- qualifying_new %>% select(c(qualifyId, q3))
#'''''''''''
#' Convert Q1 times to milliseconds
#,,,,,,,,,,
q1$mins <- as.numeric(sub("\\:.*", "", q1$q1))
q1$secs <- as.numeric(sub("\\.[0-9]+$", "", sub("..\\:*", "", q1$q1)))
q1$ms <- as.numeric(sub(".*\\.", "", q1$q1))
q1$q1_milliseconds <- ((q1$mins*60 + q1$secs)*1000) + q1$ms
#'''''''''''
#' Convert Q2 times to milliseconds
#,,,,,,,,,,
q2$mins <- as.numeric(sub("\\:.*", "", q2$q2))
q2$secs <- as.numeric(sub("\\.[0-9]+$", "", sub("..\\:*", "", q2$q2)))
q2$ms <- as.numeric(sub(".*\\.", "", q2$q2))
q2$q2_milliseconds <- ((q2$mins*60 + q2$secs)*1000) + q2$ms
#'''''''''''
#' Convert Q3 times to milliseconds
#,,,,,,,,,,
q3$mins <- as.numeric(sub("\\:.*", "", q3$q3))
q3$secs <- as.numeric(sub("\\.[0-9]+$", "", sub("..\\:*", "", q3$q3)))
q3$ms <- as.numeric(sub(".*\\.", "", q3$q3))
q3$q3_milliseconds <- ((q3$mins*60 + q3$secs)*1000) + q3$ms
#'''''''''''
#' Match milliseconds back to the qualifying data
#,,,,,,,,,,
drop <- c("q1", "q2", "q3", "mins", "secs", "ms")
# drop conversion columns
q1 <- q1[,!(names(q1) %in% drop)]
q2 <- q2[,!(names(q2) %in% drop)]
q3 <- q3[,!(names(q3) %in% drop)]
# merge qualifying times
qualifying_new <- qualifying_new %>% 
  left_join(q1, by = c("qualifyId")) %>%
  left_join(q2, by = c("qualifyId")) %>%
  left_join(q3, by = c("qualifyId"))
#save cleaned data
write.csv(qualifying_new, 
          file = "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/qualifying.csv", 
          row.names = FALSE)
#
#=========================
# Results
#=========================
results_new <- results
# check for duplicates/unique IDs
height <- nrow(results_new)
length(unique(results_new$resultId)) == height
# NA check & replace
colSums(is.na(results_new))
results_new[results_new == "\\N"] <- NA
colSums(is.na(results_new))
# Convert fastest lap times to milliseconds
results_new$mins <- as.numeric(sub("\\:.*", "", results_new$fastestLapTime))
results_new$secs <- as.numeric(sub("\\.[0-9]+$", "", sub("..\\:*", "", results_new$fastestLapTime)))
results_new$ms <- as.numeric(sub(".*\\.", "", results_new$fastestLapTime))
results_new$fastest_lap_milliseconds <- ((results_new$mins*60 + results_new$secs)*1000) + results_new$ms
#drop mins/secs/ms
drop <- c("position", "positionOrder", "mins", "secs", "ms")
results_new <- results_new[,!(names(results_new) %in% drop)]
#save cleaned data
write.csv(results_new, 
          file = "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/results.csv", 
          row.names = FALSE)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#=========================
# Import data - Adnan and Abhinav
#=========================
setwd("C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Formula1_1950To2022")
#=========================
Circuits <- read.csv("circuits.csv")
Lap_times <- read.csv("lap_times.csv")
Pit_stops <- read.csv("pit_stops.csv")
Races <- read.csv("races.csv")
Seasons <- read.csv("seasons.csv")
Sprint_results <- read.csv("sprint_results.csv")
Status <- read.csv("status.csv")
#--------------------------------
#
#=========================
# Circuits
#=========================
str(Circuits) #check the structure
circuits_new <- Circuits
# check for duplicates/unique IDs
height <- nrow(circuits_new)
length(unique(circuits_new$circuitId)) == height
# NA check
length(circuits_new[circuits_new == "\\N"]) # 2 \\N values
# replace \\N with NA values
circuits_new[circuits_new == "\\N"] <- NA
#clean-up circuit references
circuits_new$circuitRef <- str_replace(circuits_new$circuitRef,"_"," ")
circuits_new$circuitRef <- str_to_title(circuits_new$circuitRef)
circuits_new$alt <- as.numeric(circuits_new$alt)
which(is.na(circuits_new))
sum(is.na(circuits_new))
# drop unnecessary columns
drop <- c("url")
circuits_clean <- circuits_new[,!(names(circuits_new) %in% drop)]
#save cleaned data
write.csv(circuits_clean,
          "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/Circuits.csv",
          row.names = FALSE)
#
#=========================
# Lap times
#=========================
Lap_times_new <- Lap_times
str(Lap_times_new)
# check for duplicates/unique IDs
height <- nrow(Lap_times_new)
length(unique(Lap_times_new$raceId)) == height
# create a unique ID for each race/driver/lap
Lap_times_new$RaceDriverLapID <- paste(Lap_times_new$raceId, 
                                       paste(Lap_times_new$driverId, 
                                             Lap_times_new$lap,
                                             sep="."),
                                       sep=".")
length(unique(Lap_times_new$RaceDriverLapID)) == height
# NA check
length(Lap_times_new[Lap_times_new == "\\N"]) # 2 \\N values
# NA check
which(is.na(Lap_times_new))
#save cleaned data
write.csv(Lap_times_new,
          "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/Lap_times.csv",
          row.names = FALSE)
#
#=========================
# Pit stops
#=========================
Pit_stops_new <- Pit_stops
# check for duplicates/unique IDs
height <- nrow(Pit_stops_new)
length(unique(Pit_stops_new$raceId)) == height
# create a unique ID for each race/driver/lap
Pit_stops_new$RaceDriverStopID <- paste(Pit_stops_new$raceId, 
                                       paste(Pit_stops_new$driverId, 
                                             Pit_stops_new$stop,
                                             sep="."),
                                       sep=".")
length(unique(Pit_stops_new$RaceDriverStopID)) == height #ID is unique
# \\N check
length(Pit_stops_new[Pit_stops_new == "\\N"]) # 0 \\N values
# NA check
which(is.na(Pit_stops_new))
# drop unnecessary columns
drop <- c("duration", "time")
Pit_stops_new <- Pit_stops_new[,!(names(Pit_stops_new) %in% drop)]
str(Pit_stops_new)
#save cleaned data
write.csv(Pit_stops_new,
          "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/Pit_stops.csv",
          row.names = FALSE)
#
#=========================
# Races
#=========================
#library(chron)
Races_new <- Races
str(Races_new)
# check for duplicates/unique IDs
height <- nrow(Races_new)
length(unique(Races_new$raceId)) == height
# \\N check
length(Races_new[Races_new == "\\N"]) # 731 \\N values
# replace \\N values with NA
Races_new[Races_new == "\\N"] <- NA
# NA check
sum(is.na(Races_new))
colSums(is.na(Races_new))
# update format of date
is.Date(Races_new$date)
Races_new$date <- dmy(Races_new$date)
is.Date(Races_new$date)
# rename time as it is erroneous
Races_new <- rename(Races_new, "time_inconsistent"="time")
# drop unnecessary columns
drop <- c("url")
Races_new <- Races_new[,!(names(Races_new) %in% drop)]
#save cleaned data
write.csv(Races_new,
          "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/Races.csv",
          row.names = FALSE)
#
#=========================
# Seasons
#=========================
seasons_new <- Seasons
str(seasons_new)
# check for duplicates/unique IDs
height <- nrow(seasons_new)
length(unique(seasons_new$year)) == height
# \\N check
length(seasons_new[seasons_new == "\\N"]) # 0 \\N values
# NA check
sum(is.na(seasons_new))
#save cleaned data
write.csv(seasons_new,
          "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/Seasons.csv",
          row.names = FALSE)
#
#=========================
# Sprint results
#=========================
sprint_results_new <- Sprint_results
str(sprint_results_new)
# check for duplicates/unique IDs
height <- nrow(sprint_results_new)
length(unique(sprint_results_new$resultId)) == height #resultID is unique
# \\N check
length(sprint_results_new[sprint_results_new == "\\N"]) # 8 \\N values
# replace \\N values with NA
sprint_results_new[sprint_results_new == "\\N"] <- NA
# NA check
sum(is.na(sprint_results_new)) # 8 NA
colSums(is.na(sprint_results_new))
# convert columns to numbers
sprint_results_new$position <- as.numeric(sprint_results_new$position)
sprint_results_new$fastestLap <- as.numeric(sprint_results_new$fastestLap)
sprint_results_new$milliseconds <- as.numeric(sprint_results_new$milliseconds)
# create "time to finish" column
sprint_results_new$time_to_finish <- hms(sprint_results_new$milliseconds/1000)
# create "time behind leader"
sprint_results_new <- sprint_results_new %>% group_by(raceId) %>% 
  mutate(sprint_lead = min(milliseconds, na.rm = TRUE))
sprint_results_new$timeBehind <- (sprint_results_new$milliseconds - sprint_results_new$sprint_lead)
# drop unnecessary columns
drops <- c("position", "time", "sprint_lead")
sprint_results_new <- sprint_results_new[ , !(names(sprint_results_new) %in% drops)]
# NA check
sum(is.na(sprint_results_new)) # 8 NA
colSums(is.na(sprint_results_new))
test <- sprint_results_new[rowSums(is.na(sprint_results_new))>0,]
# NAs are drivers who had issues (vibration or accident)
#save cleaned data
write.csv(sprint_results_new,
          "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/Sprint_results.csv",
          row.names = FALSE)
#
#=========================
# Status
#=========================
status_new <- Status
# check for duplicates/unique IDs
height <- nrow(status_new)
length(unique(status_new$statusId)) == height #resultID is unique
# \\N check
length(status_new[status_new == "\\N"]) # 0 \\N values
# NA check
sum(is.na(status_new)) # 0 NA
#save cleaned data
write.csv(status_new,
          "C:/Users/james/OneDrive/Desktop/Uni Studies/RMIT/Applied Research Project - MATH2191/Research Project/Clean data/Status.csv",
          row.names = FALSE)

