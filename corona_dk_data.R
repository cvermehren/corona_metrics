library(data.table)
library(httr)
library(jsonlite)

getInfo <- function(endpoint){
  base <- "https://api.covid19data.dk:443/"
  call <- paste(base,endpoint,sep="")
  data <- fromJSON(call, flatten = TRUE)
}


hopkins <- getInfo("john_hopkins_data")



worldometers <- getInfo("worldometers_overview")
ssi_cases <- getInfo("ssi_cases")
ssi_hospitalized <- getInfo("ssi_hospitalized")

ssi_cases$date <- as.Date(ssi_cases$timestamp)
ssi_hospitalized$date <- as.Date(ssi_hospitalized$timestamp)

dt_cases <- data.table(ssi_cases)
dt_hosp <- data.table(ssi_hospitalized)

dt_cases <- dt_cases[, list(
  cases = sum(cases),
  deaths = sum(deaths)
), by = date]

dt_hosp <- dt_hosp[, list(
  hospitalized = sum(hospitalized),
  critical = sum(critical),
  respirator = sum(respirator)
), by = date]

# Sources
# Doubling time:  
# https://www.medrxiv.org/content/10.1101/2020.02.26.20028449v3
# https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus#doubling-time

# Time to death:
# https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus#deaths

# Assumed mortality rate (deaths to true cases)
mortality <- 0.0066

time_to_death <- 18 #mean(c(15.2, 22.3,18, 13.8)) # Average of measure numbers from source
doubling_time <- 4.6 # Some sources say even lower

# Formula for true cases given assumed mortality rate of true cases
# If mortality is 1% then we have 100 true cases at symtoms onset
# These 100 will double every 4.6 days (doubling time)
true_per_death <- 2^(time_to_death/doubling_time)*10000*mortality

trend <- merge(dt_cases, dt_hosp, by = "date", all.x = T)

trend$new_deaths <- c(NA,diff(trend$deaths))
trend[, true_new_cases := new_deaths * true_per_death]
trend[is.na(true_new_cases), true_new_cases := 0]
trend[, true_cases := cumsum(true_new_cases)]

setcolorder(trend,c( "date", "cases", "hospitalized", "critical", "respirator", "deaths", "new_deaths", "true_new_cases", "true_cases"))

trend[, deaths_to_cases := (deaths/cases)*100]
trend[, hosp_to_cases := (hospitalized/cases)*100]
trend[, resp_to_hosp := (respirator/hospitalized)*100]
trend[, deaths_to_resp := (deaths/respirator)*100]

par(mfrow=c(2,2))
plot(trend$date, trend$deaths_to_cases, type = "l", ylim = c(0, max(trend$deaths_to_cases, na.rm = T )))
plot(trend$date, trend$hosp_to_cases, type = "l", ylim = c(0, max(trend$hosp_to_cases, na.rm = T )))
plot(trend$date, trend$resp_to_hosp, type = "l", ylim = c(0, max(trend$resp_to_hosp, na.rm = T )))
plot(trend$date, trend$deaths_to_resp, type = "l", ylim = c(0, max(trend$deaths_to_resp, na.rm = T )))

par(mfrow=c(1,1))

#plot(trend$date ,trend$cases, type ="l")
plot(trend$date,
     trend$hospitalized, 
     type="l",
     xlab = as.character(paste0("Data as of ",max(trend$date))),
     ylab=""
     )
title(main = "Number of hospitalized by day", sub = )

