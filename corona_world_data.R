library(data.table)
library(readr)
library(lubridate)
library(ggplot2)

dates <- format( seq.Date(as.Date('2020-01-22'), Sys.Date()-1, by='day'), format='%m-%d-%Y')

corona_files <- list()

for (i in dates) {
  myfile <- paste0( "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", i ,".csv")
  corona_files[[i]] <- read_csv(myfile, col_types = cols(.default = "c"))  
}

corona_data <- rbindlist(corona_files, use.names = T, fill = T)

# Make valid col names
names(corona_data) <- tolower( gsub(" ", "_", names(corona_data)) )
names(corona_data) <- gsub("/", "_", names(corona_data)) 
corona_data$date <- as.Date( lubridate::parse_date_time(corona_data$last_update, orders = c("%m-%d-%y %H:%M", "%Y-%m-%d %H:%M:%S")))

corona_data$confirmed <- as.numeric(corona_data$confirmed)
corona_data$deaths <- as.numeric(corona_data$deaths)
corona_data$recovered <- as.numeric(corona_data$recovered)

corona_data[, new_conf := confirmed - shift(confirmed), by = c("country_region", "province_state")]
corona_data[is.na(new_conf), new_conf := confirmed]
corona_data[, new_deaths := deaths - shift(deaths), by = c("country_region", "province_state")]
corona_data[is.na(new_deaths), new_deaths := deaths]
corona_data[, new_recov := recovered - shift(recovered), by = c("country_region", "province_state")]
corona_data[is.na(new_recov), new_recov := recovered]

corona_data[(!(province_state=="Denmark") | is.na(province_state)) & country_region=="Denmark", country_region := NA ]

countries <- corona_data[, list( 
  new_cases = sum(new_conf, na.rm = T),
  new_deaths = sum(new_deaths, na.rm = T),
  new_recov = sum(new_recov, na.rm = T)
  ), by = c("country_region", "date")]
countries[, total_cases := cumsum(new_cases), by = country_region ]
countries[, total_deaths := cumsum(new_deaths), by = country_region]
countries[, total_recov := cumsum(new_recov), by = country_region]

View(countries[country_region=="US"])

countries[, t := as.numeric(date)]
countries[ deaths > 0, min_t :=  min(t, na.rm = TRUE), by= c("country_region") ]
countries[, day_no := t - min_t]

#countries <- corona_data[order(country_region, province_state, date, confirmed)]






corona_data$t <- as.numeric(corona_data$date)
corona_data <- corona_data[order(country_region, province_state, date, confirmed)]
corona_data[ deaths > 0, min_t :=  min(t, na.rm = TRUE), by= c("country_region", "province_state") ]
corona_data[, day_no := t - min_t]






plot(
  countries[country_region=="Sweden"]$day_no, 
  countries[country_region=="Sweden"]$total_deaths, 
  type = "l",
  xlab = "Days since first death",
  ylab = "Number of deaths"
)
title("Number of deaths by day")
lines(countries[country_region=="Denmark"]$day_no, 
      countries[country_region=="Denmark"]$total_deaths, col="red" )
lines(countries[country_region=="Norway"]$day_no, 
      countries[country_region=="Norway"]$total_deaths, col="blue" )
lines(countries[country_region=="Austria"]$day_no, 
      countries[country_region=="Austria"]$total_deaths, col="green" )
lines(countries[country_region=="Italy"]$day_no, 
      countries[country_region=="Italy"]$total_deaths, col="orange" )
legend("topleft", inset=.05, c("Sweden","Denmark","Norway", "Austria", "Italy"), fill=c("black", "red", "blue", "green", "orange"))



#######
# Italy lockdown = 9 marts
plot(
  countries[country_region=="Italy"]$day_no, 
  countries[country_region=="Italy"]$total_deaths, type="l", log = "y")
abline(v=17, col= "grey")
text(23, 4, "Nedlukning af Italien", pos = 3)
lines(countries[country_region=="US"]$day_no, countries[country_region=="US"]$total_deaths, col="orange")

abline(countries[country_region=="US"]$day_no, 
      countries[country_region=="US"]$total_deaths, col="orange", untf = T )


ggplot() +
  aes(x = countries[country_region=="Italy"]$day_no, y = countries[country_region=="Italy"]$total_deaths) + 
  geom_line() +
  scale_y_log10()

select_con <- countries[country_region %in% c("US", "Italy")]

ggplot(select_con) +
  aes(x = day_no, y = total_deaths, color=country_region) + 
  geom_line() +
  scale_y_log10()

plot(
  countries[country_region=="Italy"]$day_no, 
  countries[country_region=="Italy"]$total_deaths, 
  type = "l", 
  ylim = c(0, max(countries[country_region=="Denmark"]$total_deaths, na.rm = T)+30), 
  xlim = c(0, max(countries[country_region=="Denmark"]$day_no, na.rm = T)+40),
  xlab = "Days since first death",
  ylab = "Number of deaths"
  )
title("Number of deaths by day")
lines(countries[country_region=="Denmark"]$day_no, 
      countries[country_region=="Denmark"]$total_deaths, col="red" )
legend("topright", inset=.05, c("Italy","Denmark"), fill=c("black", "red"))
text(x=39, y=4, labels = "Italy lockdown")
points(x=39, y=1)

regions <- corona_data[ , list( total_cases = sum(new_conf, na.rm = T),
                           total_deaths = sum(new_deaths, na.rm = T)
                          ), by = c("country_region", "date")]

