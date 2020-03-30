library(tidyverse)
library(data.table)
library(coronavirus)

data(coronavirus)

# update with a question prompt
update_datasets(silence = FALSE)

# myfile <- "https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv"
# coronadata <- read_csv(myfile, col_types = cols(.default = "c"))
# coronadata$cases <- as.numeric(coronadata$cases)
# coronadata$date <- as.Date(coronadata$date)

coronadata <- data.table(coronadata)

test <- coronadata[Country.Region=="US" & type=="confirmed"]
test <- test[, list(cases = sum(cases)), by = date]
test[, tot_cases := cumsum(cases)]

plot(test$date, test$tot_cases)


coronadata[, t := as.numeric(date)]

coronadata[ type == "death" & cases > 0, min_death := min(t, na.rm = T), by = c("Country.Region", "Province.State") ]
coronadata[, day_no := t - min_death]



dk <- coronadata %>% 
  group_by(day_no, Country.Region, Province.State,  type) %>%
  summarise(total = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  ungroup() %>%
  filter(Country.Region=="Denmark" & is.na(Province.State) & !is.na(day_no) ) %>%
  mutate( 
    cum_deaths = cumsum(death),
    cum_cases = cumsum(confirmed)
    ) 

usa <- coronadata %>% 
  group_by(day_no, Country.Region, Province.State,  type) %>%
  summarise(total = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  ungroup() %>%
  filter(Country.Region=="US" & !is.na(day_no)) %>%
  mutate( 
    cum_deaths = cumsum(death),
    cum_cases = cumsum(confirmed)
  ) 

italy <- coronadata %>% 
  group_by(day_no, Country.Region, Province.State,  type) %>%
  summarise(total = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  ungroup() %>%
  filter(Country.Region=="Italy" & !is.na(day_no)) %>%
  mutate( 
    cum_deaths = cumsum(death),
    cum_cases = cumsum(confirmed)
  ) 


se <- coronadata %>% 
  group_by(day_no, Country.Region, Province.State,  type) %>%
  summarise(total = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  ungroup() %>%
  filter(Country.Region=="Sweden" & !is.na(day_no)) %>%
  mutate( 
    cum_deaths = cumsum(death),
    cum_cases = cumsum(confirmed)
  ) 


no <- coronadata %>% 
  group_by(day_no, Country.Region, Province.State,  type) %>%
  summarise(total = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  ungroup() %>%
  filter(Country.Region=="Norway" & !is.na(day_no)) %>%
  mutate( 
    cum_deaths = cumsum(death),
    cum_cases = cumsum(confirmed)
  ) 

aus <- coronadata %>% 
  group_by(day_no, Country.Region, Province.State,  type) %>%
  summarise(total = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  ungroup() %>%
  filter(Country.Region=="Austria" & !is.na(day_no)) %>%
  mutate( 
    cum_deaths = cumsum(death),
    cum_cases = cumsum(confirmed)
  ) 

plot(
  se$day_no, 
  se$cum_deaths, 
  type = "l",
  xlab = "Days since first death",
  ylab = "Number of deaths"
  
  )

lines(dk$day_no, dk$cum_deaths, col = "red")

lines(usa$day_no, usa$cum_deaths, col = "blue")
lines(italy$day_no, italy$cum_deaths, col = "orange")

lines(no$day_no, no$cum_deaths, col = "blue")
lines(aus$day_no, aus$cum_deaths, col = "green")



plot(italy$date, italy$cum_deaths, type="l")

plot(italy$date, italy$death, type = "h")


plot(italy$date, italy$confirmed, type = "h")
