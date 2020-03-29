library(data.table)

myfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
df <- readr::read_csv(myfile, col_types = readr::cols(.default = "c"))
dt <- data.table(df)
setnames(dt, old =  c("Province/State", "Country/Region"), new = c("province", "country"))

dt <- dt[ is.na(province)]
dt[, province := NULL]
dt[, Lat := NULL]
dt[, Long := NULL]

dt <- melt(dt, id = 1, measure = 2:ncol(dt), variable.name = "date", value.name = "deaths")
dt[, date := as.Date( lubridate::parse_date_time(date, orders = "%m/%d/%y")) ]
dt[, deaths := as.numeric(deaths)]
dt <- dt[deaths > 0]

dt[, t := as.numeric(date)]
dt <- dt[order(country, t)]
dt[, mint := min(t), by = country ]

dt[, day_no := t - mint]


countries <- dt[, list(deaths = sum(deaths)), by = c("country","day_no")]

dk <- countries[country == "Denmark"]
se <- countries[country == "Sweden"]
no <- countries[country == "Norway"]
us <- countries[country == "US"]
uk <- countries[country == "United Kingdom"]
aus <- countries[country == "Austria"]
it <- countries[country == "Italy"]

plot(
  it$day_no, 
  it$deaths, 
  type = "l",
  xlim = c(0, 30),
  ylim = c(0, 200),
  xlab = "Days since first death",
  ylab = "Number of deaths"
  )
title("Number of deaths by day")
lines(dk$day_no, dk$deaths, col = "red")
lines(se$day_no, se$deaths, col = "orange")
lines(no$day_no, no$deaths, col = "blue")
lines(us$day_no, us$deaths, col = "grey")
lines(aus$day_no, aus$deaths, col = "green")
lines(uk$day_no, uk$deaths, col = "brown")

legend("topleft", 
  inset = .05, 
  legend = c("Italy","Denmark", "Sweden","Norway", "US", "Austria", "UK"), 
  fill = c("black", "red", "orange" ,"blue","grey" , "green", "brown")
  )
text(27,190, as.character(paste("data as of",max(dt$date))), cex = 0.7 )


