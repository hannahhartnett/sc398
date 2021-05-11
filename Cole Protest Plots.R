library(tidyverse)

acled <- read_csv("US-ACLED.csv")

acled$event_date <- as.Date(acled$event_date, format="%d-%b-%y")
# acled$event_date2 <- format(acled$event_date, format="%Y-%m-%d")

library(leaflet)

# First Map

acled$lab <- paste(sep = "<br/>",
                   acled$event_date, acled$event_type, acled$location, acled$admin1)

max_date = "2020-03-30"
min_date = "2020-03-28"

b <- c(as.Date("2020-04-01"), as.Date("2020-04-30"))


leaflet(acled %>% filter(event_date <= max_date & event_date >= min_date)) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, label = ~lapply(lab, htmltools::HTML), popup = ~notes)

# Second Map

time = "Week After George Floyd's Death"

if (time == "Before George Floyd's Death") {
  min_date = "2020-01-01"
  max_date = "2020-05-24"
} else if (time == "Week After George Floyd's Death") {
  min_date = "2020-05-25"
  max_date = "2020-05-31"
} else if (time == "June") {
  min_date = "2020-06-01"
  max_date = "2020-06-30"
} else if (time == "July") {
  min_date = "2020-07-01"
  max_date = "2020-07-31"
} else if (time == "August") {
  min_date = "2020-08-01"
  max_date = "2020-08-31"
} else if (time == "September") {
  min_date = "2020-09-01"
  max_date = "2020-09-30"
} else if (time == "October") {
  min_date = "2020-10-01"
  max_date = "2020-10-31"
} else if (time == "November") {
  min_date = "2020-11-01"
  max_date = "2020-11-30"
} else if (time == "December") {
  min_date = "2020-12-01"
  max_date = "2020-12-31"
}


leaflet(acled %>% filter(grepl("BLM", assoc_actor_1)) %>% filter(event_date <= max_date & event_date >= min_date)) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, label = ~lapply(lab, htmltools::HTML), popup = ~notes)
