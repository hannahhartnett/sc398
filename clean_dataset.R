library(flexdashboard)
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(shinythemes)

shinythemes::themeSelector()

acled <- read_csv('./US-ACLED.csv')
head(acled)

# keep only 2020 events
acled <- acled %>% filter(year == 2020)

# format date
acled$event_date <- as.Date(acled$event_date, '%d-%b-%y')
acled$lab <- paste(sep = "<br/>",
                   acled$event_date, acled$event_type, acled$location, acled$admin1)

# categorical label for actor1 and actor 2
acled$actor_label1 <- NA
acled$actor_label2 <- NA
for(i in 1:nrow(acled)) {
  if(grepl( "Protesters", acled$actor1[i]))
    acled$actor_label1[i] <- "Protesters"
  else if(grepl( "Police", acled$actor1[i]))
    acled$actor_label1[i] <- "Police"
  else if(grepl( "Sole Perp", acled$actor1[i]))
    acled$actor_label1[i] <- "Sole Perpetrator"
  else if(grepl( "Riot", acled$actor1[i]))
    acled$actor_label1[i] <- "Rioters"
  else if(is.na(acled$actor1[i]))
    acled$actor_label2[i] <- "None"
  else
    acled$actor_label1[i] <- "Other"
}

for(i in 1:nrow(acled)) {
  if(grepl( "Protesters", acled$actor2[i]))
    acled$actor_label2[i] <- "Protesters"
  else if(grepl( "Police", acled$actor2[i]))
    acled$actor_label2[i] <- "Police"
  else if(grepl( "Sole Perp", acled$actor2[i]))
    acled$actor_label2[i] <- "Sole Perpetrator"
  else if(grepl( "Riot", acled$actor2[i]))
    acled$actor_label2[i] <- "Rioters"
  else if(is.na(acled$actor2[i]))
    acled$actor_label2[i] <- "None"
  else
    acled$actor_label2[i] <- "Other"
}

# interacting groups variable (need to remove NAs)
acled$groups <- paste(acled$actor_label1, "and", acled$actor_label2)

# break fatality into two groups
acled$fatality <- ifelse(acled$fatalities>0, "Yes", "No")

# group interaction variable
acled$groups <- paste(acled$actor_label1, "and", acled$actor_label2)

# cleaning the group interaction variable
acled$groups[acled$groups == "Other and NA"] <- "Other"
acled$groups[acled$groups == "Other and Other"] <- "Other"
acled$groups[acled$groups == "Rioters and NA"] <- "Rioters"
acled$groups[acled$groups == "Protesters and NA"] <- "Protesters"
acled$groups[acled$groups == "Sole Perpetrator and NA"] <- "Sole Perpetrator"
acled$groups[acled$groups == "Police and NA"] <- "Police"
acled$groups[acled$groups == "Protesters and Protesters"] <- "Protesters"
acled$groups[acled$groups == "Police and Rioters"] <- "Rioters and Police"
acled$groups[acled$groups == "Sole Perpetrator and Protesters"] <- "Protesters and Sole Perpetrator"
acled$groups[acled$groups == "Other and Sole Perpetrator"] <- "Sole Perpetrator and Other"
acled$groups[acled$groups == "Other and Police"] <- "Police and Other"
acled$groups[acled$groups == "Sole Perpetrator and Police"] <- "Police and Sole Perpetrator"
acled$groups[acled$groups == "Rioters and Rioters"] <- "Rioters"


# Coding actor groups to be able identify, within Protestors, for example, who was participating in events throughout 2020 -> did so based on the assoc_actor labels

acled$assoc_actor_label1 <- NA
acled$assoc_actor_label2 <- NA

for(i in 1:nrow(acled)) {
  if(grepl( "Police", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Police"
  else if(grepl( "Native", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Native American"
  else if(grepl( "Militia", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Militia"
  else if(grepl( "Government", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Government"
  else if(grepl( "Christian", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Christian"
  else if(grepl( "Jewish", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Jewish"
  else if(grepl( "Muslim", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Muslim"
  else if(grepl( "Antifa", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Leftist"
  else if(grepl( "Republican", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Republicans"
  else if(grepl( "Proud", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "AltRight"
  else if(grepl( "QAnon", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "AltRight"
  else if(grepl( "Tea", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "AltRight"
  else if(grepl( "Patriot", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "AltRight"
  else if(grepl( "Boogaloo", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "AltRight"
  else if(grepl( "White National", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "AltRight"
  else if(grepl( "Democrat", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Democrats"
  else if(grepl( "MoveOn", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Progressive"
  else if(grepl( "Indivisible", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Progressive"
  else if(grepl( "Sunrise", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Progressive"
  else if(grepl( "DSA", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Progressive"
  else if(grepl( "Socialis", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Leftist"
  else if(grepl( "Fascis", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Leftist"
  else if(grepl( "Communis", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Leftist"
  else if(grepl( "LGBT", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "LGBT"
  else if(grepl( "Labour", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Labour"
  else if(grepl( "Rent", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Housing Rights"
  else if(grepl( "Health", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Healthcare"
  else if(grepl( "Asian", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Asian"
  else if(grepl( "Latinx", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Latinx"
  else if(grepl( "Women", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Women"
  else if(grepl( "Teacher", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Teachers"
  else if(grepl( "Student", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Students"
  else if(grepl( "ICE", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Migrant Rights"
  else if(grepl( "migrant", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Migrant Rights"
  else if(grepl( "Migrant", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Migrant Rights"
  else if(grepl( "DREAM", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Migrant Rights"
  else if(grepl( "LR", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Migrant Rights"
  else if(grepl( "Refugee", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "Migrant Rights"
  else if(grepl( "BLM", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "BLM-Related"
  else if(grepl( "African American", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "BLM-Related"
  else if(grepl( "ACLU", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "BLM-Related"
  else if(grepl( "NAACP", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "BLM-Related"
  else if(grepl( "Racism", acled$assoc_actor_1[i]))
    acled$assoc_actor_label1[i] <- "BLM-Related"
  else
    acled$assoc_actor_label1[i] <- "Other"
}

for(i in 1:nrow(acled)) {
  if(grepl( "Police", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Police"
  else if(grepl( "Native", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Native American"
  else if(grepl( "Militia", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Militia"
  else if(grepl( "Government", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Government"
  else if(grepl( "Christian", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Christian"
  else if(grepl( "Jewish", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Jewish"
  else if(grepl( "Muslim", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Muslim"
  else if(grepl( "Antifa", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Leftist"
  else if(grepl( "Republican", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Republicans"
  else if(grepl( "Proud", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "AltRight"
  else if(grepl( "QAnon", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "AltRight"
  else if(grepl( "Tea", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "AltRight"
  else if(grepl( "Patriot", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "AltRight"
  else if(grepl( "Boogaloo", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "AltRight"
  else if(grepl( "White National", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "AltRight"
  else if(grepl( "Democrat", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Democrats"
  else if(grepl( "MoveOn", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Progressive"
  else if(grepl( "Indivisible", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Progressive"
  else if(grepl( "Sunrise", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Progressive"
  else if(grepl( "DSA", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Progressive"
  else if(grepl( "Socialis", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Leftist"
  else if(grepl( "Fascis", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Leftist"
  else if(grepl( "Communis", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Leftist"
  else if(grepl( "LGBT", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "LGBT"
  else if(grepl( "Labour", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Labour"
  else if(grepl( "Rent", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Housing Rights"
  else if(grepl( "Housing", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Housing Rights"
  else if(grepl( "Health", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Healthcare"
  else if(grepl( "Asian", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Asian"
  else if(grepl( "Latinx", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Latinx"
  else if(grepl( "Women", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Women"
  else if(grepl( "Teacher", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Teachers"
  else if(grepl( "Student", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Students"
  else if(grepl( "ICE", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Migrant Rights"
  else if(grepl( "migrant", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Migrant Rights"
  else if(grepl( "Migrant", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Migrant Rights"
  else if(grepl( "DREAM", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Migrant Rights"
  else if(grepl( "LR", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Migrant Rights"
  else if(grepl( "Refugee", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "Migrant Rights"
  else if(grepl( "BLM", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "BLM-Related"
  else if(grepl( "African American", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "BLM-Related"
  else if(grepl( "ACLU", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "BLM-Related"
  else if(grepl( "NAACP", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "BLM-Related"
  else if(grepl( "Racism", acled$assoc_actor_2[i]))
    acled$assoc_actor_label2[i] <- "BLM-Related"
  else
    acled$assoc_actor_label2[i] <- "Other"
}

acled$actorgroups <- paste(acled$assoc_actor_label1, "and", acled$assoc_actor_label2)

write.csv(acled, 'acled.csv')

library(zoo)

count2 <- acled %>%
  mutate(yearmon = as.yearmon(event_date)) %>%
  # use that variable to group the data
  group_by(yearmon) %>%
  # count the number of observations in each of those year-month bins. if you
  # want to summarise the data some other way, use 'summarise' here instead.
  count()

count1 <- acled %>%
  filter(fatalities>0) %>%
  # use 'as.yearmon' to create a variable identifying the unique year-month
  # combination in which each observation falls
  mutate(yearmon = as.yearmon(event_date)) %>%
  # use that variable to group the data
  group_by(yearmon) %>%
  # count the number of observations in each of those year-month bins. if you
  # want to summarise the data some other way, use 'summarise' here instead.
  count()
# plot the resulting series with yearmon on the x-axis and using 'geom_col'
# instead of 'geom_hist' to preserve the temporal ordering and avoid
# having to specify stat = "identity"
count1$PctTot <- (count1$n/count2$n)*100

write.csv(count1, 'death_count.csv')
