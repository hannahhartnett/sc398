library(tidyverse)

data$actor_label1 <- NA
data$actor_label2 <- NA
for(i in 1:nrow(data)) {
  if(grepl( "Protesters", data$actor1[i]))
    data$actor_label1[i] <- "Protesters"
  else if(grepl( "Police", data$actor1[i]))
    data$actor_label1[i] <- "Police"
  else if(grepl( "Sole Perp", data$actor1[i]))
    data$actor_label1[i] <- "Sole Perpetrator"
  else if(grepl( "Riot", data$actor1[i]))
    data$actor_label1[i] <- "Rioters"
  else
    data$actor_label1[i] <- "Other"
}

for(i in 1:nrow(data)) {
  if(grepl( "Protesters", data$actor2[i]))
    data$actor_label2[i] <- "Protesters"
  else if(grepl( "Police", data$actor2[i]))
    data$actor_label2[i] <- "Police"
  else if(grepl( "Sole Perp", data$actor2[i]))
    data$actor_label2[i] <- "Sole Perpetrator"
  else if(grepl( "Riot", data$actor2[i]))
    data$actor_label2[i] <- "Rioters"
  else
    data$actor_label2[i] <- "Other"
}

