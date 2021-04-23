# Cole's Plots

# First Plot

library("dplyr")
data(Batting, package="Lahman")

batting <- Batting %>%
  # select the variables that we want left after we filter the data
  select(yearID, H, X2B, X3B, HR) %>%
  # select the years from 1871+
  filter(yearID >= 1871) %>%
  group_by(yearID) %>%
  #  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  summarise_all(funs(sum(., na.rm=TRUE))) %>%
  # we summarize by year, and then na.rm takes care of 0's in the data
  mutate(X1 = H-(X2B+X3B+HR)) %>% #create a column for singles
  # we eventually want these as a percentage of hits, so we can do the math now
  mutate(Single = X1/H*100) %>%
  mutate(Double = X2B/H*100) %>%
  mutate(Triple = X3B/H*100) %>%
  mutate(HomeRun = HR/H*100)

bat <- batting %>%
  select(yearID, Single, Double, Triple, HomeRun)

library(reshape2)
bat_long <- melt(bat, id.vars = c("yearID"))
head(bat_long)

library(ggplot2)
hitsperyear <- ggplot(bat_long, aes(x=yearID, y= value, col=variable)) +
  geom_line() +
  xlab("Major League Baseball Season") +
  ylab("Percentage") +
  ggtitle("Hits by Type in Major League Baseball") +
  scale_x_continuous(breaks = c(1870, 1885, 1900, 1915, 1930, 1945,
                                1960, 1975, 1990, 2005, 2020 )) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  guides(colour=guide_legend(reverse=TRUE,
                             aes(ggtitle= "Type of Hit")))
hitsperyear

# Second plot

library(Lahman)
S <- summarize(group_by(Batting, yearID, playerID),
               H=sum(H), AB=sum(AB),
               BB=sum(BB), HBP=sum(HBP, na.rm=TRUE),
               SF=sum(SF, na.rm=TRUE))

ST <- summarize(group_by(Teams, yearID),
                Games = round(mean(W + L)),
                min.AB = 3 * Games)
S2 <- merge(S, ST, by="yearID")
S.regular <- filter(S2, AB >= min.AB)

SD.data <- summarize(group_by(S.regular, yearID),
                     SD.AVG = sd(H / AB),
                     SD.OBP = sd((H + BB + HBP) / (AB + BB + HBP + SF)))

sd_avg <-  ggplot(SD.data, aes(yearID, SD.AVG)) + geom_point() +
  geom_smooth(se=FALSE, span=0.35) +
  labs(title = "Standard Deviations of AVG of Regular Hitters",
       x = "Year", y = "SD of Batting Average")

sd_obp <- ggplot(SD.data, aes(yearID, SD.OBP)) +
  geom_point() + geom_smooth(se=FALSE, span=0.35) +
  labs(title = "Standard Deviations of OBP of Regular Hitters",
       x = "Year", y = "SD of On Base Percentage")

library(gridExtra)
grid.arrange(sd_avg, sd_obp, ncol = 1)

