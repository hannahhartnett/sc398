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



### Hannah plots

library(dplyr)

leagues <- Batting %>% filter(lgID == 'NL' | lgID == 'AL')

batting_league <- leagues %>%
  # select the variables that we want left after we filter the data
  select(lgID, yearID, H, X2B, X3B, HR) %>%
  # select the years from 1871+
  filter(yearID >= 1871) %>%
  group_by(across(all_of(group_vars))) %>%
  #  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  summarise_all(funs(sum(., na.rm=TRUE))) %>%
  # we summarize by year, and then na.rm takes care of 0's in the data
  mutate(X1 = H-(X2B+X3B+HR)) %>% #create a column for singles
  # we eventually want these as a percentage of hits, so we can do the math now
  mutate(Single = X1/H*100) %>%
  mutate(Double = X2B/H*100) %>%
  mutate(Triple = X3B/H*100) %>%
  mutate(HomeRun = HR/H*100) %>%

league_plot <- batting_league %>%
  select(lgID, yearID, Single, Double, Triple, HomeRun)

library(reshape2)
league_melt <- melt(league_plot, id.vars = c('lgID', "yearID"))
head(league_melt)


# this is not very useful, no meaningful differences
library(ggplot2)
ggplot(filter(league_melt, lgID == 'NL' | lgID == 'AL'), aes(x=yearID, y= value, col=variable)) +
  geom_line(aes(linetype = lgID)) +
  xlab("Major League Baseball Season") +
  ylab("Percentage") +
  labs(linetype = 'League') +
  ggtitle("Hit Type Breakdown Remains Similar Across Leagues") +
  scale_x_continuous(breaks = c(1870, 1885, 1900, 1915, 1930, 1945,
                                1960, 1975, 1990, 2005, 2020 )) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  guides(colour=guide_legend(reverse=TRUE,
                             aes(ggtitle= "Type of Hit"))) +
  hohart21_398_theme




data(AwardsPlayers, package = 'Lahman')
awards <- AwardsPlayers


awards_hits <- merge(awards, Batting, by = 'playerID')




bat <- Batting %>%
  # select the variables that we want left after we filter the data
  select(playerID, H, X2B, X3B, HR) %>%
  # select the years from 1871+
  group_by(playerID) %>%
  #  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  summarise_all(funs(sum(., na.rm=TRUE))) %>%
  # we summarize by year, and then na.rm takes care of 0's in the data
  mutate(X1 = H-(X2B+X3B+HR)) #create a column for singles
  # we eventually want these as a percentage of hits, so we can do the math now


library(tidyr)
batting_long <- bat %>% select(-H) %>%
  pivot_longer(!c(playerID), names_to = "hit_type", values_to = "count")

total_hits_long <- bat %>% select(playerID, H) %>%
  pivot_longer(!c(playerID), names_to = "total_hits", values_to = "total_hit_count")

combined_long <- merge(batting_long, select(total_hits_long, -total_hits), by = 'playerID')

awards_hit_type <- merge(select(AwardsPlayers, c(awardID, playerID)), combined_long, by = 'playerID') %>% select(-playerID) %>% group_by(awardID, hit_type) %>%
  summarise_all(funs(sum(., na.rm=TRUE)))

awards_hit_type_marginal <- awards_hit_type %>%
  group_by(awardID, hit_type) %>%
  summarize(count = count,
            total = total_hit_count,
            proportion = round(count / total, 4),
            percentage = proportion * 100)


library(forcats)
awards_hit_type_marginal <- mutate(awards_hit_type_marginal,
                 hit_type = fct_recode(hit_type,
                                  "Single" = "X1",
                                  "Double" = "X2B",
                                  "Triple" = "X3B",
                                  "Home Run" = "HR"))

ggplot(filter(awards_hit_type_marginal, awardID == 'Outstanding DH Award' | awardID == 'Cy Young Award' | awardID == 'Most Valuable Player'), aes(x = awardID, y = percentage, fill = hit_type)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Award',
       y = 'Percentage of Hit Type',
       fill = 'Hit Type',
       title = 'Number of Singles Hit Varies by Award Type') +
  hohart21_398_theme






# correlation matrix heat map

data(Batting)
Batting

# take continuous variables
batting_continuous <- Batting %>% select(G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP)

cormat <- round(cor(batting_continuous, use = 'complete.obs'), 2)


library(reshape2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  labs(fill = "Correlation") +
  scale_fill_gradient2(midpoint = 0, limits = c(-1, 1))



reorder_cormat <- function(cormat){
  # Use correlation between variables as basis for distance
  dd <- as.dist(1-abs(cormat))
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
  return(cormat)
}

# Function to get lower triangle of corr matrix
get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

lo_tri_reordered <- cormat %>%
  reorder_cormat %>%
  get_lower_tri %>%
  melt

hohart21_398_theme <-  theme_bw() + # White background, black and white theme
  theme(axis.text = element_text(size = 10, color = "azure4"),
        text = element_text(size = 10, face = "bold", color = "azure4", family = 'Georgia'))


ggplot(data = lo_tri_reordered, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(x = 'Batting Variable',
       y = 'Batting Variable',
       title = 'Correlation Heat Map for Continuous Batting Variables',
       fill = 'Correlation') +
  scale_fill_gradient2(midpoint = 0, limits = c(-1, 1)) +
  hohart21_398_theme
