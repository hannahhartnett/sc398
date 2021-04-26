# Text for the HR/AB and K/AB plot
Over the years, both home runs and strikeouts have increased, indicating a transition from small ball baseball with
where pitchers throw to contact, to high power baseball where stronger hitters try to hit home runs and pitchers
with increased velocity go for strikeouts.

# HR/AB vs. K/AB scatter w/ linear regression
hits_k <- ggplot(by_ID, aes(x = hr_ab, y = k_ab))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "HR/AB", y = "K/AB", title = "HR/AB vs. K/AB for players > 1,000 AB")

# Density plot of HR/AB
density <- ggplot(by_year, aes(x = hr_ab))+
  geom_density()+
  labs(x = "HR/AB", y = "Density of HR/AB by Year", title = "Density distribution of HR/AB from 1871-2020", )
