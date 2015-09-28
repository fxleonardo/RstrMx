library(ggplot2)

players <- c('Stephen Curry', 'LeBron James')

tmpPlayerStats <- playerStats %>% filter(Player=='Stephen Curry')
summary(tmpPlayerStats$Points)

ggplot(tmpPlayerStats) + geom_density(aes(x=Points))

ggplot(tmpPlayerStats, aes(x=Date,y=Points)) + geom_line()

ggplot(tmpPlayerStats, aes(x=Date, y=Points)) + geom_point() +
  stat_smooth(method="lm") + ylim(0, 60)

ggplot(tmpPlayerStats, aes(x=Date, y=Points)) + geom_point() +
  geom_smooth() + ylim(0, 60)

# check factors such as:
# age, opponents, DKPoints, away games, consecutive games, # games played, before/after all-stars

