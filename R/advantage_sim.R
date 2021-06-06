require(ggplot2)
require(qlcMatrix)
require(dplyr)
require(reshape2)

nroll = 1e7
rollSim = function(nn, dice) ceiling(runif(nn) * dice)

df = data.frame(normal.roll = rollSim(nroll, 20),
                advantage.roll = rowMax(cbind(rollSim(nroll, 20), 
                                              rollSim(nroll, 20))) %>% as.vector())

# density plot
ggplot(df %>% melt(), aes(x=value, fill=variable)) + 
  geom_histogram(alpha = .5, bins = 20, position = "identity") + 
  theme_bw() +
  xlab("Dice result") + ylab("Probability") +
  geom_vline(xintercept = mean(df$normal.roll), color="red") +
  geom_vline(xintercept = mean(df$advantage.roll), color="blue")
ggsave("figures/adv_density.png", width = 6, height = 3.5)

# probability-of-success plot
df2 = data.frame(DC = 1:20,
                 normal = rep(NA, 20),
                 advantage = rep(NA, 20))
for (ii in 1:20) {
  df2$normal[ii] = sum(df$normal.roll >= ii) / nroll
  df2$advantage[ii] = sum(df$advantage.roll >= ii) / nroll
}
df2 %>% melt(id.vars = "DC") %>%
  ggplot(., aes(x = DC, y= value*100, color=variable)) + geom_line() + 
  theme_bw() +
  ylab("Chance of succeeding, percent")
ggsave("figures/chance_of_success.png", width = 5, height = 3.5)

# effect of advantage
ggplot(df2, aes(x = DC, y= round((advantage-normal)*20))) + 
  geom_point() + 
  theme_bw() + xlab("Roll DC") +
  scale_y_continuous(breaks=0:5,labels=paste0("+",0:5)) +
  ylab("Effect of having advantage")
ggsave("figures/effect.png", width = 5, height = 3.5)
