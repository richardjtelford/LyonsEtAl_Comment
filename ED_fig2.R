library("ggplot2")
library("scales")

#load data
combETE <- read.csv("./data/combinedETE.csv")
combETE$old <- combETE$Age > 100

#with weights = numSig taxa

ggplot(subset(combETE, land.island != "island"), aes(x = numSites, y = percAgg, col = old, shape = old, weight = numSig)) + 
  geom_jitter(width = 0, height = 0.02) +
  geom_smooth(aes(group  = 1), method = "gam", formula = y ~s(x, k = 4), method.args = list(family = "quasibinomial"), show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_log10(breaks = c(10, 20, 50, 100, 200, 400)) + 
  scale_colour_discrete(breaks = c("TRUE","FALSE"), labels = c("True", "False")) +
  scale_shape_discrete(breaks = c("TRUE","FALSE"), labels = c("True", "False")) +
  labs(x = "Number of sites", y = "Percent aggregated pairs", colour = "Age > 100", shape = "Age > 100") + 
  theme(legend.justification=c(0, 0), legend.position=c(.76, 0)) 
ggsave("ed_fig2.png", height = 89 * 1.5, width = 89 * 1.5, units = "mm")

#no weights
ggplot(subset(combETE, land.island != "island"), aes(x = numSites, y = percAgg, col = land.island, shape = land.island)) + 
  geom_jitter(width = 0, height = 0.03) +
  geom_smooth(aes(group  = 1), method = "gam", formula = y ~s(x), method.args = list(family = "quasibinomial"), show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_log10() + 
  labs(x = "Number of sites", y = "Percent aggregated pairs", colour = "Age", shape = "Age")
       

