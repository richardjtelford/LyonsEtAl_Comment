library("ggplot2")
library("scales")

#load data
combETE <- read.csv("./data/combinedETE.csv")
levels(combETE$land.island) <- c("Fossil/historical", "island", "Modern")

#with weights = numSig taxa
ggplot(subset(combETE, land.island != "island"), aes(x = numSites, y = percAgg, col = land.island, shape = land.island, weight = numSig)) + 
  geom_jitter(width = 0, height = 0.03) +
  geom_smooth(aes(group  = 1), method = "gam", formula = y ~s(x, k = 4), method.args = list(family = "quasibinomial"), show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_log10() + 
  labs(x = "Number of sites", y = "Percent aggregated pairs", colour = "Age", shape = "Age") + geom_vline(xintercept = 20)
  theme(legend.justification=c(0, 0), legend.position=c(.75, 0)) 


#no weights
ggplot(subset(combETE, land.island != "island"), aes(x = numSites, y = percAgg, col = land.island, shape = land.island)) + 
  geom_jitter(width = 0, height = 0.03) +
  geom_smooth(aes(group  = 1), method = "gam", formula = y ~s(x), method.args = list(family = "quasibinomial"), show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  scale_x_log10() + 
  labs(x = "Number of sites", y = "Percent aggregated pairs", colour = "Age", shape = "Age")
       
