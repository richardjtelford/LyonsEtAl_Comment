library("ggplot2")
library("scales")
library("readxl")
library("dplyr")


#load data
comb <- read_excel("data/comb.data_revised_for_Telford.xlsx", sheet = 1)

comb$AGE <- cut(comb$age.published, breaks = c(0, 100, 1000000, 10^9), labels = c("Modern", "Shallow Time", "Deep Time"))

comb2 <- comb %>%
  filter(!land.island.published %in% c("island", "NA"))

comb2 %>%
  group_by(AGE) %>%
  summarise(n = n(), lt40 = mean(numSites < 40))

table(comb2$AGE)

#no weights
g <- ggplot(comb2, aes(x = numSites, y = percAgg, col = AGE, shape = AGE)) + 
  geom_jitter(width = 0, height = 0.02) +
  geom_smooth(aes(group  = 1), method = "gam", formula = y ~s(x), method.args = list(family = "quasibinomial"), show.legend = FALSE) +
  labs(x = "Number of sites", y = "Proportion aggregated pairs", colour = "Age", shape = "Age") + 
  theme(legend.justification=c(0, 0), legend.position=c(.76, 0)) 
print(g)
ggsave("ed_fig2.png", height = 89 * 1.5, width = 89 * 1.5, units = "mm")

g + geom_vline(xintercept = c(20, 30, 40, 50))

g <- ggplot(subset(combETE, land.island != "island"), aes(x = Age, y =numSites)) + 
  geom_jitter(width = 1, height = 0.02)+
  scale_x_log10() + geom_vline(xintercept = 6000)
print(g)

