#packages
library("segmented")
library("ggplot2")
library("readxl")
library("dplyr")
#original data
combETE <- read.csv("./data/combinedETE.csv")

# new data
comb <- read_excel("data/comb.data_revised_for_Telford.xlsx", sheet = 1)
names(comb)

#tables
table(comb$land.island.mod.original.scoring, useNA = "if")
table(comb$land.island.published, useNA = "if")
table(comb$land.island.mod.Sent.to.Telford, useNA = "if")

table(subset(comb, numSites >= 0 & land.island.published == "ETE" & !is.na(land.island.published), old), useNA = "if")

table(subset(comb, numSites >= 0 & !land.island.published %in% c("NA", "island"), old))
table(subset(comb, numSites >= 0 & !land.island.mod.CORRECTED.overlapping.removed %in% c("NA", "island"), old))

table(subset(comb, numSites >= 20 & !land.island.published %in% c("NA", "island"), old))
table(subset(comb, numSites >= 20 & !land.island.mod.CORRECTED.overlapping.removed %in% c("NA", "island"), old))

table(subset(comb, numSites >= 40 & !land.island.published %in% c("NA", "island"), old))
table(subset(comb, numSites >= 40 & !land.island.mod.CORRECTED.overlapping.removed %in% c("NA", "island"), old))




table(subset(comb, numSites >= 40 & !land.island.mod.CORRECTED.overlapping.removed.Telford.filter %in% c("NA", "island"), old))

comb %>%
 filter(!land.island.mod.CORRECTED.overlapping.removed %in% c("NA", "island")) %>%
 filter(numSites > 50) %>%
 select(Dataset, numSites, AGE, percAgg, land.island.mod.CORRECTED.overlapping.removed) %>%
 arrange(AGE) %>%
  group_by(AGE) %>%
  summarise(n = n(), mean = mean(percAgg), median = median(percAgg))



with(
  subset(comb, numSites >= 40 & !land.island.mod.CORRECTED.overlapping.removed.Telford.filter %in% c("NA", "island"), c(percAgg, old)),
  wilcox.test(percAgg~old)
)


comb %>%
  filter(Type == "Pollen") %>%
  select(Dataset, percAgg, numSites)
  

#original
table(combETE$land.island)

#what's new
landnew <- subset(comb, land.island.mod.Sent.to.Telford == "land", Dataset)[[1]]
landold <- subset(combETE, land.island == "land", Dataset)[[1]]
setdiff(landnew, landold)


islandnew <- subset(comb, land.island.mod.Sent.to.Telford == "island", Dataset)[[1]]
islandold <- subset(combETE, land.island == "island", Dataset)[[1]]
setdiff(islandold, islandnew)





## size of data sets
table(combETE$Age > 100)

comb$old <- comb$age.published > 100
with(subset(comb, land.island.published != "island"), tapply(numSites, old, median))
with(subset(comb, land.island.published != "island"), tapply(numSites, age.published < 6000, median))

## merge and check percAgg
merge(combETE%>%select(Dataset, percAgg, Age), comb2%>%select(Dataset, percAgg, age.published), by = "Dataset", all = TRUE)


comb %>% 
  filter(land.island.published == "land") %>%
  select(Dataset, numSites, numSpp,percAgg, land.island.mod.CORRECTED.overlapping.removed)

comb %>% 
  filter(land.island.mod.CORRECTED.overlapping.removed == "land") %>%
  filter(dispersalLimited) %>%
  select(Dataset, numSites, percAgg)

comb %>% 
  filter(land.island.mod.CORRECTED.overlapping.removed %in% c("ETE", "land")) %>%
  filter(AGE == "Modern") %>%
  filter(numSites > 40) %>%
  select(Dataset, numSites, percAgg, numAgg)

comb %>% 
  filter(land.island.mod.CORRECTED.overlapping.removed %in% c("ETE", "land")) %>%
  filter(AGE == "Shallow Time") %>%
  filter(numSites > 40) %>%
  select(Dataset, numSites, percAgg, numAgg)

comb %>% 
  filter(land.island.mod.CORRECTED.overlapping.removed %in% c("ETE", "land")) %>%
  filter(AGE == "Deep Time") %>%
  filter(numSites > 40) %>%
  select(Dataset, numSites, percAgg, numAgg)
