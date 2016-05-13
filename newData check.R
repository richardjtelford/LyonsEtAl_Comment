
#original data
combETE <- read.csv("./data/combinedETE.csv")
combETE$Age.yrs <- ifelse(combETE$Age > 33, combETE$Age, 33) # Synchronize ages of modern ETE datasets with those of the modern Gotelli & Ulrich data (median of ages = 33 years)
combETE$Age.log <- log10(combETE$Age)


#new data
newData <- read.csv("./data/comb.data_Telford.csv", stringsAsFactor = FALSE)

#more datasets?
nrow(combETE)
nrow(newData)


head(combETE)
head(newData)

setdiff(combETE$Dataset, newData$Dataset)#missing data sets (just different name)
setdiff(newData$Dataset, combETE$Dataset)#new datasets

newData$Dataset[newData$Dataset == "crypticTaxa"] <- "crypticTaxa _ Desert Rodents"#fix name
newData[newData$Dataset %in% setdiff(newData$Dataset, combETE$Dataset),] #new datasets

#merge data
allData <- merge(combETE, newData, all = TRUE)
head(allData)


allData[allData$land.island != allData$land.island.mod.ORIGINAL, ]#land island swaps

#check n
with(allData, sum(land.island != "island" & Age<100, na.rm = TRUE))
with(allData, sum(land.island.mod.ORIGINAL != "island" & age.corrected<100, na.rm = TRUE))
with(allData, sum(land.island.mod.CORRECTED != "island" & age.corrected<100, na.rm = TRUE))
with(allData, sum(land.island.mod.CORRECTED.overlapping.removed != "island" & age.corrected<100, na.rm = TRUE))
with(allData, sum(land.island.mod.CORRECTED.overlapping.removed.Telford.filter != "island" & age.corrected<100, na.rm = TRUE))
with(allData, sum(land.island.mod.CORRECTED.overlapping.removed.Telford.filter != "island" & age.corrected<100 & numSites > 10, na.rm = TRUE))
with(allData, sum(land.island.mod.CORRECTED.overlapping.removed.Telford.filter != "island" & age.corrected<100 & numSites > 20, na.rm = TRUE))
with(allData, sum( age.corrected>1000000 & numSites > 20, na.rm = TRUE))

with(combETE, mean(percAgg[land.island != "island" & Age<100], na.rm = TRUE))


#breakpoint replication
library("segmented")
library("ggplot2")
set.seed(9088)#from random.org
boot.num = 10000
smallData <- subset(allData, land.island.mod.CORRECTED.overlapping.removed.Telford.filter != "island" & !is.na(land.island.mod.CORRECTED.overlapping.removed.Telford.filter) & numSites >= 20, -(2:5))
subset(smallData, land.island != "ETE")
table(cut(smallData$Age, breaks = c(10^9, 10^6, 100, 0)))

lm.7000 <- lm(percAgg ~ Age.log, data = smallData)
seg.7000 <- segmented(lm.7000, seg.Z= ~Age.log, psi=list(Age.log = log10(c(10000)) ) , seg.control(n.boot=boot.num) )
10^confint(seg.7000)$Age # not seven thousand

ggplot(smallData, aes(x = log10(Age.yrs), y = percAgg, col = land.island.mod.CORRECTED.overlapping.removed.Telford.filter, weight = numSites)) + 
  geom_jitter(width = 0.5, height = 0, show.legend = FALSE) +
  geom_smooth(group = 1, show.legend = FALSE) +
  scale_x_reverse() +
  geom_vline(xintercept = log10(6000), linetype = 2, colour = "grey80")


#does one extra dataset with pagg == 0 change BP
smallPlus <- rbind(smallData, subset(allData, Dataset == "1000islmco.txt", -(2:5)))
lm.plus <- lm(percAgg ~ Age.log, data = smallPlus)
seg.plus <- segmented(lm.plus, seg.Z= ~Age.log, psi=list(Age.log = log10(c(10000)) ) , seg.control(n.boot=boot.num) )
10^confint(seg.plus)$Age # not seven thousand

