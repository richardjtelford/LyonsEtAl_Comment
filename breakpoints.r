#load packages
library("readxl")
library("dplyr")
library("segmented")

#functions
run.segmented <- function(data){
  mod.lm <- lm(percAgg ~ Age.log, data = data)
  mod.seg <- segmented(mod.lm, seg.Z = ~Age.log, psi = list(Age.log = log10(10000)), seg.control(n.boot = boot.num))
  davies <- davies.test(obj = mod.lm, seg.Z = ~Age.log, k = 10)
  #dfname <- as.name(deparse(substitute(data))) 
  #mod.lm$call <- call("lm", percAgg ~ Age.log, data = dfname)
  list(m0 = mod.lm, m1 = mod.seg, davies = davies)
}

make.table1 <- function(mods){
  ldply(mods, function(x){
    ci <- signif(10^confint(x$m1)$Age[1, ], 4)
    ci <- format(round(ci), big.mark=",", trim=TRUE, scientific = FALSE)
    aic <- setNames(AIC(x$m0, x$m1)[,"AIC"], c("AIC.lm", "AIC.seg"))
    res <- c(ci, round(aic, 1), davis.test.p = round(x$davies$p.value, 2))
    data.frame(as.list(res))
  })
  names(out) <- c("Data sets", "Estimate, yrs", "lower 95% CI, yrs", "upper 95% CI, yrs", "AIC lm", "AIC seg", "Davies test p")
  out
}


#load data
comb <- read_excel("data/comb.data_revised_for_Telford.xlsx", sheet = 1)

dispersalLimited<-c('canlauioco.txt', 'canlaussco.txt', 'caveio23co.txt', 'texaqhco.txt', 'orealbutco.txt', 'montanemco.txt', 'montmammco.txt','rockymamco.txt', 'gbfishco.txt', 'gbmam78co.txt', 'gbmam83co.txt', 'gbmam93co.txt')

#fix ages & dipersal limited
comb <- comb %>% 
  mutate(Age = ifelse(land.island.published == "ETE", age.published, 33)) %>%
  mutate(Age.log = log10(Age)) %>%
  mutate(dispersalLimited = Dataset %in% dispersalLimited)


#filter to published
combp <- comb %>% 
  filter(land.island.published != "NA")

table(combp$land.island.published)

#segmented
set.seed(9088)#from random.org
boot.num = 10000

#all data
mod.all <- run.segmented(combp)

#no island
mod.noisland <- run.segmented(combp %>% filter(land.island.published != "island"))

#no dispersal limited
mod.nodisp <- run.segmented(combp %>% filter(land.island.published != "island" & !dispersalLimited))

#extract

mods<-list(alldata = mod.all, noIsland = mod.noisland, noDispersalLimited = mod.nodisp)

table1 <- make.table1(mods)

#save
save(table1, file = "breakpoint2.table.Rdata")
load(file = "breakpoint2.table.Rdata")
load(file = "breakpoint.table.Rdata")

###
#segemented models with corrected and shrunk data

