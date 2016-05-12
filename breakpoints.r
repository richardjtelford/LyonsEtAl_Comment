library(segmented)
library(plyr)

#loaddata
combETE <- read.csv("./data/combinedETE.csv")
combETE$Age.log <- log10(combETE$Age)
dispersalLimited<-c('canlauioco.txt', 'canlaussco.txt', 'caveio23co.txt', 'texaqhco.txt', 'orealbutco.txt', 'montanemco.txt', 'montmammco.txt','rockymamco.txt', 'gbfishco.txt', 'gbmam78co.txt', 'gbmam83co.txt', 'gbmam93co.txt')
combETE$dispersalLimited <- combETE$Dataset%in%dispersalLimited

#segmented models

set.seed(9088)#from random.org
boot.num = 10000

#all data
lm.all <- lm(percAgg ~ Age.log, data=combETE)
seg.all <- segmented(lm.all, seg.Z= ~Age.log, psi=list(Age.log = log10(c(10000)) ) , seg.control(n.boot=boot.num) )

#no island
lm.noisland <- lm(percAgg ~ Age.log, data = combETE, subset = land.island != "island")
seg.noisland <- segmented(lm.noisland, seg.Z= ~Age.log, psi=list(Age.log = log10(c(10000)) ) , seg.control(n.boot=boot.num) )

#no dispersal limited
lm.nodisp <- lm(percAgg ~ Age.log, data=combETE, subset = land.island!="island" & !dispersalLimited)
seg.nodisp <- segmented(lm.nodisp, seg.Z= ~Age.log, psi=list(Age.log = log10(c(10000)) ) , seg.control(n.boot=boot.num) )

#extract

mods<-list(alldata = list(m0 = lm.all, m1 = seg.all), noIsland = list(m0 = lm.noisland, m1 = seg.noisland), noDispersalLimited = list(m0 = lm.nodisp, m1 = seg.nodisp))

confint(mods[[1]]$m1)#4 sig figs

ldply(mods, function(x){
  #browser()
  ci <- signif(10^confint(x$m1)$Age[1, ], 4)
  ci <- format(round(ci), big.mark=",", trim=TRUE, scientific = FALSE)
  aic <- setNames(AIC(x$m0, x$m1)[,"AIC"], c("AIC.lm", "AIC.seg"))
  davis <- davies.test(obj = x$m0, seg.Z = ~Age.log, k = 10)
  res <- c(ci, round(aic, 2), davis.test.p = round(davis$p.value, 2))
  data.frame(as.list(res))
})
