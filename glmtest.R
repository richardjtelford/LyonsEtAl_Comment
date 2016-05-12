mod1 <- glm(percAgg ~ log(numSites), family  = quasibinomial, data = combETE, weights = numSig)
mod2 <- glm(cbind(numAgg, numSeg) ~ log(numSites), family  = quasibinomial, data = combETE)
summary(mod1)
summary(mod2)
