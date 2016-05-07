#library(devtools)
#install_github("gagern/random_pivot")

library(ggplot2)
source("https://raw.githubusercontent.com/gagern/RePairs/7ad28932cb203585adb3ce7e0730e4e2713dc2a7/RePairs.R") 


#to thin
tothin<-function(x){
  rn<-rownames(x)
  sites<-lapply(x,function(col){
    col[col>0]<-rn[col>0]
    col
    })
  sites<-unlist(sites)
  sites<-sites[sites>0]
  
  species<-mapply(function(col, spname){
      col[col>0]<-spname
      col
    }, col = x, spname = colnames(x))
  species<-unlist(species)
  species<-species[species>0]

  data.frame(species=factor(species), sites=factor(sites))
}

summarise.sigpairs<-function(x){
  nseg<-nrow(x$data.seg)
  nagg<-nrow(x$data.agg)
  c(nseg=nseg, nagg=nagg, pagg=nagg/(nseg+nagg))
}

#effect of dataset size
reduced.count.test<-function(nsamp=rep(seq(20,160, 20), each=10), spp){
  sapply(nsamp, function(n){
       keep<-sample(nrow(spp), n)
       spp<-spp[keep,]
       spp<-spp[,colSums(spp)>0]
       dim(spp)
  })  

}



reduced.count<-function(nsamp=rep(seq(20,160, 20), each=10), spp, nsp=150){
  reduced.count.res<-lapply(nsamp, function(n){print(n)
                                               keep<-sample(nrow(spp), n)
                                               spp<-spp[keep,]
                                               spp<-spp[,colSums(spp)>0]
                                               spp<-spp[, sample(ncol(spp),nsp)]
                                               spp<-tothin(x = spp)
                                               rrcs.to.file("demo", rows = spp$species, cols=spp$sites, repeats = 1000)
                                               rrcs <- rrcs.from.file("demo")
                                               sig20 <- calc.sig.pairs(rrcs,nBins = 20, zeroBin=TRUE, oneBin=TRUE)
                                               sig20
  })
  
  reduced.count.res.summ<-sapply(reduced.count.res, summarise.sigpairs)
  reduced.count.res.summ<-cbind(nsamp=nsamp,t(reduced.count.res.summ))
  reduced.count.res.summ<-as.data.frame(reduced.count.res.summ)
  reduced.count.res.summ2<-data.frame(nsamp=c(nsamp, nsamp), sig=c(reduced.count.res.summ$nseg, reduced.count.res.summ$nagg), segagg=factor(rep(c("Segregated", "Aggregated"), each=nrow(reduced.count.res.summ))))
  list(reduced=reduced.count.res.summ, reduced2=reduced.count.res.summ2)
}

plot.nagg<-function(x){
  require(ggplot2)
  p1<-ggplot(x$reduced2, aes(x = nsamp, y = sig, colour = segagg, shape = segagg))+
    geom_point(position = position_jitter(width = 4, height = 0))+
    geom_smooth(show.legend = FALSE, method = "gam", formula = y ~ s(x, k = 6), method.args = list(family = "quasipoisson"))+
    xlab("Number of observations")+
    ylab("Number significant pairs") +
    guides(colour=guide_legend(title="Significant Pairs"), shape = guide_legend(title="Significant Pairs")) +
    theme(plot.title=element_text(hjust=0, size = rel(1)), legend.position="none")
  p1
}

plot.pagg<-function(x){
  require(ggplot2)
  require(scales)
  require(locfit)
  p2<-ggplot(x$reduced, aes(nsamp, pagg))+
    geom_point(position = position_jitter(width = 4, height = 0))+
    geom_smooth(show.legend = FALSE, method = "locfit", formula = y ~ lp(x), method.args = list(family = "qbinomial")) +
    #geom_smooth(show.legend = FALSE, method = "gam", formula = y ~ s(x, k = 6), method.args = list(family = "quasibinomial")) +
    # geom_smooth(method = "loess", span=.5)+
    xlab("Number of observations")+
    ylab("Percent aggregated pairs") +
    scale_y_continuous(limits = c(0, 1), labels = percent) +
    theme(plot.title = element_text(hjust = 0, size = rel(1)))
  p2
}

aggseg<-function(rrcs, sig){
  rrcs<-rrcs[,3:6]
  rrcs$fi<-as.character(rrcs$fi)
  rrcs$fj<-as.character(rrcs$fj)
  pos.seg<-apply(sig$data.seg[,3:4], 1, function(r){which(rrcs$fi==r[1]&rrcs$fj==r[2])})
  rrcs$seg<-0
  rrcs$seg[pos.seg]<-"Seg"
  pos.agg<-apply(sig$data.agg[,3:4], 1, function(r){which(rrcs$fi==r[1]&rrcs$fj==r[2])})
  rrcs$agg<-0
  rrcs$agg[pos.agg]<-"Agg"
  rrcs$aggseg<-rrcs$agg
  rrcs$aggseg[pos.seg]<-"Seg"
  
  rrcs<-with(rrcs,data.frame(fi=c(fi,fj), fj=c(fj,fi), ki=c(ki, kj), kj=c(kj, ki), seg=seg, agg=agg, aggseg=aggseg))
  flev<-c(0, "Agg", "Seg")
  rrcs$agg<-factor(rrcs$agg, levels=flev)
  rrcs$seg<-factor(rrcs$seg, levels=flev)
  rrcs$aggseg<-factor(rrcs$aggseg, levels=flev)
  rrcs$fi<-factor(rrcs$fi, levels=unique(rrcs$fi[order(rrcs$ki)]))
  rrcs$fj<-factor(rrcs$fj, levels=levels(rrcs$fi))
  
  rrcs
}

uptri<-function(x)x[as.integer(x$fi)<as.integer(x$fj),]




plot.uptri<-function(x){
  colours3<-scale_fill_manual(values=c("grey", "blue", "red"))
  gsegagg<-ggplot(uptri(x), aes(x=fi, y=fj, fill=aggseg)) +
    geom_raster(show.legend = FALSE) + colours3 + 
    labs(x = NULL, y = NULL) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  panel.grid.major = element_blank())
  gsegagg
}

plot.aggseg<-function(x, aggregated = FALSE){
  if(aggregated){
    scm <- scale_color_manual(values = c("grey", "blue"))
    tit <- "Aggregated pairs"
    g <- ggplot(x[order(x$agg),], aes(x=ki, y=kj, col = agg))
  }else{
    scm <- scale_color_manual(values=c("grey", "red"))
    tit <- "Segregated pairs"
    g <- ggplot(x[order(x$seg),], aes(x=ki, y=kj, col = seg)) 
  } 

  jit <- 0.4

  g <- g + geom_point(position=position_jitter(width = jit, height = jit), show.legend = FALSE) + scm + 
    labs(x = "Number of occurrences", y = "Number of occurrences", title = tit) +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "white")) 
  g
}

runPairs <- function(x, nrep = 1000, nBins = 20){
  demo.data <- tothin(x)
  rrcs.to.file("demo", demo.data$species, demo.data$sites, repeats = nrep)
  rrcs <- rrcs.from.file("demo")
  sig <- calc.sig.pairs(rrcs, nBins = nBins, zeroBin = TRUE, oneBin = TRUE)
  list(rrcs = rrcs, sig = sig)
}
