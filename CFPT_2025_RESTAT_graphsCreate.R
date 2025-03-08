###############################################################################
## Replication File for Cattaneo, Feng, Palomba, and Titiunik (2025)
##
## This file creates the figures and tables for the paper.
###############################################################################

theme_set(theme_bw())
dpi <- 300

predictand.list <- list("WaveAll" = "$\\widehat{\\tau}_{\\cdot t}$",
                        "Wave1" = "$\\widehat{\\tau}_{\\cdot t, <1987}$",
                        "Wave2" = "$\\widehat{\\tau}_{\\cdot t, 1987-1991}$",
                        "Wave3" = "$\\widehat{\\tau}_{\\cdot t, >1991}$")

arab.league <- c("Algeria", "Egypt", "Libya", "Morocco", "Sudan",
                 "Tunisia", "Djibouti", "Mauritania", "Somalia")

data <- haven::read_dta(paste0(path.data, "BNdata.dta"))
data <- subset(data, continent == "Africa" & !(countryname %in% arab.league))
data$lgdp <- log(data$rgdppp)

treated.units.list <- list("WaveAll" = unique(subset(data, treated == 1 & trDate <= 1994)$countryname),
                           "Wave1" = unique(subset(data, treated == 1 & trDate < 1987)$countryname),
                           "Wave2" = unique(subset(data, treated == 1 & trDate >= 1987 & trDate <= 1991)$countryname),
                           "Wave3" = unique(subset(data, treated == 1 & trDate > 1991 & trDate <= 1994)$countryname))

methods.list <- c("ridge", "simplex", "L1-L2")
robs.list <- c("", "_covs", "_placebo")


###############################################################################
## Figures 3-6 and S.1-S.16
###############################################################################


for (method in methods.list) {
  for (robs.use in robs.list) {

    if (robs.use %in% c("_covs", "_placebo")) {
      if (method != "L1-L2") {
        next
      }
    } 

    load.name <- paste0("Africa_WaveAll_", method, robs.use, ".RData")
    save.name <- paste0("Africa_WaveAll_indiv_", method, robs.use, ".png")
    save.name.w <- paste0("Africa_WaveAll_indiv_", method, robs.use, "_w.png")
    save.name.tex <- paste0("Africa_WaveAll_indiv_", method, robs.use, ".tex")
    save.name.w.tex <- paste0("Africa_WaveAll_indiv_", method, robs.use, "_w.tex")

    load(paste0(path.out, load.name))

    ######################
    # TSUS \tau_{ik}
    ######################
    
    # plot time series
    p <- scplotMulti(res, type = "series", joint = FALSE, ncol=4, scales="free")
    pp <- p$plot_out + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=11),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(face="bold", size=11),
            strip.background = element_blank(),
            strip.text = element_text(face="bold", size = 10),
            axis.ticks.y = element_blank(),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text=element_text(size=14),
            legend.position="bottom",
            legend.background = element_rect(fill='transparent', colour = NA),
            legend.box.background = element_rect(fill='transparent', colour = NA)) + 
      ylab("(log) GDP per capita (USD thsd.)") + xlab("year") + ggtitle("") +
      scale_x_continuous(breaks=breaks_pretty(n=3))
    ggsave(filename = paste0(path.fig, save.name), plot = pp, width = 12, height = 9, dpi = dpi)
    
    tikz(file = paste0(path.fig, save.name.tex), width = 12, height = 6)
    plot(pp)
    dev.off()

    # plot weights
    w <- res$est.results$w
    aux <- data.frame(w=w,
                      donor=unlist(lapply(strsplit(names(w), "\\."), "[[", 2)),
                      treated=unlist(lapply(strsplit(names(w), "\\."), "[[", 1)))
    aux <- aux %>% 
      mutate(donor = str_replace(donor, "-", " "),
             treated = str_replace(treated, "-", " "),
             treated = str_replace(treated, "Botswana", "Bots-\nwana"),
             treated = str_replace(treated, "Cameroon", "Came-\nroon"),
             treated = str_replace(treated, "Mauritius", "Mauri-\ntius"))

    p <- ggplot() + 
      geom_point(data=aux, aes(x=donor, y=w, size=abs(w))) + xlab("") + ylab("Weight") +
      geom_hline(yintercept=0, linetype="dotted") +
      geom_hline(yintercept = c(-0.5, 0.5), alpha = 0.2) +
      geom_vline(xintercept = c(1:39), alpha = 0.2) + 
      facet_wrap(~treated, ncol=ceiling(length(unique(aux$treated))/1), labeller = label_wrap_gen(width=7)) +
      scale_x_discrete(limits=rev) + 
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_text(size = 10.7, face="bold"),
            axis.text.x=element_text(angle = 45, hjust=1, size=10, colour="black"),
            axis.text.y =element_text(size=12, face="bold", colour="black"),
            axis.ticks.y = element_blank(),
            axis.title.y = element_text(size = 14),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
      coord_flip()
    
    ggsave(filename = paste0(path.fig, save.name.w), plot = p, width = 14, height = 7, dpi = dpi)
    
    tikz(file = paste0(path.fig, save.name.w.tex), width = 14, height = 7)
    plot(p)
    dev.off()
    
    ######################
    # TAUS \tau_{i.}
    ######################
    save.name <- paste0("Africa_WaveAll_unit_", method, robs.use, ".png")
    save.name.w <- paste0("Africa_WaveAll_unit_", method, robs.use, "_w.png")
    save.name.tex <- paste0("Africa_WaveAll_unit_", method, robs.use, ".tex")
    save.name.w.tex <- paste0("Africa_WaveAll_unit_", method, robs.use, "_w.tex")
    
    p <- scplotMulti(res.unit, type = "series", joint = FALSE, ncol=4, scales="free")
    pp <- p$plot_out + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=11),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(face="bold"),
            strip.background = element_blank(),
            strip.text = element_text(face="bold", size=10.5),
            axis.ticks.y = element_blank(),
            legend.key = element_rect(colour = NA, fill = NA),
            legend.text=element_text(size=14),
            legend.position="bottom",
            legend.background = element_rect(fill='transparent', colour = NA),
            legend.box.background = element_rect(fill='transparent', colour = NA)) + 
      ylab("(log) GDP per capita (USD thsd.)") + xlab("year") + ggtitle("") +
      scale_x_continuous(breaks=breaks_pretty(n=3))
    
    ggsave(filename = paste0(path.fig, save.name), plot = pp, width = 12, height = 6, dpi = dpi)

    tikz(file = paste0(path.fig, save.name.tex), width = 12, height = 6)
    plot(pp)
    dev.off()
    
    # plot weights
    w <- res.unit$est.results$w
    aux <- data.frame(w=w,
                      donor=unlist(lapply(strsplit(names(w), "\\."), "[[", 2)),
                      treated=unlist(lapply(strsplit(names(w), "\\."), "[[", 1)))

    aux <- aux %>% 
      mutate(donor = str_replace(donor, "-", " "),
             treated = str_replace(treated, "-", " "),
             treated = str_replace(treated, "Botswana", "Bots-\nwana"),
             treated = str_replace(treated, "Cameroon", "Came-\nroon"),
             treated = str_replace(treated, "Mauritius", "Mauri-\ntius"))
    
    p <- ggplot() + 
      geom_point(data=aux, aes(x=donor, y=w, size=abs(w))) + xlab("") + ylab("Weight") +
      geom_hline(yintercept=0, linetype="dotted") +
      geom_hline(yintercept = c(-0.5, 0.5), alpha = 0.2) +
      geom_vline(xintercept = c(1:39), alpha = 0.2) + 
      facet_wrap(~treated, ncol=ceiling(length(unique(aux$treated))/1), labeller = label_wrap_gen(width=7)) +
      scale_x_discrete(limits=rev) + 
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_text(size = 10.7, face="bold"),
            axis.text.x=element_text(angle = 45, hjust=1, size=10, colour="black"),
            axis.text.y =element_text(size=12, face="bold", colour="black"),
            axis.ticks.y = element_blank(),
            axis.title.y = element_text(size = 14),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
      coord_flip()
    
    ggsave(filename = paste0(path.fig, save.name.w), plot = p, width = 14, height = 7, dpi = dpi)        

    tikz(file = paste0(path.fig, save.name.w.tex), width = 14, height = 7)
    plot(p)
    dev.off()
    
    ######################
    # TSUA \tau_{.k}
    ######################
    
    for (j in seq_len(length(predictand.list))) {
      
      wave.name <- names(predictand.list)[j]
      save.name <- paste0("Africa_", wave.name, "_time_", method, robs.use, ".png")
      save.name.w <- paste0("Africa_", wave.name, "_time_", method, robs.use, "_w.png")
      save.name.tex <- paste0("Africa_", wave.name, "_time_", method, robs.use, ".tex")
      save.name.w.tex <- paste0("Africa_", wave.name, "_time_", method, robs.use, "_w.tex")
      load.name <- paste0("Africa_", wave.name, "_", method, robs.use, ".RData")
      predictand.lab <- predictand.list[[j]]
      
      load(paste0(path.out, load.name))

      if (wave.name == "WaveAll") {
        width = 12; height = 6; text.size = 12
      } else {
        width = 12; height = 6; text.size = 22
      }
            
      # plot series
      p <- scplotMulti(res.time, type = "series", joint = TRUE)
      pp <- p$plot_out + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text=element_text(size=text.size),
              axis.title.x = element_text(size = text.size),
              axis.title.y = element_text(size = text.size),
              axis.text.x = element_text(face="bold"),
              strip.background = element_blank(),
              strip.text = element_blank(),
              axis.ticks.y = element_blank(),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.text=element_text(size=text.size),
              legend.position="bottom",
              legend.background = element_rect(fill='transparent', colour = NA),
              legend.box.background = element_rect(fill='transparent', colour = NA)) + 
        ylab("(log) GDP per capita (USD thsd.)") + xlab("years to treatment") + ggtitle("") +
        scale_x_continuous(breaks=breaks_pretty(n=3))
      
      ggsave(filename = paste0(path.fig, save.name), plot = pp, width = width, height = height, dpi = dpi)

      tikz(file = paste0(path.fig, save.name.tex), width = width, height = height)
      plot(pp)
      dev.off()

      # plot weights
      w <- res.time$est.results$w
      aux <- data.frame(w=w,
                        donor=unlist(lapply(strsplit(names(w), "\\."), "[[", 2)),
                        treated=unlist(lapply(strsplit(names(w), "\\."), "[[", 1)))

      aux <- aux %>% 
        mutate(donor = str_replace(donor, "-", " "),
               treated = str_replace(treated, "-", " "))

      if (wave.name == "WaveAll") {
        ncol = 16; height = 7; width = 14
        aux <- aux %>% 
          mutate(donor = str_replace(donor, "-", " "),
                 treated = str_replace(treated, "-", " "),
                 treated = str_replace(treated, "Botswana", "Bots-\nwana"),
                 treated = str_replace(treated, "Cameroon", "Came-\nroon"),
                 treated = str_replace(treated, "Mauritius", "Mauri-\ntius"))
      } else {
        ncol = length(treated.units.list[[j]]); height = 6; width=12
      }
      
      p <- ggplot() + 
        geom_point(data=aux, aes(x=donor, y=w, size=abs(w))) + xlab("") + ylab("Weight") +
        geom_hline(yintercept=0, linetype="dotted") +
        geom_hline(yintercept = c(-0.5, 0.5), alpha = 0.2) +
        geom_vline(xintercept = c(1:39), alpha = 0.2) + 
        facet_wrap(~treated, ncol=ceiling(length(unique(aux$treated))/1), labeller = label_wrap_gen(width=10)) +
        scale_x_discrete(limits=rev) + 
        theme(legend.position = "none",
              strip.background = element_blank(),
              strip.text = element_text(size = text.size, face="bold"),
              axis.text=element_text(size=text.size),
              axis.text.x=element_text(angle = 45, hjust=1, size=16, colour="black"),
              axis.text.y =element_text(size=text.size, face="bold", colour="black"),
              axis.ticks.y = element_blank(),
              axis.title.y = element_text(size = text.size),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
        coord_flip()
      
      ggsave(filename = paste0(path.fig, save.name.w), plot = p, width = width, height = height, dpi = dpi)  
      
      tikz(file = paste0(path.fig, save.name.w.tex), width = width, height = height)
      plot(p)
      dev.off()
      
    }
  }
}


######################################################################################
# Table S.2: prediction interval comparison
######################################################################################
if (isTRUE(joint.optim)) {
  covs.list <- c(FALSE, TRUE)

  # columns are three constraint types X M=1, M=2
  df.store <- data.frame("ridge" = character(12),
                         "ridge_covs" = character(12),
                         "simplex" = character(12),
                         "simplex_covs" = character(12),
                         "L1L2" = character(12),
                         "L1L2_covs" = character(12))

  for (method in methods.list) {
    method.str <- method
    if (method=="L1-L2") method.str <- "L1L2"

    for (covs.use in covs.list) {

      if (isTRUE(covs.use)) {
        covs.str <- "_covs"
      } else {
        covs.str <- ""
      }

      col.df <- paste0(method.str, covs.str)
      
      ######################
      # \tau_{ik} - Wave All
      ######################
      
      load.name <- paste0("Africa_WaveAll_", method, covs.str, ".RData")
      load(paste0(path.out, load.name))
      
      aux <- (1 - res$inference.results$CI.in.sample[, "Length"] / res.old$inference.results$CI.in.sample[, "Length"]) * 100
      aux <- round(aux, 2)
      
      df.store[1, col.df] <- round(median(aux, na.rm=TRUE), 2)
      df.store[2, col.df] <- paste0("$[",min(aux, na.rm=TRUE),";",max(aux, na.rm=TRUE),"]$")
  
      ######################
      # \tau_{i.} - Wave All
      ######################
      
      aux <- (1 - res.unit$inference.results$CI.in.sample[, "Length"] / res.unit.old$inference.results$CI.in.sample[, "Length"]) * 100
      aux <- round(aux, 2)
      
      df.store[3, col.df] <- round(median(aux, na.rm=TRUE), 2)
      df.store[4, col.df] <- paste0("$[",min(aux, na.rm=TRUE),";",max(aux, na.rm=TRUE),"]$")
  
      ######################
      # \tau_{.k} - Wave All
      ######################
      
      aux <- (1 - res.time$inference.results$CI.in.sample[, "Length"] / res.time.old$inference.results$CI.in.sample[, "Length"]) * 100
      aux <- round(aux, 2)
      
      df.store[5, col.df] <- round(median(aux, na.rm=TRUE), 2)
      df.store[6, col.df] <- paste0("$[",min(aux, na.rm=TRUE),";",max(aux, na.rm=TRUE),"]$")
      
      ######################
      # \tau_{.k} - Wave 1
      ######################
      load.name <- paste0("Africa_Wave1_", method, covs.str, ".RData")
      load(paste0(path.out, load.name))
      
      aux <- (1 - res.time$inference.results$CI.in.sample[, "Length"] / res.time.old$inference.results$CI.in.sample[, "Length"]) * 100
      aux <- round(aux, 2)
      
      df.store[7, col.df] <- round(median(aux, na.rm=TRUE), 2)
      df.store[8, col.df] <- paste0("$[",min(aux, na.rm=TRUE),";",max(aux, na.rm=TRUE),"]$")
  
      ######################
      # \tau_{.k} - Wave 2
      ######################
      load.name <- paste0("Africa_Wave2_", method, covs.str, ".RData")
      load(paste0(path.out, load.name))
      
      aux <- (1 - res.time$inference.results$CI.in.sample[, "Length"] / res.time.old$inference.results$CI.in.sample[, "Length"]) * 100
      aux <- round(aux, 2)
      
      df.store[9, col.df] <- round(median(aux, na.rm=TRUE), 2)
      df.store[10, col.df] <- paste0("$[",min(aux, na.rm=TRUE),";",max(aux, na.rm=TRUE),"]$")

      ######################
      # \tau_{.k} - Wave 3
      ######################
      load.name <- paste0("Africa_Wave1_", method, covs.str, ".RData")
      load(paste0(path.out, load.name))

      aux <- (1 - res.time$inference.results$CI.in.sample[, "Length"] / res.time.old$inference.results$CI.in.sample[, "Length"]) * 100
      aux <- round(aux, 2)

      df.store[11, col.df] <- round(median(aux, na.rm=TRUE), 2)
      df.store[12, col.df] <- paste0("$[",min(aux, na.rm=TRUE),";",max(aux, na.rm=TRUE),"]$")

    }
  }

  writexl::write_xlsx(df.store, paste0(path.tab, "piLengthReductionAux.xlsx"))

}

######################################################################################
# Table S.3: treatment dates
######################################################################################

tr.dates <- data[data$year == 1968,c("countryname", "trDate")]

tr.dates <- tr.dates %>%
  mutate(trDate = as.character(trDate),
         trDate = ifelse(trDate == "9999", "$\\infty", trDate))

write.csv(tr.dates, paste0(path.tab, "includedCountries.csv"))

######################################################################################
# Table S.4: store tuning parameters
######################################################################################

load(paste0(path.out, "Africa_WaveAll_L1-L2.RData"))
constr.list <- res$est.results$w.constr
pre.tr.period <- res$data$specs$period.pre

load(paste0(path.out, "Africa_WaveAll_L1-L2_covs.RData"))
constr.list.covs <- res$est.results$w.constr


aux <- cbind(unlist(lapply(constr.list, function(w) w$Q2)),
             unlist(lapply(constr.list.covs, function(w) w$Q2)),
             unlist(lapply(pre.tr.period, function(prd) prd[1])),
             unlist(lapply(pre.tr.period, function(prd) prd[length(prd)])),
             unlist(lapply(pre.tr.period, function(prd) length(prd))))

write.csv(round(aux, 3), paste0(path.tab, "estValueRegularizationParams.csv"))


######################################################################################
# Figure 1: staggered adoption matrix
######################################################################################

data <- haven::read_dta(paste0(path.data, "BNdata.dta"))
data <- subset(data, continent == "Africa" & !(countryname %in% arab.league))
data$lgdp <- log(data$rgdppp)

data$countryname.f <- as.factor(data$countryname)

aux <- LalRUtils::panelMatrices(subset(data, continent == "Africa"), unit_id = "countryname.f",
                                time_id = "year", outcome = "rgdppp", treat = "liberalization")
mat.melted <- reshape2::melt(aux$W)

toplot <- data[c("countryname.f", "year", "liberalization", "continent")]
toplot <- subset(toplot, continent == "Africa")
toplot$liberalization <- factor(toplot$liberalization,
                                levels = c(0,1),
                                labels = c("closed", "open"))
theme_set(theme_bw())
p <- ggplot(toplot) + geom_tile(aes(x=year,y=countryname.f, fill=liberalization)) +
      geom_vline(xintercept = c(min(toplot$year):max(toplot$year))+0.5, alpha = 0.2) +
      geom_hline(yintercept = c(1:39)-0.5, alpha = 0.2) + xlab("") + ylab("") +
      geom_vline(xintercept = c(1987,1991)-0.5, color="black", linewidth=1.011) +
      scale_fill_manual(values=wes_palette(n=2, name="AsteroidCity1")) +
      scale_y_discrete(limits=rev) +
      scale_x_discrete(limits=c(1970,1980,1987,1991,2000)) +
      theme(legend.position="bottom", panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text=element_text(size=12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.ticks.y = element_blank(),
            legend.title=element_blank(),
            legend.text=element_text(size=14))
ggsave(filename=paste0(path.fig, "africa_matrix_treatment.png"), plot=p, height=7, width=7, dpi="retina")

tikz(file = paste0(path.fig, "africa_matrix_treatment.tex"), width = 7, height = 7)
plot(p)
dev.off()



###############################################################################
# Simulate data for Figure 2
###############################################################################

theme_set(theme_bw())

width <- 12
height <- 5

set.seed(8894)

Nco <- 10
T0 <- 30
T1 <- 7

X.ls <- VAR.sim(B=diag(Nco)*0.5, n=T0+T1, include="none")

Y.co <- apply(X.ls,2, function(x) loess(x~c(1:(T0+T1)))$fitted)

w <- c(0.2,0.4,0.3,0.1, rep(0,Nco-4))
Y.tr <- Y.co %*% w

std.Y <- sd(Y.tr[1:T0,1])
Y.tr[(T0+1):(T0+T1),1] <- Y.tr[(T0+1):(T0+T1),1] + c(1:T1)*0.5*std.Y + c(1:T1)^2*0.05*std.Y 


###############################################################################
# Figure 2a: TSUS (Time-specific unit-specific predictand)
###############################################################################

Yco <- data.frame(Y.co, year = c((2022-T1-T0+1):2022))
toplot <- reshape2::melt(Yco, id = c("year"))
toplot$type <- "control"
toplot$treated <- 0

Ydf2 <- data.frame(Y.tr, year = c((2022-T1-T0+1):2022), treated = c(rep(0,T0),rep(1,T1)))
Ydf2$type <- "treated"
Ydf2$variable <- "XT"

names(toplot) <- c("year","unit","Y","type","T")
names(Ydf2) <- c("Y", "year","T","type","unit")

df <- rbind(toplot, Ydf2)
df$unit <- as.character(df$unit)

dfprep <- scdataMulti(df, id.var = "unit", outcome.var = "Y", 
                      treatment.var = "T", time.var = "year")

dfest <- scest(dfprep)
Ydf <- data.frame(Y = c(dfest$est.results$Y.pre.fit, dfest$est.results$Y.post.fit),
                  year = c((2022-T1-T0+1):2022), T = 0, type = "synthetic", unit = "SC")

dfplot <- rbind(df, Ydf)

ub <- subset(dfplot, year==2020&type=="treated")$Y
lb <- subset(dfplot, year==2020&type=="synthetic")$Y

dfline <- data.frame(x=2020, ymin=lb, ymax=ub)

dfplot <- subset(dfplot, year >= 2012)

tikz(file = paste0(path.fig, "ill_unit_time.tex"), width = width, height = height)
ggplot() + 
  geom_linerange(data=dfline, aes(x=x,ymin=ymin, ymax=ymax), color="#FD6467", size=1.7) +
  geom_point(data=subset(dfplot, type != "control"), aes(x=year,y=Y, group=unit, color=type, shape=type)) + 
  geom_line(data=dfplot, aes(x=year,y=Y, group=unit, alpha=type, color=type, linewidth=type)) + 
  scale_alpha_manual(name = "", values=c(0.3, 1, 1)) +
  scale_linewidth_manual(name = "", values=c(0.3, 1, 1)) +
  scale_color_manual(values = c("grey46","mediumblue","black"), name = "",
                     labels = c("Donors","SC", "Treated"),
                     guide = guide_legend(override.aes = list(linetype = c('solid','solid','solid'),
                                                              shape = c(NA, 16, 17)),
                                          position = "inside")) +
  geom_vline(xintercept=2015, linetype = "dashed", alpha=0.3) +
  guides(alpha="none", shape="none", linewidth="none") +
  labs(x="$t$", y="$Y_{it}$") + 
  scale_x_continuous(breaks = c(seq(2012,2022,by=2), 2015), labels = c(seq(1984,1994,by=2), 1987)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position.inside = c(0.15, 0.25),
        legend.background = element_rect(fill='transparent', colour = NA),
        legend.box.background = element_rect(fill='transparent', colour = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size = 12),
        text = element_text(size = 15)) +
  annotate("text", x = 2020.3, y = (lb+ub)/2, label = "$\\widehat{\\tau}_{1k}$", size=5, color="#FD6467") +
  annotate("text", x = 2020.1, y = 1.65, label = "$Y_{1(T_1+k)}(T_1)$", size = 5, color="black") +
  annotate("text", x = 2020, y = -0.4, label = "$\\widehat{Y}_{1(T_1+k)}(\\infty)$", size = 5, color="mediumblue") +
  annotate("text", x = 2015.35, y = 1.7, label = "$T_1$", size= 5)
dev.off()

  

###############################################################################
# Figure 2b: TAUS (Time-averaged unit-specific predictand)
###############################################################################

dfprep <- scdataMulti(df, id.var = "unit", outcome.var = "Y", effect = "unit",
                      treatment.var = "T", time.var = "year")

dfest <- scest(dfprep)
Ydfavg <- data.frame(Y = c(rep(NA,T0-1), rep(dfest$est.results$Y.post.fit,T1+1)),
                  year = c((2022-T1-T0+1):2022), T = 0, type = "syntheticavg", unit = "SC")
Ydfavgtr <- data.frame(Y = c(rep(NA,T0-1), rep(mean(subset(dfest$data$Y.df, Treatment==1)$Y),T1+1)),
                       year = c((2022-T1-T0+1):2022), T = 0, type = "treatedavg", unit = "XT")

dfplotavg <- rbind(Ydfavg, Ydfavgtr)

ub <- subset(dfplotavg, year==2018&type=="treatedavg")$Y
lb <- subset(dfplotavg, year==2018&type=="syntheticavg")$Y

dfline <- data.frame(x=2022, ymin=lb, ymax=ub)
dfplotavg <- subset(dfplotavg, year >= 2012)

tikz(file = paste0(path.fig, "ill_unit.tex"), width = width, height = height)
ggplot() + 
  geom_linerange(data=dfline, aes(x=x,ymin=ymin, ymax=ymax), color="#FD6467", size=1.7) +
  geom_point(data=subset(dfplot, type != "control"), aes(x=year,y=Y, group=unit, alpha=type, color=type, shape=type)) + 
  geom_line(data=dfplot, aes(x=year,y=Y, group=unit, alpha=type, color=type, linetype=type, size=type)) + 
  scale_alpha_manual(values=c(0.3, .5, 1, .5, 1)) +
  scale_size_manual(values=c(0.3, 1, 1, 1, 1)) +
  scale_shape_manual(values = c(16,16,17,17)) +
  scale_linetype_manual(values=c("solid","solid","dashed","solid","dashed")) +
  scale_color_manual(values = c("grey46","mediumblue","mediumblue","black","black"), name = "",
                     labels = c("Donors","SC", "SC (avg.)", "Treated", "Treated (avg.)"),
                     guide = guide_legend(override.aes = list(linetype = c("solid","solid","dashed","solid","dashed"),
                                                              shape = c(NA, 16, 16, 17, 17)), 
                                          position="inside")) +
  geom_vline(xintercept=2015, linetype = "dashed", alpha=0.3) + 
  guides(alpha="none", shape="none", linetype="none", size="none") +
  geom_point(data=dfplotavg, aes(x=year, y=Y, group=unit,alpha=type, color=type, shape=type)) +
  geom_line(data=dfplotavg, aes(x=year,y=Y,color=type,alpha=type,group=unit, linetype=type)) + 
  annotate("text", x = 2021.6, y = (lb+ub)/2, label = "$\\widehat{\\tau}_{1 \\cdot}$", size=5, color="#FD6467") +
  annotate("text", x = 2018, y = 1.4, label = "$\\frac{1}{T-T_1+1}\\sum\\limits_{t=T_1}^{T} Y_{1t}(T_1)$", size=5, color="black") +
  annotate("text", x = 2018, y = -0.4, label = "$\\frac{1}{T-T_1+1}\\sum\\limits_{t=T_1}^{T} \\widehat{Y}_{1t}(\\infty)$", size=5, color="mediumblue") +
  annotate("text", x = 2015.35, y= 1.7, label = "$T_1$", size= 5) +
  labs(x="$t$",y="$Y_{it}$") + ylim(-1.5, 2) + 
  scale_x_continuous(breaks = c(seq(2012,2022,by=2), 2015), labels = c(seq(1984,1994,by=2), 1987)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position.inside = c(0.15, 0.2),
        legend.background = element_rect(fill='transparent', colour = NA),
        legend.box.background = element_rect(fill='transparent', colour = NA),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(size = 12),
        text = element_text(size = 15))
dev.off()


###############################################################################
# Figure 2c: TSUA (Time-specific unit-averaged predictand)
###############################################################################
delta.tr <- 4
T0.list <- c(T0,T0+delta.tr)
T1.list <- c(T1,T1-delta.tr)

w2 <- c(0.1,0,0,0.9,rep(0,6))
Y.tr <- Y.co %*% w2

std.Y <- sd(Y.tr[1:T0.list[2],1])
Y.tr[(T0.list[2]+1):(T0.list[2]+T1.list[2]),1] <- Y.tr[(T0.list[2]+1):(T0.list[2]+T1.list[2]),1] + 
  c(1:T1.list[2])*1*std.Y + c(1:T1.list[2])^2*0.15*std.Y 

Ydf2 <- data.frame(Y=Y.tr, year = c((2022-T1.list[2]-T0.list[2]+1):2022), 
                   T = c(rep(0,T0.list[2]),rep(1,T1.list[2])))
Ydf2$type <- "treated"
Ydf2$unit <- "XT2"

dff <- rbind(df, Ydf2)

dfprep <- scdataMulti(dff, id.var = "unit", outcome.var = "Y", 
                      treatment.var = "T", time.var = "year")

dfest <- scest(dfprep)

yfit <- c(dfest$est.results$Y.pre.fit[1:T0.list[1]], 
          dfest$est.results$Y.post.fit[1:T1.list[1]],
          dfest$est.results$Y.pre.fit[(T0.list[1]+1):sum(T0.list)], 
          dfest$est.results$Y.post.fit[(T1.list[1]+1):sum(T1.list)])

Ydf <- data.frame(Y = yfit, year = rep(c((2022-T1-T0+1):2022),2), 
                  T = 0, type = "synthetic", unit = c(rep("SC1",T0+T1),rep("SC2",T0+T1)))

dffplot <- rbind(dff, Ydf)
dffplot <- subset(dffplot, year >= 2014)

k.t <- 3
k <- k.t + 2015

ub <- subset(dffplot, year==k&unit=="XT")$Y
lb <- subset(dffplot, year==(k+delta.tr)&unit=="XT2")$Y
ub2 <- subset(dffplot, year==k&unit=="SC1")$Y
lb2 <- subset(dffplot, year==(k+delta.tr)&unit=="SC2")$Y

scatteravg <- data.frame(x=k+k.t/2+0.5, y=c((lb+ub)/2,(lb2+ub2)/2),type=c("tr","sc"))
dfline <- data.frame(x=k+k.t/2+0.5,ymin=(lb2+ub2)/2, ymax=(lb+ub)/2)
dfdiag1 <- data.frame(xmin=k,xmax=k+delta.tr,ymin=lb,ymax=ub)
dfdiag2 <- data.frame(xmin=k,xmax=k+delta.tr,ymin=ub2,ymax=lb2)

tikz(file = paste0(path.fig, "ill_time.tex"), width = width, height = height)
ggplot() + 
  geom_linerange(data=dfline, aes(x=x,ymin=ymin, ymax=ymax), color="#FD6467", size=1.7) +
  geom_point(data=subset(dffplot, type != "control"), aes(x=year,y=Y, group=unit, color=type, shape=type)) + 
  geom_line(data=dffplot, aes(x=year,y=Y, group=unit, alpha=type, color=type, size=type)) + 
  scale_alpha_manual(name = "",values=c(0.3, 1, 1)) +
  scale_size_manual(name = "",values=c(0.3, 1, 1)) +
  scale_color_manual(values = c("grey46","mediumblue","black"), name = "",
                     labels = c("Donors","SC", "Treated"),
                     guide = guide_legend(position="inside",
                                          override.aes = list(linetype = c('solid','solid','solid'),
                                                              shape = c(NA, 16, 17)))) +
  geom_vline(xintercept=2015, linetype = "dashed", alpha=0.3) + guides(alpha="none", shape="none", size="none") +
  geom_vline(xintercept=2015+delta.tr, linetype = "dashed", alpha=0.3) + 
  geom_point(data=subset(scatteravg, type=="tr"), aes(x=x,y=y), color="black", shape = 17, size=2) +
  geom_point(data=subset(scatteravg, type=="sc"), aes(x=x,y=y), color="mediumblue", size=2) +
  geom_segment(data=dfdiag1,aes(x=xmin,xend=xmax,yend=ymin,y=ymax), color="black") + 
  geom_segment(data=dfdiag2,aes(xend=xmin,x=xmax,yend=ymin,y=ymax), color="mediumblue") + 
  annotate("text", x = 2020.3, y = mean(scatteravg$y)-0.2, label = "$\\widehat{\\tau}_{\\mathcal{Q} k}$", size=5, color="#FD6467") +
  annotate("text", x = 2020.2, y = scatteravg$y[1]+0.25, label = "$\\frac{1}{Q}\\sum\\limits_{i:T_i\\in\\mathcal{Q}}Y_{i(T_i+k)}(T_i)$", size = 3.65, color="black") +
  annotate("text", x = 2019.1, y = scatteravg$y[2]+0.03, label = "$\\frac{1}{Q}\\sum\\limits_{i:T_i\\in\\mathcal{Q}}\\widehat{Y}_{i(T_i+k)}(\\infty)$", color="mediumblue", size = 3.65) +
  annotate("text", x = k, y = ub + .2, label = "$Y_{1(T_1+k)}(T_1)$", size = 3.5, color="black") +
  annotate("text", x = 2021.8, y = lb + 0.25, label = "$Y_{2(T_2+k)}(T_2)$", size = 3.5, color="black") +
  annotate("text", x = k-0.2, y = 0.35, label = "$\\widehat{Y}_{1(T_1+k)}(\\infty)$", size = 3.5, color="mediumblue") +
  annotate("text", x = 2021.8, y = lb2 - .13, label = "$\\widehat{Y}_{2(T_2+k)}(\\infty)$", size = 3.5, color="mediumblue") +
  annotate("text", x = 2015.25, y= 1.5, label = "$T_1$", size= 5) +
  annotate("text", x = 2019.25, y= 1.5, label = "$T_2$", size= 5) +
  labs(x="$t$",y="$Y_{it}$") + 
  scale_x_continuous(breaks = c(seq(2012,2022,by=2), 2015, 2019), labels = c(seq(1984,1994,by=2), 1987, 1991)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position.inside = c(0.1, 0.2),
        legend.background = element_rect(fill='transparent', colour = NA),
        legend.box.background = element_rect(fill='transparent', colour = NA),
        legend.key.size = unit(1, 'cm'),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.text = element_text(size = 13),
        text = element_text(size = 15))
dev.off()
