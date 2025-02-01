###############################################################################
## Replication File for Cattaneo, Feng, Palomba, and Titiunik (2025)
###############################################################################

theme_set(theme_bw())
dpi <- 300

predictand.list <- list("WaveAll" = "$\\widehat{\\tau}_{\\cdot t}$",
                        "Wave1" = "$\\widehat{\\tau}_{\\cdot t, <1987}$",
                        "Wave2" = "$\\widehat{\\tau}_{\\cdot t, 1987-1991}$",
                        "Wave3" = "$\\widehat{\\tau}_{\\cdot t, >1991}$")

arab.league <- c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", 
                 "Tunisia", "Djibouti", "Mauritania", "Somalia")

data <- haven::read_dta(paste0(path.data, "final_data.dta"))
data <- subset(data, continent == "Africa" & !(countryname %in% arab.league))
data$lgdp <- log(data$rgdppp)

treated.units.list <- list("WaveAll" = unique(subset(data, treated == 1 & trDate <= 1994)$countryname),
                           "Wave1" = unique(subset(data, treated == 1 & trDate < 1987)$countryname),
                           "Wave2" = unique(subset(data, treated == 1 & trDate >= 1987 & trDate <= 1991)$countryname),
                           "Wave3" = unique(subset(data, treated == 1 & trDate > 1991 & trDate <= 1994)$countryname))

methods.list <- c("ridge", "simplex", "L1-L2")
robs.list <- c("", "_covs", "_placebo")

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
# Table XX: treatment dates
######################################################################################

tr.dates <- data[data$year == 1968,c("countryname", "trDate")]

tr.dates <- tr.dates %>%
  mutate(trDate = as.character(trDate),
         trDate = ifelse(trDate == "9999", "$\\infty", trDate))

write.csv(tr.dates, paste0(path.tab, "includedCountries.csv"))

######################################################################################
# Table XX: store tuning parameters
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
# Table XX: prediction interval comparison
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
# Figure 1: staggered adoption matrix
######################################################################################

data <- haven::read_dta(paste0(path.data, "final_data.dta"))
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


