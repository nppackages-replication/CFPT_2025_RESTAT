###############################################################################
## Replication File for Cattaneo, Feng, Palomba, and Titiunik (2025)
##
## This code replicates the main analysis of the paper and the robustness
## exercises in the supplemental appendix.
###############################################################################

rm(list = ls(all = TRUE))

##########################################
# Load stuff
pacman::p_load(scpi, haven, wesanderson, ggplot2, reshape2, tikzDevice, 
               devtools, dplyr, tidyverse, scales, writexl)
devtools::install_github("apoorvalal/LalRUtils")
theme_set(theme_bw())
##########################################
# Set paths
path <- "YOUR_PATH"
cores <- 1

path.data <- paste0(path, "data/")
path.fig  <- paste0(path, "fig/")
path.tab  <- paste0(path, "tables/")
path.out  <- paste0(path, "out/")
path.code <- paste0(path, "code/")
source(paste0(path.code, "0001_funs.R"))

joint.optim <- FALSE # turn to T if want to replicate Table S.3

##########################################
# Set parameters/lists

arab.league <- c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", 
                 "Tunisia", "Djibouti", "Mauritania", "Somalia")
sims <- 200L
post.est <- 5L
anticipation <- 0L
covs.adj <- features <- NULL
coig.data <- TRUE
constant <- TRUE


##########################################
# Load data
##########################################

data <- haven::read_dta(paste0(path.data, "final_data.dta"))
data$lgdp <- log(data$rgdppp)

# remove countries in the arab league
data <- subset(data, continent == "Africa" & !(countryname %in% arab.league))

##########################################
## 1) Main Empirical Analysis
##########################################

set.seed(8894)

# We consider four different groups of treated units
treated.units.list <- list("WaveAll" = unique(subset(data, treated == 1 & trDate <= 1994)$countryname),
                           "Wave1" = unique(subset(data, treated == 1 & trDate < 1987)$countryname),
                           "Wave2" = unique(subset(data, treated == 1 & trDate >= 1987 & trDate <= 1991)$countryname),
                           "Wave3" = unique(subset(data, treated == 1 & trDate > 1991 & trDate <= 1994)$countryname))

# and three different methodologies
methods.list <- c("ridge", "simplex", "L1-L2")

##########################################
## 1a) one feature (log rgdp per capita)

for (method in methods.list) {
  for (j in seq_len(length(treated.units.list))) {
    treated.units <- treated.units.list[[j]]
    wave.name <- names(treated.units.list)[j]
    save.name <- paste0("Africa_", wave.name, "_", method)
    
    if (wave.name=="WaveAll") {
      eff.it <- eff.i <- eff.t <- TRUE
    } else {
      eff.it <- eff.i <- FALSE; eff.t <- TRUE
    }
    
    # save .RData 
    predictandsGet(data, treated.units, w.constr=list("name" = method), 
                   save.path=path.out, save.name=save.name,
                   anticipation=anticipation, post.est=post.est, sims=sims,
                   constant=TRUE, cores=cores, joint.optim=joint.optim,
                   eff.it=eff.it, eff.i=eff.i, eff.t=eff.t)
  }
}

##############################################################
## 1b) two features (log rgdp per capita, investment over gdp)

features <- list(c("lgdp", "inv_ratio"))
covs.adj <- list("lgdp"= c("constant", "trend"), 
                 "inv_ratio"= c("constant", "trend"))

for (method in methods.list) {
  for (j in seq_len(length(treated.units.list))) {

    treated.units <- treated.units.list[[j]]
    wave.name <- names(treated.units.list)[j]
    save.name <- paste0("Africa_", wave.name, "_", method, "_covs")
 
    if (wave.name=="WaveAll") {
      eff.it <- eff.i <- eff.t <- TRUE
    } else {
      eff.it <- eff.i <- FALSE; eff.t <- TRUE
    }    
     
    # save .RData 
    predictandsGetCovs(data, treated.units, w.constr=list("name" = method), 
                       save.path=path.out, save.name=save.name,
                       features=features, covs.adj=covs.adj, 
                       anticipation=anticipation, post.est=post.est, sims=sims,
                       constant=TRUE, cores=cores, joint.optim=joint.optim,
                       eff.it=eff.it, eff.i=eff.i, eff.t=eff.t)
  }
}


##########################################
## 2) Robustness Exercises 
##########################################

##########################################
## 2a) Placebo date

Pl.delta <- 11
data$trDate <- data$trDate - Pl.delta
data$liberalization <- 1*(data$year >= data$trDate)
data <- subset(data, countryname != "Mauritius") # Mauritius just has 5 pre-treatment periods

treated.units.list <- list("WaveAll" = unique(subset(data, treated == 1 & trDate <= (1994 - Pl.delta))$countryname),
                           "Wave1" = unique(subset(data, treated == 1 & trDate < (1987 - Pl.delta))$countryname),
                           "Wave2" = unique(subset(data, treated == 1 & trDate >= (1987 - Pl.delta) & trDate <= (1991 - Pl.delta))$countryname),
                           "Wave3" = unique(subset(data, treated == 1 & trDate > (1991-Pl.delta) & trDate <= (1994-Pl.delta))$countryname))

method <- "L1-L2"

for (j in seq_len(length(treated.units.list))) {
  treated.units <- treated.units.list[[j]]
  wave.name <- names(treated.units.list)[j]
  save.name <- paste0("Africa_", wave.name, "_", method, "_placebo")
  
  if (wave.name=="WaveAll") {
    eff.it <- eff.i <- eff.t <- TRUE
  } else {
    eff.it <- eff.i <- FALSE; eff.t <- TRUE
  }
  
  # save .RData 
  predictandsGet(data, treated.units, w.constr=list("name" = method), 
                 save.path=path.out, save.name=save.name,
                 anticipation=anticipation, post.est=post.est, sims=sims,
                 constant=TRUE, cores=cores, 
                 eff.it=eff.it, eff.i=eff.i, eff.t=eff.t)
}

##########################################
# 2b) Leave-one-out exercise

data <- haven::read_dta(paste0(path.data, "final_data.dta"))
data$lgdp <- log(data$rgdppp)

# remove countries in the arab league
data <- subset(data, continent == "Africa" & !(countryname %in% arab.league))

method <- "L1-L2"
robs.save <- "_loo"

dpi <- 300

post.est <- 5L
anticipation <- 0L
covs.adj <- features <- NULL
coig.data <- TRUE
constant <- TRUE
verbose <- TRUE


load.name <- paste0("Africa_WaveAll_", method, ".RData")
load(paste0(path.out, load.name))

# We consider four different groups of treated units
treated.units.list <- list("WaveAll" = unique(subset(data, treated == 1 & trDate <= 1994)$countryname),
                           "Wave1" = unique(subset(data, treated == 1 & trDate < 1987)$countryname),
                           "Wave2" = unique(subset(data, treated == 1 & trDate >= 1987 & trDate <= 1991)$countryname),
                           "Wave3" = unique(subset(data, treated == 1 & trDate > 1991 & trDate <= 1994)$countryname))

############################################
## TSUS all treated - leave-one-out donors 

save.name <- paste0("Africa_WaveAll_indiv_", method, robs.save, ".png")
save.name.tex <- paste0("Africa_WaveAll_indiv_", method, robs.save, ".tex")

result <- res

toplot.obj <- outcomeGet(Y.pre.fit=result$est.results$Y.pre.fit,
                         Y.post.fit=result$est.results$Y.post.fit,
                         Y.df=result$data$Y.df,
                         units.est=result$data$specs$units.est,
                         treated.units=result$data$specs$treated.units,
                         plot.type=result$data$specs$effect,
                         anticipation=anticipation,
                         period.post=result$data$specs$period.post,
                         sparse.matrices=result$data$specs$sparse.matrices)

df.loo <- toplot.obj$toplot
df.loo$Excluded <- "None"
treated.reception <- toplot.obj$treated.reception

donors <- unique(unlist(result$data$specs$donors.list))
treated.units <- treated.units.list[["WaveAll"]]

for (j in seq_len(length(donors))) {
  donor.out <- donors[j]
  donors.loo <- list(donors[-j])
  
  df.unit <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "unit-time",
                         treatment.var = "liberalization", time.var = "year", constant = constant, 
                         cointegrated.data = coig.data, post.est = post.est, 
                         units.est = treated.units, features=features, cov.adj = covs.adj,
                         anticipation = anticipation, donors.est = donors.loo)
  
  res.loo <- scest(df.unit, w.constr = list("name" = method))
  
  toplot.obj <- outcomeGet(Y.pre.fit=res.loo$est.results$Y.pre.fit,
                           Y.post.fit=res.loo$est.results$Y.post.fit,
                           Y.df=res.loo$data$Y.df,
                           units.est=res.loo$data$specs$units.est,
                           treated.units=res.loo$data$specs$treated.units,
                           plot.type=res.loo$data$specs$effect,
                           anticipation=anticipation,
                           period.post=res.loo$data$specs$period.post,
                           sparse.matrices=res.loo$data$specs$sparse.matrices)$toplot
  toplot.obj$Excluded <- donor.out
  df.loo <- rbind(df.loo, toplot.obj)  
}

toplot <- merge(df.loo, treated.reception, by = "ID") # compute periods since treated
toplot$Effect <- toplot$Actual - toplot$Synthetic # compute treatment effect
toplot$Highlight <- factor(ifelse(toplot$Excluded == "None", 1, 0),
                           levels = c(0, 1),
                           labels = c("LOO", "Original"))

aux <- toplot %>%
  filter(Highlight == "LOO") %>%
  group_by(ID, Time) %>%               
  summarise(ub = max(Effect), lb = min(Effect))     

toplot.sub <- subset(toplot, Excluded == "None")
toplot.sub <- merge(toplot.sub, aux, by = c("ID", "Time"), all = TRUE)

p <- ggplot(toplot.sub) +
  geom_line(aes(x=Time, y=Effect)) +
  geom_vline(data = treated.reception, aes(xintercept=Tdate)) +
  geom_hline(aes(yintercept= 0), linetype = "dashed") +
  geom_ribbon(aes(x=Time, ymin=lb, ymax=ub), alpha=0.4) +
  facet_wrap(~ID, ncol = 4, scales = "free") + 
  scale_alpha_manual(name = "", values=c(0.2, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(face="bold", size=11),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size = 10),
        axis.ticks.y = element_blank()) + 
  ylab("effect") + xlab("year") + ggtitle("") +
  scale_x_continuous(breaks=breaks_pretty(n=3))

ggsave(filename = paste0(path.fig, save.name), plot = p, width = 12, height = 9, dpi = dpi)

tikz(file = paste0(path.fig, save.name.tex), width = 12, height = 6)
plot(p)
dev.off()


############################################
## TAUS all treated - leave-one-out donors 

save.name <- paste0("Africa_WaveAll_unit_", method, robs.save, ".png")
save.name.tex <- paste0("Africa_WaveAll_unit_", method, robs.save, ".tex")

result <- res.unit

toplot.obj <- outcomeGet(Y.pre.fit=result$est.results$Y.pre.fit,
                         Y.post.fit=result$est.results$Y.post.fit,
                         Y.df=result$data$Y.df,
                         units.est=result$data$specs$units.est,
                         treated.units=result$data$specs$treated.units,
                         plot.type=result$data$specs$effect,
                         anticipation=anticipation,
                         period.post=result$data$specs$period.post,
                         sparse.matrices=result$data$specs$sparse.matrices)

df.loo <- toplot.obj$toplot
df.loo$Excluded <- "None"
treated.reception <- toplot.obj$treated.reception

donors <- unique(unlist(result$data$specs$donors.list))
treated.units <- treated.units.list[["WaveAll"]]

for (j in seq_len(length(donors))) {
  donor.out <- donors[j]
  donors.loo <- list(donors[-j])
  
  df.unit <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "unit",
                         treatment.var = "liberalization", time.var = "year", constant = constant, 
                         cointegrated.data = coig.data, post.est = post.est, 
                         units.est = treated.units, features=features, cov.adj = covs.adj,
                         anticipation = anticipation, donors.est = donors.loo)
  
  res.loo <- scest(df.unit, w.constr = list("name" = method))
  
  toplot.obj <- outcomeGet(Y.pre.fit=res.loo$est.results$Y.pre.fit,
                           Y.post.fit=res.loo$est.results$Y.post.fit,
                           Y.df=res.loo$data$Y.df,
                           units.est=res.loo$data$specs$units.est,
                           treated.units=res.loo$data$specs$treated.units,
                           plot.type=res.loo$data$specs$effect,
                           anticipation=anticipation,
                           period.post=res.loo$data$specs$period.post,
                           sparse.matrices=res.loo$data$specs$sparse.matrices)$toplot
  toplot.obj$Excluded <- donor.out
  df.loo <- rbind(df.loo, toplot.obj)  
}

toplot <- merge(df.loo, treated.reception, by = "ID") # compute periods since treated
toplot$Effect <- toplot$Actual - toplot$Synthetic # compute treatment effect
toplot$Highlight <- factor(ifelse(toplot$Excluded == "None", 1, 0),
                           levels = c(0, 1),
                           labels = c("LOO", "Original"))

aux <- toplot %>%
  filter(Highlight == "LOO") %>%
  group_by(ID, Time) %>%               
  summarise(ub = max(Effect), lb = min(Effect))     

toplot.sub <- subset(toplot, Excluded == "None")
toplot.sub <- merge(toplot.sub, aux, by = c("ID", "Time"), all = TRUE)

p <- ggplot(toplot.sub) +
  geom_line(data=subset(toplot.sub, Time < Tdate), aes(x=Time, y=Effect)) +
  geom_point(data=subset(toplot.sub, Time >= Tdate), aes(x=Time, y=Effect)) +
  geom_vline(data = treated.reception, aes(xintercept=Tdate)) +
  geom_hline(aes(yintercept= 0), linetype = "dashed") +
  geom_ribbon(data=subset(toplot.sub, Time < Tdate), aes(x=Time, ymin=lb, ymax=ub), alpha=0.4) +
  geom_errorbar(data=subset(toplot.sub, Time >= Tdate),
                aes(x=Time, ymin=lb, ymax=ub), width = 0.5, linetype = 'solid') +
  facet_wrap(~ID, ncol = 4, scales = "free") + 
  scale_alpha_manual(name = "", values=c(0.2, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(face="bold", size=11),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size = 10),
        axis.ticks.y = element_blank()) + 
  ylab("effect") + xlab("year") + ggtitle("") +
  scale_x_continuous(breaks=breaks_pretty(n=3))

ggsave(filename = paste0(path.fig, save.name), plot = p, width = 12, height = 9, dpi = dpi)

tikz(file = paste0(path.fig, save.name.tex), width = 12, height = 6)
plot(p)
dev.off()


############################################
## TSUA all treated - leave-one-out donors 

save.name <- paste0("Africa_WaveAll_time_", method, robs.save, ".png")
save.name.tex <- paste0("Africa_WaveAll_time_", method, robs.save, ".tex")

result <- res.time

toplot.obj <- outcomeGet(Y.pre.fit=result$est.results$Y.pre.fit,
                         Y.post.fit=result$est.results$Y.post.fit,
                         Y.df=result$data$Y.df,
                         units.est=result$data$specs$units.est,
                         treated.units=result$data$specs$treated.units,
                         plot.type=result$data$specs$effect,
                         anticipation=anticipation,
                         period.post=result$data$specs$period.post,
                         sparse.matrices=result$data$specs$sparse.matrices)

df.loo <- toplot.obj$toplot
df.loo$Excluded <- "None"
treated.reception <- toplot.obj$treated.reception

donors <- unique(unlist(result$data$specs$donors.list))
treated.units <- treated.units.list[["WaveAll"]]
treated.units <- treated.units[treated.units != "Mauritius"]

for (j in seq_len(length(donors))) {
  donor.out <- donors[j]
  donors.loo <- list(donors[-j])
  
  df.unit <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "time",
                         treatment.var = "liberalization", time.var = "year", constant = constant, 
                         cointegrated.data = coig.data, post.est = post.est, 
                         units.est = treated.units, features=features, cov.adj = covs.adj,
                         anticipation = anticipation, donors.est = donors.loo)
  
  res.loo <- scest(df.unit, w.constr = list("name" = method))
  
  toplot.obj <- outcomeGet(Y.pre.fit=res.loo$est.results$Y.pre.fit,
                           Y.post.fit=res.loo$est.results$Y.post.fit,
                           Y.df=res.loo$data$Y.df,
                           units.est=res.loo$data$specs$units.est,
                           treated.units=res.loo$data$specs$treated.units,
                           plot.type=res.loo$data$specs$effect,
                           anticipation=anticipation,
                           period.post=res.loo$data$specs$period.post,
                           sparse.matrices=res.loo$data$specs$sparse.matrices)$toplot
  toplot.obj$Excluded <- donor.out
  df.loo <- rbind(df.loo, toplot.obj)  
}

toplot <- merge(df.loo, treated.reception, by = "ID") # compute periods since treated
toplot$Effect <- toplot$Actual - toplot$Synthetic # compute treatment effect
toplot$Highlight <- factor(ifelse(toplot$Excluded == "None", 1, 0),
                           levels = c(0, 1),
                           labels = c("LOO", "Original"))

aux <- toplot %>%
  filter(Highlight == "LOO") %>%
  group_by(ID, Time) %>%               
  summarise(ub = max(Effect), lb = min(Effect))     

toplot.sub <- subset(toplot, Excluded == "None")
toplot.sub <- merge(toplot.sub, aux, by = c("ID", "Time"), all = TRUE)

p <- ggplot(toplot.sub) +
  geom_line(aes(x=Time, y=Effect), linewidth = 1.2) +
  geom_vline(data = treated.reception, aes(xintercept=Tdate)) +
  geom_hline(aes(yintercept= 0), linetype = "dashed") +
  geom_ribbon(aes(x=Time, ymin=lb, ymax=ub), alpha=0.4) +
  scale_alpha_manual(name = "", values=c(0.2, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(face="bold", size=11),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size = 10),
        axis.ticks.y = element_blank()) + 
  ylab("effect") + xlab("year") + ggtitle("") +
  scale_x_continuous(breaks=breaks_pretty(n=3))

ggsave(filename = paste0(path.fig, save.name), plot = p, width = 12, height = 9, dpi = dpi)

tikz(file = paste0(path.fig, save.name.tex), width = 12, height = 6)
plot(p)
dev.off()

#############################################
## TSUA treated < 1987 - leave-one-out donors 

load.name <- paste0("Africa_Wave1_", method, ".RData")
load(paste0(path.out, load.name))

save.name <- paste0("Africa_Wave1_time_", method, robs.save, ".png")
save.name.tex <- paste0("Africa_Wave1_time_", method, robs.save, ".tex")

result <- res.time

toplot.obj <- outcomeGet(Y.pre.fit=result$est.results$Y.pre.fit,
                         Y.post.fit=result$est.results$Y.post.fit,
                         Y.df=result$data$Y.df,
                         units.est=result$data$specs$units.est,
                         treated.units=result$data$specs$treated.units,
                         plot.type=result$data$specs$effect,
                         anticipation=anticipation,
                         period.post=result$data$specs$period.post,
                         sparse.matrices=result$data$specs$sparse.matrices)

df.loo <- toplot.obj$toplot
df.loo$Excluded <- "None"
treated.reception <- toplot.obj$treated.reception

donors <- unique(unlist(result$data$specs$donors.list))
treated.units <- treated.units.list[["Wave1"]]
treated.units <- treated.units[treated.units != "Mauritius"]

for (j in seq_len(length(donors))) {
  donor.out <- donors[j]
  donors.loo <- list(donors[-j])
  
  df.unit <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "time",
                         treatment.var = "liberalization", time.var = "year", constant = constant, 
                         cointegrated.data = coig.data, post.est = post.est, 
                         units.est = treated.units, features=features, cov.adj = covs.adj,
                         anticipation = anticipation, donors.est = donors.loo)
  
  res.loo <- scest(df.unit, w.constr = list("name" = method))
  
  toplot.obj <- outcomeGet(Y.pre.fit=res.loo$est.results$Y.pre.fit,
                           Y.post.fit=res.loo$est.results$Y.post.fit,
                           Y.df=res.loo$data$Y.df,
                           units.est=res.loo$data$specs$units.est,
                           treated.units=res.loo$data$specs$treated.units,
                           plot.type=res.loo$data$specs$effect,
                           anticipation=anticipation,
                           period.post=res.loo$data$specs$period.post,
                           sparse.matrices=res.loo$data$specs$sparse.matrices)$toplot
  toplot.obj$Excluded <- donor.out
  df.loo <- rbind(df.loo, toplot.obj)  
}

toplot <- merge(df.loo, treated.reception, by = "ID") # compute periods since treated
toplot$Effect <- toplot$Actual - toplot$Synthetic # compute treatment effect
toplot$Highlight <- factor(ifelse(toplot$Excluded == "None", 1, 0),
                           levels = c(0, 1),
                           labels = c("LOO", "Original"))

aux <- toplot %>%
  filter(Highlight == "LOO") %>%
  group_by(ID, Time) %>%               
  summarise(ub = max(Effect, na.rm=TRUE), lb = min(Effect, na.rm=TRUE))     

toplot.sub <- subset(toplot, Excluded == "None")
toplot.sub <- merge(toplot.sub, aux, by = c("ID", "Time"), all = TRUE)

p <- ggplot(toplot.sub) +
  geom_line(aes(x=Time, y=Effect), linewidth = 1.2) +
  geom_vline(data = treated.reception, aes(xintercept=Tdate)) +
  geom_hline(aes(yintercept= 0), linetype = "dashed") +
  geom_ribbon(aes(x=Time, ymin=lb, ymax=ub), alpha=0.4) +
  scale_alpha_manual(name = "", values=c(0.2, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(face="bold", size=11),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size = 10),
        axis.ticks.y = element_blank()) + 
  ylab("effect") + xlab("year") + ggtitle("") +
  scale_x_continuous(breaks=breaks_pretty(n=3))

ggsave(filename = paste0(path.fig, save.name), plot = p, width = 12, height = 9, dpi = dpi)

tikz(file = paste0(path.fig, save.name.tex), width = 12, height = 6)
plot(p)
dev.off()



################################################
## TSUA treated 1987/1991 - leave-one-out donors 

load.name <- paste0("Africa_Wave2_", method, ".RData")
load(paste0(path.out, load.name))

save.name <- paste0("Africa_Wave2_time_", method, robs.save, ".png")
save.name.tex <- paste0("Africa_Wave2_time_", method, robs.save, ".tex")

result <- res.time

toplot.obj <- outcomeGet(Y.pre.fit=result$est.results$Y.pre.fit,
                         Y.post.fit=result$est.results$Y.post.fit,
                         Y.df=result$data$Y.df,
                         units.est=result$data$specs$units.est,
                         treated.units=result$data$specs$treated.units,
                         plot.type=result$data$specs$effect,
                         anticipation=anticipation,
                         period.post=result$data$specs$period.post,
                         sparse.matrices=result$data$specs$sparse.matrices)

df.loo <- toplot.obj$toplot
df.loo$Excluded <- "None"
treated.reception <- toplot.obj$treated.reception

donors <- unique(unlist(result$data$specs$donors.list))
treated.units <- treated.units.list[["Wave2"]]
treated.units <- treated.units[treated.units != "Mauritius"]

for (j in seq_len(length(donors))) {
  donor.out <- donors[j]
  donors.loo <- list(donors[-j])
  
  df.unit <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "time",
                         treatment.var = "liberalization", time.var = "year", constant = constant, 
                         cointegrated.data = coig.data, post.est = post.est, 
                         units.est = treated.units, features=features, cov.adj = covs.adj,
                         anticipation = anticipation, donors.est = donors.loo)
  
  res.loo <- scest(df.unit, w.constr = list("name" = method))
  
  toplot.obj <- outcomeGet(Y.pre.fit=res.loo$est.results$Y.pre.fit,
                           Y.post.fit=res.loo$est.results$Y.post.fit,
                           Y.df=res.loo$data$Y.df,
                           units.est=res.loo$data$specs$units.est,
                           treated.units=res.loo$data$specs$treated.units,
                           plot.type=res.loo$data$specs$effect,
                           anticipation=anticipation,
                           period.post=res.loo$data$specs$period.post,
                           sparse.matrices=res.loo$data$specs$sparse.matrices)$toplot
  toplot.obj$Excluded <- donor.out
  df.loo <- rbind(df.loo, toplot.obj)  
}

toplot <- merge(df.loo, treated.reception, by = "ID") # compute periods since treated
toplot$Effect <- toplot$Actual - toplot$Synthetic # compute treatment effect
toplot$Highlight <- factor(ifelse(toplot$Excluded == "None", 1, 0),
                           levels = c(0, 1),
                           labels = c("LOO", "Original"))

aux <- toplot %>%
  filter(Highlight == "LOO") %>%
  group_by(ID, Time) %>%               
  summarise(ub = max(Effect, na.rm=TRUE), lb = min(Effect, na.rm=TRUE))     

toplot.sub <- subset(toplot, Excluded == "None")
toplot.sub <- merge(toplot.sub, aux, by = c("ID", "Time"), all = TRUE)

p <- ggplot(toplot.sub) +
  geom_line(aes(x=Time, y=Effect), linewidth = 1.2) +
  geom_vline(data = treated.reception, aes(xintercept=Tdate)) +
  geom_hline(aes(yintercept= 0), linetype = "dashed") +
  geom_ribbon(aes(x=Time, ymin=lb, ymax=ub), alpha=0.4) +
  scale_alpha_manual(name = "", values=c(0.2, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(face="bold", size=11),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size = 10),
        axis.ticks.y = element_blank()) + 
  ylab("effect") + xlab("year") + ggtitle("") +
  scale_x_continuous(breaks=breaks_pretty(n=3))

ggsave(filename = paste0(path.fig, save.name), plot = p, width = 12, height = 9, dpi = dpi)

tikz(file = paste0(path.fig, save.name.tex), width = 12, height = 6)
plot(p)
dev.off()

#############################################
## TSUA treated >=1992 - leave-one-out donors 

load.name <- paste0("Africa_Wave3_", method, ".RData")
load(paste0(path.out, load.name))

save.name <- paste0("Africa_Wave3_time_", method, robs.save, ".png")
save.name.tex <- paste0("Africa_Wave3_time_", method, robs.save, ".tex")

result <- res.time

toplot.obj <- outcomeGet(Y.pre.fit=result$est.results$Y.pre.fit,
                         Y.post.fit=result$est.results$Y.post.fit,
                         Y.df=result$data$Y.df,
                         units.est=result$data$specs$units.est,
                         treated.units=result$data$specs$treated.units,
                         plot.type=result$data$specs$effect,
                         anticipation=anticipation,
                         period.post=result$data$specs$period.post,
                         sparse.matrices=result$data$specs$sparse.matrices)

df.loo <- toplot.obj$toplot
df.loo$Excluded <- "None"
treated.reception <- toplot.obj$treated.reception

donors <- unique(unlist(result$data$specs$donors.list))
treated.units <- treated.units.list[["Wave3"]]
treated.units <- treated.units[treated.units != "Mauritius"]

for (j in seq_len(length(donors))) {
  donor.out <- donors[j]
  donors.loo <- list(donors[-j])
  
  df.unit <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "time",
                         treatment.var = "liberalization", time.var = "year", constant = constant, 
                         cointegrated.data = coig.data, post.est = post.est, 
                         units.est = treated.units, features=features, cov.adj = covs.adj,
                         anticipation = anticipation, donors.est = donors.loo)
  
  res.loo <- scest(df.unit, w.constr = list("name" = method))
  
  toplot.obj <- outcomeGet(Y.pre.fit=res.loo$est.results$Y.pre.fit,
                           Y.post.fit=res.loo$est.results$Y.post.fit,
                           Y.df=res.loo$data$Y.df,
                           units.est=res.loo$data$specs$units.est,
                           treated.units=res.loo$data$specs$treated.units,
                           plot.type=res.loo$data$specs$effect,
                           anticipation=anticipation,
                           period.post=res.loo$data$specs$period.post,
                           sparse.matrices=res.loo$data$specs$sparse.matrices)$toplot
  toplot.obj$Excluded <- donor.out
  df.loo <- rbind(df.loo, toplot.obj)  
}

toplot <- merge(df.loo, treated.reception, by = "ID") # compute periods since treated
toplot$Effect <- toplot$Actual - toplot$Synthetic # compute treatment effect
toplot$Highlight <- factor(ifelse(toplot$Excluded == "None", 1, 0),
                           levels = c(0, 1),
                           labels = c("LOO", "Original"))

aux <- toplot %>%
  filter(Highlight == "LOO") %>%
  group_by(ID, Time) %>%               
  summarise(ub = max(Effect, na.rm=TRUE), lb = min(Effect, na.rm=TRUE))     

toplot.sub <- subset(toplot, Excluded == "None")
toplot.sub <- merge(toplot.sub, aux, by = c("ID", "Time"), all = TRUE)

p <- ggplot(toplot.sub) +
  geom_line(aes(x=Time, y=Effect), linewidth = 1.2) +
  geom_vline(data = treated.reception, aes(xintercept=Tdate)) +
  geom_hline(aes(yintercept= 0), linetype = "dashed") +
  geom_ribbon(aes(x=Time, ymin=lb, ymax=ub), alpha=0.4) +
  scale_alpha_manual(name = "", values=c(0.2, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(face="bold", size=11),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size = 10),
        axis.ticks.y = element_blank()) + 
  ylab("effect") + xlab("year") + ggtitle("") +
  scale_x_continuous(breaks=breaks_pretty(n=3))

ggsave(filename = paste0(path.fig, save.name), plot = p, width = 12, height = 9, dpi = dpi)

tikz(file = paste0(path.fig, save.name.tex), width = 12, height = 6)
plot(p)
dev.off()

source(paste0(path.code, "0001_graphsCreate.R"))