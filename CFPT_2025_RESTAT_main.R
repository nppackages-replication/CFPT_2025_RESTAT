###############################################################################
## Replication File for Cattaneo, Feng, Palomba, and Titiunik (2025)
##  
## This code replicates the main analysis of the paper and the robustness
## exercises in the supplemental appendix.
###############################################################################

rm(list=ls(all=TRUE))

##########################################
# Load stuff 
pacman::p_load(scpi, haven, wesanderson, ggplot2, reshape2, devtools, tidyverse)
devtools::install_github("apoorvalal/LalRUtils")

##########################################
# Set paths
path <- "YOUR_PATH"
cores <- 1

path.data <- paste0(path, "data/")
path.fig  <- paste0(path, "fig/")
path.out  <- paste0(path, "out/")
path.code <- paste0(path, "code/")
source(paste0(path.code, "0001_funs.R"))

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
                   constant=TRUE, cores=cores, 
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
                       constant=TRUE, cores=cores, 
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
                 eff.it=eff.it, eff.i=eff.i, eff.t=eff.t, placebo.time=TRUE)
}


