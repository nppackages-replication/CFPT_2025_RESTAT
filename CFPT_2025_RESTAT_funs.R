predictandsGet <- function(data, treated.units, w.constr, save.path, save.name, features=NULL, 
                           covs.adj=NULL, anticipation=0, post.est=5L, sims=10L, constant=TRUE, 
                           coig.data=TRUE, cores=1L, verbose=TRUE, eff.it=TRUE, eff.t=TRUE,
                           eff.i=TRUE, joint.optim=FALSE) {
  
  # unit-time predictand
  if (isTRUE(eff.it)) {
    df <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", 
                      treatment.var = "liberalization", time.var = "year", constant = constant, 
                      cointegrated.data = coig.data, post.est = post.est, 
                      units.est = treated.units,
                      features=features, cov.adj = covs.adj, anticipation = anticipation)

    res <- scpi(df, sims = sims, cores = cores, w.constr = w.constr, 
                u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose)

    if (isTRUE(joint.optim)) {
      res.old <- scpi(df, sims = sims, cores = cores, w.constr = w.constr, 
                      u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose,
                      force.joint.PI.optim = TRUE)
    } else {
      res.old <- NULL
    }
    
  } else {
    res <- res.old <- NULL
  }
  
  
  # unit predictand
  if (isTRUE(eff.i)) {
    df.unit <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "unit",
                         treatment.var = "liberalization", time.var = "year", constant = constant, 
                         cointegrated.data = coig.data, post.est = post.est, 
                         units.est = treated.units,
                         features=features, cov.adj = covs.adj, anticipation = anticipation)
  
    res.unit <- scpi(df.unit, sims = sims, cores = cores, w.constr = w.constr,
                     u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose)

    if (isTRUE(joint.optim)) {
      res.unit.old <- scpi(df.unit, sims = sims, cores = cores, w.constr = w.constr,
                           u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose,
                           force.joint.PI.optim = TRUE)
    } else {
      res.unit.old <- NULL
    }

  } else {
    res.unit <- res.unit.old <- NULL
  }
  
  # time predictand
  if (isTRUE(eff.t)) {
    treated.units.time <- treated.units[treated.units != "Mauritius"]
    df.time<- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "time",
                          treatment.var = "liberalization", time.var = "year", constant = constant, 
                          cointegrated.data = coig.data, post.est = post.est, 
                          units.est = treated.units.time,
                          features=features, cov.adj = covs.adj, anticipation = anticipation)
    
    res.time <- scpi(df.time, sims = sims, cores = cores, w.constr = w.constr,
                     u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose)  

    if (isTRUE(joint.optim)) {
      res.time.old <- scpi(df.time, sims = sims, cores = cores, w.constr = w.constr,
                           u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose,
                           force.joint.PI.optim = TRUE)  
    } else {
      res.time.old <- NULL
    }
    
    } else {
    res.time <- res.time.old <- NULL
  }
  
  save(res, res.unit, res.time, res.old, res.unit.old, res.time.old,
       file=paste0(save.path,"/",save.name,".RData"))
  
}


predictandsGetCovs <- function(data, treated.units, w.constr, save.path, save.name, features, 
                               covs.adj, anticipation=0, post.est=5L, sims=10L, constant=TRUE,
                               coig.data=TRUE, cores=1L, verbose=TRUE, eff.it=TRUE, eff.t=TRUE,
                               eff.i=TRUE, joint.optim=FALSE) {
  
  # unit-time predictand
  if (isTRUE(eff.it)) {
    df <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", 
                      treatment.var = "liberalization", time.var = "year", constant = constant, 
                      cointegrated.data = coig.data, post.est = post.est, 
                      units.est = treated.units,
                      features=features, cov.adj = covs.adj, anticipation = anticipation)
    
    col.trunit <- unlist(lapply(strsplit(colnames(df$B), "\\."), "[[", 1))
    row.trunit <- unlist(lapply(strsplit(rownames(df$B), "\\."), "[[", 1))
    sd.vec <- unlist(lapply(treated.units, function(tr) apply(df$B[row.trunit == tr, col.trunit == tr],
                                                              1, function(row) sd(row, na.rm=T))))
    V <- diag(1/sd.vec)
    
    res <- scpi(df, V.mat=V, sims = sims, cores = cores, w.constr = w.constr, 
                u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose)

    if (isTRUE(joint.optim)) {
      res.old <- scpi(df, V.mat=V, sims = sims, cores = cores, w.constr = w.constr, 
                      u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose,
                      force.joint.PI.optim = TRUE)
    } else {
      res.old <- NULL
    }
    
  } else {
    res <- res.old <- NULL
  }
    
  # unit predictand
  if (isTRUE(eff.i)) {
    df.unit <- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "unit",
                           treatment.var = "liberalization", time.var = "year", constant = constant, 
                           cointegrated.data = coig.data, post.est = post.est, 
                           units.est = treated.units,
                           features=features, cov.adj = covs.adj, anticipation = anticipation)
    
    col.trunit <- unlist(lapply(strsplit(colnames(df.unit$B), "\\."), "[[", 1))
    row.trunit <- unlist(lapply(strsplit(rownames(df.unit$B), "\\."), "[[", 1))
    sd.vec <- unlist(lapply(treated.units, function(tr) apply(df.unit$B[row.trunit == tr, col.trunit == tr],
                                                              1, function(row) sd(row, na.rm=T))))
    V <- diag(1/sd.vec)
    
    res.unit <- scpi(df.unit, V.mat=V, sims = sims, cores = cores, w.constr = w.constr, 
                     u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose)

    if (isTRUE(joint.optim)) {
      res.unit.old <- scpi(df.unit, V.mat=V, sims = sims, cores = cores, w.constr = w.constr, 
                           u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose,
                           force.joint.PI.optim = TRUE)
    } else {
      res.unit.old <- NULL
    }
    
  } else {
    res.unit <- res.unit.old <- NULL
  }
  
  # time predictand
  if (isTRUE(eff.t)) {
    treated.units.time <- treated.units[treated.units != "Mauritius"]
    df.time<- scdataMulti(data, id.var = "countryname", outcome.var = "lgdp", effect = "time",
                          treatment.var = "liberalization", time.var = "year", constant = constant, 
                          cointegrated.data = coig.data, post.est = post.est, 
                          units.est = treated.units.time,
                          features=features, cov.adj = covs.adj, anticipation = anticipation)
    
    col.trunit <- unlist(lapply(strsplit(colnames(df.time$B), "\\."), "[[", 1))
    row.trunit <- unlist(lapply(strsplit(rownames(df.time$B), "\\."), "[[", 1))
    sd.vec <- unlist(lapply(treated.units, function(tr) apply(df.time$B[row.trunit == tr, col.trunit == tr],
                                                              1, function(row) sd(row, na.rm=T))))
    V <- diag(1/sd.vec)
    
    res.time <- scpi(df.time, V.mat=V, sims = sims, cores = cores, w.constr = w.constr,
                            u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose)  

    if (isTRUE(joint.optim)) {
      res.time.old <- scpi(df.time, V.mat=V, sims = sims, cores = cores, w.constr = w.constr,
                           u.order = 1, u.lags = 1, e.method = "gaussian", verbose = verbose,
                           force.joint.PI.optim = TRUE)  
    } else {
      res.time.old <- NULL
    }
    
  } else {
    res.time <- res.time.old <- NULL
  }
  
  save(res, res.unit, res.time, res.old, res.unit.old, res.time.old,
       file=paste0(save.path,"/",save.name,".RData"))
  
}



outcomeGet <- function(Y.pre.fit, Y.post.fit, Y.df, units.est, treated.units,
                       plot.type, anticipation, period.post, sparse.matrices) {
  
  # shortcut to avoid "no visible binding for global variable 'X' when checking the package
  Treatment <- ID <- Time <- Tdate <- Type <- tstd <- NULL
  
  if (sparse.matrices == TRUE) {
    Y.pre.fit <- as.matrix(Y.pre.fit)
    Y.post.fit <- as.matrix(Y.post.fit)
  }
  
  # avoids problems when units have numeric ID
  if (plot.type == "time") {
    rownames(Y.post.fit) <- paste0("agg.", rownames(Y.post.fit))
  }
  
  synth.mat <- rbind(Y.pre.fit, Y.post.fit)
  names(Y.df) <- c(names(Y.df)[1:3], "Actual")
  res.df <- subset(Y.df, ID %in% units.est)
  
  if (plot.type == "unit") {
    Y.actual.pre <- subset(res.df, Treatment == 0)
    Y.actual.post <- subset(res.df, Treatment == 1)
    Y.actual.post.agg <- aggregate(Actual ~ ID, data = Y.actual.post, mean)
    Y.actual.post.agg$Treatment <- 1
    names <- strsplit(rownames(Y.post.fit), "\\.")
    Y.actual.post.agg$Time <- as.numeric(unlist(lapply(names, "[[", 2)))
    Y.actual <- rbind(Y.actual.pre, Y.actual.post.agg)
    
  } else {
    Y.actual <- res.df # dataframe
  }
  
  treated.periods <- subset(Y.actual, Treatment == 1, select = c(Time, ID)) # post treatment period for each treated unit
  treated.reception <- aggregate(Time ~ ID, data = treated.periods, min)
  names(treated.reception) <- c("ID", "Tdate")
  treated.reception$Tdate <- as.numeric(treated.reception$Tdate) - anticipation - 1 / 2
  treated.reception <- subset(treated.reception, ID %in% units.est)
  
  if (plot.type == "time") {
    
    if (methods::is(res.df[["Time"]], "Date")) { # flag that will be used later to handle synth labels
      timeVarIsDate <- TRUE
    } else {
      timeVarIsDate <- FALSE
    }
    
    res.df["Time"] <- as.numeric(res.df[["Time"]]) # handles as.Date() format which is now useless due to std time
    res.df <- merge(res.df, treated.reception, by = "ID")
    Y.actual.pre <- subset(res.df, Time < Tdate)
    Y.actual.post <- subset(res.df, Time > Tdate)
    Y.actual.pre$Tdate <- Y.actual.pre$Tdate + 1/2
    Y.actual.post$Tdate <- Y.actual.post$Tdate + 1/2
    Y.actual.pre$tstd <- Y.actual.pre$Time - Y.actual.pre$Tdate
    Y.actual.post$tstd <- Y.actual.post$Time - Y.actual.post$Tdate
    
    names <- strsplit(rownames(synth.mat), "\\.")
    unit <- unlist(lapply(names, "[[", 1))
    no.agg <- unit %in% treated.units
    
    if (timeVarIsDate == TRUE) { # first convert from string to date and then numeric
      time <- as.numeric(as.Date(unlist(lapply(names[1:sum(no.agg)], "[[", 2))))
    } else {
      time <- unlist(lapply(names[1:sum(no.agg)], "[[", 2))
    }
    synth.pre <- data.frame(ID = unit[no.agg == TRUE],
                            Synthetic = synth.mat[no.agg == TRUE, 1],
                            Time = time)
    
    Y.pre <- merge(Y.actual.pre, synth.pre, by=c("ID", "Time"))
    max.pre <- max(aggregate(tstd ~ ID, data = Y.pre, min)$tstd)
    min.post <- min(unlist(lapply(period.post, length))) - 1
    
    Y.pre.agg <- aggregate(x = Y.pre[c("Actual", "Synthetic")],
                           by = Y.pre[c("tstd")],
                           FUN = mean, na.rm = TRUE)
    names(Y.pre.agg) <- c("Time", "Actual", "Synthetic")
    Y.pre.agg <- subset(Y.pre.agg, Time >= max.pre)
    
    Y.post.agg <- aggregate(x = Y.actual.post[c("Actual")],
                            by = Y.actual.post[c("tstd")],
                            FUN = mean, na.rm = TRUE)
    
    Y.post.agg <- subset(Y.post.agg, tstd <= min.post)
    
    Y.post.agg <- data.frame(ID = unit[no.agg == FALSE],
                             Actual = Y.post.agg$Actual,
                             Synthetic = synth.mat[no.agg == FALSE, 1],
                             Time = c(0:(sum(no.agg==FALSE)-1))) 
    
    Y.pre.agg$Treatment <- 0
    Y.post.agg$Treatment <- 1
    
    Y.pre.agg$ID <- "aggregate"
    Y.post.agg$ID <- "aggregate"
    
    Y.actual <- rbind(Y.pre.agg, Y.post.agg)
    Y.actual$Tdate <- 0
    
    plot.type <- "unit-time"
    I <- 1
    treated.reception <- data.frame(ID="aggregate", Tdate = 1/2)
    toplot <- Y.actual
    toplot$Time <- toplot$Time + 1
    
  } else {
    # Merge synthetic
    names <- strsplit(rownames(synth.mat), "\\.")
    unit <- unlist(lapply(names, "[[", 1))
    period <- unlist(lapply(names, "[[", 2))
    
    synth <- data.frame(ID = unit, Time = period, Synthetic = synth.mat)
    toplot <- merge(Y.actual, synth, by = c("ID", "Time"), all = FALSE) # keep only treated units
  }
  
  return(list(toplot=toplot, treated.reception=treated.reception, plot.type=plot.type)) 
}


