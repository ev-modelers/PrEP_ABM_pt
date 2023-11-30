hivtrans_msm_new <- function(dat, at) {
  
  # Variables -----------------------------------------------------------
  
  # Attributes
  vl <- dat$attr$vl
  stage <- dat$attr$stage
  circ <- dat$attr$circ
  status <- dat$attr$status
  prepStat <- dat$attr$prepStat
  prepClass <- dat$attr$prepClass
  rGC <- dat$attr$rGC
  uGC <- dat$attr$uGC
  rCT <- dat$attr$rCT
  uCT <- dat$attr$uCT
  race <- dat$attr$race
  tx.status <- dat$attr$tx.status
  
  # Parameters
  URAI.prob <- dat$param$URAI.prob
  UIAI.prob <- dat$param$UIAI.prob
  trans.scale <- dat$param$trans.scale
  acute.rr <- dat$param$acute.rr
  
  cond.eff <- dat$param$cond.eff
  cond.fail <- dat$param$cond.fail
  
  circ.rr <- dat$param$circ.rr
  prep.hr <- dat$param$prep.adhr.hr
  hiv.ugc.rr <- dat$param$hiv.ugc.rr
  hiv.uct.rr <- dat$param$hiv.uct.rr
  hiv.rgc.rr <- dat$param$hiv.rgc.rr
  hiv.rct.rr <- dat$param$hiv.rct.rr
  hiv.dual.rr <- dat$param$hiv.dual.rr
  
  
  # Data
  al <- dat$temp$al
  dal <- al[which(status[al[, 1]] == 1 & status[al[, 2]] == 0), ]
  if (is.na(nrow(dal))|nrow(dal) <3) {
    return(dat)
  }
  
  dal<-data.frame(dal)
  
  dal <- dal[sample(1:nrow(dal)), ]
  ncols <- dim(dal)[2]
  


  ## Reorder by role: ins on the left, rec on the right
  disc.ip <- dal[dal[, "ins"] == 1, ]
  disc.rp <- dal[dal[, "ins"] == 0, c(2:1, 3:ncols)]
  
 
  
  #print(colnames(disc.ip))
  #print(colnames(disc.rp))
  
  colnames(disc.ip)[1:2] <- c("ins", "rec")
  colnames(disc.rp)[1:2] <- c("ins", "rec")
  
  
  # PATP: Insertive Man Infected (Col 1) --------------------------------
  
  # Attributes of infected
  ip.vl <- vl[disc.ip[, 1]]
  ip.stage <- stage[disc.ip[, 1]]
  ip.txStat <- tx.status[disc.ip[, 1]]
  
  # Attributes of susceptible
  ip.prep <- prepStat[disc.ip[, 2]]
  ip.prepcl <- prepClass[disc.ip[, 2]]
  ip.rGC <- rGC[disc.ip[, 2]]
  ip.rCT <- rCT[disc.ip[, 2]]
  
  # Base TP from VL
  ip.tprob <- pmin(0.99, URAI.prob * 2.45^(ip.vl - 4.5))
  
  # Adjustment (based on Supervie JAIDS) for VL Suppressed, on ART
  ip.noTrans <- which(ip.vl <= log10(200) & ip.txStat == 1)
  ip.tprob[ip.noTrans] <- 2.2/1e5
  
  # Transform to log odds
  ip.tlo <- log(ip.tprob/(1 - ip.tprob))
  
  # Condom use
  not.UAI <- which(disc.ip[, "uai"] == 0)
  condom.rr <- rep(NA, nrow(disc.ip))
  races <- sort(unique(race[disc.ip[, 1]]))
  for (i in races) {
    not.UAI.race <- intersect(not.UAI, which(race[disc.ip[, 1]] == i))
    condom.rr[not.UAI.race] <- 1 - (cond.eff - cond.fail[i])
  }
  ip.tlo[not.UAI] <- ip.tlo[not.UAI] + log(condom.rr[not.UAI])
  
  # PrEP, by adherence class
  ip.on.prep <- which(ip.prep == 1)
  ip.tlo[ip.on.prep] <- ip.tlo[ip.on.prep] + log(prep.hr[ip.prepcl[ip.on.prep]])
  
  # Acute-stage multipliers
  isAcute <- which(ip.stage %in% 1:2)
  ip.tlo[isAcute] <- ip.tlo[isAcute] + log(acute.rr)
  
  ## Multiplier for STI
  is.rGC <- which(ip.rGC == 1)
  is.rCT <- which(ip.rCT == 1)
  is.rect.dual <- intersect(is.rGC, is.rCT)
  is.rGC.sing <- setdiff(is.rGC, is.rect.dual)
  is.rCT.sing <- setdiff(is.rCT, is.rect.dual)
  ip.tlo[is.rGC.sing] <- ip.tlo[is.rGC.sing] + log(hiv.rgc.rr)
  ip.tlo[is.rCT.sing] <- ip.tlo[is.rCT.sing] + log(hiv.rct.rr)
  ip.tlo[is.rect.dual] <- ip.tlo[is.rect.dual] +
    max(log(hiv.rgc.rr), log(hiv.rct.rr)) +
    min(log(hiv.rgc.rr), log(hiv.rct.rr)) * hiv.dual.rr
  
  # Race-specific scalar for calibration
  races <- race[disc.ip[, 2]]
  ip.tlo <- ip.tlo + log(trans.scale[races])
  
  # Convert back to probability
  ip.tprob <- plogis(ip.tlo)
  stopifnot(ip.tprob >= 0, ip.tprob <= 1)
  
  
  # PATP: Receptive Man Infected (Col 2) --------------------------------
  
  # Attributes of infected
  rp.vl <- vl[disc.rp[, 2]]
  rp.stage <- stage[disc.rp[, 2]]
  rp.txStat <- tx.status[disc.rp[, 2]]
  
  # Attributes of susceptible
  rp.circ <- circ[disc.rp[, 1]]
  rp.prep <- prepStat[disc.rp[, 1]]
  rp.prepcl <- prepClass[disc.rp[, 1]]
  rp.uGC <- uGC[disc.rp[, 1]]
  rp.uCT <- uCT[disc.rp[, 1]]
  
  # Base TP from VL
  rp.tprob <- pmin(0.99, UIAI.prob * 2.45^(rp.vl - 4.5))
  
  # Adjustment (based on Supervie JAIDS) for VL Sueeppressed, on ART
  rp.noTrans <- which(rp.vl <= log10(200) & rp.txStat == 1)
  rp.tprob[rp.noTrans] <- 2.2/1e5
  
  # Transform to log odds
  rp.tlo <- log(rp.tprob/(1 - rp.tprob))
  
  # Circumcision
  #rp.tlo[rp.circ == 1] <- rp.tlo[rp.circ == 1] + log(circ.rr)
  
  # Condom use
  not.UAI <- which(disc.rp[, "uai"] == 0)
  condom.rr <- rep(NA, nrow(disc.rp))
  races <- sort(unique(race[disc.rp[, 1]]))
  for (i in races) {
    not.UAI.race <- intersect(not.UAI, which(race[disc.rp[, 1]] == i))
    condom.rr[not.UAI.race] <- 1 - (cond.eff - cond.fail[i])
  }
  rp.tlo[not.UAI] <- rp.tlo[not.UAI] + log(condom.rr[not.UAI])
  
  # PrEP, by adherence class
  rp.on.prep <- which(rp.prep == 1)
  rp.tlo[rp.on.prep] <- rp.tlo[rp.on.prep] + log(prep.hr[rp.prepcl[rp.on.prep]])
  
  # Acute-stage multipliers
  isAcute <- which(rp.stage %in% 1:2)
  rp.tlo[isAcute] <- rp.tlo[isAcute] + log(acute.rr)
  
  ## Multiplier for STI
  is.uGC <- which(rp.uGC == 1)
  is.uCT <- which(rp.uCT == 1)
  is.ureth.dual <- intersect(is.uGC, is.uCT)
  is.uGC.sing <- setdiff(is.uGC, is.ureth.dual)
  is.uCT.sing <- setdiff(is.uCT, is.ureth.dual)
  rp.tlo[is.uGC.sing] <- rp.tlo[is.uGC.sing] + log(hiv.ugc.rr)
  rp.tlo[is.uCT.sing] <- rp.tlo[is.uCT.sing] + log(hiv.uct.rr)
  rp.tlo[is.ureth.dual] <- rp.tlo[is.ureth.dual] +
    max(log(hiv.ugc.rr), log(hiv.uct.rr)) +
    min(log(hiv.ugc.rr), log(hiv.uct.rr)) * hiv.dual.rr
  
  # Race-specific scalar for calibration
  races <- race[disc.rp[, 1]]
  rp.tlo <- rp.tlo + log(trans.scale[races])
  
  # Convert back to probability
  rp.tprob <- plogis(rp.tlo)
  stopifnot(rp.tprob >= 0, rp.tprob <= 1)
  
  
  # Transmission --------------------------------------------------------
  
  trans.ip <- rbinom(length(ip.tprob), 1, ip.tprob)
  trans.rp <- rbinom(length(rp.tprob), 1, rp.tprob)
  
  
  # Output --------------------------------------------------------------
  
  infected <- NULL
  if (sum(trans.ip, trans.rp) > 0) {
    infected <- c(disc.ip[trans.ip == 1, 2],
                  disc.rp[trans.rp == 1, 1])
    
    # Attributes of newly infected
    dat$attr$status[infected] <- 1
    dat$attr$inf.time[infected] <- at
    dat$attr$vl[infected] <- 0
    dat$attr$stage[infected] <- 1
    dat$attr$stage.time[infected] <- 0
    dat$attr$diag.status[infected] <- 0
    dat$attr$tx.status[infected] <- 0
    dat$attr$cuml.time.on.tx[infected] <- 0
    dat$attr$cuml.time.off.tx[infected] <- 0
    
    # Attributes of transmitter
    transmitter <- as.numeric(c(disc.ip[trans.ip == 1, 1],
                                disc.rp[trans.rp == 1, 2]))
    tab.trans <- table(transmitter)
    uni.trans <- as.numeric(names(tab.trans))
    dat$attr$count.trans[uni.trans] <- dat$attr$count.trans[uni.trans] +
      as.numeric(tab.trans)
  }
  
  # Summary Output
  dat$epi$incid[at] <- length(infected)
  dat$epi$incid.B[at] <- sum(dat$attr$race[infected] == 1)
  dat$epi$incid.H[at] <- sum(dat$attr$race[infected] == 2)
  dat$epi$incid.W[at] <- sum(dat$attr$race[infected] == 3)
  
  if (length(infected) > 0) {
    dat$epi$incid.undx[at] <- sum(dat$attr$diag.status[transmitter] == 0)
    dat$epi$incid.dx[at] <- sum(dat$attr$diag.status[transmitter] == 1 &
                                  dat$attr$cuml.time.on.tx[transmitter] == 0)
    dat$epi$incid.linked[at] <- sum(dat$attr$diag.status[transmitter] == 1 &
                                      dat$attr$cuml.time.on.tx[transmitter] > 0 &
                                      dat$attr$vl[transmitter] > log10(200))
    dat$epi$incid.vsupp[at] <- sum(dat$attr$diag.status[transmitter] == 1 &
                                     dat$attr$cuml.time.on.tx[transmitter] > 0 &
                                     dat$attr$vl[transmitter] <= log10(200))
  } else {
    dat$epi$incid.undx[at] <- 0
    dat$epi$incid.dx[at] <- 0
    dat$epi$incid.linked[at] <- 0
    dat$epi$incid.vsupp[at] <- 0
  }
  
  return(dat)
}
