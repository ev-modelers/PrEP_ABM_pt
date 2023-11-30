#INITIATINIZED PART
initialize_msm_new <- function(x, param, init, control, s) { #So this is what sets up the network that is then changed by simnet
 
  dat <- create_dat_object(param, init, control) #Ah, this is why everything depends on init
  #### Network Setup ####
  # Initial network setup
  # Simulate each network on the basis of their fits
  # Add to dat object 
  
  dat[["nw"]] <- list()
  nnets <- 3
  for (i in 1:nnets) {
    dat[["nw"]][[i]] <- simulate( 
      x[[i]][["fit"]],
      basis = x[[i]][["fit"]][["newnetwork"]],
      dynamic = FALSE
    )
  }
  nw <- dat[["nw"]]
  
  # Pull Network parameters
  dat[["nwparam"]] <- list()
  for (i in 1:nnets) {
    dat[["nwparam"]][i] <- list(x[[i]][-which(names(x[[i]]) == "fit")])
  }
  
  # Convert to tergmLite method
  dat <- init_tergmLite(dat)
  
  #### Nodal Attributes Setup ####
  dat[["attr"]] <- param[["netstats"]][["attr"]]
  
  num <- network.size(nw[[1]])
  dat <- append_core_attr(dat, 1, num)
  
  # Pull in attributes on network. 
  # We pull from the one-time network because this has both deg.main and deg.pers attributes 
  # (see estimation script)
  nwattr.all <- names(nw[[3]][["val"]][[3]])
  nwattr.use <- nwattr.all[!nwattr.all %in% c("na", "vertex.names")]
  for (i in seq_along(nwattr.use)) {
    dat$attr[[nwattr.use[i]]] <- get.vertex.attribute(nw[[3]], nwattr.use[i])
  }
  
  # Add other attributes 
  
  # First we need some initial conditions parameters
  init.vacc <- get_param(dat, "init.vacc")
  init.inf <- get_param(dat, "init.inf")
  
  
  # Generate status vector based on nums init vaccinated and init infected (non-overlapping groups)
  # starting values s / i / v
  # initial vaccinations are currently random (if vaccine module is list in control function)
  # initial infections among highest-activity groups unless init size is > than size of those groups 
  
  status <- rep(0, num)
  riskg <- get_attr(dat, "riskg")
  
  # initial vaccinations
  vaccinated <- sample(1:num, init.vacc, replace=FALSE)
  status[vaccinated] <- 0 
  
  # initial infecteds
  risk.high <- which(status == 1 & (riskg == "5" | riskg == "6"))
  
  if (length(risk.high) > init.inf) {
    infected <- sample(risk.high, init.inf, replace=FALSE)
  }
  else {infected <- sample(which(status==0), init.inf, replace=FALSE)}
  
  status[infected] <- 1
  
  # vaccinated time vector (NA until vaccinated)
  vaccTime <- rep(NA, num)
  vaccTime[vaccinated] <- 1
  
  # infection time vector (NA until infected)
  infTime <- rep(NA, num)
  infTime[infected] <- 1
  
  # secondary infections from node vector (NA until an infection is transmitted from node to partner)
  secInfs <- rep(NA, num)
  secInfs[infected] <- 0
  
  # which generation is this infection
  infGen <- rep(NA, num)
  infGen[infected] <- 1
  
  # Pull sqrt age to get age 
  sqrt.age <- get_attr(dat, "sqrt.age")
  age <- sqrt.age^2
  
  # Set nodal attribute for tx seeking if nodal.tx=TRUE
  if (dat$param$nodal.tx){
    tx.prob <- get_param(dat, "treatment.prob")
    tx.seek <- rep(0, num)
    seek <- sample(1:num, tx.prob*num, replace=FALSE)
    tx.seek[seek] <- 1
    dat <- set_attr(dat, "tx.seek", tx.seek)
  }
  
  # set attributes
  dat <- set_attr(dat, "secInfs", secInfs)     # record how many infections per infection
  dat <- set_attr(dat, "infTime", infTime)     # record time step of infection
  dat <- set_attr(dat, "infGen", infGen)       # generation of infection
  dat <- set_attr(dat, "vaccTime", vaccTime)   # record time step of vaccination
  dat <- set_attr(dat, "status", status)       # initial status
  dat <- set_attr(dat, "age", sqrt.age^2)      # age attribute to accompany the sqrt.age attribute
  dat <- set_attr(dat, "recTime", rep(NA,num)) # infection recovery time 
  
  #### Other Setup ####
  dat[["stats"]] <- list()
  dat[["stats"]][["nwstats"]] <- list()
  dat[["temp"]] <- list()
  dat[["epi"]] <- list()
  dat[["mpx_degdist"]] <- list()
  
  dat <- set_epi(dat, "num", at = 1,  num)
  dat <- set_epi(dat, "cuml.infs", at = 1, init.inf)
  dat <- set_epi(dat, "cuml.cases", at = 1, 0)
  dat <- set_epi(dat, "prev", at = 1, init.inf)
  
  
  # Setup Partner List for all 3 networks 
  # (only gets updated if tracking turned on in control function)
  for (n_network in seq_len(3)) {
    dat <- update_cumulative_edgelist(dat, n_network)
  }
  
  # Network statistics
  if (dat[["control"]][["save.nwstats"]]) {
    for (i in seq_along(x)) {
      nwL <- networkLite(dat[["el"]][[i]], dat[["attr"]])
      nwstats <- summary(
        dat[["control"]][["nwstats.formulas"]][[i]],
        basis = nwL,
        term.options = dat[["control"]][["mcmc.control"]][[i]][["term.options"]],
        dynamic = i < 3
      )
      
      dat[["stats"]][["nwstats"]][[i]] <- matrix(
        nwstats, 
        nrow = 1, ncol = length(nwstats),
        dimnames = list(NULL, names(nwstats))
      )
      
      dat[["stats"]][["nwstats"]][[i]] <- 
        as.data.frame(dat[["stats"]][["nwstats"]][[i]])
    }
  }
  
  
  
  #####################################################################################
  
  ## Nodal Attributes Setup ##
  #dat$attr <- param$netstats$attr
  
  num <- network.size(nw[[1]])
  dat$attr$active <- rep(1, num)
  dat$attr$arrival.time <- rep(1, num)
  dat$attr$uid <- 1:num
  
  # Circumcision
  #rates <- param$circ.prob[dat$attr$race]
  rates <- param$circ.prob[1]
  dat$attr$circ <- rbinom(length(rates), 1, rates)
  
  # Insertivity Quotient
  ins.quot <- rep(NA, num)
  
  role.class <- dat$attr$role.class
  ins.quot[role.class == 0]  <- 1
  ins.quot[role.class == 1]  <- 0
  ins.quot[role.class == 2]  <- runif(sum(role.class == 2))
  dat$attr$ins.quot <- ins.quot
  
  # HIV-related attributes
  dat <- init_status_msm(dat)
  
  # STI Status
  dat <- init_sti_msm(dat)
  
  # PrEP-related attributes
  dat$attr$prepClass <- rep(NA, num)
  dat$attr$prepElig <- rep(NA, num)
  dat$attr$prepStat <- rep(0, num)
  dat$attr$prepStartTime <- rep(NA, num)
  dat$attr$prepLastRisk <- rep(NA, num)
  dat$attr$prepLastStiScreen <- rep(NA, num)
  
  ## Other Setup ##
  #dat$stats <- list()
  #dat$stats$nwstats <- list()
  #dat$temp <- list()
  #dat$epi <- list()
  
  # Prevalence Tracking
  dat$temp$max.uid <- num
  #dat <- prevalence_msm(dat, at = 1)
  
  
  
  # Setup Partner List
  plist <- cbind(dat$el[[1]], ptype = 1)
  plist <- rbind(plist, cbind(dat$el[[2]], ptype = 2))
  plist <- cbind(plist, start = 1, stop = NA)
  colnames(plist)[1:2] <- c("p1", "p2")
  dat$temp$plist <- plist
  
  
#####################################################################################
  

  class(dat) <- "dat"
  return(dat)
  
}



init_status_msm <- function(dat) {
  
  num <- sum(dat$attr$active == 1)
  
  # Sub in diag.status from model for status
  status <- dat$attr$status
  #############
  #diag.status<-
  status<-sample(c(0,1),num,TRUE,c(1-dat$param$startprev,dat$param$startprev))
  dat$attr$diag.status<-status
  dat$attr$status<-status
  ###############
  # Late (AIDS-stage) tester type
  #rates <- dat$param$hiv.test.late.prob[dat$attr$race]
  rates <- dat$param$hiv.test.late.prob[1]
  dat$attr$late.tester <- rbinom(length(rates), 1, rates)
  
  # Treatment trajectory
  tt.traj <- rep(NA, num)
  
  tt.traj <- sample(1:3, num, replace=TRUE,
                            c(0.2,
                                0.4,
                                0.4))
  
  #for (i in races) {
    #ids.race <- which(dat$attr$race == i)
    #tt.traj[ids.race] <- sample(1:3, length(ids.race), TRUE,
    #                            c(dat$param$tt.part.supp[i],
    #                              dat$param$tt.full.supp[i],
    #                              dat$param$tt.dur.supp[i]))
    #
  #}
  dat$attr$tt.traj <- tt.traj
  
  ## Infection-related attributes
  idsInf <- which(status == 1)
  
  #age <- dat$attr$age
  #min.ages <- min(dat$param$netstats$demog$ages)
  #time.sex.active <- pmax(1, round((365/7)*age[idsInf] - (365/7)*min.ages, 0))
  age <- dat$attr$age
  time.sex.active <- pmax(1, round((365/7)*age[idsInf] - (365/7)*15, 0))
  min.hiv.time <- round(dat$param$vl.acute.rise.int + dat$param$vl.acute.fall.int)
  max.hiv.time <- dat$param$vl.aids.onset.int
  
  time.infected <- round(pmax(min.hiv.time,
                              pmin(time.sex.active,
                                   sample(min.hiv.time:max.hiv.time, length(idsInf), TRUE))))
  
  dat$attr$inf.time <- rep(NA, num)
  dat$attr$inf.time[idsInf] <- -time.infected
  
  dat$attr$stage <- rep(NA, num)
  dat$attr$stage.time <- rep(NA, num)
  dat$attr$aids.time <- rep(NA, num)
  dat$attr$stage[idsInf] <- 3
  dat$attr$stage.time[idsInf] <- time.infected - min.hiv.time
  
  dat$attr$diag.stage <- rep(NA, num)
  dat$attr$diag.stage[idsInf] <- dat$attr$stage[idsInf]
  
  dat$attr$vl <- rep(NA, num)
  dat$attr$vl[idsInf] <- dat$param$vl.set.point
  dat$attr$vl.last.usupp <- rep(NA, num)
  dat$attr$vl.last.supp <- rep(NA, num)
  
  dat$attr$diag.time <- rep(NA, num)
  dat$attr$diag.time[idsInf] <- dat$attr$inf.time[idsInf] + round(mean(1/dat$param$hiv.test.rate))
  dat$attr$last.neg.test <- rep(NA, num)
  
  dat$attr$tx.status <- rep(NA, num)
  dat$attr$tx.status[idsInf] <- 0
  dat$attr$cuml.time.on.tx <- rep(NA, num)
  dat$attr$cuml.time.on.tx[idsInf] <- 0
  dat$attr$cuml.time.off.tx <- rep(NA, num)
  dat$attr$cuml.time.off.tx[idsInf] <- time.infected
  dat$attr$tx.period.first <- rep(NA, num)
  dat$attr$tx.period.last <- rep(NA, num)
  dat$attr$tx.init.time <- rep(NA, num)
  
  dat$attr$count.trans <- rep(0, num)
  
  dat$attr$WHO_guide <- rep(0, num)
  
  dat$attr$num.neg.tests <- rep(0, length(status))
  
  return(dat)
}



#' @title Initialize the STI status of persons in the network
#'
#' @description Sets the initial individual-level disease status of persons
#'              in the network, as well as disease-related attributes for
#'              infected persons.
#'
#' @param dat Data object created in initialization module.
#'
#' @export
#' @keywords initiation utility msm
#'
init_sti_msm <- function(dat) {
  
  role.class <- dat$attr$role.class
  num <- length(role.class)
  
  idsUreth <- which(role.class %in% c(0, 2))
  idsRect <- which(role.class %in% c(1, 2))
  
  uGC <- rGC <- rep(0, num)
  uCT <- rCT <- rep(0, num)
  
  # Initialize GC infection at both sites
  idsUGC <- sample(idsUreth, size = round(dat$init$prev.ugc * num), FALSE)
  uGC[idsUGC] <- 1
  
  idsRGC <- sample(setdiff(idsRect, idsUGC), size = round(dat$init$prev.rgc * num), FALSE)
  rGC[idsRGC] <- 1
  
  dat$attr$rGC <- rGC
  dat$attr$uGC <- uGC
  
  dat$attr$rGC.sympt <- dat$attr$uGC.sympt <- rep(NA, num)
  dat$attr$rGC.sympt[rGC == 1] <- rbinom(sum(rGC == 1), 1, dat$param$rgc.sympt.prob)
  dat$attr$uGC.sympt[uGC == 1] <- rbinom(sum(uGC == 1), 1, dat$param$ugc.sympt.prob)
  
  dat$attr$rGC.infTime <- dat$attr$uGC.infTime <- rep(NA, length(dat$attr$active))
  dat$attr$rGC.infTime[rGC == 1] <- 1
  dat$attr$uGC.infTime[uGC == 1] <- 1
  
  dat$attr$rGC.timesInf <- rep(0, num)
  dat$attr$rGC.timesInf[rGC == 1] <- 1
  dat$attr$uGC.timesInf <- rep(0, num)
  dat$attr$uGC.timesInf[uGC == 1] <- 1
  
  dat$attr$rGC.tx <- dat$attr$uGC.tx <- rep(NA, num)
  dat$attr$rGC.tx.prep <- dat$attr$uGC.tx.prep <- rep(NA, num)
  
  # Initialize CT infection at both sites
  idsUCT <- sample(idsUreth, size = round(dat$init$prev.uct * num), FALSE)
  uCT[idsUCT] <- 1
  
  idsRCT <- sample(setdiff(idsRect, idsUCT), size = round(dat$init$prev.rct * num), FALSE)
  rCT[idsRCT] <- 1
  
  dat$attr$rCT <- rCT
  dat$attr$uCT <- uCT
  
  dat$attr$rCT.sympt <- dat$attr$uCT.sympt <- rep(NA, num)
  dat$attr$rCT.sympt[rCT == 1] <- rbinom(sum(rCT == 1), 1, dat$param$rct.sympt.prob)
  dat$attr$uCT.sympt[uCT == 1] <- rbinom(sum(uCT == 1), 1, dat$param$uct.sympt.prob)
  
  dat$attr$rCT.infTime <- dat$attr$uCT.infTime <- rep(NA, num)
  dat$attr$rCT.infTime[dat$attr$rCT == 1] <- 1
  dat$attr$uCT.infTime[dat$attr$uCT == 1] <- 1
  
  dat$attr$rCT.timesInf <- rep(0, num)
  dat$attr$rCT.timesInf[rCT == 1] <- 1
  dat$attr$uCT.timesInf <- rep(0, num)
  dat$attr$uCT.timesInf[uCT == 1] <- 1
  
  dat$attr$rCT.tx <- dat$attr$uCT.tx <- rep(NA, num)
  dat$attr$rCT.tx.prep <- dat$attr$uCT.tx.prep <- rep(NA, num)
  
  return(dat)
  
}


#' @title Re-Initialization Module
#'
#' @description This function reinitializes an epidemic model to restart at a
#'              specified time step given an input \code{netsim} object.
#'
#' @param x An \code{EpiModel} object of class \code{\link{netsim}}.
#' @inheritParams initialize_msm
#'
#' @details
#' Currently, the necessary components that must be on \code{x} for a simulation
#' to be restarted must be: param, control, nwparam, epi, attr, temp, el, p.
#' TODO: describe this more.
#'
#' @return
#' This function resets the data elements on the \code{dat} master data object
#' in the needed ways for the time loop to function.
#'
#' @export
#' @keywords module msm
#'
reinit_msm <- function(x, param, init, control, s) {
  
  need.for.reinit <- c("param", "control", "nwparam", "epi", "attr", "temp", "el", "p")
  if (!all(need.for.reinit %in% names(x))) {
    stop("x must contain the following elements for restarting: ",
         "param, control, nwparam, epi, attr, temp, el, p",
         call. = FALSE)
  }
  
  if (length(x$el) == 1) {
    s <- 1
  }
  
  dat <- list()
  
  dat$param <- param
  dat$param$modes <- 1
  dat$control <- control
  dat$nwparam <- x$nwparam
  
  dat$epi <- sapply(x$epi, function(var) var[s])
  names(dat$epi) <- names(x$epi)
  
  dat$el <- x$el[[s]]
  dat$p <- x$p[[s]]
  
  dat$attr <- x$attr[[s]]
  
  if (!is.null(x$stats)) {
    dat$stats <- list()
    if (!is.null(x$stats$nwstats)) {
      dat$stats$nwstats <- x$stats$nwstats[[s]]
    }
  }
  
  dat$temp <- x$temp[[s]]
  
  class(dat) <- "dat"
  return(dat)
}

