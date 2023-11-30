aging_msm <- function(dat, at) {
  
  age <- dat$attr$age
  
  active <- dat$attr$active
  age.grp <- dat$attr$age.grp

  age[active == 1] <- age[active == 1] + 7 / 365
  
  age.breaks <- dat$param$netstats$demog$age.breaks
  age.grp[active == 1] <- cut(age[active == 1], age.breaks, labels = FALSE)

  dat$attr$sqrt.age<- sqrt(age)
  dat$attr$age.grp <- age.grp
  dat$attr$age <- age
  
  return(dat)
}



arrival_msm <- function(dat, at){
  
  ## Variables
  
  # Parameters
  a.rate <- dat$param$a.rate
  
  ## Process
  num <- dat$epi$num[1]
  
  nNew <- rpois(1, a.rate * num)
  
  ## Update Attr
  if (nNew > 0) {
    dat <- setNewAttr_msm(dat, at, nNew)
  }
  
  
  # Update Networks
  if (nNew > 0) {
    for (i in 1:3) {
      dat$el[[i]] <- tergmLite::add_vertices(dat$el[[i]], nNew)
    }
  }
  
  ## Output
  dat$epi$nNew[at] <- nNew
  
  return(dat)
}


setNewAttr_msm <- function(dat, at, nNew) {
  
  # Set all attributes NA by default
  dat$attr <- lapply(dat$attr, {
    function(x)
      c(x, rep(NA, nNew))
  })
  newIds <- which(is.na(dat$attr$active))
  # Demographic
  dat$attr$active[newIds] <- rep(1, nNew)
  dat$attr$uid[newIds] <- dat$temp$max.uid + (1:nNew)
  dat$temp$max.uid <- dat$temp$max.uid + nNew
  
  dat$attr$arrival.time[newIds] <- rep(at, nNew)
  
  #race.dist <- prop.table(table(dat$param$netstats$attr$race))
  #Assume the proportion is same
  race <- sample(sort(unique(dat$attr$race)), nNew, TRUE, c(0.5,0.5))
  dat$attr$race[newIds] <- race
  
  dat$attr$age[newIds] <- rep(dat$param$arrival.age, nNew)
  age.breaks <- dat$param$netstats$demog$age.breaks
  attr_age.grp <- cut(dat$attr$age[newIds], age.breaks, labels = FALSE)
  dat$attr$age.grp[newIds] <- attr_age.grp
  dat$attr$sqrt.age[newIds]<- sqrt(dat$attr$age[newIds])
  
  # Disease status and related
  dat$attr$status[newIds] <- rep(0, nNew)
  dat$attr$diag.status[newIds] <- rep(0, nNew)
  dat$attr$rGC[newIds] <- dat$attr$GC.timesInf[newIds] <- 0
  dat$attr$uGC[newIds] <- dat$attr$GC.timesInf[newIds] <- 0
  dat$attr$rCT[newIds] <- dat$attr$CT.timesInf[newIds] <- 0
  dat$attr$uCT[newIds] <- dat$attr$CT.timesInf[newIds] <- 0
  
  dat$attr$count.trans[newIds] <- 0
  #ADD RACE RESTRICTION
  race<-1
  rates <- dat$param$hiv.test.late.prob[race]
  dat$attr$late.tester[newIds] <- rbinom(length(rates), 1, rates)
  
  races <- sort(unique(dat$attr$race[newIds]))
  #tt.traj <- rep(NA, nNew)
  #for (i in races) {
  #  ids.race <- which(dat$attr$race[newIds] == i)
  #  tt.traj[ids.race] <- sample(1:3, length(ids.race), TRUE,
  #                              c(dat$param$tt.part.supp[i],
  #                                dat$param$tt.full.supp[i],
  #                                dat$param$tt.dur.supp[i]))
    
  #}
  #A NEW CONSTANT TEST
  tt.traj <- sample(1:3, nNew, replace=TRUE,
                    c(0.2,
                      0.4,
                      0.4))
  ##########
  dat$attr$tt.traj[newIds] <- tt.traj
  
  # Circumcision
  circ <- rep(NA, nNew)
#  for (i in races) {
#    ids.race <- which(dat$attr$race[newIds] == i)
#    circ[ids.race] <- rbinom(length(ids.race), 1, dat$param$circ.prob[i])
#  }
  circ<- rbinom(nNew, 1, dat$param$circ.prob[1])
  dat$attr$circ[newIds] <- circ
  
  # Role
  ns <- dat$param$netstats$attr
  role.class <- rep(NA, nNew)
  
  #for (i in races) {
  #  ids.race <- which(dat$attr$race[newIds] == i)
  #  rc.probs <- prop.table(table(ns$role.class[ns$race == i]))
  #  role.class[ids.race] <- sample(0:2, length(ids.race), TRUE, rc.probs)
  #}
  role.class<- sample(0:2, nNew, replace=TRUE,
                      c(0.26,
                        0.08,
                        0.66))
  dat$attr$role.class[newIds] <- role.class
  ins.quot <- rep(NA, nNew)
  ins.quot[dat$attr$role.class[newIds] == 0]  <- 1
  ins.quot[dat$attr$role.class[newIds] == 1]  <- 0
  ins.quot[dat$attr$role.class[newIds] == 2]  <-
    runif(sum(dat$attr$role.class[newIds] == 2))
  dat$attr$ins.quot[newIds] <- ins.quot
  
  # Degree
  dat$attr$deg.main[newIds] <- 0
  dat$attr$deg.casl[newIds] <- 0
  dat$attr$deg.tot[newIds] <- 0
  
  #dist.B <- rowSums(st$deg.mp.B)
  #dist.W <- rowSums(s$deg.mp.W)
  sam_values_1<-c(0,1)
  sam_values_2<-c(0,1,2)

  #dat[dat$attr$race =='B',]$attr$deg.main[newIds]<-sample(sam_values_1, length(dat$attr$deg.main[newIds][dat$attr$race =='B']), replace = TRUE,rowSums(st$deg.mp.B))
  #dat[dat$attr$race =='W',]$attr$deg.main[newIds]<-sample(sam_values_1, length(dat$attr$deg.main[newIds][dat$attr$race =='W']), replace = TRUE,rowSums(st$deg.mp.W))
    
  #dat[dat$attr$race =='B',]$attr$deg.pers[newIds]<-sample(sam_values_2, length(dat$attr$deg.pers[newIds][dat$attr$race =='B']), replace = TRUE,colSums(st$deg.mp.B))
  #dat[dat$attr$race =='W',]$attr$deg.pers[newIds]<-sample(sam_values_2, length(dat$attr$deg.pers[newIds][dat$attr$race =='W']), replace = TRUE,colSums(st$deg.mp.B))
  
  dat$attr$deg.main[newIds]<-sample(sam_values_1, nNew, replace = TRUE,rowSums(st$deg.mp.B))
  dat$attr$deg.pers[newIds]<-sample(sam_values_2, nNew, replace = TRUE,colSums(st$deg.mp.B))
  
  
  
  # One-off risk group
  dat$attr$risk.grp[newIds] <- sample(1:5, nNew, TRUE)
  
  # PrEP
  dat$attr$prepStat[newIds] <- 0
  
  # HIV screening
  dat$attr$num.neg.tests[newIds] <- 0
  
  # Update clinical history
  #if (dat$control$save.clin.hist == TRUE & length(newIds) > 0) {
  #  m <- dat$temp$clin.hist
  #  for (i in 1:length(m)) {
  #    new.m <- array(dim = c(length(newIds), dat$control$nsteps))
  #    m[[i]] <- rbind(m[[i]], new.m)
  #  }
  #  dat$temp$clin.hist <- m
  #}
  
  ## Check attributes written as expected
  # cbind(sapply(dat$attr, function(x) is.na(tail(x, 1))))
  ###GIVE HIV
  dat$attr$hiv[newIds] <- sample(c(0,1),nNew,TRUE,c(1,0))
  dat$attr$WHO_guide[newIds]<-0
  
  return(dat)
}



departure_msm_new <- function(dat, at) {
  
  ## General departures
  active <- dat$attr$active
  age <- floor(dat$attr$age)
  race <- dat$attr$race
  status <- dat$attr$status
  stage <- dat$attr$stage
  tx.status <- dat$attr$tx.status
  
  aids.mr <- dat$param$aids.mr
  asmr <- 1/(52*1000)
  
  idsElig <- which(active == 1)
  rates <- rep(NA, length(idsElig))
  rates<- asmr
  
  idsDep <- idsElig[rbinom(length(rates), 1, rates) == 1]
  
  ## HIV-related deaths
  idsEligAIDS <- which(stage == 4)
  idsDepAIDS <- idsEligAIDS[rbinom(length(idsEligAIDS), 1, aids.mr) == 1]

  idsDepAll <- unique(c(idsDep, idsDepAIDS))
  depHIV <- intersect(idsDepAll, which(status == 1))
  depHIV.old <- intersect(depHIV, which(age >= 39))
  
  # Cumulative R0 calculations
  # if (at == 2) {
  #   dat$temp$R0 <- NA
  # }
  # if (length(depHIV) > 0) {
  #   newR0 <- dat$attr$count.trans[depHIV]
  #   dat$temp$R0 <- c(dat$temp$R0, newR0)
  # }
  
  if (length(idsDepAll) > 0) {
    dat$attr$active[idsDepAll] <- 0
    for (i in 1:3) {
      dat$el[[i]] <- tergmLite::delete_vertices(dat$el[[i]], idsDepAll)
    }
    dat$attr <- deleteAttr(dat$attr, idsDepAll)
    if (unique(sapply(dat$attr, length)) != attributes(dat$el[[1]])$n) {
      stop("mismatch between el and attr length in departures mod")
    }
  }
  
  # Update clinical history
  #if (dat$control$save.clin.hist == TRUE & length(idsDepAll) > 0) {
  #  m <- dat$temp$clin.hist
  #  for (i in 1:length(m)) {
  #    m[[i]] <- m[[i]][-idsDepAll, ]
  #  }
  #  dat$temp$clin.hist <- m
  #}
  
  #dat$attr <- deleteAttr(dat$attr, depHIV)
  ## Summary Output
  dat$epi$dep.gen[at] <- length(idsDep)
  dat$epi$dep.AIDS[at] <- length(idsDepAIDS)
  dat$epi$dep.HIV[at] <- length(depHIV)
  dat$epi$dep.HIV.old[at] <- length(depHIV.old)
  
  return(dat)
}



