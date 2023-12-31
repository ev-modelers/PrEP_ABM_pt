base_nw_msm <- function(nwstats) {
  
  num.B <- nwstats$num.B
  num.W <- nwstats$num.W
  
  # Initialize network
  n <- num.B + num.W
  nw <- network::network.initialize(n, directed = FALSE)
  
  # Calculate attributes
  race <- c(rep(1, num.B), rep(2, num.W))
  race <- sample(race)
  
  ager <- nwstats$ages
  ages <- seq(min(ager), max(ager) + 1, 1 / (365 / nwstats$time.unit))
  age <- sample(ages, n, TRUE)
  sqrt.age <- sqrt(age)
  
  age.grp <- age
  age.grp[age.grp<25] <- 1
  age.grp[age.grp>24 & age.grp <35] <- 2
  age.grp[age.grp>34] <- 3
  
  role.B <- sample(apportion_lr(num.B, c(0, 1, 2), nwstats$role.B.prob))
  role.W <- sample(apportion_lr(num.W, c(0, 1, 2), nwstats$role.W.prob))
  role <- rep(NA, n)
  role[race == 1] <- role.B
  role[race == 2] <- role.W
  
  if (nwstats$top5==TRUE){
    riskg.B <- sample(apportion_lr(num.B, 1:6, c(rep(0.19, 5), 0.05)))
    riskg.W <- sample(apportion_lr(num.W, 1:6, c(rep(0.19, 5), 0.05)))
  } else {
    riskg.B <- sample(apportion_lr(num.B, 1:5, rep(0.2, 5)))
    riskg.W <- sample(apportion_lr(num.W, 1:5, rep(0.2, 5)))
  }
  
  riskg <- rep(NA, n)
  riskg[race == 1] <- riskg.B
  riskg[race == 2] <- riskg.W
  
  # HIV status
  hiv.prev <- c(0.044, 0.109, 0.154, 0.183)
  hiv <- rep(0, n)
  age.floor <- floor(age)
  age18 <- which(age.floor %in% 18:24)
  age25 <- which(age.floor %in% 25:29)
  age30 <- which(age.floor %in% 30:34)
  age35 <- which(age.floor %in% 35:39)
  hiv[sample(age18, hiv.prev[1] * length(age18))] <- 1
  hiv[sample(age25, hiv.prev[2] * length(age25))] <- 1
  hiv[sample(age30, hiv.prev[3] * length(age30))] <- 1
  hiv[sample(age35, hiv.prev[4] * length(age35))] <- 1
  
  attr.names <- c("race", "riskg", "sqrt.age", "role.class", "age.grp", "hiv")
  attr.values <- list(race, riskg, sqrt.age, role, age.grp, hiv)
  nw <- network::set.vertex.attribute(nw, attr.names, attr.values)
  
  return(nw)
}



assign_degree <- function(nw, deg.type, nwstats) {
  
  if (!("network" %in% class(nw))) {
    stop("nw must be of class network")
  }
  
  if (deg.type == "main") {
    attr.name <- "deg.main"
    dist.B <- rowSums(nwstats$deg.mp.B)
    dist.W <- rowSums(nwstats$deg.mp.W)
  }
  if (deg.type == "pers") {
    attr.name <- "deg.pers"
    dist.B <- colSums(nwstats$deg.mp.B)
    dist.W <- colSums(nwstats$deg.mp.W)
  }
  
  if (!isTRUE(all.equal(sum(colSums(nwstats$deg.mp.B)), 1, tolerance = 5e-6))) {
    stop("B degree distributions do not sum to 1")
  }
  
  if (!isTRUE(all.equal(sum(colSums(nwstats$deg.mp.W)), 1, tolerance = 5e-6))) {
    stop("W degree distributions do not sum to 1")
  }
  
  race <- get.vertex.attribute(nw, "race")
  vB <- which(race == 1)
  vW <- which(race == 2)
  nB <- length(vB)
  nW <- length(vW)
  
  num.degrees.B <- length(dist.B)
  num.degrees.W <- length(dist.W)
  
  deg.B <- apportion_lr(nB, 0:(num.degrees.B - 1), dist.B, shuffled = TRUE)
  deg.W <- apportion_lr(nW, 0:(num.degrees.W - 1), dist.W, shuffled = TRUE)
  
  if (nwstats$method == 2) {
    deg.B <- paste0(1, deg.B)
    deg.W <- paste0(2, deg.W)
  }
  
  nw <- set.vertex.attribute(nw, attrname = attr.name, value = deg.B, v = vB)
  nw <- set.vertex.attribute(nw, attrname = attr.name, value = deg.W, v = vW)
  
  return(nw)
}






calc_nwstats_msm <- function(time.unit,
                             method,
                             top5,
                             num.B,
                             num.W,
                             deg.mp.B,
                             deg.mp.W,
                             mdeg.inst.B,
                             mdeg.inst.W,
                             qnts.B,
                             qnts.W,
                             prop.hom.mpi.B,
                             prop.hom.mpi.W,
                             balance = "mean",
                             sqrt.adiff.BB,
                             sqrt.adiff.WW,
                             sqrt.adiff.BW,
                             diss.main,
                             diss.pers,
                             durs.main,
                             durs.pers,
                             ages,
                             asmr.B,
                             asmr.W,
                             role.B.prob,
                             role.W.prob,
                             main.weights=NULL,
                             pers.weights=NULL,
                             inst.weights=NULL,
                             mdeg.hiv.pers = NULL,
                             mdeg.hiv.inst = NULL) {
  
  if (sum(deg.mp.B) != 1) {
    stop("deg.mp.B must sum to 1.")
  }
  if (sum(deg.mp.W) != 1) {
    stop("deg.mp.W must sum to 1.")
  }
  if (!(method %in% 1:2)) {
    stop("method must either be 1 for one-race models or 2 for two-race models", call. = FALSE)
  }
  
  num <- num.B + num.W
  
  # deg.pers nodal attribute
  if (method == 2) {
    deg.pers.B <- apportion_lr(num.B, c("B0", "B1", "B2"), colSums(deg.mp.B))
    deg.pers.W <- apportion_lr(num.W, c("W0", "W1", "W2"), colSums(deg.mp.W))
  }
  if (method == 1) {
    deg.pers <- apportion_lr(num, 0:2, colSums(deg.mp.W))
  }
  
  # deg main nodal attribute
  if (method == 2) {
    deg.main.B <- apportion_lr(num.B, c("B0", "B1"), rowSums(deg.mp.B))
    deg.main.W <- apportion_lr(num.W, c("W0", "W1"), rowSums(deg.mp.W))
  }
  if (method == 1) {
    deg.main <- apportion_lr(num, 0:1, rowSums(deg.mp.W))
  }
  
  
  # Main partnerships -------------------------------------------------------
  
  # Persons in partnerships by casual degree
  if (method == 2) {
    totdeg.m.by.dp <- c(num.B * deg.mp.B[2, ], num.W * deg.mp.W[2, ])
  }
  if (method == 1) {
    totdeg.m.by.dp <- c(num * deg.mp.B[2, ])
  }
  
  # Persons in partnerships by race
  if (method == 2) {
    totdeg.m.by.race <- c(sum(totdeg.m.by.dp[1:3]), sum(totdeg.m.by.dp[4:6]))
  }
  
  # Number of partnerships
  edges.m <- (sum(totdeg.m.by.dp)) / 2
  
  # Mixing
  if (method == 2) {
    # Number of mixed-race partnerships, with balancing to decide
    edges.m.B2W <- totdeg.m.by.race[1] * (1 - prop.hom.mpi.B[1])
    edges.m.W2B <- totdeg.m.by.race[2] * (1 - prop.hom.mpi.W[1])
    edges.het.m <- switch(balance,
                          black = edges.m.B2W,
                          white = edges.m.W2B,
                          mean = (edges.m.B2W + edges.m.W2B) / 2)
    
    # Number of same-race partnerships
    edges.hom.m <- (totdeg.m.by.race - edges.het.m) / 2
    
    # Nodemix target stat: numer of BB, BW, WW partnerships
    edges.nodemix.m <- c(edges.hom.m[1], edges.het.m, edges.hom.m[2])
  }
  
  # Sqrt absdiff term for age
  if (method == 2) {
    sqrt.adiff.m <- edges.nodemix.m * c(sqrt.adiff.BB[1], sqrt.adiff.BW[1], sqrt.adiff.WW[1])
  }
  if (method == 1) {
    sqrt.adiff.m <- edges.m * mean(c(sqrt.adiff.BB[1], sqrt.adiff.BW[1], sqrt.adiff.WW[1]))
  }
  
  # Activity by Age Group 
  if (!is.null(main.weights)) {
    age.edges.m <- edges.m * main.weights *2
  }
  
  # Compile target stats
  if (method == 2) {
    stats.m <- c(edges.m, edges.nodemix.m[2:3], totdeg.m.by.dp[c(2:3, 5:6)], sqrt.adiff.m)
  }
  if (method == 1) {
    stats.m <- c(edges.m, totdeg.m.by.dp[2:3], sqrt.adiff.m)
    
    if (!is.null(main.weights)) {
      stats.m <- c(edges.m, totdeg.m.by.dp[2:3], sqrt.adiff.m, age.edges.m[2:3])
    }
  }
  
  # Dissolution model
  exp.mort <- (mean(asmr.B[ages]) + mean(asmr.W[ages])) / 2
  
  coef.diss.m <- dissolution_coefs(dissolution = diss.main,
                                   duration = durs.main / time.unit,
                                   d.rate = exp.mort)
  
  
  
  # Casual partnerships -----------------------------------------------------
  
  # Persons in partnerships by main degree
  if (method == 2) {
    totdeg.p.by.dm <- c(num.B * deg.mp.B[, 2] + num.B * deg.mp.B[, 3] * 2,
                        num.W * deg.mp.W[, 2] + num.W * deg.mp.W[, 3] * 2)
  }
  if (method == 1) {
    totdeg.p.by.dm <- c(num * deg.mp.B[, 2] + num * deg.mp.B[, 3] * 2)
  }
  
  # Persons in partnerships by race
  if (method == 2) {
    totdeg.p.by.race <- c(sum(totdeg.p.by.dm[1:2]), sum(totdeg.p.by.dm[3:4]))
  }
  
  # Persons concurrent
  if (method == 2) {
    conc.p.by.race <- c(sum(deg.mp.B[, 3]) * num.B, sum(deg.mp.W[, 3]) * num.W)
  }
  if (method == 1) {
    conc.p <- sum(deg.mp.B[, 3] * num)
  }
  
  # Number of partnerships
  edges.p <- sum(totdeg.p.by.dm) / 2
  
  # Mixing
  if (method == 2) {
    # Number of mixed-race partnerships, with balancing to decide
    edges.p.B2W <- totdeg.p.by.race[1] * (1 - prop.hom.mpi.B[2])
    edges.p.W2B <- totdeg.p.by.race[2] * (1 - prop.hom.mpi.W[2])
    edges.het.p <- switch(balance,
                          black = edges.p.B2W, white = edges.p.W2B,
                          mean = (edges.p.B2W + edges.p.W2B) / 2)
    
    # Number of same-race partnerships
    edges.hom.p <- (totdeg.p.by.race - edges.het.p) / 2
    
    # Nodemix target stat: number of BB, BW, WW partnerships
    edges.nodemix.p <- c(edges.hom.p[1], edges.het.p, edges.hom.p[2])
  }
  
  # Sqrt absdiff term for age
  if (method == 2) {
    sqrt.adiff.p <- edges.nodemix.p * c(sqrt.adiff.BB[2], sqrt.adiff.BW[2], sqrt.adiff.WW[2])
  }
  if (method == 1) {
    sqrt.adiff.p <- edges.p * mean(c(sqrt.adiff.BB[2], sqrt.adiff.BW[2], sqrt.adiff.WW[2]))
  }
  
  
  # Activity by Age Group 
  if (!is.null(pers.weights)) {
    age.edges.p <- edges.p * pers.weights * 2
  }
  
  # Activity by HIV status 
  if (!is.null(mdeg.hiv.pers)) {
    #if(num != 10000) { stop("hiv counts not correct at this size")}
    hiv.edges.p <- mdeg.hiv.pers * c(0.8855, 0.1145) * num
  }
  
  
  
  # Compile target statistics
  if (method == 2) {
    stats.p <- c(edges.p, edges.nodemix.p[2:3], totdeg.p.by.dm[c(2, 4)],
                 conc.p.by.race, sqrt.adiff.p)
  }
  if (method == 1) {
    stats.p <- c(edges.p, totdeg.p.by.dm[2], conc.p, sqrt.adiff.p)
    
    if (!is.null(pers.weights)) {
      stats.p <- c(edges.p, totdeg.p.by.dm[2], conc.p, sqrt.adiff.p, age.edges.p[c(2:3)])
    }
    
    if (!is.null(mdeg.hiv.pers)) {
      stats.p <- c(edges.p, totdeg.p.by.dm[2], conc.p, sqrt.adiff.p, hiv.edges.p[2])
    }
  }
  
  # Dissolution model
  coef.diss.p <- dissolution_coefs(dissolution = diss.pers,
                                   duration = durs.pers / time.unit,
                                   d.rate = exp.mort)
  
  
  
  # Instant partnerships ----------------------------------------------------
  
  # Number of instant partnerships per time step, by main and casl degree
  if (method == 2) {
    num.inst.B <- num.B * deg.mp.B * mdeg.inst.B * time.unit
    num.inst.W <- num.W * deg.mp.W * mdeg.inst.W * time.unit
  }
  if (method == 1) {
    num.inst <- num * deg.mp.W * mdeg.inst.W * time.unit
  }
  
  # Risk quantiles
  if (!is.na(qnts.B[1]) & !is.na(qnts.W[1])) {
    if (method == 2) {
      num.riskg.B <- (0.2*num.B) * qnts.B * time.unit
      num.riskg.W <- (0.2*num.W) * qnts.W * time.unit
    }
    if (method == 1) {
      if (top5==TRUE) {
        num.riskg <- c(rep(0.19, 5), 0.05) * num * qnts.B * time.unit
      } 
      else {num.riskg <- 0.2 * num * qnts.B * time.unit}
    }
  }
  
  # Number of instant partnerships per time step, by race
  if (method == 2) {
    totdeg.i <- c(sum(num.inst.B), sum(num.inst.W))
  }
  if (method == 1) {
    totdeg.i <- sum(num.inst)
  }
  
  # Number of partnerships
  edges.i <- sum(totdeg.i) / 2
  
  # Activity by HIV status 
  if (!is.null(mdeg.hiv.inst)) {
    #if(num != 10000) { stop("hiv counts not correct at this size")}
    hiv.edges.i <- mdeg.hiv.inst * c(0.8855, 0.1145) * num
  }
  
  
  # Mixing
  if (method == 2) {
    # Number of mixed-race partnerships, with balancing to decide
    edges.i.B2W <- totdeg.i[1] * (1 - prop.hom.mpi.B[3])
    edges.i.W2B <- totdeg.i[2] * (1 - prop.hom.mpi.W[3])
    edges.het.i <- switch(balance,
                          black = edges.i.B2W, white = edges.i.W2B,
                          mean = (edges.i.B2W + edges.i.W2B) / 2)
    
    # Number of same-race partnerships
    edges.hom.i <- edges.i - edges.het.i
    
    # Nodemix target stat: number of BB, BW, WW partnerships
    edges.nodemix.i <- c((totdeg.i[1] - edges.het.i) / 2,
                         edges.het.i,
                         (totdeg.i[1] - edges.het.i) / 2)
  }
  
  if (method == 2) {
    sqrt.adiff.i <- edges.nodemix.i * c(sqrt.adiff.BB[3], sqrt.adiff.BW[3], sqrt.adiff.WW[3])
  }
  if (method == 1) {
    sqrt.adiff.i <- edges.i * mean(c(sqrt.adiff.BB[3], sqrt.adiff.BW[3], sqrt.adiff.WW[3]))
  }
  
  if (!is.na(qnts.B[1]) & !is.na(qnts.W[1])) {
    if (method == 2) {
      stats.i <- c(edges.i, num.inst.B[-1], num.inst.W,
                   num.riskg.B[-3], num.riskg.W[-3],
                   edges.hom.i, sqrt.adiff.i)
    }
    if (method == 1) {
      stats.i <- c(edges.i, num.inst[-1], num.riskg[-2], sqrt.adiff.i)
      
      if(!is.null(mdeg.hiv.inst)){
        stats.i <- c(edges.i, num.inst[-1], num.riskg[-2], sqrt.adiff.i, hiv.edges.i[2])
      }
    }
    
  } else {
    if (method == 2) {
      stats.i <- c(edges.i, num.inst.B[-1], num.inst.W, edges.hom.i, sqrt.adiff.i)
    }
    if (method == 1) {
      stats.i <- c(edges.i, num.inst[-1], sqrt.adiff.i)
    }
  }
  
  
  # Compile results ---------------------------------------------------------
  out <- list()
  out$method <- method
  if (method == 2) {
    out$deg.pers <- c(deg.pers.B, deg.pers.W)
    out$deg.main <- c(deg.main.B, deg.main.W)
  }
  if (method == 1) {
    out$deg.pers <- deg.pers
    out$deg.main <- deg.main
  }
  
  out$stats.m <- stats.m
  out$stats.p <- stats.p
  out$stats.i <- stats.i
  
  out$coef.diss.m <- coef.diss.m
  out$coef.diss.p <- coef.diss.p
  
  out$ages <- ages
  out$asmr.B <- asmr.B
  out$asmr.W <- asmr.W
  
  out$time.unit <- time.unit
  out$num.B <- num.B
  out$num.W <- num.W
  
  out$deg.mp.B <- deg.mp.B
  out$deg.mp.W <- deg.mp.W
  
  out$role.B.prob <- role.B.prob
  out$role.W.prob <- role.W.prob
  
  out$top5 <- top5
  
  class(out) <- "nwstats"
  return(out)
}

