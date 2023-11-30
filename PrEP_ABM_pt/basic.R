#get Control and params
library(EpiModel)



#GET THE NETWORK
###USE THE BASE_NW.R TO INFER GET THE FUNCTION
source("Base_nw.R")




Networkparam<-function(pop,roleprob){
  pop.size = 1000
  num.B = pop.size-1
  num.W = 1
  
  # mean/pers degree distributions matrices.
  deg.mp.B = (matrix(c(0.471, 0.167, 0.074,
                       0.22, 0.047, 0.021), byrow = TRUE, nrow = 2))
  
  deg.mp.W =
    (matrix(c(0.471, 0.167, 0.074,
              0.22, 0.047, 0.021), byrow = TRUE, nrow = 2))
  
  # Instant rates
  mdeg.inst.B = (matrix(c(0.065/7, 0.087/7, 0.086/7,
                          0.056/7, 0.055/7, 0.055/7), byrow = TRUE, nrow = 2))
  mdeg.inst.W =
    (matrix(c(0.065/7, 0.087/7, 0.086/7,
              0.056/7, 0.055/7, 0.055/7), byrow = TRUE, nrow = 2))
  
  top5 = TRUE 
  # Instant rates (with additional partners based on highest risk group)
  # We add to each category weighted on original dist of instant rates by other partnerships 
  
  
  # Quintile distribution of overall AI rates
  # adding risk group for top 5% based on Weiss et al. 2020
  qnts.W =c(0.0000,
            0.007/7,
            0.038/7,
            0.071/7,
            0.221/7)
  qnts.B = c(0.0000,
             0.007/7,
             0.038/7,
             0.071/7,
             0.221/7)
  
  
  # Proportion in same-race partnerships (main, casl, inst)
  prop.hom.mpi.B = (c(0.9484, 0.9019, 0.9085) +
                      c(0.9154, 0.8509, 0.8944))/2
  prop.hom.mpi.W = (c(0.9484, 0.9019, 0.9085) +
                      c(0.9154, 0.8509, 0.8944))/2
  
  # Mean age diffs (main, casl, inst)
  sqrt.adiff.BB = c(0.464, 0.586, 0.544)
  sqrt.adiff.BW = c(0.464, 0.586, 0.544)
  sqrt.adiff.WW = c(0.464, 0.586, 0.544)
  
  # Mean durations
  rates.main = mean(c(1/407,
                      1/407,
                      1/407))
  rates.pers = mean(c(1/166,
                      1/166,
                      1/166))
  
  durs.main = 1/rates.main 
  durs.pers = 1/rates.pers 
  
  # Age-sex-specific mortality rates
  ages = 18:39
  asmr.W =  c(rep(0, 17),
              1-(1-c(rep(0.00159, 7),
                     rep(0.00225, 10),
                     rep(0.00348, 5)))^(1/(365/1)), 1)
  asmr.B = c(rep(0, 17),
             1-(1-c(rep(0.00159, 7),
                    rep(0.00225, 10),
                    rep(0.00348, 5)))^(1/(365/1)), 1)
  
  #asmr.W <- c(rep(0, 17),
  # 1-(1-c(rep(0.00103, 7),
  #       rep(0.00133, 10),
  #      rep(0.00214, 5)))^(1/(365/1)), 1)
  
  # I, R, V role frequencies
  #role.B.prob <- role.W.prob <- (c(0.242, 0.321, 0.437))
  role.B.prob = roleprob
  role.W.prob = roleprob
  
  time.unit = 7# days
  
  mdeg.hiv.pers = c(0.38631, 0.54083)
  mdeg.hiv.inst = c(0.022021, 0.034353)
  
  
  st <- calc_nwstats_msm(
    method = 1, #1 for 1 race models, 2 for two race models
    top5 = top5,
    time.unit = time.unit,
    num.B = num.B,
    num.W = num.W,
    deg.mp.B = deg.mp.B,
    deg.mp.W = deg.mp.W,
    mdeg.inst.B = mdeg.inst.B,
    mdeg.inst.W = mdeg.inst.W,
    qnts.B = qnts.B,
    qnts.W = qnts.W,
    prop.hom.mpi.B = prop.hom.mpi.B,
    prop.hom.mpi.W = prop.hom.mpi.W,
    balance = "mean",
    sqrt.adiff.BB = sqrt.adiff.BB,
    sqrt.adiff.WW = sqrt.adiff.WW,
    sqrt.adiff.BW = sqrt.adiff.BW,
    diss.main = ~offset(edges),
    diss.pers = ~offset(edges),
    durs.main = durs.main,
    durs.pers = durs.pers,
    ages = ages,
    asmr.B = asmr.B,
    asmr.W = asmr.W,
    role.B.prob = role.B.prob,
    role.W.prob = role.W.prob,
    mdeg.hiv.pers = mdeg.hiv.pers,
    mdeg.hiv.inst = mdeg.hiv.inst)
  
  
  
  nw.main <- base_nw_msm(st)
  
  # Assign degree
  nw.main <- assign_degree(nw.main, deg.type = "pers", nwstats = st)
  
  # Formulas
  formation.m <- ~edges +
    nodefactor("deg.pers") +
    absdiff("sqrt.age") +
    offset(nodematch("role.class", diff = TRUE, levels = 1:2))
  
  # Fit model
  fit.m <- netest(nw.main,
                  formation = formation.m,
                  coef.form = c(-Inf, -Inf),
                  target.stats = st$stats.m,
                  coef.diss = st$coef.diss.m,
                  constraints = ~bd(maxout = 1),
                  set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e10,
                                                  init.method = "zeros",
                                                  MCMLE.maxit = 250))
  
  
  # 2. Casual Model ---------------------------------------------------------
  
  # Initialize network
  nw.pers <- nw.main
  
  # Assign degree
  nw.pers <- assign_degree(nw.pers, deg.type = "main", nwstats = st)
  
  # Formulas
  formation.p <- ~edges +
    nodefactor("deg.main") +
    concurrent +
    absdiff("sqrt.age") +
    #nodefactor("hiv", levels=2) +
    offset(nodematch("role.class", diff = TRUE, levels = 1:2))
  
  # Fit model
  fit.p <- netest(nw.pers,
                  formation = formation.p,
                  coef.form = c(-Inf, -Inf),
                  target.stats = st$stats.p[1:4],
                  coef.diss = st$coef.diss.p,
                  constraints = ~bd(maxout = 2),
                  set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e9,
                                                  init.method = "zeros",
                                                  MCMLE.maxit = 250))
  
  
  
  
  
  
  # Fit inst model ----------------------------------------------------------
  
  # Initialize network
  nw.inst <- nw.main
  
  # Assign degree
  nw.inst <- set.vertex.attribute(nw.inst, "deg.main", nw.pers %v% "deg.main")
  nw.inst <- set.vertex.attribute(nw.inst, "deg.pers", nw.main %v% "deg.pers")
  table(nw.inst %v% "deg.main", nw.inst %v% "deg.pers")
  
  # Formulas
  formation.i <- ~edges +
    nodefactor(c("deg.main", "deg.pers")) +
    nodefactor("riskg", levels = -2) +
    absdiff("sqrt.age") +
    nodefactor("hiv", levels=2) +
    offset(nodematch("role.class", diff = TRUE, levels = 1:2))
  
  # Fit model
  fit.i <- netest(nw.inst,
                  formation = formation.i,
                  target.stats = st$stats.i,
                  coef.form = c(-Inf, -Inf),
                  coef.diss = dissolution_coefs(~offset(edges), 1),
                  set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e7,
                                                  MCMLE.maxit = 250)) 
  
  
  est_model <- list(fit.m, fit.p, fit.p) 
  
  return(list(st = st, est = est_model))
}













source("Base_nw.R")








param_msm <- function(init.inf = 10,                                # initial number of infected people 
                      init.hiv.age = c(0.044, 0.109, 0.154, 0.183), # prev rate among 18:24, 25:29, 30:34, 35:39
                      population.size = 1000,
                      behavior.change = FALSE,
                      startprev = 0.3,
                      act.rate.main = 1.54/7, 
                      act.rate.casual = 0.96/7, 
                      act.rate.instant = 1, 
                      e.to.a.rate = 1/6.6,     # transition rate from latent (e) to asymptomatically infectious (a)
                      a.to.i.rate = 1/1,       # transition rate from a to symptomatic. Currently have it set so that a does not transmit
                      treatment.rate = 1/8,    # will take 8 days on average (median) for symptomatic person to seek treatment
                      
                      param.set = 1,       # which parameter set used 
                      inf.prob = 0.9999,       # probability of infection upon sexual contact
                      treatment.prob = 0.01, # prob of inf. person seeking treatment at all
                      i.to.r.rate = 1/100,  # natural clearance rate
                      
                      nodal.tx = TRUE, 
                      init.vacc = 0,
                      vacc.effect = 0.85,
                      
                      hiv.test.rate = c(0.01, 0.01, 0.01),
                      hiv.test.late.prob = c(0.28, 0.28, 0.28),
                      test.window.int = 21/7,
                      tt.part.supp = c(0.20, 0.20, 0.20),
                      tt.full.supp = c(0.40, 0.40, 0.40),
                      tt.dur.supp = c(0.40, 0.40, 0.40),
                      tx.init.prob = c(0.092, 0.092, 0.127),
                      tx.halt.part.prob = c(0.0102, 0.0102, 0.0071),
                      tx.halt.full.rr = c(0.9, 0.9, 0.9),
                      tx.halt.dur.rr = c(0.5, 0.5, 0.5),
                      tx.reinit.part.prob = c(0.00066, 0.00066, 0.00291),
                      tx.reinit.full.rr = c(1.0, 1.0, 1.0),
                      tx.reinit.dur.rr = c(1.0, 1.0, 1.0),
                      
                      # HIV natural history
                      max.time.off.tx.full.int = 52 * 15,
                      max.time.on.tx.part.int = 52 * 10,
                      max.time.off.tx.part.int = 52 * 10,
                      vl.acute.rise.int = 6.4,
                      vl.acute.peak = 6.886,
                      vl.acute.fall.int = 6.4,
                      vl.set.point = 4.5,
                      vl.aids.onset.int = 520,
                      vl.aids.int = 104,
                      vl.aids.peak = 7,
                      vl.full.supp = 1.5,
                      vl.part.supp = 3.5,
                      vl.tx.down.slope = 0.25,
                      vl.tx.aids.down.slope = 0.25,
                      vl.tx.up.slope = 0.25,
                      aids.mr = 1/104,
                      
                      # Demographic
                      #a.rate = 0.00052,
                      a.rate = 0.0005,
                      arrival.age = 18,
                      
                      # HIV transmission prob
                      URAI.prob = 0.008938,
                      UIAI.prob = 0.003379,
                      trans.scale = c(1, 1, 1),
                      acute.rr = 6,
                      #circ.rr = 0.4,
                      #CIRC, PREP, 1 is effctiveness is 0%, HR
                      circ.rr = 1,
                      cond.eff = 0.95,
                      cond.fail = c(0.25, 0.25, 0.25),
                      circ.prob = c(0.02, 0.02, 0.02),
                      
                      # Behavioral
                      acts.aids.vl = 5.75,
                      acts.scale = 2,
                      cond.scale = 1,
                      
                      # STI epi
                      rgc.tprob = 0.35,
                      ugc.tprob = 0.25,
                      rct.tprob = 0.20,
                      uct.tprob = 0.16,
                      rgc.sympt.prob = 0.16,
                      ugc.sympt.prob = 0.80,
                      rct.sympt.prob = 0.14,
                      uct.sympt.prob = 0.58,
                      rgc.ntx.int = 16.8,
                      ugc.ntx.int = 16.8,
                      gc.tx.int = 1.4,
                      rct.ntx.int = 32,
                      uct.ntx.int = 32,
                      ct.tx.int = 1.4,
                      gc.sympt.prob.tx = c(0.95, 0.95, 0.95),
                      ct.sympt.prob.tx = c(0.9, 0.9, 0.9),
                      gc.asympt.prob.tx = c(0.15, 0.15, 0.15),
                      ct.asympt.prob.tx = c(0.15, 0.15, 0.15),
                      sti.cond.eff = 0.9,
                      sti.cond.fail = c(0.20, 0.20, 0.20),
                      hiv.rgc.rr = 2.78,
                      hiv.ugc.rr = 1.73,
                      hiv.rct.rr = 2.78,
                      hiv.uct.rr = 1.73,
                      hiv.dual.rr = 0.2,
                      
                      # PrEP
                      riskh.start = Inf,
                      prep.start = Inf,
                      prep.start.prob = 0,
                      prep.adhr.dist = c(0.089, 0.127, 0.784),
                      prep.adhr.hr = c(0.69, 0.19, 0.01),
                      prep.discont.rate = 1 - (2^(-1/(224.4237/7))),
                      prep.tst.int = 90/7,
                      prep.risk.int = 182/7,
                      prep.sti.screen.int = 182/7,
                      prep.sti.prob.tx = 1,
                      prep.risk.reassess.method = "year",
                      prep.require.lnt = TRUE,
                      senario='US',
                      ...) {
  
  ## Process parameters
  
  p <- get_args(formal.args = formals(sys.function()),
                dot.args = list(...))
  
  class(p) <- "param.net"
  return(p)
  
}





control_msm <- function(simno = 1,
                        nsteps = 200,
                        start = 1,
                        nsims = 1,
                        ncores = 4,
                        cumulative.edgelist = FALSE,
                        truncate.el.cuml = 0,
                        initialize.FUN = initialize_msm_new,
                        aging.FUN =aging_msm,
                        arrival.FUN = arrival_msm,
                        depart.FUN = departure_msm_new,
                        HIVtest.FUN = hivtest_msm_new,
                        HIVtx.FUN = hivtx_msm_new,
                        HIVprog.FUN = hivprogress_msm_new,
                        HIVvvl.FUN = hivvl_msm_new,
                        resim_nets.FUN = simnet_msm_new,
                        acts.FUN = acts_msm_new,
                        condom.FUN = condoms_msm_new,
                        position.FUN = position_msm_new,
                        prep.FUN = prep_msm_new,
                        trans.FUN = hivtrans_msm_new,
                        prevalence.FUN = prevalence_msm_new,
                        verbose.FUN = verbose.net,
                        module.order = NULL,
                        save.nwstats = FALSE,
                        save.other = c("el", "attr"),
                        tergmLite = TRUE,
                        tergmLite.track.duration = FALSE, # CPN2
                        set.control.ergm = control.simulate.formula(MCMC.burnin = 2e5),
                        set.control.stergm = control.simulate.network(),
                        verbose = TRUE,
                        skip.check = TRUE,
                        ...) {
  
  formal.args <- formals(sys.function())
  dot.args <- list(...)
  p <- get_args(formal.args, dot.args)
  
  p$skip.check <- TRUE
  p$save.transmat <- FALSE
  
  bi.mods <- grep(".FUN", names(formal.args), value = TRUE)
  bi.mods <- bi.mods[which(sapply(bi.mods, function(x) !is.null(eval(parse(text = x))),
                                  USE.NAMES = FALSE) == TRUE)]
  p$bi.mods <- bi.mods
  p$user.mods <- grep(".FUN", names(dot.args), value = TRUE)
  p[["f.names"]] <- c(p[["bi.mods"]], p[["user.mods"]])
  p$save.other <- c("attr", "temp", "el", "p", "mpx_degdist")
  
  p$save.network <- FALSE
  if (is.null(p$verbose.int)) {
    p$verbose.int <- 1
  }
  
  p <- set.control.class("control.net", p) # CPN2
  return(p)
}


init_msm <- function(prev.ugc = 0.005,
                     prev.rgc = 0.005,
                     prev.uct = 0.013,
                     prev.rct = 0.013,
                     ...) {
  
  p <- get_args(formal.args = formals(sys.function()),
                dot.args = list(...))
  
  class(p) <- "init.net"
  return(p)
}





