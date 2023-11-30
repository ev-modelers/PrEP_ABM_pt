condoms_msm_new <- function(dat, at) {
  
  # Attributes
  race <- dat$attr$race
  age <- dat$attr$age
  diag.status <- dat$attr$diag.status
  prepStat <- dat$attr$prepStat
  
  
  epistats<-readRDS(file.path("epistats.rda"))
  # Condom Use Models
  cond.mc.mod <- epistats$cond.mc.mod
  cond.oo.mod <- epistats$cond.oo.mod

  cond.scale <- dat$param$cond.scale
  
  # Temp edgelist
  el <- dat$temp$el

  race.combo <- rep(NA, nrow(el))
  race.combo[race[el[, 1]] == 1 & race[el[, 2]] == 1] <- 1
  race.combo[race[el[, 1]] == 1 & race[el[, 2]] %in% 2:3] <- 2
  race.combo[race[el[, 1]] == 2 & race[el[, 2]] %in% c(1, 3)] <- 3
  race.combo[race[el[, 1]] == 2 & race[el[, 2]] == 2] <- 4
  race.combo[race[el[, 1]] == 3 & race[el[, 2]] %in% 1:2] <- 5
  race.combo[race[el[, 1]] == 3 & race[el[, 2]] == 3] <- 6
  
  comb.age <- age[el[, 1]] + age[el[, 2]]
  
  hiv.concord.pos <- rep(0, nrow(el))
  cp <- which(diag.status[el[, 1]] == 1 & diag.status[el[, 2]] == 1)
  hiv.concord.pos[cp] <- 1
  
  any.prep <- as.numeric((prepStat[el[, 1]] + prepStat[el[, 2]]) > 0)
  
  ## Main/casual partnerships ##
  mc.parts <- which(el[, "ptype"] != 3)
  el.mc <- el[mc.parts, ]
  
  #x <- data.frame(ptype = el.mc[, "ptype"],
                  #duration = el.mc[, "durations"],
                  #race.combo = race.combo[mc.parts],
                  #comb.age = comb.age[mc.parts],
                  #hiv.concord.pos = hiv.concord.pos[mc.parts],
                  #prep = any.prep[mc.parts],
                  #city = 1)
  #cond.prob <- unname(predict(cond.mc.mod, newdata = x, type = "response"))
  cond.prob<-rep(0.5,nrow(el.mc))
  el.mc <- cbind(el.mc, cond.prob)
  
  ## One-off partnerships ##
  oo.parts <- which(el[, "ptype"] == 3)
  el.oo <- el[oo.parts, ]
  #x <- data.frame(race.combo = race.combo[oo.parts],
  #                comb.age = comb.age[oo.parts],
  #                hiv.concord.pos = hiv.concord.pos[oo.parts],
  #                prep = any.prep[oo.parts],
  #                city = 1)
  #cond.prob <- unname(predict(cond.oo.mod, newdata = x, type = "response"))
  cond.prob<-rep(0.5,nrow(el.oo))
  
  el.oo <- cbind(el.oo, cond.prob)
  
  ## Bind el together
  el <- rbind(el.mc, el.oo)
  
  # Acts
  ai.vec <- el[, "ai"]
  
  #if (nrow(el[, "ai"]) == 0) {
  #  return(dat)
  #}


  pid <- rep(1:length(ai.vec), ai.vec)
  p1 <- rep(el[, "p1"], ai.vec)
  p2 <- rep(el[, "p2"], ai.vec)
  ptype <- rep(el[, "ptype"], ai.vec)
  cond.prob <- rep(el[, "cond.prob"], ai.vec)
  

  #p1 <- el[, "p1"]
  #p2 <- el[, "p2"]
  #ptype <- el[, "ptype"]
  #cond.prob <- ai.vec*0.5
  
  

  cond.prob <- cond.prob * cond.scale
  

  # UAI draw per act
  uai <- rbinom(length(cond.prob), 1, 1 - cond.prob)
  
  # Act list construction
  al <- cbind(p1, p2, ptype, uai, pid)
  dat$temp$al <- al
  dat$temp$el <- el

  
  return(dat)
}