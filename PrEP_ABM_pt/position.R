#position
position_msm_new <- function(dat, at) {
  
  al <- dat$temp$al

  if (nrow(al) == 0) {
    return(dat)
  }
  
  # Attributes
  role.class <- dat$attr$role.class
  ins.quot <- dat$attr$ins.quot
  
  # Parameters
  
  ## Process
  p1.role.class <- role.class[al[, "p1"]]
  p2.role.class <- role.class[al[, "p2"]]
  
  ins <- rep(NA, length(p1.role.class))
  ins[which(p1.role.class == 0)] <- 1
  ins[which(p1.role.class == 1)] <- 0
  ins[which(p2.role.class == 0)] <- 0
  ins[which(p2.role.class == 1)] <- 1
  
  # Versatile MSM
  vv <- which(p1.role.class == 2 & p2.role.class == 2)
  p1.ins.prob <- ins.quot[al[, 1][vv]] /
    (ins.quot[al[, 1][vv]] + ins.quot[al[, 2][vv]])
  p1.ins <- rbinom(length(vv), 1, p1.ins.prob)
  ins[vv[p1.ins == 1]] <- 1
  ins[vv[p1.ins == 0]] <- 0
  
  ## Output
  dat$temp$al <- cbind(al, ins)
  
  return(dat)
}
