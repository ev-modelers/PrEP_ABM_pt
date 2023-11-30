#HIV PROGRESS

hivprogress_msm_new <- function(dat, at) {
  
  # Attributes
  active <- dat$attr$active
  status <- dat$attr$status
  time.since.inf <- at - dat$attr$inf.time
  cuml.time.on.tx <- dat$attr$cuml.time.on.tx
  cuml.time.off.tx <- dat$attr$cuml.time.off.tx
  stage <- dat$attr$stage
  stage.time <- dat$attr$stage.time
  aids.time <- dat$attr$aids.time
  tt.traj <- dat$attr$tt.traj
  tx.status <- dat$attr$tx.status
  
  # Parameters
  vl.acute.rise.int <- dat$param$vl.acute.rise.int
  vl.acute.fall.int <- dat$param$vl.acute.fall.int
  vl.aids.onset.int <- dat$param$vl.aids.onset.int
  max.time.off.tx.part.int <- dat$param$max.time.off.tx.part.int
  max.time.on.tx.part.int <- dat$param$max.time.on.tx.part.int
  max.time.off.tx.full.int <- dat$param$max.time.off.tx.full.int
  
  
  ## Process
  
  # Increment day
  stage.time[active == 1] <- stage.time[active == 1] + 1
  
  # Change stage to Acute Falling
  toAF <- which(active == 1 & stage == 1 & time.since.inf >= (vl.acute.rise.int + 1))
  stage[toAF] <- 2
  stage.time[toAF] <- 1
  
  # Change stage to Chronic
  toC <- which(active == 1 & stage == 2 & time.since.inf >= (vl.acute.rise.int +
                                                               vl.acute.fall.int + 1))
  stage[toC] <- 3
  stage.time[toC] <- 1
  
  # Change stage to AIDS
  aids.tx.naive <- which(active == 1 & status == 1 & cuml.time.on.tx == 0 &
                           (time.since.inf >= vl.aids.onset.int) & stage != 4)
  
  part.tx.score <- (cuml.time.off.tx / max.time.off.tx.part.int) +
    (cuml.time.on.tx / max.time.on.tx.part.int)
  
  aids.part.escape <- which(active == 1 & cuml.time.on.tx > 0 & tt.traj == 1 &
                              stage == 3 & part.tx.score >= 1 & stage != 4)
  
  aids.off.tx.full.escape <- which(active == 1 & tx.status == 0 & tt.traj %in% 2:3 &
                                     cuml.time.on.tx > 0 &
                                     cuml.time.off.tx >= max.time.off.tx.full.int &
                                     stage != 4)
  
  isAIDS <- c(aids.tx.naive, aids.part.escape, aids.off.tx.full.escape)
  stage[isAIDS] <- 4
  stage.time[isAIDS] <- 1
  aids.time[isAIDS] <- at
  
  ## Output
  dat$attr$stage <- stage
  dat$attr$stage.time <- stage.time
  dat$attr$aids.time <- aids.time
  
  dat$epi$new.aids.tot[at] <- length(isAIDS)
  dat$epi$new.aids.part[at] <- length(aids.part.escape)
  dat$epi$new.aids.full[at] <- length(aids.off.tx.full.escape)
  
  return(dat)
}