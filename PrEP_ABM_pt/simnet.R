simnet_msm_new <- function(dat, at) {
  
  #########################
  ## Nest the edges_correct function
  # (workaround for some functions not being accessible when running in parallel even if in global environ)
  
  edges_correct_msm <- function(dat, at) {
    
    behavior.change         <- get_param(dat, "behavior.change")
    
    if(behavior.change == TRUE){    
      old.num <- dat$epi$num[at - 1]                     #sets number of nodes at prior timestep
      new.num <- sum(dat$attr$active == 1, na.rm = TRUE) #sets number of nodes at this timestep
      
      for (i in 1:length(dat$nwparam)) {
        
        adjust <- log(new.num) - log(old.num)              #calculates log difference between those two
        if(at == 75 & i == 3){adjust <- log(new.num*0.6) - log(old.num)}
        
        coef.form1 <- get_nwparam(dat, network = i)$coef.form #get formation coefficient
        coef.form1[1] <- coef.form1[1] + adjust               #says to increase or decrease formation of edges
        dat$nwparam[[i]]$coef.form <- coef.form1              #re-records this number
      }
      
      return(dat)}
    
    if(behavior.change == FALSE){
      old.num <- dat$epi$num[at - 1]                     #sets number of nodes at prior timestep
      new.num <- sum(dat$attr$active == 1, na.rm = TRUE) #sets number of nodes at this timestep
      adjust <- log(old.num) - log(new.num)              #calculates log difference between those two
      
      for (i in 1:length(dat$nwparam)) {
        
        coef.form1 <- get_nwparam(dat, network = i)$coef.form #get formation coefficient
        coef.form1[1] <- coef.form1[1] + adjust               #says to increase or decrease formation of edges
        dat$nwparam[[i]]$coef.form <- coef.form1              #re-records this number
      }
      
      return(dat)}
    
    
    
  }
  
  
  
  ##end nesting##
  #########################
  
  
  ## Grab parameters from dat object 
  cumulative.edgelist <- get_control(dat, "cumulative.edgelist") # are we tracking the cumulative edgelist (T/F)
  truncate.el.cuml <- get_control(dat, "truncate.el.cuml")       # how long in the past do we keep edgelist
  set.control.stergm <- get_control(dat, "set.control.stergm")   # specific control settings for network simulation
  set.control.ergm <- get_control(dat, "set.control.ergm")       # specific control settings for network simulation
  
  
  # Update edges coefficients
  dat <- edges_correct_msm(dat, at)    # decides what the formation coefficient is
  
  ## Main network
  for (i in 1:length(dat$el)) {    #I believe this is where loops through overlapping networks
    nwparam <- EpiModel::get_nwparam(dat, network = i)   #get parameters of this network
    isTERGM <- ifelse(nwparam$coef.diss$duration > 1, TRUE, FALSE) #set isTERGM to true is relationships non-instantaneous
    
    nwL <- networkLite(dat[["el"]][[i]], dat[["attr"]])
    
    
    if (get_control(dat, "tergmLite.track.duration") == TRUE) { #figure out how long relationships have lasted
      nwL %n% "time" <- dat[["nw"]][[i]] %n% "time"
      nwL %n% "lasttoggle" <- dat[["nw"]][[i]] %n% "lasttoggle"
    }
    
    if (isTERGM == TRUE) { #The following happens only if tergm (relationships last)
      dat[["nw"]][[i]] <- simulate( #forms, dissolves contacts. generic function. simulate distribution corresponding to fitted model object
        nwL, #what are the attributes of each node
        formation = nwparam[["formation"]],
        dissolution = nwparam[["coef.diss"]][["dissolution"]],
        coef.form = nwparam[["coef.form"]],
        coef.diss = nwparam[["coef.diss"]][["coef.adj"]],
        constraints = nwparam[["constraints"]],
        time.start = at - 1,
        time.slices = 1,
        time.offset = 1,
        control = set.control.stergm,
        output = "final")
        
      
    } else {  #The following happens if tergm is false, e.g. instantaneous relationships
      dat[["nw"]][[i]] <- simulate( #forms contacts
        basis = nwL,
        object = nwparam[["formation"]],
        coef = nwparam[["coef.form"]],
        constraints = nwparam[["constraints"]],
        control = set.control.ergm,
        dynamic = FALSE,
        nsim = 1,
        output = "network"
      )
 
    }
    
    dat[["el"]][[i]] <- as.edgelist(dat[["nw"]][[i]])
    
    if (get_control(dat, "save.nwstats") == TRUE) {
      term.options <- if (isTERGM == TRUE) {
        set.control.stergm$term.options
      } else {
        set.control.ergm$term.options
      }
      dat$stats$nwstats[[i]] <- rbind(dat$stats$nwstats[[i]],
                                      summary(dat$control$nwstats.formulas[[i]],
                                              basis = nwL,
                                              term.options = term.options,
                                              dynamic = isTERGM))
    }
    
  }
  
  if (get_control(dat, "cumulative.edgelist") == TRUE) {
    for (n_network in seq_len(3)) {
      dat <- update_cumulative_edgelist(dat, n_network, truncate.el.cuml)
    }
  }
  
  # update main degree (nodal attribute based on network status)
  dat$attr$deg.main <- rep(0,length(dat$attr$deg.main))
  el <- get_edgelist(dat, 1)
  if (nrow(el) > 0) {
    el <- el[sample(1:nrow(el)), , drop = FALSE]
    for(j in 1:nrow(el)){
      dat$attr$deg.main[el[j,1]] <- 1
      dat$attr$deg.main[el[j,2]] <- 1
    }
  }
  
 
  
  # adjust pers degree
  dat$attr$deg.pers <- rep(0,length(dat$attr$deg.pers))
  el <- get_edgelist(dat, 2)
  if (nrow(el) > 0) {
    el <- el[sample(1:nrow(el)), , drop = FALSE]
    for(j in 1:nrow(el)){
      dat$attr$deg.pers[el[j,1]] <- dat$attr$deg.pers[el[j,1]] + 1
      if(dat$attr$deg.pers[el[j,1]] > 2){dat$attr$deg.pers[el[j,1]] <- 2}
      dat$attr$deg.pers[el[j,2]] <- dat$attr$deg.pers[el[j,2]] + 1
      if(dat$attr$deg.pers[el[j,2]] > 2){dat$attr$deg.pers[el[j,2]] <- 2}
    }
  }
  
  plist1 <- update_plist(dat, at, ptype = 1)
  plist2 <- update_plist(dat, at, ptype = 2)
  dat$temp$plist <- rbind(plist1, plist2)
  if (TRUE) {
    to.keep <- which(is.na(dat$temp$plist[, "stop"]))
    dat$temp$plist <- dat$temp$plist[to.keep, ]
  }
  
  return(dat)
}




update_plist <- function(dat, at, ptype) {
  # pull existing partner type specific list
  plist1 <- dat$temp$plist[dat$temp$plist[, "ptype"] == ptype, ]
  
  # look up dissolutions, update stop time
  uid <- dat$attr$uid
  news <- attr(dat$el[[ptype]], "changes")
  if(is.null(news)){
    return(plist1)
  }
  print(news)
  news_uid <- cbind(matrix(uid[news[, 1:2]], ncol = 2), news[, 3])
  news_uid_stop <- news_uid[news_uid[, 3] == 0, , drop = FALSE]
  pid_plist1 <- plist1[, 1]*1e7 + plist1[, 2]
  pid_stop <- news_uid_stop[, 1]*1e7 + news_uid_stop[, 2]
  matches_stop <- match(pid_stop, pid_plist1)
  plist1[matches_stop, "stop"] <- at
  
  # look up new formations, row bind them
  news_uid_start <- news_uid[news_uid[, 3] == 1, , drop = FALSE]
  print(news_uid_start[, 1:2, drop = FALSE],ptype,at)
  plist1 <- rbind(plist1, cbind(news_uid_start[, 1:2, drop = FALSE], ptype, at, NA))
  
  return(plist1)
}




