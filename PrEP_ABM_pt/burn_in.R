library(EasyABC)



source("Initialization.R")
source("Agging.R")
source("sexact.R")
source("hivtest.R")
source("HIVTreatment.R")
source("HIV_progress.R")
source("HIVVL.R")
source("condom.R")
source("position.R")
source("prep.R")
source("hivtrans.R")
source("simnet.R")
source("Prevalence.R")
source("basic.R")



get_params <- function(x) {
  tryCatch({
    set.seed(x[1])
    params <- param_msm(startprev = 0.2,a.rate = x[2],acts.scale = x[3],senario='US')
    inits <- EpiModelHIV::init_msm()
    controls <- control_msm(nsteps = 600, nsims=1, ncores=1)
    sim <- EpiModel::netsim(est, params, inits, controls) 
    df <- tail(as.data.frame(sim), 200)
    out1 <- mean(df$i.prev,na.rm = TRUE)
    out2 <- unname(coef(lm(df$i.prev ~ seq_along(df$time)))[2])
    out<- c(out1, out2)
    return(out)
  }, error = function(e) {
    message("An error occurred: ", e$message)
    return(list(out1 = NA, out2 = NA))
  })
}

priors <- list(c("unif", 0.0004, 0.0007),c("unif", 0.5, 1.8))
targets <- c(0.12, 0)


#NETWORKPARAM need population size and the proportion of sexual role.
network_info<-Networkparam(1000,c(0.1,0.3,0.6))
st<-network_info$st
est<-network_info$est

a <- ABC_sequential(method = "Lenormand",
                    model = get_params,
                    prior = priors,
                    nb_simul = 10 ,
                    summary_stat_target = targets,
                    progress_bar = TRUE,
                    p_acc_min = 0.2,
                    use_seed = TRUE)






