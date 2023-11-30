#'WHO', 'EACS', 'PNHS', 'US'


save()

params_senario <- function(x){
  q<-param_msm(population.size = 1000, # Clinical
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
               prep.start.prob = 0.2,
               prep.adhr.dist = c(0.5, 0, 0.5),
               prep.adhr.hr = c(0.85, 1, 0.1),
               prep.discont.rate = 1 - (2^(-1/(224.4237/7))),
               prep.tst.int = 90/7,
               prep.risk.int = 182/7,
               prep.sti.screen.int = 182/7,
               prep.sti.prob.tx = 1,
               prep.risk.reassess.method = "year",
               prep.require.lnt = TRUE,
               senario = x)
  return(q)
  }
controls <- control_msm(nsteps = 1500, nsims=1, ncores=1, 
                        initialize.FUN = initialize_msm_new,
                        infection.FUN = NULL,
                        aging.FUN =aging_msm,
                        arrival.FUN = arrival_msm,
                        depart.FUN = departure_msm_new,
                        HIVtest.FUN = hivtest_msm_new,
                        HIVtx.FUN = hivtx_msm_new,
                        HIVprog.FUN = hivprogress_msm_new,
                        HIVvvl.FUN = hivvl_msm_new,
                        progress.FUN = NULL,
                        resim_nets.FUN = simnet_msm_new,
                        acts.FUN = acts_msm_new,
                        condom.FUN = condoms_msm_new,
                        prep.FUN = prep_msm_new,
                        prev.FUN = NULL,
                        position.FUN = position_msm_new,
                        trans.FUN = hivtrans_msm_new,
                        prevalence.FUN = prevalence_msm_new,
                        verbose.FUN = verbose.net)






# Initialize the lists
results_who <- list()
results_EACS <- list()
results_PNHS <- list()
results_US <- list()

# Number of simulations

# Run the simulations
for(i in 1:25) {
  try({
    # WHO scenario
    param_who <- params_senario('WHO')
    results_who[[i]] <- EpiModel::netsim(est, param_who, inits, controls)
    
    # EACS scenario
    param_EACS <- params_senario('EACS')
    results_EACS[[i]] <- EpiModel::netsim(est, param_EACS, inits, controls)
    
    # PNHS scenario
    param_PNHS <- params_senario('PNHS')
    results_PNHS[[i]] <- EpiModel::netsim(est, param_PNHS, inits, controls)
    
    # US scenario
    param_US <- params_senario('US')
    results_US[[i]] <- EpiModel::netsim(est, param_US, inits, controls)
  }, silent = TRUE)
}

for (i in 1:25){
  results_who[[i]]<-as.data.frame(results_who[[i]])
  results_EACS[[i]]<-as.data.frame(results_EACS[[i]])
  results_PNHS[[i]]<-as.data.frame(results_PNHS[[i]])
  results_US[[i]]<-as.data.frame(results_US[[i]])
}



save(results_who, file = "results_who_final_25.RData")
save(results_EACS, file = "results_EACS_final_25.RData")
save(results_PNHS, file = "results_PNHS_final_25.RData")
save(results_US, file = "results_US_final_25.RData")



load("results_who.RData")
load("results_EACS.RData")
load("results_PNHS.RData")
load("results_US.RData")







results_who_prev <- data.frame(lapply(results_who, function(x) x$i.prev))
results_EACS_prev <- data.frame(lapply(results_EACS, function(x) x$i.prev))
results_PNHS_prev <- data.frame(lapply(results_PNHS, function(x) x$i.prev))
results_US_prev <- data.frame(lapply(results_US, function(x) x$i.prev))
results_prev <- data.frame(lapply(results, function(x) x$i.prev))



# Define function to calculate median, lower and upper interval
calculate_summary <- function(x) {
  c(median = median(x, na.rm = TRUE),
    lower = quantile(x, 0.25, na.rm = TRUE),
    upper = quantile(x, 0.75, na.rm = TRUE))
}

# Apply function to each row
results_who_summary <- data.frame(t(apply(results_who_prev, 1, calculate_summary)))
results_EACS_summary <- data.frame(t(apply(results_EACS_prev, 1, calculate_summary)))
results_PNHS_summary <- data.frame(t(apply(results_PNHS_prev, 1, calculate_summary)))
results_US_summary <- data.frame(t(apply(results_US_prev, 1, calculate_summary)))
results_summary <- data.frame(t(apply(results_prev, 1, calculate_summary)))



time_vector <- 1:nrow(results_US_summary)

# Add time vector to each dataframe
results_who_summary$time <- time_vector
results_EACS_summary$time <- time_vector
results_PNHS_summary$time <- time_vector
results_US_summary$time <- time_vector
results_summary$time <- time_vector



df <- bind_rows(
  mutate(results_who_summary, scenario = "WHO"),
  mutate(results_EACS_summary, scenario = "EACS"),
  mutate(results_PNHS_summary, scenario = "PNHS"),
  mutate(results_US_summary, scenario = "US"),
  mutate(results_summary,scenario='NO PrEP')
)

df<-na.omit(df)

# Add time to the df


policyprevalence<-ggplot(df, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3)+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500),  # specify the positions of the breaks
                     labels = c('2010', '2020', '2030', '2040'))+
  scale_fill_npg() +
  theme_minimal() +
  xlab("Time") +
  ylab("Prevalence") +
  scale_y_continuous(limits = c(0, 0.2)) +
  labs(color = "scenario")+scale_color_npg()  + theme_classic()+
  theme(legend.key.width = unit(1.5, "cm"),
        panel.grid = element_blank(),
        legend.text = element_text(face="bold",size = 10),
        legend.title = element_text(face="bold",size = 12),
        axis.text.x = element_text(face="bold",size = 14),
        axis.text.y = element_text(face="bold",size = 14),
        axis.title.x = element_text(face="bold",size = 15),
        axis.title.y = element_text(face="bold",size = 15))

ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/policyprevalence.png',policyprevalence, height = 4 , width = 7)













ggplot(df, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3) +
  geom_vline(xintercept = 500, linetype = "dashed") +
  geom_text(x = 250, y = 0.175, label = "Burn in", hjust = 1, vjust = 0.5,size =6) +
  theme_minimal() +
  xlab("Time") +
  ylab("Prevalence") +
  scale_y_continuous(limits = c(0, 0.2)) +
  labs(color = "Scenario")+
  theme(axis.title.y = element_text(size = 20))








ggplot(df, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab("Prevalence") +
  scale_y_continuous(limits = c(0, 0.2)) +
  labs(color = "Scenario")

