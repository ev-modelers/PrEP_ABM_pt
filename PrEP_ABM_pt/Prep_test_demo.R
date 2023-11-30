#test 
param_noprep <- param_msm(population.size = 1000, # Clinical
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
                                  a.rate = 0.00052,
                                  arrival.age = 15,
                                  
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
                                  acts.scale = 1.5,
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
                                  senario='US')


param_prep <- param_msm(population.size = 1000, # Clinical
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
                          a.rate = 0.00052,
                          arrival.age = 15,
                          
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
                          acts.scale = 1.5,
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
                          prep.start.prob = 1,
                          prep.adhr.dist = c(0.089, 0.127, 0.784),
                          prep.adhr.hr = c(0.69, 0.19, 0.01),
                          prep.discont.rate = 1 - (2^(-1/(224.4237/7))),
                          prep.tst.int = 90/7,
                          prep.risk.int = 182/7,
                          prep.sti.screen.int = 182/7,
                          prep.sti.prob.tx = 1,
                          prep.risk.reassess.method = "year",
                          prep.require.lnt = TRUE,
                          senario='US')



results_prep<-list()
results_noprep<-list()

for(i in 1:10) {
  try(results_prep[[i]] <- EpiModel::netsim(est, param_prep, inits, controls) )
  results_prep[[i]]<-as.data.frame(results_prep[[i]])
}

for(i in 1:10) {
  try(results_noprep[[i]] <- EpiModel::netsim(est, param_noprep, inits, controls)) 
  results_noprep[[i]]<-as.data.frame(results_noprep[[i]])
}


results_prep_frame<-data.frame(lapply(results_prep, function(x) x$i.num))
results_noprep_frame<-data.frame(lapply(results_noprep, function(x) x$i.num))

mean(results_prep_frame[200,])
mean(results_noprep_frame[200,])


calculate_summary <- function(x) {
  c(median = median(x, na.rm = TRUE),
    lower = quantile(x, 0.25, na.rm = TRUE),
    upper = quantile(x, 0.75, na.rm = TRUE))
}


time_vector <- 1:nrow(results_prep_frame)
results_prep_frame$time <- time_vector
results_noprep_frame$time <- time_vector

##NO PROBLEM IN PREP


df <- bind_rows(
  mutate(results_prep_frame, scenario = "prep"),
  mutate(results_noprep_frame, scenario = "noprep")
)

df<-na.omit(df)


ggplot(df, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3) +
  theme_minimal() +
  xlab("Prevalence") +
  ylab("Value") +
  scale_y_continuous(limits = c(0, 0.2)) +
  labs(color = "Scenario")

