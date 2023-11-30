#low adhearence

params_low<- param_msm(population.size = 1000, # Clinical
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
                    prep.adhr.dist = c(0.8, 0, 0.2),
                    prep.adhr.hr = c(0.85, 0, 0.1),
                    prep.discont.rate = 1 - (2^(-1/(224.4237/7))),
                    prep.tst.int = 90/7,
                    prep.risk.int = 182/7,
                    prep.sti.screen.int = 182/7,
                    prep.sti.prob.tx = 1,
                    prep.risk.reassess.method = "year",
                    prep.require.lnt = TRUE,
                    senario='PNHS')

params_mid<- param_msm(population.size = 1000, # Clinical
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
                       prep.adhr.hr = c(0.85, 0, 0.1),
                       prep.discont.rate = 1 - (2^(-1/(224.4237/7))),
                       prep.tst.int = 90/7,
                       prep.risk.int = 182/7,
                       prep.sti.screen.int = 182/7,
                       prep.sti.prob.tx = 1,
                       prep.risk.reassess.method = "year",
                       prep.require.lnt = TRUE,
                       senario='PNHS')


params_high<- param_msm(population.size = 1000, # Clinical
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
                       prep.adhr.dist = c(0.1, 0, 0.9),
                       prep.adhr.hr = c(0.85, 0, 0.1),
                       prep.discont.rate = 1 - (2^(-1/(224.4237/7))),
                       prep.tst.int = 90/7,
                       prep.risk.int = 182/7,
                       prep.sti.screen.int = 182/7,
                       prep.sti.prob.tx = 1,
                       prep.risk.reassess.method = "year",
                       prep.require.lnt = TRUE,
                       senario='PNHS')





result_lowprep<-list()
for (i in 1:25){
  try(
    result_lowprep[[i]]<-EpiModel::netsim(est, params_low, inits, controls)
  )
  result_lowprep[[i]]<-as.data.frame(result_lowprep[[i]])
}
save(result_lowprep, file = "result_lowprep_late.RData")


result_midprep<-list()
for (i in 1:25){
  try(
    result_midprep[[i]]<-EpiModel::netsim(est, params_mid, inits, controls)
  )
  result_midprep[[i]]<-as.data.frame(result_midprep[[i]])
}
save(result_midprep, file = "result_midprep_late.RData")



result_highprep<-list()
for (i in 1:25){
  try(
    result_highprep[[i]]<-EpiModel::netsim(est, params_high, inits, controls)
  )
  result_highprep[[i]]<-as.data.frame(result_highprep[[i]])
}
save(result_highprep, file = "result_highprep_late.RData")

#Now let's get the table of cases and prevalence



###
get_the_prevnum<-function(z){
  num_table <- data.frame(lapply(z, function(x) x$i.prev))
  summary_z<-data.frame(t(apply(num_table, 1, calculate_summary)))
  time_vector <- 1:nrow(summary_z)
  summary_z$time <- time_vector 
  return(summary_z)
}

summary_no_prep_prev<-get_the_prevnum(results)
summary_low_prep_prev<-get_the_prevnum(result_lowprep)
summary_mid_prep_prev<-get_the_prevnum(result_midprep)
summary_high_prep_prev<-get_the_prevnum(result_highprep)




prev_number_sensi <- bind_rows(
  mutate(summary_high_prep_prev, scenario = "High adhr"),
  mutate(summary_mid_prep_prev, scenario = "Mid adhr"),
  mutate(summary_low_prep_prev, scenario = "Low adhr"),
  mutate(summary_no_prep_prev,scenario='NO PrEP')
)
prev_number_sensi<-na.omit(prev_number_sensi)





sensi_prev<-
  ggplot(prev_number_sensi, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3) +scale_fill_npg() +
  geom_vline(xintercept = 500, linetype = "dashed") +
  geom_text(x = 250, y = 0.1, label = "Burn in", hjust = 1, vjust = 0.5,size =6)+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500),  # specify the positions of the breaks
                     labels = c('2010', '2020', '2030', '2040'))   +
  theme_minimal() +
  xlab("Time") +
  ylab("Prevalence") +
  labs(color = "scenario")+
  theme(axis.title.y = element_text(size = 20))+scale_color_npg()  + theme_classic()+
  theme(legend.key.width = unit(1.5, "cm"),
        panel.grid = element_blank(),
        legend.text = element_text(face="bold",size = 10),
        legend.title = element_text(face="bold",size = 12),
        axis.text.x = element_text(face="bold",size = 14),
        axis.text.y = element_text(face="bold",size = 14),
        axis.title.x = element_text(face="bold",size = 15),
        axis.title.y = element_text(face="bold",size = 15))
sensi_prev
ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/ssensi_prev.png',sensi_prev, height = 4 , width = 7)







######
get_the_inum<-function(z){
  num_table <- data.frame(lapply(z, function(x) x$i.num))
  summary_z<-data.frame(t(apply(num_table, 1, calculate_summary)))
  time_vector <- 1:nrow(summary_z)
  summary_z$time <- time_vector 
  return(summary_z)
}

summary_no_prep<-get_the_inum(results)
summary_low_prep<-get_the_inum(result_lowprep)
summary_mid_prep<-get_the_inum(result_midprep)
summary_high_prep<-get_the_inum(result_highprep)




I_number_sensi <- bind_rows(
  mutate(summary_high_prep, scenario = "High adhr"),
  mutate(summary_mid_prep, scenario = "Mid adhr"),
  mutate(summary_low_prep, scenario = "Low adhr"),
  mutate(summary_no_prep,scenario='NO PrEP')
)
I_number_sensi<-na.omit(I_number_sensi)






sensi_infection<-ggplot(I_number_sensi, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3) +scale_fill_npg() +
  geom_vline(xintercept = 500, linetype = "dashed") +
  geom_text(x = 250, y = 200, label = "Burn in", hjust = 1, vjust = 0.5,size =6) +
  scale_x_continuous(breaks = c(0, 500, 1000, 1500),  # specify the positions of the breaks
                     labels = c('2010', '2020', '2030', '2040')) +
  theme_minimal() +
  xlab("Time") +
  ylab("Infection") +
  labs(color = "scenario")+
  theme(axis.title.y = element_text(size = 20))+scale_color_npg()  + theme_classic()+
  theme(legend.key.width = unit(1.5, "cm"),
        panel.grid = element_blank(),
        legend.text = element_text(face="bold",size = 10),
        legend.title = element_text(face="bold",size = 12),
        axis.text.x = element_text(face="bold",size = 14),
        axis.text.y = element_text(face="bold",size = 14),
        axis.title.x = element_text(face="bold",size = 15),
        axis.title.y = element_text(face="bold",size = 15))
sensi_infection
ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/sensi_infection.png',sensi_infection, height = 4 , width = 7)


#######TABlE OF AVERTED
Averted_table<-I_number_sensi%>%
  filter(scenario!='NO PrEP')%>%
  filter(time>500)
Averted_table$median<-186-Averted_table$median
Averted_table$lower.25.<-186-Averted_table$lower.25.
Averted_table$upper.75.<-186-Averted_table$upper.75.


sensi_avert<-ggplot(Averted_table, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = upper.75., ymax = lower.25. , fill = scenario), alpha = 0.3) +scale_fill_npg() +
  scale_x_continuous(breaks = c(500, 750, 1000, 1250,1500),  # specify the positions of the breaks
                     labels = c('2020','2025', '2030','2035', '2040')) +
  theme_minimal() +
  xlab("Time") +
  ylab("Number of averted infection") +
  labs(color = "scenario")+
  theme(axis.title.y = element_text(size = 20))+scale_color_npg()  + theme_classic()+
  theme(legend.key.width = unit(1.5, "cm"),
        panel.grid = element_blank(),
        legend.text = element_text(face="bold",size = 10),
        legend.title = element_text(face="bold",size = 12),
        axis.text.x = element_text(face="bold",size = 14),
        axis.text.y = element_text(face="bold",size = 14),
        axis.title.x = element_text(face="bold",size = 15),
        axis.title.y = element_text(face="bold",size = 15))
  sensi_avert
ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/sensi_avert.png',sensi_avert, height = 4 , width = 7)


###GET THE TABLE

Averted_table_wide <- Averted_table %>%
  filter(time %% 100 == 0) %>%
  pivot_wider(names_from = scenario, values_from = c(median, lower.25., upper.75.))
Averted_table_wide$time<-(Averted_table_wide$time-500)/50


library(gt)

gt_table_adhr_avert <- gt(Averted_table_wide) %>%
  cols_label(
    time = "Year",
    "lower.25._Low adhr" = "75th Percentile",
    "median_Low adhr" = "Median",
    "upper.75._Low adhr" = "25th Percentile",

    "lower.25._Mid adhr" = "75th Percentile",
    "median_Mid adhr" = "Median",
    "upper.75._Mid adhr" = "25th Percentile",
    
    "lower.25._High adhr" = "75th Percentile",
    "median_High adhr" = "Median",
    "upper.75._High adhr" = "25th Percentile"
  ) %>%
  tab_header(
    title = "Number of infections averted by different PrEP adherence over 20 years"
  ) %>%
  tab_spanner(
    label = "Low Adherence",
    columns = c( "upper.75._Low adhr","median_Low adhr", "lower.25._Low adhr")
  )%>%
  tab_spanner(
    label = "Middle Adherence",
    columns = c( "upper.75._Mid adhr", "median_Mid adhr","lower.25._Mid adhr")
  )  %>%
  tab_spanner(
    label = "High Adherence",
    columns = c( "upper.75._High adhr","median_High adhr", "lower.25._High adhr")
  )
gtsave(gt_table_adhr_avert,  "/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/avertsensitable.png")


gt_table_adhr_avert




# LETS get a cost table









#CALCULATE THE AVERTED cases of the differenr scenario


summary_low_prep_avert<-summary_no_prep-summary_low_prep
summary_mid_prep_avert<-summary_no_prep-summary_mid_prep
summary_high_prep_avert<-summary_no_prep-summary_high_prep


Avert_case <- bind_rows(
  mutate(summary_high_prep_avert, scenario = "High adhr"),
  mutate(summary_mid_prep_avert, scenario = "Mid adhr"),
  mutate(summary_low_prep_avert, scenario = "Low adhr")
)

Avert_case<-na.omit(Avert_case)

Avert_case<- Avert_case%>%
  group_by(scenario) %>%
  mutate(time = row_number()) 

ggplot(Avert_case, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_vline(xintercept = 500, linetype = "dashed") +
  geom_text(x = 250, y = 160, label = "Burn in", hjust = 1, vjust = 0.5,size =6) +
  theme_minimal() +
  xlab("Time") +
  ylab("Averted cases") +
  labs(color = "Scenario")+
  theme(axis.title.y = element_text(size = 20))




#GET the distribution of last time point












summary_no_prep_table<-data.frame(lapply(results, function(x) x$i.num))
summary_low_prep_table<-data.frame(lapply(result_lowprep, function(x) x$i.num))
summary_mid_prep_table<-data.frame(lapply(result_midprep, function(x) x$i.num))
summary_high_prep_table<-data.frame(lapply(result_highprep, function(x) x$i.num))


colnames(summary_no_prep_table)<-NULL
colnames(summary_low_prep_table)<-NULL
colnames(summary_mid_prep_table)<-NULL
colnames(summary_high_prep_table)<-NULL


last_time_sensitive_case <- bind_rows(
  mutate(data.frame(t(tail(summary_high_prep_table, 1))), scenario = "High adhr"),
  mutate(data.frame(t(tail(summary_mid_prep_table, 1))), scenario = "Mid adhr"),
  mutate(data.frame(t(tail(summary_low_prep_table, 1))), scenario = "Low adhr"),
  mutate(data.frame(t(tail(summary_no_prep_table, 1))), scenario = "No PrEP")
) 


# Reset the levels of the "scenario" variable
last_time_sensitive_case$scenario <- factor(last_time_sensitive_case$scenario, levels = c("No PrEP", "Low adhr", "Mid adhr", "High adhr"))
levels(last_time_sensitive_case$scenario) <- c("No PrEP", "Low", "Middle", "High")

last_time_sensitive_case%>%
  filter(scenario == 'No PrEP')%>%
  summarise(median_X1500 = median(X1500, na.rm = TRUE))

last_time_sensitive_case$X1500<-last_time_sensitive_case$X1500-182


Avert_case_final<-last_time_sensitive_case %>%
  filter(scenario != 'No PrEP') %>%
  ggplot(aes(scenario, -X1500)) +
  geom_violin(aes(fill = scenario)) +
  ylab('Avert Cases after 20 years') +
  labs(x = "Level of Adherence") +
  scale_fill_npg(name = "Level") +  # Change legend title here
  theme_classic() +
  scale_y_continuous(breaks = c(-50,-25, 0,25, 50, 75)) + # Custom y ticks
  # Custom y ticks
  theme(panel.grid = element_blank(),
        legend.text = element_text(face="bold", size = 10),
        legend.title = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size = 14),
        axis.text.y = element_text(face="bold", size = 14),
        axis.title.x = element_text(face="bold", size = 15),
        axis.title.y = element_text(face="bold", size = 15))

Avert_case_final
ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/avertcase_sensi.png',Avert_case_final, height = 4 , width = 6)



#prevalence violin






summary_no_prep_table_prev<-data.frame(lapply(results, function(x) x$i.prev))
summary_low_prep_table_prev<-data.frame(lapply(result_lowprep, function(x) x$i.prev))
summary_mid_prep_table_prev<-data.frame(lapply(result_midprep, function(x) x$i.prev))
summary_high_prep_table_prev<-data.frame(lapply(result_highprep, function(x) x$i.prev))


colnames(summary_no_prep_table_prev)<-NULL
colnames(summary_low_prep_table_prev)<-NULL
colnames(summary_mid_prep_table_prev)<-NULL
colnames(summary_high_prep_table_prev)<-NULL


last_time_sensitive_prev <- bind_rows(
  mutate(data.frame(t(tail(summary_high_prep_table_prev, 1))), scenario = "High adhr"),
  mutate(data.frame(t(tail(summary_mid_prep_table_prev, 1))), scenario = "Mid adhr"),
  mutate(data.frame(t(tail(summary_low_prep_table_prev, 1))), scenario = "Low adhr"),
  mutate(data.frame(t(tail(summary_no_prep_table_prev, 1))), scenario = "No PrEP")
) 

last_time_sensitive_prev

last_time_sensitive_prev$scenario <- factor(last_time_sensitive_prev$scenario, levels = c("No PrEP", "Low adhr", "Mid adhr", "High adhr"))

ggplot(last_time_sensitive_prev,aes((scenario),X1500))+
  geom_violin(aes(fill = (scenario)))+
  ylab('Prevalence')




#############MADE THE COST EFFECTIVENESS ANALYSIS




###FUNCTION FOR QALY LOSS
combine_i_nums <- function(z) {
  # Initialize an empty dataframe to store the results
  combined_df <- data.frame()
  # Loop through each element in result_lowprep
  for (i in seq_along(z)) {
    # Extract the i.num column from the current element
    i_num <- z[[i]]$i.num
    i_num<-as.data.frame(i_num)
    i_num[is.na(i_num)]<-1
    # Multiply each number by 0.2/52
    i_num_scaled <- i_num * 0.2/52
    
    # Calculate cumulative sum in each column
    i_num_cumsum <- cumsum(i_num_scaled)
    if (nrow(combined_df) == 0) {
      combined_df <- i_num_cumsum
    } else {
      combined_df <- cbind(combined_df, i_num_cumsum)
    }
  }
  

  # Return the combined dataframe
  return(combined_df)
}


all_no_prep_QALYloss<-combine_i_nums(results)
all_low_prep_QALYloss<-combine_i_nums(result_lowprep)
all_mid_prep_QALYloss<-combine_i_nums(result_midprep)
all_high_prep_QALYloss<-combine_i_nums(result_highprep)


###FUNCTION FOR QALY GAIND
combine_nums <- function(z) {
  # Initialize an empty dataframe to store the results
  combined_df <- data.frame()
  # Loop through each element in result_lowprep
  for (i in seq_along(z)) {
    # Extract the i.num column from the current element
    num <- z[[i]]$num
    num<-as.data.frame(num)
    # Multiply each number by 0.2/52
    i_num_scaled <- num * 1/52
    
    # Calculate cumulative sum in each column
    i_num_cumsum <- cumsum(i_num_scaled)
    if (nrow(combined_df) == 0) {
      combined_df <- i_num_cumsum
    } else {
      combined_df <- cbind(combined_df, i_num_cumsum)
    }
  }
  
  # Return the combined dataframe
  return(combined_df)
}


all_no_prep_QALYnum<-combine_nums(results)
all_low_prep_QALYnum<-combine_nums(result_lowprep)
all_mid_prep_QALYnum<-combine_nums(result_midprep)
all_high_prep_QALYnum<-combine_nums(result_highprep)




##GET QALY

all_no_prep_QALY<-all_no_prep_QALYnum-all_no_prep_QALYloss
all_low_prep_QALY<-all_low_prep_QALYnum-all_low_prep_QALYloss
all_mid_prep_QALY<-all_mid_prep_QALYnum-all_mid_prep_QALYloss
all_high_prep_QALY<-all_high_prep_QALYnum-all_high_prep_QALYloss




####GET TOTAL COST


combine_prep <- function(z) {
  # Initialize an empty dataframe to store the results
  combined_df <- data.frame()
  # Loop through each element in result_lowprep
  for (i in seq_along(z)) {
    # Extract the i.num column from the current element
    prep_num <- z[[i]]$prepCurr
    prep_num<-as.data.frame(prep_num)
    prep_num[is.na(prep_num)]<-1
    # Multiply each number by 0.2/52
    prep_num_scaled <- prep_num * 10
    
    # Calculate cumulative sum in each column
    prep_num_cumsum <- cumsum(prep_num_scaled)
    if (nrow(combined_df) == 0) {
      combined_df <- prep_num_cumsum
    } else {
      combined_df <- cbind(combined_df, prep_num_cumsum)
    }
  }
  
  
  # Return the combined dataframe
  return(combined_df)
}




all_no_prep_prepnum<-combine_prep(results)
all_low_prep_prepnum<-combine_prep(result_lowprep)
all_mid_prep_prepnum<-combine_prep(result_midprep)
all_high_prep_prepnum<-combine_prep(result_highprep)





all_no_prep_art<-all_no_prep_QALYnum*52*0.8*200
all_low_prep_art<-all_low_prep_QALYnum*52*0.8*200
all_mid_prep_art<-all_mid_prep_QALYnum*52*0.8*200
all_high_prep_art<-all_high_prep_QALYnum*52*0.8*200


total_cost_noprep<-all_no_prep_prepnum+all_no_prep_art
total_cost_lowprep<-all_low_prep_prepnum+all_low_prep_art
total_cost_midprep<-all_mid_prep_prepnum+all_mid_prep_art
total_cost_highprep<-all_high_prep_prepnum+all_high_prep_art

rowMedians <- function(data) {
  apply(data, 1, median)
}

Median_total_cost_noprep<-rowMedians(total_cost_noprep)
Median_total_QALY_noprep<-rowMedians(all_no_prep_QALY)


Median_total_cost_highprep<-rowMedians(total_cost_highprep)
Median_total_QALY_highprep<-rowMedians(all_high_prep_QALY)
#####GET ICER TABLE
Median_ICER<-(Median_total_cost_highprep-Median_total_cost_noprep)/(Median_total_QALY_highprep-Median_total_QALY_noprep)

dataframe_median_ICER<-data.frame(Median_ICER,Median_total_cost_noprep,Median_total_QALY_noprep,Median_total_cost_highprep,Median_total_QALY_highprep,Median_total_QALY_highprep)
dataframe_median_ICER$time<-1:nrow(dataframe_median_ICER)

dataframe_median_ICER<-dataframe_median_ICER%>%
  filter(time>499)%>%
  filter(time%%200==0)%>%
  mutate(time=time/200)


###########




Total_ICER_low<-  apply(total_cost_lowprep, 2, function(x) x -Median_total_cost_noprep)/apply(all_low_prep_QALY, 2, function(x) x -Median_total_QALY_noprep)
Total_ICER_mid<-  apply(total_cost_midprep, 2, function(x) x -Median_total_cost_noprep)/apply(all_mid_prep_QALY, 2, function(x) x -Median_total_QALY_noprep)
Total_ICER_high<-  apply(total_cost_highprep, 2, function(x) x -Median_total_cost_noprep)/apply(all_high_prep_QALY, 2, function(x) x -Median_total_QALY_noprep)
  

Total_ICER_low<-as.data.frame(Total_ICER_low)
Total_ICER_mid<-as.data.frame(Total_ICER_mid)
Total_ICER_high<-as.data.frame(Total_ICER_high)



Cost_high<-apply(total_cost_highprep, 2, function(x) x -Median_total_cost_noprep)
Effect_high<-apply(all_high_prep_QALY, 2, function(x) x -Median_total_QALY_noprep)


Total_ICER_high<-Cost_high/Effect_high



### GET ACCEPTABLE PROBALITY
condition <- (Effect_high > 0 & Total_ICER_high < 20000) 

probabilities <- rowMeans(condition)
probabilities<-tail(probabilities,1000)
prob_table <- data.frame(
  Accept = probabilities,
  time = 1:length(probabilities)
)


condition_eff <- (Effect_high > 0) 

probabilities_eff <- rowMeans(condition_eff)
probabilities_eff<-tail(probabilities_eff,1000)
prob_table_eff <- data.frame(
  Accept = probabilities_eff,
  time = 1:length(probabilities_eff)
)








# Merge the two dataframes
merged_table <- prob_table %>%
  mutate(QALY_positive = prob_table_eff$Accept)
merged_table$time<-2020+merged_table$time%/%52
# Create the plot



# Update time range

# Update time range

library(extrafont)
font_import()
loadfonts(device = "pdf")


acccept_plot<-ggplot(merged_table) +
  geom_smooth(aes(x = time, y = QALY_positive, color = "Probability of QALY increase"), se = FALSE) +
  geom_smooth(aes(x = time, y = Accept, color = "Probability of ICER < 20000"), se = FALSE) +
  labs(x = "Time", y = "Probability", color = "Legend") +
  scale_color_npg() + theme_classic()+
  theme(legend.position = "bottom",
                        legend.direction = "horizontal",
                        legend.key.width = unit(1.5, "cm"),
                        panel.grid = element_blank(),
                        legend.text = element_text(face="bold",size = 10),
                        legend.title = element_text(face="bold",size = 12),
                        axis.text.x = element_text(face="bold",size = 14),
                        axis.text.y = element_text(face="bold",size = 14),
                        axis.title.x = element_text(face="bold",size = 15),
                        axis.title.y = element_text(face="bold",size = 15))
acccept_plot

ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/acccept_plot.png',acccept_plot, height = 4 , width = 7)





