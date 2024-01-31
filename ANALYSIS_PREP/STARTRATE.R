#######
prep_all_get_start<-function(noprep_data,EACS_data,US_data,WHO_data){
  noprep_data_prev <- combine_prep(noprep_data)
  EACS_data_prev <- combine_prep(EACS_data)
  US_data_prev<- combine_prep(US_data)
  WHO_data_prev<- combine_prep(WHO_data)
  noprep_data_prev$Source <- "LOW"
  EACS_data_prev$Source <- "MIDLLE"
  US_data_prev$Source <- "HIGH"
  WHO_data_prev$Source <- "VERYHIGH"
  prev_all <- rbind(noprep_data_prev, EACS_data_prev, US_data_prev, WHO_data_prev)
  return(prev_all)
}



###POLAND#####
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_WHO_low.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_WHO_middle.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_WHO_high.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_WHO_veryhigh.RData")

prep_pol_WHO_all<- prep_all_get_start(result_pol_WHO_low,result_pol_WHO_middle,result_pol_WHO_high,result_pol_WHO_veryhigh)
plot_all_prev(prep_pol_WHO_all,'Poland, PREP')
plot_all_prev(prev_pol_WHO_all,'Poland, PREVALENCE')

rm(result_pol_WHO_low,result_pol_WHO_middle,result_pol_WHO_high,result_pol_WHO_veryhigh)

load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_EACS_start_low.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_EACS_start_middle.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_EACS_start_high.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_EACS_start_veryhigh.RData")

prep_pol_EACS_all<- prep_all_get(result_pol_EACS_start_low,result_pol_EACS_start_middle,result_pol_EACS_start_high,result_pol_EACS_start_veryhigh)
plot_all_prev(prep_pol_EACS_all,'Poland, PREP')
plot_all_prev(prep_pol_EACS_all,'Poland, PREVALENCE')

rm(result_pol_EACS_start_low,result_pol_EACS_start_middle,result_pol_EACS_start_high,result_pol_EACS_start_veryhigh)
