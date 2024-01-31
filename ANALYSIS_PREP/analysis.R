library(ggplot2)
library(dplyr)
library(patchwork)

###PLOT####


#####GET MEDIAN####


combine_prev <- function(simulation_list) {
  # Initialize an empty data frame to store the combined data
  combined_data <- data.frame(Time = integer(), Value = numeric(), Simulation = integer())
  
  # Loop through each simulation in the list
  for (i in seq_along(simulation_list)) {
    # Check if the simulation data is not NULL
    if (!is.null(simulation_list[[i]]$epi$i.prev)) {
      # Extract the time series data
      sim_data <- simulation_list[[i]]$epi$i.prev
      # Create a Time sequence
      time_points <- 1:nrow(sim_data)
      
      # Prepare the data frame for this simulation
      sim_data_frame <- data.frame(
        Time = time_points,
        Value = sim_data[[1]], # assuming the first column contains the values
        Simulation = rep(i, length(time_points))
      )
      
      # Combine with the main data frame
      combined_data <- rbind(combined_data, sim_data_frame)
    }
  }
  
  
  combined_data<- combined_data %>% group_by(Time)%>% 
    summarise(
      Median = median(Value, na.rm = TRUE),
      CI_lower = quantile(Value, probs = 0.10, na.rm = TRUE),
      CI_upper = quantile(Value, probs = 0.90, na.rm = TRUE)
    )
  
  return(combined_data)
}



combine_inum <- function(simulation_list) {
  # Initialize an empty data frame to store the combined data
  combined_data <- data.frame(Time = integer(), Value = numeric(), Simulation = integer())
  
  # Loop through each simulation in the list
  for (i in seq_along(simulation_list)) {
    # Check if the simulation data is not NULL
    if (!is.null(simulation_list[[i]]$epi$i.num)) {
      # Extract the time series data
      sim_data <- simulation_list[[i]]$epi$i.num
      # Create a Time sequence
      time_points <- 1:nrow(sim_data)
      
      # Prepare the data frame for this simulation
      sim_data_frame <- data.frame(
        Time = time_points,
        Value = sim_data[[1]], # assuming the first column contains the values
        Simulation = rep(i, length(time_points))
      )
      
      # Combine with the main data frame
      combined_data <- rbind(combined_data, sim_data_frame)
    }
  }
  
  
  combined_data<- combined_data %>% group_by(Time)%>% 
    summarise(
      Median = median(Value, na.rm = TRUE),
      CI_lower = quantile(Value, probs = 0.10, na.rm = TRUE),
      CI_upper = quantile(Value, probs = 0.90, na.rm = TRUE)
    )
  
  return(combined_data)
}



combine_prep <- function(simulation_list) {
  # Initialize an empty data frame to store the combined data
  combined_data <- data.frame(Time = integer(), Value = numeric(), Simulation = integer())
  
  # Loop through each simulation in the list
  for (i in seq_along(simulation_list)) {
    # Check if the simulation data is not NULL
    if (!is.null(simulation_list[[i]]$epi$prepCurr)) {
      # Extract the time series data
      sim_data <- simulation_list[[i]]$epi$prepCurr
      # Create a Time sequence
      time_points <- 1:nrow(sim_data)
      
      # Prepare the data frame for this simulation
      sim_data_frame <- data.frame(
        Time = time_points,
        Value = sim_data[[1]], # assuming the first column contains the values
        Simulation = rep(i, length(time_points))
      )
      
      # Combine with the main data frame
      combined_data <- rbind(combined_data, sim_data_frame)
    }
  }
  
  
  combined_data<- combined_data %>% group_by(Time)%>% 
    summarise(
      Median = median(Value, na.rm = TRUE),
      CI_lower = quantile(Value, probs = 0.10, na.rm = TRUE),
      CI_upper = quantile(Value, probs = 0.90, na.rm = TRUE)
    )
  
  return(combined_data)
}


combine_ART <- function(simulation_list) {
  # Initialize an empty data frame to store the combined data
  combined_data <- data.frame(Time = integer(), Value = numeric(), Simulation = integer())
  
  # Loop through each simulation in the list
  for (i in seq_along(simulation_list)) {
    # Check if the simulation data is not NULL
    if (!is.null(simulation_list[[i]]$epi$cc.dx)) {
      # Extract the time series data
      sim_data <- simulation_list[[i]]$epi$cc.dx
      # Create a Time sequence
      time_points <- 1:nrow(sim_data)
      
      # Prepare the data frame for this simulation
      sim_data_frame <- data.frame(
        Time = time_points,
        Value = sim_data[[1]], # assuming the first column contains the values
        Simulation = rep(i, length(time_points))
      )
      
      # Combine with the main data frame
      combined_data <- rbind(combined_data, sim_data_frame)
    }
  }
  
  
  combined_data<- combined_data %>% group_by(Time)%>% 
    summarise(
      Median = median(Value, na.rm = TRUE),
      CI_lower = quantile(Value, probs = 0.10, na.rm = TRUE),
      CI_upper = quantile(Value, probs = 0.90, na.rm = TRUE)
    )
  
  return(combined_data)
}


combine_pop <- function(simulation_list) {
  # Initialize an empty data frame to store the combined data
  combined_data <- data.frame(Time = integer(), Value = numeric(), Simulation = integer())
  
  # Loop through each simulation in the list
  for (i in seq_along(simulation_list)) {
    # Check if the simulation data is not NULL
    if (!is.null(simulation_list[[i]]$epi$num)) {
      # Extract the time series data
      sim_data <- simulation_list[[i]]$epi$num
      # Create a Time sequence
      time_points <- 1:nrow(sim_data)
      
      # Prepare the data frame for this simulation
      sim_data_frame <- data.frame(
        Time = time_points,
        Value = sim_data[[1]], # assuming the first column contains the values
        Simulation = rep(i, length(time_points))
      )
      
      # Combine with the main data frame
      combined_data <- rbind(combined_data, sim_data_frame)
    }
  }
  
  
  combined_data<- combined_data %>% group_by(Time)%>% 
    summarise(
      Median = median(Value, na.rm = TRUE),
      CI_lower = quantile(Value, probs = 0.10, na.rm = TRUE),
      CI_upper = quantile(Value, probs = 0.90, na.rm = TRUE)
    )
  
  return(combined_data)
}

### FUNCTION TO GET ALL###
prev_all_get<-function(noprep_data,EACS_data,US_data,WHO_data,PNHS_data,BEL_data){
  noprep_data_prev <- combine_prev(noprep_data)
  EACS_data_prev <- combine_prev(EACS_data)
  US_data_prev<- combine_prev(US_data)
  WHO_data_prev<- combine_prev(WHO_data)
  PNHS_data_prev<- combine_prev(PNHS_data)
  BEL_data_prev<- combine_prev(BEL_data)
  noprep_data_prev$Source <- "No Prep"
  EACS_data_prev$Source <- "EACS"
  US_data_prev$Source <- "US"
  WHO_data_prev$Source <- "WHO"
  PNHS_data_prev$Source <- "PNHS"
  BEL_data_prev$Source <- "BEL"
  prev_all <- rbind(noprep_data_prev, EACS_data_prev, US_data_prev, WHO_data_prev,PNHS_data_prev,BEL_data_prev)
  return(prev_all)
}

inum_all_get<-function(noprep_data,EACS_data,US_data,WHO_data,PNHS_data,BEL_data){
  noprep_data_prev <- combine_inum(noprep_data)
  EACS_data_prev <- combine_inum(EACS_data)
  US_data_prev<- combine_inum(US_data)
  WHO_data_prev<- combine_inum(WHO_data)
  PNHS_data_prev<- combine_inum(PNHS_data)
  BEL_data_prev<- combine_inum(BEL_data)
  noprep_data_prev$Source <- "No Prep"
  EACS_data_prev$Source <- "EACS"
  US_data_prev$Source <- "US"
  WHO_data_prev$Source <- "WHO"
  PNHS_data_prev$Source <- "PNHS"
  BEL_data_prev$Source <- "BEL"
  prev_all <- rbind(noprep_data_prev, EACS_data_prev, US_data_prev, WHO_data_prev,PNHS_data_prev,BEL_data_prev)
  return(prev_all)
}

prep_all_get<-function(noprep_data,EACS_data,US_data,WHO_data,PNHS_data,BEL_data){
  noprep_data_prev <- combine_prep(noprep_data)
  EACS_data_prev <- combine_prep(EACS_data)
  US_data_prev<- combine_prep(US_data)
  WHO_data_prev<- combine_prep(WHO_data)
  PNHS_data_prev<- combine_prep(PNHS_data)
  BEL_data_prev<- combine_prep(BEL_data)
  noprep_data_prev$Source <- "No Prep"
  EACS_data_prev$Source <- "EACS"
  US_data_prev$Source <- "US"
  WHO_data_prev$Source <- "WHO"
  PNHS_data_prev$Source <- "PNHS"
  BEL_data_prev$Source <- "BEL"
  prev_all <- rbind(noprep_data_prev, EACS_data_prev, US_data_prev, WHO_data_prev,PNHS_data_prev,BEL_data_prev)
  return(prev_all)
}


ART_all_get<-function(noprep_data,EACS_data,US_data,WHO_data,PNHS_data,BEL_data){
  noprep_data_prev <- combine_ART(noprep_data)
  EACS_data_prev <- combine_ART(EACS_data)
  US_data_prev<- combine_ART(US_data)
  WHO_data_prev<- combine_ART(WHO_data)
  PNHS_data_prev<- combine_ART(PNHS_data)
  BEL_data_prev<- combine_ART(BEL_data)
  noprep_data_prev$Source <- "No Prep"
  EACS_data_prev$Source <- "EACS"
  US_data_prev$Source <- "US"
  WHO_data_prev$Source <- "WHO"
  PNHS_data_prev$Source <- "PNHS"
  BEL_data_prev$Source <- "BEL"
  prev_all <- rbind(noprep_data_prev, EACS_data_prev, US_data_prev, WHO_data_prev,PNHS_data_prev,BEL_data_prev)
  return(prev_all)
}

pop_all_get<-function(noprep_data,EACS_data,US_data,WHO_data,PNHS_data,BEL_data){
  noprep_data_prev <- combine_pop(noprep_data)
  EACS_data_prev <- combine_pop(EACS_data)
  US_data_prev<- combine_pop(US_data)
  WHO_data_prev<- combine_pop(WHO_data)
  PNHS_data_prev<- combine_pop(PNHS_data)
  BEL_data_prev<- combine_pop(BEL_data)
  noprep_data_prev$Source <- "No Prep"
  EACS_data_prev$Source <- "EACS"
  US_data_prev$Source <- "US"
  WHO_data_prev$Source <- "WHO"
  PNHS_data_prev$Source <- "PNHS"
  BEL_data_prev$Source <- "BEL"
  prev_all <- rbind(noprep_data_prev, EACS_data_prev, US_data_prev, WHO_data_prev,PNHS_data_prev,BEL_data_prev)
  return(prev_all)
}

###PLOT ALL PREV####
plot_all_prev<-function(prev_all,name_plot){
  ggplot(prev_all, aes(x = Time, y = Median, group = Source, color = Source)) +
    geom_line() +  # Line for each source's median values
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Source), alpha = 0.2) +
    labs(title = name_plot, 
         x = "Time", 
         y = "Value") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme(legend.title = element_blank())
  
}



prev_gre_all<- prev_all_get(result_gre_noprep,result_gre_EACS,result_gre_US,result_gre_WHO,result_gre_PNHS,result_gre_BEL)
plot_all_prev(prev_gre_all,'GREECE, Prevalence')

###HUN#####

load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_EACS1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_noprep1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_US1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_WHO1.RData")

prev_hun_all<- prev_all_get(result_hun_noprep,result_hun_EACS,result_hun_US,result_hun_WHO)
plot_all_prev(prev_hun_all,'Hungary, Prevalence')
rm(result_hun_EACS,result_hun_noprep,result_hun_US,result_hun_WHO)


###SLO#####

load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_EACS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_noprep.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_US.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_WHO.RData")

prev_slo_all<- prev_all_get(result_slo_noprep,result_slo_EACS,result_slo_US,result_slo_WHO)
plot_all_prev(prev_slo_all,'Slo, Prevalence')
rm(result_slo_noprep,result_slo_EACS,result_slo_US,result_slo_WHO)




###Latvia#####

load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_EACS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_noprep.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_US.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_WHO.RData")

prev_lat_all<- prev_all_get(result_lat_noprep,result_lat_EACS,result_lat_US,result_lat_WHO)
plot_all_prev(prev_lat_all,'Latvia, Prevalence')
rm(result_lat_noprep,result_lat_EACS,result_lat_US,result_lat_WHO)




ART_net_all<- ART_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO)
plot_all_prev(ART_net_all,'NETHERLAND, INFECTION')
ART_net_all[,2:4]<-ART_net_all[,2:4]*inum_net_all[,2:4]
plot_all_prev(ART_net_all,'NETHERLAND, INFECTION')


rm(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO)

### GET THE COST FUNCTION###
get_cost<- function(rawtable, cost_week){
  rawtable_cost<-rawtable
  rawtable_cost[,2:4]<-rawtable_cost[,2:4]*cost_week
  rawtable_cost_cum<-rawtable_cost
  rawtable_cost_cum<-na.omit(rawtable_cost_cum)
  rawtable_cost_cum <- rawtable_cost_cum %>%
    group_by(Source) %>%
    mutate(
      Median = cumsum(Median),
      CI_lower = cumsum(CI_lower),
      CI_upper = cumsum(CI_upper)
    ) %>%
    ungroup()
  return(list(rawtable_cost, rawtable_cost_cum))
}
##COST OF ART###

result_cost<-get_cost(ART_net_all,8000/52)
ART_net_all_cost <- result_cost[[1]]
ART_net_all_cost_cum <- result_cost[[2]]


##COST OF prep###
result_cost<-get_cost(prep_net_all,638)
prep_net_all_cost <- result_cost[[1]]
prep_net_all_cost_cum <- result_cost[[2]]

plot_all_prev(ART_net_all_cost,'NETHERLAND, INFECTION')

total_cost_net<-prep_net_all_cost_cum
total_cost_net[,2:4] <- prep_net_all_cost_cum[,2:4]+ART_net_all_cost_cum[,2:4]

#####GET THE QALY####
remove_firstna<-function(removedata){
  removedata<-removedata %>%
    group_by(Source) %>%
    slice(-1) %>%
    ungroup()
  return(removedata)
}
qaly_get<-function(pop_all,inum_all){
  all_qaly<-get_cost(pop_all,1/52)
  hiv_qaly<-get_cost(inum_all,1/52*0.3)
  hiv_qaly[[1]]<- remove_firstna(hiv_qaly[[1]])
  all_qaly[[1]]<- remove_firstna(all_qaly[[1]])
  all_qaly[[2]]<- remove_firstna(all_qaly[[2]])
  
  
  country_all_qaly<-all_qaly[[1]]
  country_all_qaly[,2:4] <- all_qaly[[1]][,2:4]-hiv_qaly[[1]][,2:4]
  country_all_qaly_cum<-all_qaly[[2]]
  country_all_qaly_cum[,2:4] <- all_qaly[[2]][,2:4]-hiv_qaly[[2]][,2:4]
  return(list(country_all_qaly, country_all_qaly_cum))
}

qaly_result<-qaly_get(pop_net_all,inum_net_all)
qaly_net_all<-qaly_result[[1]]
qaly_net_all_cum<-qaly_result[[2]]
plot_all_prev(qaly_net_all_cum,'NETHERLAND, INFECTION')
plot_all_prev(qaly_net_all,'NETHERLAND, INFECTION')




#####GET THE DIFFERENCE####

calculate_differences <- function(dataframe) {
  library(dplyr)
  
  # Check if Source column exists
  if("Source" %in% names(dataframe)) {
    # Separate 'No Prep' group
    no_prep_data <- dataframe %>%
      filter(Source == "No Prep") %>%
      select(Time, Median_no_prep = Median, CI_lower_no_prep = CI_lower, CI_upper_no_prep = CI_upper)
    
    # Join with the main dataset and calculate differences
    difference_data <- dataframe %>%
      left_join(no_prep_data, by = "Time") %>%
      mutate(
        Median_diff = Median - Median_no_prep,
        CI_lower_diff = CI_lower - CI_lower_no_prep,
        CI_upper_diff = CI_upper - CI_upper_no_prep
      ) %>%
      select(-c(Median_no_prep, CI_lower_no_prep, CI_upper_no_prep))
    
    # Optional: Filter out 'No Prep' rows after calculation if they are not needed
    difference_data <- difference_data %>%
      filter(Source != "No Prep")
    
    return(difference_data)
  } else {
    stop("The dataframe does not contain a 'Source' column.")
  }
}

plot_differences <- function(dataframe,title_name) {
  library(ggplot2)
  
  # Check for necessary columns
  required_columns <- c("Time", "Source", "Median_diff", "CI_lower_diff", "CI_upper_diff")
  if (!all(required_columns %in% names(dataframe))) {
    stop("Dataframe does not contain all required columns.")
  }
  
  # Plot
  ggplot(dataframe, aes(x = Time, y = Median_diff, group = Source, color = Source)) +
    geom_line() +
    geom_ribbon(aes(ymin = CI_lower_diff, ymax = CI_upper_diff), alpha = 0.2) +
    labs(title = title_name,
         x = "Time",
         y = "Difference in Median") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
}


qaly_net_all_cum_difference<- calculate_differences(qaly_net_all_cum)
plot_differences(qaly_net_all_cum_difference,'Net Qaly difference')


total_cost_net_difference<- calculate_differences(total_cost_net)
plot_differences(total_cost_net_difference,'Net COST difference')



####GET THE ICER####
ICER_calc<- function(cost_data, qaly_data){
  cost_data<-cost_data %>%
    select(-c(Median, CI_lower, CI_upper))
  qaly_data<-qaly_data %>%
    select(-c(Median, CI_lower, CI_upper))  
  output_data<-cost_data
  output_data[,3:5]<-cost_data[,3:5]/qaly_data[,3:5]
  return(output_data)
}

ICER_net<- ICER_calc(total_cost_net_difference,qaly_net_all_cum_difference)
plot_differences(ICER_net,'Net ICER')



qaly_get(pop_rom_all,inum_rom_all)[[2]]
###GET AFUNCTION TO CACULATE COST (ART&PREP),QALY, DIFFERENCE, AND ICER
calculate_all<-function(ART_all,prep_all,pop_all,inum_all,artcost,prepcost){
  ###COST OF ART
  result_cost<-get_cost(ART_all,artcost)
  ART_all_cost <- result_cost[[1]]
  ART_all_cost_cum <- result_cost[[2]]
  
  ##COST OF prep
  result_cost<-get_cost(prep_all,prepcost)
  prep_all_cost <- result_cost[[1]]
  prep_all_cost_cum <- result_cost[[2]]
  
  #total cost
  total_cost<-prep_all_cost_cum
  total_cost[,2:4] <- prep_all_cost_cum[,2:4]+ART_all_cost_cum[,2:4]
  ##TOTAL QALY
  qaly_result<-qaly_get(pop_all,inum_all)
  qaly_all<-qaly_result[[1]]
  qaly_all_cum<-qaly_result[[2]]
 
  #GET QALY
  qaly_all_cum_difference<- calculate_differences(qaly_all_cum)
  total_cost_difference<- calculate_differences(total_cost)
  
  ICER<- ICER_calc(total_cost_difference,qaly_all_cum_difference)

  return(list(ART_all_cost,ART_all_cost_cum,prep_all_cost,prep_all_cost_cum,total_cost,qaly_all,qaly_all_cum,ICER,total_cost_difference,qaly_all_cum_difference))
  }

ICER_bulgaria<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,8139/52,5500/52)[[8]]


####NETHERLAND

test_test<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,8000/52,638)


load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_EACS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_noprep.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_US.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_WHO.RData")

prev_net_all<- prev_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO)
plot_all_prev(prev_net_all,'NETHERLAND, Prevalence')
inum_net_all<- inum_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO)
plot_all_prev(inum_net_all,'NETHERLAND, INFECTION')
prep_net_all<- prep_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO)
plot_all_prev(prep_net_all,'NETHERLAND, INFECTION')
pop_net_all<- pop_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO)
plot_all_prev(pop_net_all,'NETHERLAND, INFECTION')
ART_net_all<- ART_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO)
plot_all_prev(ART_net_all,'NETHERLAND, INFECTION')
ART_net_all[,2:4]<-ART_net_all[,2:4]*inum_net_all[,2:4]
plot_all_prev(ART_net_all,'NETHERLAND, INFECTION')
rm(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO)

test_test<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,8000/52,638)









#slo_prev_plot<-plot_all_prev(prev_slo_all,'3')
hun_prev_plot<-plot_all_prev(prev_hun_all,'5')
rom_prev_plot<-plot_all_prev(prev_rom_all,'7')
pol_prev_plot<-plot_all_prev(prev_pol_all,'9')
gre_prev_plot<-plot_all_prev(prev_gre_all,'11')
lat_prev_plot<-plot_all_prev(prev_lat_all,'13')
net_prev_plot<-plot_all_prev(prev_net_all,'15')

library(gridExtra)

grid.arrange(hun_prev_plot, rom_prev_plot, pol_prev_plot,
             gre_prev_plot, lat_prev_plot, net_prev_plot,
             ncol = 2)

#combined_plot <- combine((hun_prev_plot | rom_prev_plot) / 
#  (pol_prev_plot | gre_prev_plot) / 
#  (lat_prev_plot | net_prev_plot))





plot_all_prev<-function(prev_all,name_plot){
  ggplot(prev_all, aes(x = Time, y = Median, group = Source, color = Source)) +
    geom_line() +  # Line for each source's median values
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Source), alpha = 0.2) +
    labs(title = name_plot, 
         x = "Time", 
         y = "Prevalence") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") 
    #theme(legend.title = element_blank(), legend.position = "none") 
}



###GET THE RANK###
create_heatmap <- function(data, source_name, time_points) {
  # Rank the Median values within each time point
  ranked_data <- data %>%
    filter(Time %in% time_points) %>%
    group_by(Time) %>%
    mutate(Rank = rank(-Median, ties.method = "first")) %>%
    ungroup()
  
  # Create the heatmap
  ggplot(ranked_data, aes(x = as.factor(Time), y = Source, fill = as.factor(Rank))) +
    geom_tile() +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    labs(title = paste("Prevalence rank for", source_name), x = "Time Point", y = "Policy", fill = "Legend")  
    #+theme(legend.position = "none")
}

# Specified time points
time_points <- c(700, 900, 1100, 1300, 1499)




#heatmap_slo <- create_heatmap(prev_slo_all, "SLOVAKIA,3%", time_points)
heatmap_hun <- create_heatmap(prev_hun_all, "HUNGARY,5%", time_points)
heatmap_rom <- create_heatmap(prev_rom_all, "ROMANIA,7%", time_points)
heatmap_pol <- create_heatmap(prev_pol_all, "POLAND,9%", time_points)
heatmap_gre <- create_heatmap(prev_gre_all, "GREECE,11%", time_points)
heatmap_lat <- create_heatmap(prev_lat_all, "LATVIA,13%", time_points)
heatmap_net <- create_heatmap(prev_net_all, "NETHERLAND,15%", time_points)


# Combine all heatmaps into one figure
combined_heatmaps <-  heatmap_hun + heatmap_rom + 
  heatmap_pol + heatmap_gre + heatmap_lat + 
  heatmap_net +
  plot_layout(ncol = 2, heights = c(1, 1, 1, 1))



####GET COST EFFECTIVENESS###


#######LOAD DATA######


load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_EACS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_noprep.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_US.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_WHO.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_PNHS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_net_BEL.RData")

prev_net_all<- prev_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO,result_net_PNHS,result_net_BEL)
inum_net_all<- inum_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO,result_net_PNHS,result_net_BEL)
prep_net_all<- prep_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO,result_net_PNHS,result_net_BEL)
pop_net_all<- pop_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO,result_net_PNHS,result_net_BEL)
ART_net_all<- ART_all_get(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO,result_net_PNHS,result_net_BEL)
ART_net_all[,2:4]<-ART_net_all[,2:4]*inum_net_all[,2:4]
rm(result_net_noprep,result_net_EACS,result_net_US,result_net_WHO,result_net_PNHS,result_net_BEL)



#####GREECE##############
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_gre_EACS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_gre_noprep.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_gre_US.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_gre_WHO.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_gre_PNHS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_gre_BEL.RData")

prev_gre_all<- prev_all_get(result_gre_noprep,result_gre_EACS,result_gre_US,result_gre_WHO,result_gre_PNHS,result_gre_BEL)
inum_gre_all<- inum_all_get(result_gre_noprep,result_gre_EACS,result_gre_US,result_gre_WHO,result_gre_PNHS,result_gre_BEL)
prep_gre_all<- prep_all_get(result_gre_noprep,result_gre_EACS,result_gre_US,result_gre_WHO,result_gre_PNHS,result_gre_BEL)
pop_gre_all<- pop_all_get(result_gre_noprep,result_gre_EACS,result_gre_US,result_gre_WHO,result_gre_PNHS,result_gre_BEL)
ART_gre_all<- ART_all_get(result_gre_noprep,result_gre_EACS,result_gre_US,result_gre_WHO,result_gre_PNHS,result_gre_BEL)
ART_gre_all[,2:4]<-ART_gre_all[,2:4]*inum_gre_all[,2:4]

rm(result_gre_noprep,result_gre_EACS,result_gre_US,result_gre_WHO,result_gre_PNHS,result_gre_BEL)

###HUN#####

load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_EACS1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_noprep1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_US1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_WHO1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_PNHS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_hun_BEL.RData")

prev_hun_all<- prev_all_get(result_hun_noprep,result_hun_EACS,result_hun_US,result_hun_WHO,result_hun_PNHS,result_hun_BEL)
inum_hun_all<- inum_all_get(result_hun_noprep,result_hun_EACS,result_hun_US,result_hun_WHO,result_hun_PNHS,result_hun_BEL)
prep_hun_all<- prep_all_get(result_hun_noprep,result_hun_EACS,result_hun_US,result_hun_WHO,result_hun_PNHS,result_hun_BEL)
pop_hun_all<- pop_all_get(result_hun_noprep,result_hun_EACS,result_hun_US,result_hun_WHO,result_hun_PNHS,result_hun_BEL)
ART_hun_all<- ART_all_get(result_hun_noprep,result_hun_EACS,result_hun_US,result_hun_WHO,result_hun_PNHS,result_hun_BEL)
ART_hun_all[,2:4]<-ART_hun_all[,2:4]*inum_hun_all[,2:4]
rm(result_hun_EACS,result_hun_noprep,result_hun_US,result_hun_WHO,result_hun_PNHS,result_hun_BEL)


###SLO#####

#load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_EACS.RData")
#load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_noprep.RData")
#load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_US.RData")
#load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_WHO.RData")
#load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_P
#load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_slo_WHO.RData")

#prev_slo_all<- prev_all_get(result_slo_noprep,result_slo_EACS,result_slo_US,result_slo_WHO)
#inum_slo_all<- inum_all_get(result_slo_noprep,result_slo_EACS,result_slo_US,result_slo_WHO)
#prep_slo_all<- prep_all_get(result_slo_noprep,result_slo_EACS,result_slo_US,result_slo_WHO)
#pop_slo_all<- pop_all_get(result_slo_noprep,result_slo_EACS,result_slo_US,result_slo_WHO)
#ART_slo_all<- ART_all_get(result_slo_noprep,result_slo_EACS,result_slo_US,result_slo_WHO)
#ART_slo_all[,2:4]<-ART_slo_all[,2:4]*inum_slo_all[,2:4]
#rm(result_slo_noprep,result_slo_EACS,result_slo_US,result_slo_WHO)




###Latvia#####

load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_EACS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_noprep.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_US.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_WHO.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_PNHS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_lat_BEL.RData")

prev_lat_all<- prev_all_get(result_lat_noprep,result_lat_EACS,result_lat_US,result_lat_WHO,result_lat_PNHS,result_lat_BEL)
inum_lat_all<- inum_all_get(result_lat_noprep,result_lat_EACS,result_lat_US,result_lat_WHO,result_lat_PNHS,result_lat_BEL)
prep_lat_all<- prep_all_get(result_lat_noprep,result_lat_EACS,result_lat_US,result_lat_WHO,result_lat_PNHS,result_lat_BEL)
pop_lat_all<- pop_all_get(result_lat_noprep,result_lat_EACS,result_lat_US,result_lat_WHO,result_lat_PNHS,result_lat_BEL)
ART_lat_all<- ART_all_get(result_lat_noprep,result_lat_EACS,result_lat_US,result_lat_WHO,result_lat_PNHS,result_lat_BEL)
ART_lat_all[,2:4]<-ART_lat_all[,2:4]*inum_lat_all[,2:4]
rm(result_lat_noprep,result_lat_EACS,result_lat_US,result_lat_WHO,result_lat_PNHS,result_lat_BEL)

###POLAND#####
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_EACS1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_noprep1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_US1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_WHO1.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_PNHS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_pol_BEL.RData")

prev_pol_all<- prev_all_get(result_pol_noprep,result_pol_EACS,result_pol_US,result_pol_WHO,result_pol_PNHS,result_pol_BEL)
inum_pol_all<- inum_all_get(result_pol_noprep,result_pol_EACS,result_pol_US,result_pol_WHO,result_pol_PNHS,result_pol_BEL)
prep_pol_all<- prep_all_get(result_pol_noprep,result_pol_EACS,result_pol_US,result_pol_WHO,result_pol_PNHS,result_pol_BEL)
pop_pol_all<- pop_all_get(result_pol_noprep,result_pol_EACS,result_pol_US,result_pol_WHO,result_pol_PNHS,result_pol_BEL)
ART_pol_all<- ART_all_get(result_pol_noprep,result_pol_EACS,result_pol_US,result_pol_WHO,result_pol_PNHS,result_pol_BEL)
ART_pol_all[,2:4]<-ART_pol_all[,2:4]*inum_pol_all[,2:4]
rm(result_pol_noprep,result_pol_EACS,result_pol_US,result_pol_WHO,result_pol_PNHS,result_pol_BEL)



###Romania#####
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_rom_EACS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_rom_noprep.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_rom_US.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_rom_WHO.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_rom_PNHS.RData")
load("/Users/boxuan/Desktop/Intership/Potugual/sim_result/result_rom_BEL.RData")

prev_rom_all<- prev_all_get(result_rom_noprep,result_rom_EACS,result_rom_US,result_rom_WHO,result_rom_PNHS,result_rom_BEL)
inum_rom_all<- inum_all_get(result_rom_noprep,result_rom_EACS,result_rom_US,result_rom_WHO,result_rom_PNHS,result_rom_BEL)
prep_rom_all<- prep_all_get(result_rom_noprep,result_rom_EACS,result_rom_US,result_rom_WHO,result_rom_PNHS,result_rom_BEL)
pop_rom_all<- pop_all_get(result_rom_noprep,result_rom_EACS,result_rom_US,result_rom_WHO,result_rom_PNHS,result_rom_BEL)
ART_rom_all<- ART_all_get(result_rom_noprep,result_rom_EACS,result_rom_US,result_rom_WHO,result_rom_PNHS,result_rom_BEL)
ART_rom_all[,2:4]<-ART_rom_all[,2:4]*inum_rom_all[,2:4]
rm(result_rom_noprep,result_rom_EACS,result_rom_US,result_rom_WHO,result_rom_PNHS,result_rom_BEL)




heatmap_slo <- create_heatmap(prev_slo_all, "SLOVAKIA,3%", time_points)
heatmap_hun <- create_heatmap(prev_hun_all, "HUNGARY,5%", time_points)
heatmap_rom <- create_heatmap(prev_rom_all, "ROMANIA,7%", time_points)
heatmap_pol <- create_heatmap(prev_pol_all, "POLAND,9%", time_points)
heatmap_gre <- create_heatmap(prev_gre_all, "GREECE,11%", time_points)
heatmap_lat <- create_heatmap(prev_lat_all, "LATVIA,13%", time_points)
heatmap_net <- create_heatmap(prev_net_all, "NETHERLAND,15%", time_points)



##ICER##
ICER_bulgaria<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,8139/52,5500/52)[[8]]
ICER_croatia<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,7532/52,5500/52)[[8]]
ICER_estonia<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,4190/52,5500/52)[[8]]
ICER_hungary<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,8284/52,5500/52)[[8]]
ICER_sweden<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,9755/52,5500/52)[[8]]
#no austria so use the germany
ICER_austria<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,20075/52,5500/52)[[8]]
#no cze so use the slovakia
ICER_czechia<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,1000/52,5500/52)[[8]]
#no cze so use the sweden
ICER_finland<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,9755/52,5500/52)[[8]]
ICER_ireland<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,15312/52,5500/52)[[8]]
ICER_lithuania<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,6000/52,5500/52)[[8]]
ICER_malta<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,12000/52,5500/52)[[8]]
ICER_romania<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,5783/52,5500/52)[[8]]
#no slovenia so use the data of roatia
ICER_slovenia<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,7532/52,5500/52)[[8]]
ICER_poland<-calculate_all(ART_pol_all,prep_pol_all,pop_pol_all,inum_pol_all,9704/52,5500/52)[[8]]
ICER_luxembourg<-calculate_all(ART_pol_all,prep_pol_all,pop_pol_all,inum_pol_all,9600/52,5500/52)[[8]]


ICER_germany<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,20075/52,5500/52)[[8]]
ICER_greece<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,9000/52,5500/52)[[8]]
ICER_italy<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,7463/52,5500/52)[[8]]

#no belguim,so use france
ICER_belgium<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,13000/52,5500/52)[[8]]
ICER_cyprus<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,9598/52,5500/52)[[8]]
ICER_denmark<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,9396/52,5500/52)[[8]]
ICER_france<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,13000/52,5500/52)[[8]]
ICER_latvia<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,4278/52,5500/52)[[8]]
ICER_spain<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,6593/52,5500/52)[[8]]

ICER_netherland<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,8000/52,5500/52)[[8]]
ICER_portugal<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,9012/52,5500/52)[[8]]




#  return(list(ART_all_cost,ART_all_cost_cum,prep_all_cost,prep_all_cost_cum,total_cost,qaly_all,qaly_all_cum,ICER))
#####ALSO GET THE ALLCOST####
costall_bulgaria<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,8139/52,5500/52)[[5]]
costall_croatia<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,7532/52,5500/52)[[5]]
costall_estonia<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,4190/52,5500/52)[[5]]
costall_hungary<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,8284/52,5500/52)[[5]]
costall_sweden<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,9755/52,5500/52)[[5]]
#no austria so use the germany
costall_austria<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,20075/52,5500/52)[[5]]
#no cze so use the slovakia
costall_czechia<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,1000/52,5500/52)[[5]]
#no cze so use the sweden
costall_finland<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,9755/52,5500/52)[[5]]
costall_ireland<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,15312/52,5500/52)[[5]]
costall_lithuania<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,6000/52,5500/52)[[5]]
costall_malta<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,12000/52,5500/52)[[5]]
costall_romania<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,5783/52,5500/52)[[5]]
#no slovenia so use the data of roatia
costall_slovenia<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,7532/52,5500/52)[[5]]
costall_poland<-calculate_all(ART_pol_all,prep_pol_all,pop_pol_all,inum_pol_all,9704/52,5500/52)[[5]]
costall_luxembourg<-calculate_all(ART_pol_all,prep_pol_all,pop_pol_all,inum_pol_all,9600/52,5500/52)[[5]]


costall_germany<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,20075/52,5500/52)[[5]]
costall_greece<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,9000/52,5500/52)[[5]]
costall_italy<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,7463/52,5500/52)[[5]]

#no belguim,so use france
costall_belgium<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,13000/52,5500/52)[[5]]
costall_cyprus<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,9598/52,5500/52)[[5]]
costall_denmark<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,9396/52,5500/52)[[5]]
costall_france<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,13000/52,5500/52)[[5]]
costall_latvia<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,4278/52,5500/52)[[5]]
costall_spain<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,6593/52,5500/52)[[5]]

costall_netherland<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,8000/52,5500/52)[[5]]
costall_portugal<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,9012/52,5500/52)[[5]]


####ALSO GET THE QALY#######


allqaly_bulgaria<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,8139/52,5500/52)[[7]]
allqaly_croatia<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,7532/52,5500/52)[[7]]
allqaly_estonia<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,4190/52,5500/52)[[7]]
allqaly_hungary<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,8284/52,5500/52)[[7]]
allqaly_sweden<-calculate_all(ART_hun_all,prep_hun_all,pop_hun_all,inum_hun_all,9755/52,5500/52)[[7]]
#no austria so use the germany
allqaly_austria<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,20075/52,5500/52)[[7]]
#no cze so use the slovakia
allqaly_czechia<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,1000/52,5500/52)[[7]]
#no cze so use the sweden
allqaly_finland<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,9755/52,5500/52)[[7]]
allqaly_ireland<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,15312/52,5500/52)[[7]]
allqaly_lithuania<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,6000/52,5500/52)[[7]]
allqaly_malta<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,12000/52,5500/52)[[7]]
allqaly_romania<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,5783/52,5500/52)[[7]]
#no slovenia so use the data of roatia
allqaly_slovenia<-calculate_all(ART_rom_all,prep_rom_all,pop_rom_all,inum_rom_all,7532/52,5500/52)[[7]]
allqaly_poland<-calculate_all(ART_pol_all,prep_pol_all,pop_pol_all,inum_pol_all,9704/52,5500/52)[[7]]
allqaly_luxembourg<-calculate_all(ART_pol_all,prep_pol_all,pop_pol_all,inum_pol_all,9600/52,5500/52)[[7]]


allqaly_germany<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,20075/52,5500/52)[[7]]
allqaly_greece<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,9000/52,5500/52)[[7]]
allqaly_italy<-calculate_all(ART_gre_all,prep_gre_all,pop_gre_all,inum_gre_all,7463/52,5500/52)[[7]]

#no belguim,so use france
allqaly_belgium<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,13000/52,5500/52)[[7]]
allqaly_cyprus<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,9598/52,5500/52)[[7]]
allqaly_denmark<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,9396/52,5500/52)[[7]]
allqaly_france<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,13000/52,5500/52)[[7]]
allqaly_latvia<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,4278/52,5500/52)[[7]]
allqaly_spain<-calculate_all(ART_lat_all,prep_lat_all,pop_lat_all,inum_lat_all,6593/52,5500/52)[[7]]

allqaly_netherland<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,8000/52,5500/52)[[7]]
allqaly_portugal<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,9012/52,5500/52)[[7]]








###OK LET'S get the ICER###

extract_median_diff <- function(data, dataset_name, time_points) {
  data %>%
    filter(Time %in% time_points) %>%
    select(Time, Source, Median_diff) %>%
    mutate(Dataset = dataset_name)
}

# Specified time points
time_points <- c(700, 900,1100,1300, 1500)


# Apply the function to each dataset
median_diff_bulgaria <- extract_median_diff(ICER_bulgaria, "Bulgaria", time_points)
median_diff_croatia <- extract_median_diff(ICER_croatia, "Croatia", time_points)
median_diff_estonia <- extract_median_diff(ICER_estonia, "Estonia", time_points)
median_diff_hungary <- extract_median_diff(ICER_hungary, "Hungary", time_points)
median_diff_sweden <- extract_median_diff(ICER_sweden, "Sweden", time_points)
median_diff_austria <- extract_median_diff(ICER_austria, "Austria", time_points)
median_diff_czechia <- extract_median_diff(ICER_czechia, "Czechia", time_points)
median_diff_finland <- extract_median_diff(ICER_finland, "Finland", time_points)
median_diff_ireland <- extract_median_diff(ICER_ireland, "Ireland", time_points)
median_diff_lithuania <- extract_median_diff(ICER_lithuania, "Lithuania", time_points)
median_diff_malta <- extract_median_diff(ICER_malta, "Malta", time_points)
median_diff_romania <- extract_median_diff(ICER_romania, "Romania", time_points)
median_diff_slovenia <- extract_median_diff(ICER_slovenia, "Slovenia", time_points)
median_diff_poland <- extract_median_diff(ICER_poland, "Poland", time_points)
median_diff_luxembourg <- extract_median_diff(ICER_luxembourg, "Luxembourg", time_points)
median_diff_germany <- extract_median_diff(ICER_germany, "Germany", time_points)
median_diff_greece <- extract_median_diff(ICER_greece, "Greece", time_points)
median_diff_italy <- extract_median_diff(ICER_italy, "Italy", time_points)
median_diff_belgium <- extract_median_diff(ICER_belgium, "Belgium", time_points)
median_diff_cyprus <- extract_median_diff(ICER_cyprus, "Cyprus", time_points)
median_diff_denmark <- extract_median_diff(ICER_denmark, "Denmark", time_points)
median_diff_france <- extract_median_diff(ICER_france, "France", time_points)
median_diff_latvia <- extract_median_diff(ICER_latvia, "Latvia", time_points)
median_diff_spain <- extract_median_diff(ICER_spain, "Spain", time_points)
median_diff_netherland <- extract_median_diff(ICER_netherland, "Netherland", time_points)
median_diff_portugal <- extract_median_diff(ICER_portugal, "Portugal", time_points)

# Combine all results into one dataframe
all_median_diffs <- bind_rows(median_diff_bulgaria, median_diff_croatia, median_diff_estonia, 
                              median_diff_hungary, median_diff_sweden, median_diff_austria, 
                              median_diff_czechia, median_diff_finland, median_diff_ireland, 
                              median_diff_lithuania, median_diff_malta, median_diff_romania, 
                              median_diff_slovenia, median_diff_poland, median_diff_luxembourg, 
                              median_diff_germany, median_diff_greece, median_diff_italy, 
                              median_diff_belgium, median_diff_cyprus, median_diff_denmark, 
                              median_diff_france, median_diff_latvia, median_diff_spain, 
                              median_diff_netherland, median_diff_portugal)

# View the combined results
print(all_median_diffs)
library(gridExtra)



all_time_country_icer_heatmap <- function(data) {
  plots <- list()
  countries <- unique(data$Dataset)
  
  for (country in countries) {
    country_data <- subset(data, Dataset == country)
    p <- ggplot(country_data, aes(x = as.factor(Time), y = Source, fill = Median_diff)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", Median_diff)), size = 3, color = "black") +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(title = paste("Heatmap for", country), x = "Time Point", y = "Source", fill = "Median Diff") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[[country]] <- p
  }
  
  do.call(grid.arrange, c(plots, ncol = 3)) # Adjust ncol as needed
}


all_time_country_icer_heatmap(all_median_diffs)




View(all_median_diffs%>%
       filter(Time==1000)%>%
       filter(Source == 'EACS'))
View(all_median_diffs%>%
       filter(Time==1500)%>%
       filter(Source == 'EACS'))
View(all_median_diffs%>%
       filter(Time==500)%>%
       filter(Source == 'EACS'))



write.csv(all_median_diffs,"/Users/boxuan/Desktop/Intership/Potugual/sim_result/all_median_diffs.csv")

testcosteffectiveall<-read.csv('/Users/boxuan/Downloads/cost_effectiveness_all_median_diffs.csv')
View(testcosteffectiveall%>%
  filter(Source=='US'))
###ALL COST EFFECTIVENESS###

# Rename the combined data frame
# List of ICER data frames without a "Country" column
icer_data_list <- list(
  ICER_bulgaria, ICER_croatia, ICER_estonia, ICER_hungary,
  ICER_sweden, ICER_austria, ICER_czechia, ICER_finland,
  ICER_ireland, ICER_lithuania, ICER_malta, ICER_romania,
  ICER_slovenia, ICER_poland, ICER_luxembourg, ICER_germany,
  ICER_greece, ICER_italy, ICER_belgium, ICER_cyprus,
  ICER_denmark, ICER_france, ICER_latvia, ICER_spain,
  ICER_netherland, ICER_portugal
)

# Country names corresponding to each data frame
country_names <- c(
  "Bulgaria", "Croatia", "Estonia", "Hungary", "Sweden",
  "Austria", "Czechia", "Finland", "Ireland", "Lithuania",
  "Malta", "Romania", "Slovenia", "Poland", "Luxembourg",
  "Germany", "Greece", "Italy", "Belgium", "Cyprus",
  "Denmark", "France", "Latvia", "Spain", "Netherland", "Portugal"
)

# Add the "Country" column to each data frame
icer_data_list <- lapply(1:length(icer_data_list), function(i) {
  icer_data_list[[i]] %>%
    mutate(Country = country_names[i])
})

# Combine the data frames into one
all_costeff_data <- do.call(bind_rows, icer_data_list)

# Create a color palette for the different sources
source_colors <- scales::hue_pal()(n_distinct(all_costeff_data$Source))

# Filter the data for Time > 700
filtered_data_icer <- all_costeff_data %>% filter(Time > 700)

# Create a color palette for the different sources
source_colors <- scales::hue_pal()(n_distinct(filtered_data_icer$Source))

filtered_data_icer <- filtered_data_icer %>%
  filter(!(Median_diff > 1e6 | Median_diff < -1e6))

# Plot the filtered data by country with different colors for sources
ggplot(filtered_data_icer, aes(x = Time, y = Median_diff, color = Source)) +
  geom_line()+
  geom_line(size = 1.5) +
  facet_wrap(~Country, scales = "free_y") + # Use scales = "free_y" to allow separate y-axes
  scale_color_manual(values = source_colors) +
  labs(x = "Time", y = "ICER") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12)) 





###PLOT ALL COST CUM###
costall_data_list <- list(
  costall_bulgaria, costall_croatia, costall_estonia, costall_hungary,
  costall_sweden, costall_austria, costall_czechia, costall_finland,
  costall_ireland, costall_lithuania, costall_malta, costall_romania,
  costall_slovenia, costall_poland, costall_luxembourg, costall_germany,
  costall_greece, costall_italy, costall_belgium, costall_cyprus,
  costall_denmark, costall_france, costall_latvia, costall_spain,
  costall_netherland, costall_portugal
)
costall_data_list <- lapply(1:length(costall_data_list), function(i) {
  costall_data_list[[i]] %>%
    mutate(Country = country_names[i])
})

all_costall_data <- do.call(bind_rows, costall_data_list)

all_costall_data <- all_costall_data %>% filter(Time > 700)

ggplot(all_costall_data, aes(x = Time, y = Median, color = Source)) +
  geom_line()+
  geom_line(size = 1.5) +
  facet_wrap(~Country, scales = "free_y") + # Use scales = "free_y" to allow separate y-axes
  labs(x = "Time", y = "Total_cost") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12)) 

###PLOT ALL QALY CUM###

allqaly_data_list <- list(
  allqaly_bulgaria, allqaly_croatia, allqaly_estonia, allqaly_hungary,
  allqaly_sweden, allqaly_austria, allqaly_czechia, allqaly_finland,
  allqaly_ireland, allqaly_lithuania, allqaly_malta, allqaly_romania,
  allqaly_slovenia, allqaly_poland, allqaly_luxembourg, allqaly_germany,
  allqaly_greece, allqaly_italy, allqaly_belgium, allqaly_cyprus,
  allqaly_denmark, allqaly_france, allqaly_latvia, allqaly_spain,
  allqaly_netherland, allqaly_portugal
)

# Add the "Country" column to each data frame
allqaly_data_list <- lapply(1:length(allqaly_data_list), function(i) {
  allqaly_data_list[[i]] %>%
    mutate(Country = country_names[i])
})

# Combine the data frames into one
all_allqaly_data <- do.call(bind_rows, allqaly_data_list)


all_allqaly_data <- all_allqaly_data %>% filter(Time > 700)

ggplot(all_allqaly_data, aes(x = Time, y = Median, color = Source)) +
  geom_line()+
  geom_line(size = 1.5) +
  facet_wrap(~Country, scales = "free_y") + # Use scales = "free_y" to allow separate y-axes
  labs(x = "Time", y = "Total_QALY") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12)) 







# Create a list of allqaly data frames without a "Country" column
allqalydiff_data_list <- list(
  calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 8139/52, 5500/52)[[10]],
  calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 7532/52, 5500/52)[[10]],
  calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 4190/52, 5500/52)[[10]],
  calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 8284/52, 5500/52)[[10]],
  calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 9755/52, 5500/52)[[10]],
  calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 20075/52, 5500/52)[[10]],
  calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 1000/52, 5500/52)[[10]],
  calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 9755/52, 5500/52)[[10]],
  calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 15312/52, 5500/52)[[10]],
  calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 6000/52, 5500/52)[[10]],
  calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 12000/52, 5500/52)[[10]],
  calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 5783/52, 5500/52)[[10]],
  calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 7532/52, 5500/52)[[10]],
  calculate_all(ART_pol_all, prep_pol_all, pop_pol_all, inum_pol_all, 9704/52, 5500/52)[[10]],
  calculate_all(ART_pol_all, prep_pol_all, pop_pol_all, inum_pol_all, 9600/52, 5500/52)[[10]],
  calculate_all(ART_gre_all, prep_gre_all, pop_gre_all, inum_gre_all, 20075/52, 5500/52)[[10]],
  calculate_all(ART_gre_all, prep_gre_all, pop_gre_all, inum_gre_all, 9000/52, 5500/52)[[10]],
  calculate_all(ART_gre_all, prep_gre_all, pop_gre_all, inum_gre_all, 7463/52, 5500/52)[[10]],
  calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 13000/52, 5500/52)[[10]],
  calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 9598/52, 5500/52)[[10]],
  calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 9396/52, 5500/52)[[10]],
  calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 13000/52, 5500/52)[[10]],
  calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 4278/52, 5500/52)[[10]],
  calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 6593/52, 5500/52)[[10]],
  calculate_all(ART_net_all, prep_net_all, pop_net_all, inum_net_all, 8000/52, 5500/52)[[10]],
  calculate_all(ART_net_all, prep_net_all, pop_net_all, inum_net_all, 9012/52, 5500/52)[[10]]
)

# Add the "Country" column to each data frame
allqalydiff_data_list <- lapply(1:length(allqalydiff_data_list), function(i) {
  allqalydiff_data_list[[i]] %>%
    mutate(Country = country_names[i])
})

# Combine the data frames into one
allqalydiff_data <- do.call(bind_rows, allqalydiff_data_list)


allqalydiff_data <- allqalydiff_data %>% filter(Time > 600)

ggplot(allqalydiff_data, aes(x = Time, y = Median_diff, color = Source)) +
  geom_line()+
  geom_line(size = 1.5) +
  facet_wrap(~Country, scales = "free_y") + # Use scales = "free_y" to allow separate y-axes
  labs(x = "Time", y = "QALY DIFFERENCE") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12)) 

write.csv(allqalydiff_data,"/Users/boxuan/Desktop/Intership/Potugual/sim_result/allqalydiff_data.csv" )




####HEAT MAP FOR DIFFERENT TIME RANK###

all_median_diffs_rank<-all_median_diffs%>%
  mutate(Median_diff = ifelse(Median_diff < 1, 999999999999, Median_diff))%>%
  group_by(Time,Dataset) %>%
  mutate(Rank = rank(-Median_diff, ties.method = "first")) %>%
  ungroup()
all_median_diffs_rank$Rank<-as.factor(all_median_diffs_rank$Rank)

all_median_diffs_rank%>%
  ggplot(aes(x = Dataset, y = Source, fill = Rank)) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "blue", "2" = "green", "3" = "red")) + # Define colors for ranks
  labs(title = "ICER Rank for different time, policy, country",
       x = "Country ",
       y = "Policy",
       fill = "Rank") +
  facet_wrap(~Time, scales = "free_x", ncol = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

















##########WHAT HAPPENED ON GENERIC#######


ICER_generic_bulgaria <- calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 8139/52, 30/4)[[8]]
ICER_generic_croatia <- calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 7532/52, 30/4)[[8]]
ICER_generic_estonia <- calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 4190/52, 30/4)[[8]]
ICER_generic_hungary <- calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 8284/52, 30/4)[[8]]
ICER_generic_sweden <- calculate_all(ART_hun_all, prep_hun_all, pop_hun_all, inum_hun_all, 9755/52, 30/4)[[8]]

# No Austria so use the Germany data
ICER_generic_austria <- calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 20075/52, 30/4)[[8]]

# No Czechia so use the Slovakia data
ICER_generic_czechia <- calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 1000/52, 30/4)[[8]]

# No Finland so use the Sweden data
ICER_generic_finland <- calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 9755/52, 30/4)[[8]]

ICER_generic_ireland <- calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 15312/52, 30/4)[[8]]
ICER_generic_lithuania <- calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 6000/52, 30/4)[[8]]
ICER_generic_malta <- calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 12000/52, 30/4)[[8]]
ICER_generic_romania <- calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 5783/52, 30/4)[[8]]

# No Slovenia so use the Croatia data
ICER_generic_slovenia <- calculate_all(ART_rom_all, prep_rom_all, pop_rom_all, inum_rom_all, 7532/52, 30/4)[[8]]

ICER_generic_poland <- calculate_all(ART_pol_all, prep_pol_all, pop_pol_all, inum_pol_all, 9704/52, 30/4)[[8]]
ICER_generic_luxembourg <- calculate_all(ART_pol_all, prep_pol_all, pop_pol_all, inum_pol_all, 9600/52, 30/4)[[8]]

ICER_generic_germany <- calculate_all(ART_gre_all, prep_gre_all, pop_gre_all, inum_gre_all, 20075/52, 30/4)[[8]]
ICER_generic_greece <- calculate_all(ART_gre_all, prep_gre_all, pop_gre_all, inum_gre_all, 9000/52, 30/4)[[8]]
ICER_generic_italy <- calculate_all(ART_gre_all, prep_gre_all, pop_gre_all,inum_gre_all, 7463/52, 30/4)[[8]]
ICER_generic_belgium <- calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 13000/52, 30/4)[[8]]
ICER_generic_cyprus <- calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 9598/52, 30/4)[[8]]
ICER_generic_denmark <- calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 9396/52, 30/4)[[8]]
ICER_generic_france <- calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 13000/52, 30/4)[[8]]
ICER_generic_latvia <- calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 4278/52, 30/4)[[8]]
ICER_generic_spain <- calculate_all(ART_lat_all, prep_lat_all, pop_lat_all, inum_lat_all, 6593/52, 30/4)[[8]]

ICER_generic_netherlands <- calculate_all(ART_net_all, prep_net_all, pop_net_all, inum_net_all, 8000/52, 30/4)[[8]]
ICER_generic_portugal <- calculate_all(ART_net_all, prep_net_all, pop_net_all, inum_net_all, 9012/52, 30/4)[[8]]





                                    

# Apply the function to each dataset
median_diff_generic_bulgaria <- extract_median_diff(ICER_generic_bulgaria, "Bulgaria", time_points)
median_diff_generic_croatia <- extract_median_diff(ICER_generic_croatia, "Croatia", time_points)
median_diff_generic_estonia <- extract_median_diff(ICER_generic_estonia, "Estonia", time_points)
median_diff_generic_hungary <- extract_median_diff(ICER_generic_hungary, "Hungary", time_points)
median_diff_generic_sweden <- extract_median_diff(ICER_generic_sweden, "Sweden", time_points)
median_diff_generic_austria <- extract_median_diff(ICER_generic_austria, "Austria", time_points)
median_diff_generic_czechia <- extract_median_diff(ICER_generic_czechia, "Czechia", time_points)
median_diff_generic_finland <- extract_median_diff(ICER_generic_finland, "Finland", time_points)
median_diff_generic_ireland <- extract_median_diff(ICER_generic_ireland, "Ireland", time_points)
median_diff_generic_lithuania <- extract_median_diff(ICER_generic_lithuania, "Lithuania", time_points)
median_diff_generic_malta <- extract_median_diff(ICER_generic_malta, "Malta", time_points)
median_diff_generic_romania <- extract_median_diff(ICER_generic_romania, "Romania", time_points)
median_diff_generic_slovenia <- extract_median_diff(ICER_generic_slovenia, "Slovenia", time_points)
median_diff_generic_poland <- extract_median_diff(ICER_generic_poland, "Poland", time_points)
median_diff_generic_luxembourg <- extract_median_diff(ICER_generic_luxembourg, "Luxembourg", time_points)
median_diff_generic_germany <- extract_median_diff(ICER_generic_germany, "Germany", time_points)
median_diff_generic_greece <- extract_median_diff(ICER_generic_greece, "Greece", time_points)
median_diff_generic_italy <- extract_median_diff(ICER_generic_italy, "Italy", time_points)
median_diff_generic_belgium <- extract_median_diff(ICER_generic_belgium, "Belgium", time_points)
median_diff_generic_cyprus <- extract_median_diff(ICER_generic_cyprus, "Cyprus", time_points)
median_diff_generic_denmark <- extract_median_diff(ICER_generic_denmark, "Denmark", time_points)
median_diff_generic_france <- extract_median_diff(ICER_generic_france, "France", time_points)
median_diff_generic_latvia <- extract_median_diff(ICER_generic_latvia, "Latvia", time_points)
median_diff_generic_spain <- extract_median_diff(ICER_generic_spain, "Spain", time_points)
median_diff_generic_netherlands <- extract_median_diff(ICER_generic_netherlands, "Netherlands", time_points)
median_diff_generic_portugal <- extract_median_diff(ICER_generic_portugal, "Portugal", time_points)

# Combine all results into one dataframe
all_median_diffs_generic <- bind_rows(median_diff_generic_bulgaria, median_diff_generic_croatia, median_diff_generic_estonia, 
                              median_diff_generic_hungary, median_diff_generic_sweden, median_diff_generic_austria, 
                              median_diff_generic_czechia, median_diff_generic_finland, median_diff_generic_ireland, 
                              median_diff_generic_lithuania, median_diff_generic_malta, median_diff_generic_romania, 
                              median_diff_generic_slovenia, median_diff_generic_poland, median_diff_generic_luxembourg, 
                              median_diff_generic_germany, median_diff_generic_greece, median_diff_generic_italy, 
                              median_diff_generic_belgium, median_diff_generic_cyprus, median_diff_generic_denmark, 
                              median_diff_generic_france, median_diff_generic_latvia, median_diff_generic_spain, 
                              median_diff_generic_netherlands, median_diff_generic_portugal)





write.csv(all_median_diffs_generic,"/Users/boxuan/Desktop/Intership/Potugual/sim_result/all_median_diffs_generic.csv")








































#return(list(ART_all_cost,ART_all_cost_cum,prep_all_cost,prep_all_cost_cum,total_cost,qaly_all,qaly_all_cum,ICER))
art_costtest_portugal<-calculate_all(ART_net_all,prep_net_all,pop_net_all,inum_net_all,9012/52,5500/52)[[3]]



ggplot(art_costtest_portugal, aes(x = Time, y = Median, color = Source)) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Source), alpha = 0.2) +  # CI bands
  geom_line() +  # Line for median values
  labs(title = "Median Cost Over Time by Source", 
       x = "Time", 
       y = "Median Cost") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

