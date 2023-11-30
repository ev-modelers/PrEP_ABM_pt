#cost_effectiveness analysis


load('/Users/boxuan/Desktop/Intership/Potugual/test_ABM/results_who_final_50.RData')
load('/Users/boxuan/Desktop/Intership/Potugual/test_ABM/results_US_final_50.RData')
load('/Users/boxuan/Desktop/Intership/Potugual/test_ABM/results_PNHS_final_50.RData')
load('/Users/boxuan/Desktop/Intership/Potugual/test_ABM/results_EACS_final_50.RData')


remove_list<-c(5,6,13,16,23,38,43,49)
results_who<-results_who[-remove_list]
results_EACS<-results_EACS[-remove_list]
results_PNHS<-results_PNHS[-remove_list]
results_US<-results_US[-remove_list]




calculate_summary <- function(x) {
  c(median = median(x, na.rm = TRUE),
    lower = quantile(x, 0.05, na.rm = TRUE),
    upper = quantile(x, 0.95, na.rm = TRUE))
}

#GET HIV NUMBER


results_who_i_num <- data.frame(lapply(results_who, function(x) x$i.num))
results_EACS_i_num <- data.frame(lapply(results_EACS, function(x) x$i.num))
results_PNHS_i_num <- data.frame(lapply(results_PNHS, function(x) x$i.num))
results_US_i_num <- data.frame(lapply(results_US, function(x) x$i.num))
results_i_num <- data.frame(lapply(results, function(x) x$i.num))

results_who_summary_i <- data.frame(t(apply(results_who_i_num, 1, calculate_summary)))
results_EACS_summary_i <- data.frame(t(apply(results_EACS_i_num, 1, calculate_summary)))
results_PNHS_summary_i <- data.frame(t(apply(results_PNHS_i_num, 1, calculate_summary)))
results_US_summary_i <- data.frame(t(apply(results_US_i_num, 1, calculate_summary)))
results_summary_i <- data.frame(t(apply(results_i_num, 1, calculate_summary)))


time_vector <- 1:nrow(results_who_summary_i)


results_who_summary_i$time <- time_vector
results_EACS_summary_i$time <- time_vector
results_PNHS_summary_i$time <- time_vector
results_US_summary_i$time <- time_vector
results_summary_i$time <- time_vector



I_number <- bind_rows(
  mutate(results_who_summary_i, scenario = "WHO"),
  mutate(results_EACS_summary_i, scenario = "EACS"),
  mutate(results_PNHS_summary_i, scenario = "PNHS"),
  mutate(results_US_summary_i, scenario = "US"),
  mutate(results_summary_i,scenario='NO PrEP')
)
I_number<-na.omit(I_number)







ggplot(I_number, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3) +
  theme_minimal() +
  xlab("Infected") +
  ylab("Value") +
  labs(color = "Scenario")


####GET AVERTED CASES


library(dplyr)

no_prep_data <- I_number %>% 
  filter(scenario == 'NO PrEP')

I_number_minus_no_prep <- I_number %>% 
  left_join(no_prep_data, by = 'time', suffix = c('', '.no_prep')) %>% 
  filter(scenario != 'NO PrEP') %>% 
  mutate(median = -median + median.no_prep,
         lower.25. = -lower.25. + median.no_prep,
         upper.75. = -upper.75. + median.no_prep) %>% 
  select(-ends_with('.no_prep'))


policyavert<-ggplot(I_number_minus_no_prep, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3)+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500),  # specify the positions of the breaks
                     labels = c('2010', '2020', '2030', '2040')) +scale_fill_npg()+
  theme_minimal() +
  xlab("Time") +
  ylab("Averted Cases") +
  labs(color = "scenario")+scale_color_npg()  + theme_classic()+
  theme(legend.key.width = unit(1.5, "cm"),
        panel.grid = element_blank(),
        legend.text = element_text(face="bold",size = 10),
        legend.title = element_text(face="bold",size = 12),
        axis.text.x = element_text(face="bold",size = 14),
        axis.text.y = element_text(face="bold",size = 14),
        axis.title.x = element_text(face="bold",size = 15),
        axis.title.y = element_text(face="bold",size = 15))

ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/policyavert.png',policyavert, height = 4 , width = 7)



##GET AVERTED TABLE



Averted_table_wide_policy <- I_number_minus_no_prep %>%
  filter(time > 500) %>%
  filter(time %% 100 == 0) %>%
  pivot_wider(names_from = scenario, values_from = c(median, lower.25., upper.75.))
Averted_table_wide_policy$time<-(Averted_table_wide_policy$time-500)/50


gt_table_policy_avert <- gt(Averted_table_wide_policy) %>%
  cols_label(
    time = "Year",
    "median_WHO" = "Median",
    "lower.25._WHO" = "75th Percentile",
    "upper.75._WHO"  = "25th Percentile",
    "median_EACS" = "Median",
    "lower.25._EACS" = "75th Percentile",
    "upper.75._EACS" = "25th Percentile",
    "median_PNHS" = "Median",
    "lower.25._PNHS" = "75th Percentile",
    "upper.75._PNHS" = "25th Percentile",
    "median_US" = "Median",
    "lower.25._US" = "75th Percentile",
    "upper.75._US"  = "25th Percentile"
  ) %>%
  tab_header(
    title = "Number of infections averted by different policy over 20 years"
  ) %>%
  tab_spanner(
    label = "WHO",
    columns = c("upper.75._WHO", "median_WHO", "lower.25._WHO")
  )%>%
  tab_spanner(
    label = "EACS",
    columns = c("upper.75._EACS", "median_EACS", "lower.25._EACS")
  )  %>%
  tab_spanner(
    label = "PNHS",
    columns = c("upper.75._PNHS", "median_PNHS","lower.25._PNHS")
  )%>%
  tab_spanner(
    label = "US",
    columns = c("upper.75._US", "median_US","lower.25._US")
  )

gtsave(gt_table_policy_avert,  "/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/gt_table_policy_avert.png")











#GET PREP  NUMBER


results_who_prep <- data.frame(lapply(results_who, function(x) x$prepCurr))
results_EACS_prep <- data.frame(lapply(results_EACS, function(x) x$prepCurr))
results_PNHS_prep <- data.frame(lapply(results_PNHS, function(x) x$prepCurr))
results_US_prep <- data.frame(lapply(results_US, function(x) x$prepCurr))
results_prep <- data.frame(lapply(results, function(x) x$prepCurr))

results_who_summary_prep <- data.frame(t(apply(results_who_prep, 1, calculate_summary)))
results_EACS_summary_prep <- data.frame(t(apply(results_EACS_prep, 1, calculate_summary)))
results_PNHS_summary_prep <- data.frame(t(apply(results_PNHS_prep, 1, calculate_summary)))
results_US_summary_prep <- data.frame(t(apply(results_US_prep, 1, calculate_summary)))
results_summary_prep <- data.frame(t(apply(results_prep, 1, calculate_summary)))


time_vector <- 1:nrow(results_US_summary_prep)


results_who_summary_prep$time <- time_vector
results_EACS_summary_prep$time <- time_vector
results_PNHS_summary_prep$time <- time_vector
results_US_summary_prep$time <- time_vector
results_summary_prep$time <- time_vector



prep_number <- bind_rows(
  mutate(results_who_summary_prep, scenario = "WHO"),
  mutate(results_EACS_summary_prep, scenario = "EACS"),
  mutate(results_PNHS_summary_prep, scenario = "PNHS"),
  mutate(results_US_summary_prep, scenario = "US"),
  mutate(results_summary_prep,scenario='NO PrEP')
)
prep_number<-na.omit(prep_number)



ggplot(prep_number, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3) +
  theme_minimal() +
  xlab("Number of PrEP") +
  ylab("Time") +
  labs(color = "scenario")









#GET TOTAL HUMAN NUMBER


results_who_num <- data.frame(lapply(results_who, function(x) x$num))
results_EACS_num <- data.frame(lapply(results_EACS, function(x) x$num))
results_PNHS_num <- data.frame(lapply(results_PNHS, function(x) x$num))
results_US_num <- data.frame(lapply(results_US, function(x) x$num))
results_num <- data.frame(lapply(results, function(x) x$num))

results_who_summary_num <- data.frame(t(apply(results_who_num, 1, calculate_summary)))
results_EACS_summary_num <- data.frame(t(apply(results_EACS_num, 1, calculate_summary)))
results_PNHS_summary_num <- data.frame(t(apply(results_PNHS_num, 1, calculate_summary)))
results_US_summary_num <- data.frame(t(apply(results_US_num, 1, calculate_summary)))
results_summary_num <- data.frame(t(apply(results_num, 1, calculate_summary)))


time_vector <- 1:nrow(results_US_summary_num)


results_who_summary_num$time <- time_vector
results_EACS_summary_num$time <- time_vector
results_PNHS_summary_num$time <- time_vector
results_US_summary_num$time <- time_vector
results_summary_num$time <- time_vector



human_number <- bind_rows(
  mutate(results_who_summary_num, scenario = "WHO"),
  mutate(results_EACS_summary_num, scenario = "EACS"),
  mutate(results_PNHS_summary_num, scenario = "PNHS"),
  mutate(results_US_summary_num, scenario = "US"),
  mutate(results_summary_num,scenario='NO PrEP')
)
human_number<-na.omit(human_number)



ggplot(human_number, aes(x = time, y = median, group = scenario)) +
  geom_line(aes(color = scenario)) +
  geom_ribbon(aes(ymin = lower.25., ymax = upper.75., fill = scenario), alpha = 0.3) +
  theme_minimal() +
  xlab("total_number") +
  ylab("Value") +
  labs(color = "Scenario")





#GET Total COST

total_cost<- data.frame(I_number$median,I_number$scenario,I_number$time)
colnames(total_cost)<-c('I_number.median','scenario','time')
total_cost<-left_join(total_cost,prep_number[,c('median','scenario','time')],by=c('scenario','time'))
colnames(total_cost)<-c('I_number','scenario','time','prep_number')
total_cost[is.na(total_cost)]<-0


#GET PREP LOST AND 
total_cost$ARTcost<-total_cost$I_number*0.8*200
total_cost$PrEPcost<-total_cost$prep_number*10
reshaped_data <- melt(total_cost, id.vars = c("time", "scenario"), measure.vars = c("ARTcost", "PrEPcost"), variable.name = "tag", value.name = "value")
reshaped_data$tag <- factor(reshaped_data$tag, levels = c('PrEPcost','ARTcost'))
filtered_data <- reshaped_data[reshaped_data$scenario != "NO PrEP", ]






# Create the plot
soft_colors <-  c("#2C7BB6", "#D7191C")
cost_source<-ggplot(filtered_data, aes(time, value)) +
  geom_area(aes(fill = tag)) +
  labs(fill = "Cost Type")+
  facet_wrap(~ scenario, nrow = 2)+
  scale_fill_manual(values = soft_colors)+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500),  # specify the positions of the breaks
                     labels = c('2010', '2020', '2030', '2040'))+
  theme_bw( base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(face="bold", size = 12, color = "black"),
    axis.title = element_text(face="bold", size = 14, color = "black"),
    strip.text = element_text(face="bold", size = 12, color = "black"),
    legend.text = element_text(face="bold", size = 10, color = "black"),
    legend.title = element_text(face="bold", size = 12, color = "black"),
    legend.position = "bottom",  # Move the legend to the bottom
    legend.box = "horizontal",   # Display the legend horizontally
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Remove margin around the legend
    strip.background = element_blank(),
    panel.border = element_blank()  # Remove the border around the entire plot
    # Remove frame/grid border of facet_wrap figure title
  )

cost_source

ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/cost_source_policy.png',cost_source, height = 7 , width = 7)






#get the 
total_cost$totalcost<-total_cost$I_number*0.8*250+total_cost$prep_number*10



ggplot(total_cost, aes(x = time, y = totalcost, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab("Total_cost") +
  labs(color = "Scenario")




#Let's get a cost table







#get the total time loss


total_cost<-left_join(total_cost,human_number[,c('median','scenario','time')],by=c('scenario','time'))
colnames(total_cost)[colnames(total_cost) == 'median'] <- 'total_population'

total_cost$QALY<-(total_cost$total_population*1-total_cost$I_number*0.2)/52



ggplot(total_cost, aes(x = time, y = QALY, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab("QALY") +
  labs(color = "Scenario")


ggplot(total_cost, aes(x = time, y = I_number, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab("Infection") +
  labs(color = "Scenario")


ggplot(total_cost, aes(x = time, y = total_population, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab("Total_number") +
  labs(color = "Scenario")


#GET ACCUMULATIVE VALUE

total_cost <- total_cost %>%
  group_by(scenario) %>%
  mutate(accumulative_QALY = cumsum(QALY))


total_cost <- total_cost %>%
  group_by(scenario) %>%
  mutate(accumulative_prep = cumsum(PrEPcost))

total_cost <- total_cost %>%
  group_by(scenario) %>%
  mutate(accumulative_art = cumsum(ARTcost))

total_cost$accumulative_cost<-total_cost$accumulative_art+total_cost$accumulative_prep


##########GET ACCUMULATIVE TABLE
total_cost_for_table<-total_cost%>%
  filter(time >500)%>%
  filter(time %% 500 == 0)

total_cost_for_table<-total_cost_for_table[,c('time', "scenario","accumulative_art","accumulative_prep","accumulative_cost","accumulative_QALY")]








no_prep <- total_cost_for_table %>% 
  filter(scenario == "NO PrEP") %>% 
  select(-scenario) %>%
  rename_with(~paste0(., "_no_prep"), -time)

# Then join this back to the original dataframe
total_cost_for_table <- left_join(total_cost_for_table, no_prep, by = "time")

# Now, we can calculate the ICER for each scenario and time point
total_cost_for_table <- total_cost_for_table %>%
  mutate(
    delta_cost = accumulative_cost - accumulative_cost_no_prep,
    delta_qaly = accumulative_QALY - accumulative_QALY_no_prep,
    ICER = delta_cost / delta_qaly
  )

# Let's keep only necessary columns
total_cost_for_table <- total_cost_for_table %>% select(time, scenario, ICER,accumulative_art,accumulative_prep,accumulative_cost,accumulative_QALY)


total_cost_for_table<-as.data.frame(total_cost_for_table)






###########

total_cost$totalcost<-total_cost$ARTcost+total_cost$PrEPcost
total_cost <- total_cost %>%
  group_by(scenario) %>%
  mutate(accumulative_cost = cumsum(totalcost))


total_cost$QALY_diff<-total_cost$accumulative_QALY-rep(total_cost[which(total_cost$scenario=='NO PrEP'),]$accumulative_QALY,5)
total_cost$Cost_diff<-total_cost$accumulative_cost-rep(total_cost[which(total_cost$scenario=='NO PrEP'),]$accumulative_cost,5)


total_cost_noraw <- total_cost %>% 
  filter(scenario != "NO PrEP")


total_cost_noraw$ICER<-total_cost_noraw$Cost_diff/total_cost_noraw$QALY_diff

total_cost_noraw<-total_cost_noraw%>%
  filter(time%% 52 == 0)
  
  

total_cost_noraw<-na.omit(total_cost_noraw)
ggplot(total_cost_noraw[which(total_cost_noraw$time>500),], aes(x = time, y = ICER, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab("ICER") +
  labs(color = "Scenario")

###MADE HEAT MAP
total_cost_noraw$time<-2010+total_cost_noraw$time%/%52






# Define color palette for ICER ranges
color_palette <- c( "#d73027",'#FF6C6C',"#d9ef8b","#355E3B")









total_cost_noraw <- total_cost_noraw %>%
  mutate(ICER_cut = cut(ICER, c(-Inf, 0, 20000, 44000, Inf),
                        labels = c("NULL EFFECTIVENESS", "0-20000€", "20000-44000€", "44000+ €")))

# Manually reorder the factor levels
total_cost_noraw$ICER_cut <- factor(total_cost_noraw$ICER_cut,
                                    levels = c("NULL EFFECTIVENESS", "44000+ €", "20000-44000€", "0-20000€"))

heatmap <- ggplot(total_cost_noraw[which(total_cost_noraw$time > 2024),], 
                  aes(x = time, y = scenario, fill = ICER_cut)) +
  geom_tile(width = 1, height = 1) +
  scale_fill_manual(values = color_palette, drop = FALSE) +
  theme_minimal() +
  xlab("Time") +
  ylab("Scenario") +
  labs(fill = "ICER") +
  theme(axis.text.x = element_text(face = "bold", angle = 90, vjust = 0.5),
        axis.text.y = element_text(face = "bold"))

heatmap



ggsave('/Users/boxuan/Desktop/Intership/Potugual/PLOT for thesis/heatmap.png',heatmap, height = 2 , width = 7)


###########




ggplot(total_cost_noraw, aes(x = time, y = Cost_diff, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab(TeX("$Cost_{PrEP} - Cost_{NoPrEP}$")) +
  labs(color = "Scenario")+
  theme(text = element_text(size = 20, family = "serif"))



ggplot(total_cost_noraw, aes(x = time, y = QALY_diff, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab(TeX("$QALY_{PrEP} - QALY_{NoPrEP}$")) +
  labs(color = "Scenario")+
  theme(text = element_text(size = 20, family = "serif"))


total_cost_noraw%>%
  filter(scenario!='US'&scenario!='EACS')%>%
  filter(time>400)%>%
  ggplot( aes(x = time, y = ICER, group = scenario)) +
  geom_line(aes(color = scenario)) +
  theme_minimal() +
  xlab("Time") +
  ylab("ICER") +
  labs(color = "Scenario")+  theme(text = element_text(size = 20, family = "serif"))




