

library(EpiModel)
library(EpiModelHIV)
library(EpiModelHPC)
library(tergmLite)
library(dplyr)


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


sessionInfo()

#GET param estumatuion
#NETWORKPARAM need population size and the proportion of sexual role.
network_info<-Networkparam(1000,c(0.1,0.3,0.6))
st<-network_info$st
est<-network_info$est


# inits <- EpiModelHIV::init_msm()


#INITAILAZATION
inits <- init_msm()
params <- param_msm(startprev = 0.2)
controls <- control_msm(nsteps = 100, nsims=1, ncores=1)


#load('/Users/boxuan/Desktop/Intership/Potugual/test_ABM/est.RData')
#ST USED IN ARRIVAL
#load('/Users/boxuan/Desktop/Intership/Potugual/test_ABM/st.RData')
#####NOT USE NOW##########
#system.time(
#test<-EpiModel::netsim(est, params, inits, controls)
#)

#####NOT USE NOW##########

###############
#test_df<- as.data.frame(test$epi)
#test$epi$tot.tests
#test$epi$hstage.acute
#test$epi$hstage.chronic
#tail(test$epi$hstage.aids,500)
###########


#test_epi<-as.data.frame(test$epi)
#write.csv(test_df, "/data/boxuan/test_simulation.csv", row.names = FALSE)




get_params <- function(x) {
    params <- param_msm(startprev = 0.2,a.rate = x[1],acts.scale = x[2],senario='US')
    inits <- EpiModelHIV::init_msm()
    controls <- control_msm(nsteps = 1000, nsims=1, ncores=1)
    sim <- EpiModel::netsim(est, params, inits, controls) 
    df <- tail(as.data.frame(sim), 200)
    out1 <- mean(df$i.prev,na.rm = TRUE)
    out2 <- unname(coef(lm(df$i.prev ~ seq_along(df$time)))[2])
    out<- c(out1, out2)
    return(out)
}








# Define your parameter ranges
param_ranges <- list(
  seq(0.0004, 0.0007, length.out = 20), # Sequence for first parameter
  seq(0.5, 2.5, length.out = 20)        # Sequence for second parameter
)

# Create a data frame to hold the results
results <- data.frame(
  param1 = numeric(),
  param2 = numeric(),
  out1 = numeric(),
  out2 = numeric()
)

# Iterate over every combination of the parameter ranges
for (param1 in param_ranges[[1]]) {
  for (param2 in param_ranges[[2]]) {
    # Use tryCatch to handle errors and continue the loop
    tryCatch({
      # Call your function with current parameters
      output <- get_params(c(param1, param2))
      
      # Combine the results with the parameters into a new row
      new_row <- data.frame(param1 = param1, param2 = param2, out1 = output[1], out2 = output[2])
      
      # Append the new row to the results data frame
      results <- rbind(results, new_row)
      write.csv(results, "/data/boxuan/results_fitting.csv", row.names = FALSE)
    }, error = function(e) {
      message("An error occurred with parameters: ", param1, ", ", param2, " - ", e$message)
      # Optionally, append NA values to indicate a failure for this parameter set
      results <- rbind(results, data.frame(param1 = param1, param2 = param2, out1 = NA, out2 = NA))
    })
  }
}

# Display the results
write.csv(results, "/data/boxuan/results_fitting.csv", row.names = FALSE)

View(read.csv("/Users/boxuan/Desktop/Intership/Potugual/HPC_ABM/results_fitting.csv"))






test_simu<-  read.csv( "/Users/boxuan/Desktop/Intership/Potugual/HPC_ABM/results_fitting.csv")

ggplot(test_simu, aes(x = param1, y = param2, fill = out2)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", limits = c(-0.00025, 0.00025), breaks = seq(-0.00025, 0.00025, by = 0.00005)) +
  labs(x = "param1", y = "param2", fill = "out2") +
  theme_minimal()


test_simu1<-  read.csv( "/Users/boxuan/Desktop/Intership/Potugual/HPC_ABM/results_fitting1.csv")
test_simu2<-  read.csv( "/Users/boxuan/Desktop/Intership/Potugual/HPC_ABM/results_fitting2.csv")
test_simu3<-  read.csv( "/Users/boxuan/Desktop/Intership/Potugual/HPC_ABM/results_fitting3.csv")
test_simu4<-  read.csv( "/Users/boxuan/Desktop/Intership/Potugual/HPC_ABM/results_fitting4.csv")

combined_data_heat <- rbind(test_simu1, test_simu2, test_simu3, test_simu4)

ggplot(combined_data_heat, aes(x = param1, y = param2, fill = out2)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +  # Customize the color scale
  labs(x = "param1", y = "param2", fill = "out3") +  # Set axis and legend labels
  theme_minimal() 


combined_data_heat$out1<-(combined_data_heat$out1-0.05)/0.05
combined_data_heat$out1<-abs(combined_data_heat$out1)
combined_data_heat$out3<-combined_data_heat$out1*combined_data_heat$out2
  

combined_data_heat$out2<-abs(combined_data_heat$out2)
