library(tidyverse)
library(ggplot2)
library(ggpubr)
library(deSolve)
library(parallel)
library(foreach)
library(doParallel)
library(dplyr)


setwd("//home//bentssj//white")



npi_data <- read.csv("npi_data_40.csv") %>%
  dplyr::select(location, npi_efficacy, time) %>%
  mutate(time = as.numeric(time), npi_efficacy = as.numeric(npi_efficacy))

## code for multi - subtype RSV model from White et al. 2004

parms_EW <- c( ## global parameters (fixed between locations)
  r = 0.9159, ## group B average transmission coefficient relative to group A
  n = 0.4126, ## altered infectiousness factor
  sigma_ho = 0.3569,  ## altered susceptibility to homologous secondary infection
  sigma_he = 0.8426,   ## altered susceptibility to heterologous secondary infection
  v = 40.56/365, ## recovery rate
  w = 0.51/365, ## rate of loss of partial immunity 
  
  ## Local parameters
  
  b_A = 113.99/365, ## group a average transmission coefficient
  mu = 0.014/(365),  ## birth rate
  phi = 0.929, ## point in the year of peak transmission coefficient
  a = 0.815, # amplitude of transmission coefficient
  s_f = 397.3, # scaling factor
  npi = 0,  # NPI factor
  npi_start = 4*365, 
  npi_duration = 4, 
  country = 1)          


parms_finland <- c( ## global parameters (fixed between locations)
  r = 0.9159 , ## group B average transmission coefficient relative to group A
  n = 0.4126, ## altered infectiousness factor
  sigma_ho = 0.3569,  ## altered susceptibility to homologous secondary infection
  sigma_he = 0.8426,   ## altered susceptibility to heterologous secondary infection
  v = 40.56/365, ## recovery rate
  w = 0.51/365, ## rate of loss of partial immunity 
  
  ## Local parameters
  
  b_A = 99.51/365, ## group a average transmission coefficient
  mu = 0.012/(365),  ## birth rate
  phi = 0.97, ## point in the year of peak transmission coefficient
  a = 0.347, # amplitude of transmission coefficient
  s_f = 6.638, # scaling factor
  npi = 0,  # NPI factor
  npi_start = 4*365, 
  npi_duration = 4, 
  country = 0) 


inits <- c(## uninfected
  X = 1 - 0.02,  # Proportion uninfected with no previous infection
  X_A = 0,  # Proportion uninfected with a previous infection w/ group A
  X_B = 0,  # Proportion uninfected with a previous infection w/ group B
  X_AB = 0,  # Proportion uninfected with previous infections w/ groups A and B
  
  ## primary infection (for a given subtype)
  P_A = 0.015,   ##  currently infected with group A, no previous infection
  P_B = 0.005,    ##  currently infected with group B, no previous infection
  P_BA = 0,   ##  currently infected with group A, previously infected w/ group B
  P_AB = 0,   ##  currently infected with group B, previously infected w/ group A
  
  ## secondary/subsequent infections
  Y_A = 0,  ##  currently infected with group A, previously infected w/ group A
  Y_B = 0,  ##  currently infected with group B, previously infected w/ group B
  Y_AB = 0,  ##  currently infected with group B, previously infected w/ group A *and* B
  Y_BA = 0)  ##  currently infected with group A, previously infected w/ group A *and* B


duration <- 200 # 1000
dt <- seq(1, duration*365, 1)


interpolate_stringency <- function(x, data) {
  approx(x=data$rowid, y=data$npi_efficacy, xout=x)$y
}

## now modeling b_A as a function of stringency
betaFun <- function(t, b_A, data) {
  
  b_A*(1-interpolate_stringency(t, data))
  
}


White_model_NPI_time_varying <- function(t, x, parms, data){
  
  with(as.list(c(parms,x)),{
    
    npi_effect <- betaFun(t, b_A, data)
    beta_A <- (a*cos(2*pi*(t/365-phi))+1) * npi_effect
    beta_B <- r*beta_A
    
    lambda_A <- beta_A*(P_A + n*(P_BA + Y_A + Y_BA))
    lambda_B <- beta_B*(P_B + n*(P_AB + Y_B + Y_AB))
    
    dX <- mu - (lambda_A + lambda_B + mu)*X + w*(X_A + X_B)
    
    dP_A <- lambda_A*X - (v+mu)*P_A
    dP_B <- lambda_B*X - (v+mu)*P_B
    
    dX_A <- v*(P_A + Y_A) - (sigma_ho*lambda_A + sigma_he*lambda_B + w + mu)*X_A + w*X_AB
    dX_B <- v*(P_B + Y_B) - (sigma_ho*lambda_B + sigma_he*lambda_A + w + mu)*X_B + w*X_AB
    
    dP_BA <- sigma_he*lambda_A*X_B - (v + mu)*P_BA
    dP_AB <- sigma_he*lambda_B*X_A - (v + mu)*P_AB
    
    dY_A <- sigma_ho*lambda_A*X_A - (v + mu)*Y_A
    dY_B <- sigma_ho*lambda_B*X_B - (v + mu)*Y_B
    
    dX_AB <- v*(P_AB + P_BA + Y_AB + Y_BA) - (sigma_he*sigma_ho*(lambda_A + lambda_B) + 2*w + mu)*X_AB
    
    dY_BA <- sigma_he*sigma_ho*lambda_A*X_AB - (v + mu)*Y_BA
    dY_AB <- sigma_he*sigma_ho*lambda_B*X_AB - (v + mu)*Y_AB
    
    
    der <- c(dX, 
             dX_A, dX_B, 
             dX_AB, 
             dP_A, dP_B, 
             dP_BA, dP_AB, 
             dY_A, dY_B, 
             dY_AB, dY_BA)
    
    list(der, npi_effect = npi_effect, beta_A = beta_A, lambda_A = lambda_A, lambda_B = lambda_B) 
    
  })
} 

#npi_start_list <- c(seq(100, 111.75, 0.25))
#npi_start_list <- c(seq(50, 61.75, 0.25))
#npi_start_list <- c(seq(200, 211.75, 0.25))
#npi_start_list <- c(seq(104, 111.75, 0.25))
#npi_start_list <- c(seq(106.25, 111.75, 0.25)) #started 100
location_list <- c("Finland", "United Kingdom")
#npi_start_list <- c(seq(108.5, 111.75, 0.25)) 
#npi_start_list <- c(seq(109.75, 111.75, 0.25)) 
#npi_start_list <- c(seq(111, 111.75, 0.25)) 

npi_start_list <- c(seq(110.25, 111.25, 1))

j_list <- c(1:length(npi_start_list))
#numCores <- detectCores()-1
#registerDoParallel(numCores)

# RAN 5 OF THEM 
# foreach(j=j_list, .combine=rbind) %dopar% {
for(j in j_list){
  
  simulation_npi <- NULL
  
  for(m in list(parms_EW, parms_finland)){
    
    parms <- m
    
    parms["npi_start"] <-  npi_start_list[j]*365
    
    for(c in location_list) {
      
      npi_data %>% 
        subset(location == c) %>% 
        select(npi_efficacy) -> working_data
      
      data.frame("npi_efficacy" = rep(0, length(c(1:(npi_start_list[j]*365))))) %>% 
        rbind(working_data) %>% 
        rbind(data.frame("npi_efficacy" = rep(0, length(c((nrow(working_data)+length(c(1:(npi_start_list[j]*365)))):(duration*365)))))) %>%
        rowid_to_column() -> npi_timeseries
      
      simulation <- as.data.frame(ode(inits, dt, White_model_NPI_time_varying, parms=parms, data=npi_timeseries))
      
      simulation %>%
        # subset(time >= 995*365) %>% 
        mutate(npi_start = npi_start_list[j], 
               country = ifelse(parms["country"]==1, "E&W", "Finland"), 
               npis_country = c, 
               total = X + P_A + P_B + X_A + X_B + P_BA + P_AB + Y_A + Y_B + X_AB + Y_BA + Y_AB,
               primary_A = P_A/(P_A + P_BA + Y_A + Y_BA), 
               primary_B = P_B/(P_B + P_AB + Y_B + Y_AB), 
               proportion_primary = (P_A + P_B) / (P_A + P_BA + Y_A + Y_BA + P_B + P_AB + Y_B + Y_AB), 
               proportion_A = (P_A + P_BA + Y_A + Y_BA) / (P_A + P_BA + Y_A + Y_BA + P_B + P_AB + Y_B + Y_AB)) %>%
        pivot_longer(X:Y_BA, names_to = "state", values_to = "N") %>%
        mutate(strain = case_when(state %in% c("P_A", "P_BA", "Y_A", "Y_BA") ~ "subtype A", 
                                  state %in% c("P_B", "P_AB", "Y_B", "Y_AB") ~ "subtype B", 
                                  state %in% c("X", "X_A", "X_B", "X_AB") ~ "uninfected", 
                                  TRUE ~ ""), 
               infection = case_when(state %in% c("P_A", "P_B") ~ "primary", 
                                     state %in% c("Y_A", "Y_B", "Y_BA", "Y_AB", "P_AB", "P_BA") ~ "subsequent", 
                                     state %in% c("X", "X_A", "X_B", "X_AB") ~ "uninfected", 
                                     TRUE ~ "")) %>%
        rbind(simulation_npi) -> simulation_npi
      
    }
  }
  # write.csv(simulation_npi, paste0("RSV_NPI_simulation_",j,"_",s,"_",sigma,"_",r_reduction,".csv"))
  saveRDS(simulation_npi, paste0("RSV_NPI_timeseries_simulation_40next_",j,".rds"))
  
}