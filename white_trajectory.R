
setwd("//home//bentssj//white")

#library(tidyverse)
library(ggplot2)
library(ggpubr)
library(viridis)
#library(deSolve)
library(dplyr)
#library(tidyverse)


simulation = readRDS("sim_1_45_50npi_final.rds")

print(unique(simulation$country))
print(unique(simulation$npis_country))

simulation1 = simulation %>% 
  mutate(slice = ifelse(country == "Finland" & npis_country == "United Kingdom", "remove", "keep")) %>%
  mutate(slice1 = ifelse(country == "E&W" & npis_country == "Finland", "remove", "keep")) %>%
  filter(slice != "remove") %>%
  filter(slice1 != "remove")

  

check = simulation1 %>% filter(country == "Finland")
print(unique(check$npis_country))

check1 = simulation1 %>% filter(country == "E&W")
print(unique(check1$npis_country))



simulation2 = simulation1 %>%
  # mutate(npi_start = npi_start_list[i]) %>%
  # subset(sigma_he == sigma & r == r_reduction & npi_effect == s) %>% 
  mutate(time = time - (npi_start*365)) %>% 
  dplyr::select(-c(lambda_B, beta_A)) %>%
  subset(time >= -100*365 & time <= 365*100) %>% 
  mutate(start_quarter = case_when(npi_start %in% c(100:112) ~ "January", 
                                   npi_start %in% c(c(100:111)+0.25) ~ "April", 
                                   npi_start %in% c(c(100:111)+0.5) ~ "July", 
                                   npi_start %in% c(c(100:111)+0.75) ~ "October", 
                                   TRUE ~ "error")) %>%
  group_by(strain, time, country, npis_country) %>%
  mutate(total = sum(N))


simulation2 %>% 
  mutate(time = floor(time)) %>%
  subset(npi_start != 1012) -> summary   # %>%
 # group_by(time, country, npis_country, strain, start_quarter) %>%
#  dplyr::summarize(mean_infected = mean(total),
 #                  mean_proportion_A = mean(proportion_A), 
#                   se_proportion_A = std.error(proportion_A),
#                   mean_proportion_primary = mean(proportion_primary), 
#                   mean_primary_A = mean(primary_A), 
#                   mean_primary_B = mean(primary_B)) %>%
#  ungroup() -> summary  

#m <- "Finland"

summary %>%
  subset(start_quarter=="April")%>%
  subset(strain != "uninfected" ) -> temp_figure #%>%
 # subset(strain != "uninfected" & country == m) -> temp_figure  


# my code
npi_start = unique(temp_figure$npi_start)
sim_num = seq(1, length(npi_start ), 1)

set = data.frame(npi_start, sim_num)

simulation_set = left_join(temp_figure, set, by  = "npi_start"  )

#print(unique(simulation_set$sim_num))

##sim_set = simulation_set %>%
##  mutate(yr = as.integer(time/1825)) %>%
##  group_by(yr, sim_num, country) %>% # chcek country with npi
##  mutate(prop_A = mean(proportion_A)) %>%
##  distinct(country, prop_A, yr, sim_num) %>%
##  filter(yr < 11) %>%
##  filter(yr > -11) 



##heat_map =  ggplot(data = sim_set, aes(x = as.factor(yr), y = as.factor(sim_num), fill = prop_A)) +
##  geom_tile() +
##  labs(x = "Time - 5 year increments",
##       y = "Simulation") +
##  facet_wrap(vars(country)) +
##  theme_bw() +
##  scale_fill_viridis(option = "H") +
##  ggtitle("Mean yearly proportion A")



#ggarrange(heat_map)

#ggsave(paste0("HM_5yr_mean_vir1.png"), units = "in", height = 6, width = 12)


# max from time period
# this is weekly so remove it 

#sim_set1 = simulation_set %>%
#  mutate(yr = as.integer(time/7)) %>%
#  group_by(yr, sim_num, country) %>% # chcek country with npi
#  mutate(prop_A = mean(proportion_A)) %>%
#  distinct(country, prop_A, yr, sim_num) %>%
#  filter(yr < 52*5) %>%
#  filter(yr > -52*5) 

#heat_map1 =  ggplot(data = sim_set1, aes(x = as.factor(yr), y = as.factor(sim_num), fill = prop_A)) +
#  geom_tile() +
# labs(x = "Years since NPI",
#       y = "Simulation") +
#  facet_wrap(vars(country)) +
#  theme_bw() +
# scale_fill_viridis(option = "B", direction = -1) +
#  ggtitle("Mean weekly proportion A by simulaiton") +
#  geom_vline(xintercept = "0", col = "black",lwd = 2)  +
#  scale_x_discrete(breaks = c( -5*52, 0, 5*52),
#                   labels=c('-5', '0', '5'))

#  scale_x_continuous(limits = c(-10*52, 10*52), breaks = c(seq(-10*52, 10*52, 52*1)), labels = c(seq(-10, 10, 1)))


#ggarrange(heat_map1)
#ggsave(paste0("oct31_1_weekly.png"), units = "in", height = 6, width = 12)









# this is the final yearly plot !! nov 2 

# 
sim_set2 = simulation_set %>%
  mutate(yr = as.integer(time/365)) %>%
  group_by(yr, sim_num, country) %>% # chcek country with npi
  mutate(prop_A = mean(proportion_A)) %>%
  distinct(country, prop_A, yr, sim_num) %>%
  mutate(country = ifelse(country == "E&W", "United Kingdom", "Finland")) %>%
  filter(yr > -26) %>%
  filter(yr < 76)        # %>%


heat_map2 =  ggplot(data = sim_set2, aes(x = as.factor(yr), y = as.factor(sim_num), fill = prop_A)) +
  geom_tile() +
  labs(x = "Years since NPI implementation",
       y = "Simulation") +
  facet_wrap(vars(country), nrow = 2) +
  theme_bw() +
  scale_fill_viridis(option = "G",  begin = .28,
                     end = 1, direction = -1) +   # direction = -1,
  ggtitle("Mean yearly proportion A by simulation") +
 geom_vline(xintercept = "0", col = "black", lwd =1.5, linetype = "dashed")  +
  scale_x_discrete(breaks = c(-25,  0, 25, 50, 75),labels=c('-25', '0', '25', '50', '75'))+
  labs(fill = "Proportion A") +
  theme(strip.background = element_rect(colour="black",
                                        fill="white"))



ggarrange(heat_map2)
ggsave(paste0("nov10_yearly_75yr_yo.png"), units = "in", height = 6, width = 9)

ggarrange(heat_map2)
ggsave(paste0("nov10_yearly_75yr_yo.png"), units = "in", height = 6, width = 7)

#sim_set3 = simulation_set %>%
#  mutate(yr = as.integer(time/1095)) %>%
# group_by(yr, sim_num, country) %>% # chcek country with npi
#  mutate(prop_A = mean(proportion_A)) %>%
#  distinct(country, prop_A, yr, sim_num) %>%
#  filter(yr < (1095*4 + 1)) %>%
#  filter(yr > (-1095*4 - 1))

#heat_map3 =  ggplot(data = sim_set3, aes(x = as.factor(yr), y = as.factor(sim_num), fill = prop_A)) +
 # geom_tile() +
#  labs(x = "Years since NPI",
#       y = "Simulation") +
#  facet_wrap(vars(country)) +
#  theme_bw() +
#  scale_fill_viridis(option = "B", direction = -1) +
#  ggtitle("Mean 3 yearly proportion A by simulaiton") +
#  geom_vline(xintercept = "0", col = "black", lwd = 2)  +
#  scale_x_discrete(breaks = c( -4*1095, -3*1095, -2*1095, -1*1095, 0, 1*1095, 2*1095, 3*1095, 4*1095),
#                   labels=c('-12', '-9','-6', '-3', '0', '3','6', '9', '12'))


#ggarrange(heat_map3)
#ggsave(paste0("oct31_3_3yr.png"), units = "in", height = 6, width = 12)



#)_______________________________________________________________________
# map out individal simulations 
# this is final

#ind_traject = simulation_set %>%
#  mutate(yr = time/365) %>%
# filter(yr < 11) %>%
#  filter(yr > -6) 
  

# individual trajectory plot
#ind_traject$sim_num = factor(ind_traject$sim_num , levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))

#ind_traj_sim = ggplot(data = ind_traject, aes(x = yr, y = proportion_A, col = factor(sim_num, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")))) +
#  geom_line() +
 # geom_vline(xintercept = 0, linetype = "dashed", col = "black")  +
#  xlab('Years since NPI')+
#  ylab("Proportion A") +
#  theme_light() + 
#  facet_grid(rows = vars(sim_num), cols = vars(country)) + 
#  scale_color_viridis_d(option = "G", begin = .2,
#                        end = .8) +
#  labs(color = "Simulation")




# facto sim 1-10
#ggarrange(ind_traj_sim)

#ggsave(paste0("ind_traj_sim_nov2.png"), units = "in", height = 10, width = 15)




    

#________________________________________________________________


#sim_num x yr, filled my prop A

#time = c(seq(1, 10, 1), seq(1, 10,1))
#a = rnorm(20, 10, 10.2)
#simn = c(rep(1, 10), rep(2, 10))
#check = data.frame(time, a, simn) 

#check1 = check %>%
#  mutate(no = ifelse(time == 2 & simn == 1, "0", "1"))

#mutate(paired = ifelse(sample_ID %in% ps_id, "YES", "NO"))%>%
  
  
#ggplot(data = check, aes(x = as.factor(time), y = as.character(simn), col = a)) +
##  geom_tile() +
#  labs(title = "Correlation Heatmap",
#      x = "Variable 1",
#     y = "Variable 2") +
#  scale_color_viridis(option = "H")
#  scale_fill_gradient2(low = "#075AFF",
#                       mid = "#FFFFCC",
#                      high = "#FF0000") +
#  theme_bw()

#filter(simulation, country == "E&W" & npis_country == "Finland")

#ggplot(data1, aes(x = Var1, y = Var2, fill = value)) +
#  geom_tile() +
#  labs(title = "Correlation Heatmap",
#       x = "Variable 1",
#       y = "Variable 2")


# write code for heat  map

#set.seed(110)

# Create example data
#data <- matrix(rnorm(100, 0, 5), nrow = 10, ncol = 10)

# Column names
#colnames(data) <- paste0("col", 1:10)
#rownames(data) <- paste0("row", 1:10)

# Draw a heatmap
#heatmap(data)   

#print(unique(simulation$N))

#sim_time = simulation %>% mutate(time = time - (npi_start*365)) %>%
#  mutate(years = time/5) %>%
#  filter(years > -5) %>%
#  filter(years < 5)

#traject = ggplot(data = sim_time) +
#  geom_line(aes(x = years, y = proportion_A))+
#  facet_wrap(vars(country))


#ggarrange(traject)

#ggsave(paste0("traject.png"), units = "in", height = 7, width = 20) #w


# na \h 
# try in weeks 

#sim_set2 = simulation_set %>%
#  mutate(yr = as.integer(time/52)) %>%
#  group_by(yr, sim_num, country) %>% # chcek country with npi
#  mutate(prop_A = mean(proportion_A)) %>%
#  distinct(country, prop_A, yr, sim_num) %>%
#  filter(yr > -26*52) %>%
#  filter(yr < 51*52)        # %>%


#heat_map2 =  ggplot(data = sim_set2, aes(x = as.factor(yr), y = as.factor(sim_num), fill = prop_A)) +
#  geom_tile() +
##  labs(x = "Years since NPI",
#       y = "Simulation") +
#  facet_wrap(vars(country)) +
#  theme_bw() +
#  scale_fill_viridis(option = "G",  begin = .28,
#                     end = 1, direction = -1) +   # direction = -1,
#  ggtitle("Mean weekly proportion A by simulation") +
#  geom_vline(xintercept = "0", col = "black", lwd =1.5, linetype = "dashed")  +
#  scale_x_discrete(breaks = c(-25*52,  0, 25*52, 50*52),labels=c('-25', '0', '25', '50'))+
#  labs(fill = "Proportion A")



#ggarrange(heat_map2)
#ggsave(paste0("nov2_yearly_50yr_week.png"), units = "in", height = 6, width = 14)



