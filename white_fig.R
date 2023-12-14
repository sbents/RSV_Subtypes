
setwd("//home//bentssj//white")

#library(tidyverse)
library(ggplot2)
library(ggpubr)
#library(deSolve)
library(dplyr)
#library(tidyverse)


                                                               
                                                               
std.error <- function(x) sd(x)/sqrt(length(x))

#simulation = readRDS("sim_1_45_50npi_final.rds")

simulation1 = readRDS("RSV_NPI_timeseries_simulation_40_1.rds")
simulation2 = readRDS("RSV_NPI_timeseries_simulation_40_2.rds")
simulation3 = readRDS("RSV_NPI_timeseries_simulation_40_3.rds")
simulation4 = readRDS("RSV_NPI_timeseries_simulation_40_4.rds")
simulation5 = readRDS("RSV_NPI_timeseries_simulation_40_5.rds")
simulation6 = readRDS("RSV_NPI_timeseries_simulation_40_6.rds")
simulation7 = readRDS("RSV_NPI_timeseries_simulation_40_7.rds")
simulation8 = readRDS("RSV_NPI_timeseries_simulation_40_8.rds")
simulation9 = readRDS("RSV_NPI_timeseries_simulation_40_9.rds")
simulation10 = readRDS("RSV_NPI_timeseries_simulation_40_10.rds")
simulation11 = readRDS("RSV_NPI_timeseries_simulation_40_11.rds")

simulation_40 = rbind(simulation1, simulation2, simulation3, simulation4, simulation5,
                   simulation6, simulation7, simulation8, simulation9, 
                   simulation10, simulation11)


simulation = simulation_40 %>%
  # mutate(npi_start = npi_start_list[i]) %>%
  # subset(sigma_he == sigma & r == r_reduction & npi_effect == s) %>% 
  mutate(time = time - (npi_start*365)) %>% 
  dplyr::select(-c(lambda_B, beta_A)) %>%
  subset(time >= -10*365 & time <= 365*100) %>% 
  mutate(start_quarter = case_when(npi_start %in% c(100:112) ~ "January", 
                                   npi_start %in% c(c(100:111)+0.25) ~ "April", 
                                   npi_start %in% c(c(100:111)+0.5) ~ "July", 
                                   npi_start %in% c(c(100:111)+0.75) ~ "October", 
                                   TRUE ~ "error")) %>%
  group_by(strain, time, country, npis_country) %>%
  mutate(total = sum(N))



simulation %>% 
  mutate(time = floor(time)) %>%
  subset(npi_start != 1012) %>%
  group_by(time, country, npis_country, strain, start_quarter) %>%
  dplyr::summarize(mean_infected = mean(total),
            mean_proportion_A = mean(proportion_A), 
            se_proportion_A = std.error(proportion_A),
            mean_proportion_primary = mean(proportion_primary), 
            mean_primary_A = mean(primary_A), 
            mean_primary_B = mean(primary_B)) %>%
  ungroup() -> summary  

m <- "Finland"

summary %>%
  subset(start_quarter=="April")%>%
  subset(strain != "uninfected" & country == m) -> temp_figure  






# figure 1 
simulation %>%
  # subset(start_quarter == "April") %>%
  subset(npi_start != 1012) %>%
  subset(country == m) %>%
  subset(strain != "uninfected" ) %>% # country == m & & sigma_he == 0.846 & r == 0.9159
  mutate(original_b = case_when(country == "Finland" ~ 99.51/365, 
                                country == "E&W" ~ 113.99/365, 
                                TRUE ~ 0), 
         npi_stringency = - ((npi_effect / original_b )- 1)) %>% 
  ggplot()+
  geom_hline(yintercept = 0.5, color = "black", lwd = 0.6)+
  geom_vline(xintercept = 0, color = "black", lwd = 0.6, lty = "dashed") + #turquise
  geom_line(aes(x = time, y = npi_stringency), lwd = 0.9, color = "burlywood3")+
  geom_line(data= temp_figure, aes(x = time, y = mean_proportion_A), color = "royalblue4", lwd = 0.9)+
  geom_ribbon(data= temp_figure, aes(x = time, ymin=mean_proportion_A-1.96*se_proportion_A, ymax=mean_proportion_A+1.96*se_proportion_A), 
              alpha=0.25, fill = "royalblue4", color = NA)+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Cases proportion A",
    
    # Add a second axis and specify its features
   sec.axis = sec_axis(~.*1, name="Reduction in transmission coefficient (%)")
 ) +
 #scale_x_continuous(limits = c(-5*365, 25*365), breaks = c(seq(-5*365, 365*25, 365*5)), labels = c(seq(-5, 25, 5)))+
  scale_x_continuous(limits = c(-3*365, 5*365), breaks = c(seq(-3*365, 365*5, 365)), labels = c(seq(-3, 5, 1)))+
  labs(x = "Years since NPI implementation", tag = "A", title = paste0("NPI effect = ", 0.4*100, "% maximum transmission reduction","\n\nMean proportion of subtype A cases following NPI lift")) + #  y = "Cases proportion A",
   facet_wrap(~ npis_country, nrow = 1)+
  coord_cartesian(ylim=c(0, 1)) +
  theme(axis.title.y = element_text(size = 1)) +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"), 
       axis.title.y = element_text(color = "black", size = 8, face = "plain", , vjust = 2),
        axis.title.x = element_text(color = "black", size = 8, face = "plain"),
       axis.text.y = element_text(color = "black", size = 5, face = "plain"),
       axis.text.x = element_text(color = "black", size = 5, face = "plain")) -> proportion_cases_long


ggarrange(proportion_cases_long)

#sim 2
simulation %>%
  subset(start_quarter=="April")%>%
  subset(npi_start != 1012) %>%
  subset(country == m) %>%
  subset(strain != "uninfected" ) %>% #country == m &  & sigma_he == 0.846 & r == 0.9159
  ggplot() +
  geom_vline(xintercept = 0, color = "black", lwd = 0.6, lty = "dashed") +
  geom_col(data = temp_figure, aes(x = time, y = mean_infected, fill = strain), color = NA, alpha = 1)+ 
  scale_fill_manual(labels = c("A", "B"), values = c("darkseagreen2", "turquoise4")) + 
  scale_x_continuous(limits = c(-3*365, 5*365), breaks = c(seq(-3*365, 365*5, 365*1)), labels = c(seq(-3, 5, 1)))+
  labs(x = "Years since NPI implementation", color = "Subtype", y = "Percent population infected", title = "Mean percent population infected with RSV by subtype", tag = "B") +
  # facet_wrap(~factor(start_quarter, levels = c("January", "April", "July", "October")), labeller = as_labeller(npi_durations_figure), nrow=1) + 
  facet_wrap(~ npis_country, nrow = 1)+
  guides(fill=guide_legend(title="Subtype"))+
  theme(axis.title.y = element_text(color = "black", size = .25)) +
  theme_bw() + 
  # coord_cartesian(ylim=c(0, 0.3)) +
  theme(strip.background =element_rect(fill="white")) + 
  theme(legend.position = "bottom", 
        axis.title.y = element_text(color = "black", size = 8, face = "plain", vjust = 2),
        axis.title.x = element_text(color = "black", size = 8, face = "plain"), 
        axis.text.y = element_text(color = "black", size = 5, face = "plain"),
        axis.text.x = element_text(color = "black", size = 5, face = "plain")) -> subtype_cases_long



ggarrange(proportion_cases_long, subtype_cases_long, nrow=2)
ggsave(paste0("time_varying_NPI_40fig_dec12",m,".png"), units = "in", height = 7, width = 10)




read.csv("hi.csv")

# other plot for prop a and before and after 



data_fin_1 = simulation_40 %>% dplyr::select(time, npi_effect, npi_start, country, 
                                          npis_country, total, primary_A, proportion_A) %>%
  mutate(npi_int = as.integer(npi_start)) %>%
  mutate(diff = npi_start - npi_int) %>%
  filter(diff == .25) %>%
  mutate(start_quarter = as.character(diff)) %>%
  mutate(time = time - npi_start*365) 

data_fin_1 %>%
  subset((country == "Finland" & npis_country == "Finland") | ((country == "E&W" & npis_country == "United Kingdom"))) %>%
  mutate(time = floor(time)) %>% 
  subset(time > -365 & time < 0) %>% 
  ungroup() %>%
  select(time, npi_start, start_quarter, country, npis_country, proportion_A) %>%
  group_by(npi_start, start_quarter, country, npis_country) %>%
  mutate(pre_pandemic = mean(proportion_A)) %>%
  select(-time, -proportion_A) %>%
  unique() -> temp

data_fin_1 %>%
  subset((country == "Finland" & npis_country == "Finland") | ((country == "E&W" & npis_country == "United Kingdom"))) %>%
  mutate(time = floor(time)) %>% 
  subset(time > 0 & time < (365*3)) %>% 
  ungroup() %>%
  select(time, npi_start, start_quarter, country, npis_country, proportion_A) %>%
  group_by(npi_start, start_quarter, country, npis_country) %>%
  mutate(year_1 = mean(proportion_A)) %>%
  select(-time, -proportion_A) %>%
  unique() -> temp1



# two years
library(tidyverse)

left_join(temp, temp1) %>%
  rowid_to_column(var = "simID") %>%
  pivot_longer(cols = c("pre_pandemic", "year_1")) -> temp_data

subtype_labs =  c("Predominantly subtype B before pandemic", "Predominantly subtype A before pandemic")
names(subtype_labs) = c(0, 1)

temp_data %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(type_A = case_when(pre_pandemic >= 0.5 ~ 1, 
                            TRUE ~ 0)) %>%
  pivot_longer(cols = c("pre_pandemic", "year_1")) %>%
  ggplot()+
  geom_boxplot(aes(x = name, y = value, fill = name), alpha = 0.8, lwd = .8) + 
  labs(x = "", y = "Proportion of cases in subtype A", fill = "")+
  scale_fill_manual(values = c("darkseagreen2", "turquoise4")) + 
  facet_wrap(~ type_A, labeller = labeller(type_A=subtype_labs)) +
  scale_x_discrete(labels = c('Pre-pandemic','Three years since NPI implementation')) +
  theme_bw() + 
  theme(legend.position = "none")+
  theme(
    strip.text = element_text(size = 10)) +
  theme(strip.background = element_rect(colour="black",
                                        fill="white")) -> prop



ggarrange(prop)
ggsave(paste0("prop40",m,".png"), units = "in", height = 6, width = 11)



ggarrange(prop)
ggsave(paste0("prop40_2",m,".png"), units = "in", height = 8, width = 11)



ggarrange(prop)
ggsave(paste0("prop40_3",m,".png"), units = "in", height = 7, width = 11)






                                                               
# i used these
#ggsave(paste0("time_varying_NPI_figures_long_25yearbday1_",m,".png"), units = "in", height = 10, width = 7) #width was 14
#ggsave(paste0("time_varying_NPI_figures_long_25yearbday2_",m,".png"), units = "in", height = 7, width = 10) 
#ggsave(paste0("time_varying_NPI_figures_long_25yearbday3_",m,".png"), units = "in", height = 10, width = 10) 
#ggsave(paste0("time_varying_NPI_figures_long_25yearbday4_",m,".png"), units = "in", height = 7, width = 14) 
#ggsave(paste0("time_varying_NPI_figures_long_25yearbday5_",m,".png"), units = "in", height = 14, width = 7) 












#ggsave(paste0("time_varying_NPI_figures_long_25yearB_",m,".png"), units = "in", height = 10, width = 4) #width was 14


#ggsave(paste0("time_varying_NPI_figures_long_25yearC_",m,".png"), units = "in", height = 10, width = 6) #width was 14

#ggsave(paste0("time_varying_NPI_figures_long_25yearD_",m,".png"), units = "in", height = 10, width = 10) #width was 14

#ggsave(paste0("time_varying_NPI_figures_long_25yearE_",m,".png"), units = "in", height = 20, width = 15) #w

#ggsave(paste0("time_varying_NPI_figures_long_25year_",m,".png"), units = "in", height = 7, width = 13) #width was 14

#ggsave(paste0("time_varying_NPI_figures_long_25year1_",m,".png"), units = "in", height = 7, width = 10) #width was 14


#ggsave(paste0("time_varying_NPI_figures_long_25year2_",m,".png"), units = "in", height = 7, width = 7) #width was 14

# this was save for somem reason
#ggsave(paste0("time_varying_NPI_figures_long_5yheyo_",m,".png"), units = "in", height = 14, width = 10) 

