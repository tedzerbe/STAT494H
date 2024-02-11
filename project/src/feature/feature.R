# Front Matter ---------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Load FanGraphs and Baseball Savant data
all_data <- read.csv("project/volume/data/interim/all.csv")



# Data Manipulation ---------------------------------------------------------------------
# Create ID variable
all_data <- all_data %>%
  mutate(PlayerID = paste0(PlayerId, "_", MLBAMID)) %>%
  select(PlayerID, Name, Season, everything(), -PlayerId, -MLBAMID, -NameASCII, -Team)

# Separate data frames into seasons
data_23 <- all_data %>% filter(Season == 2023)
data_22 <- all_data %>% filter(Season == 2022)
data_21 <- all_data %>% filter(Season == 2021)
data_20 <- all_data %>% filter(Season == 2020)
data_19 <- all_data %>% filter(Season == 2019)
data_18 <- all_data %>% filter(Season == 2018)
data_17 <- all_data %>% filter(Season == 2017)
data_16 <- all_data %>% filter(Season == 2016)
data_15 <- all_data %>% filter(Season == 2015)

# Restructure data
predict_2023 <- data_23 %>%
  rename_with(~paste0(., "_future"), -(1:2)) %>% select(-contains("_future"), Season_future, Start_IP_future, ERA_Minus_future, Age_future) %>%
  left_join(data_22 %>% rename_with(~paste0(., "_past1"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past1, -Age_past1) %>%
  left_join(data_21 %>% rename_with(~paste0(., "_past2"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past2, -Age_past2) %>%
  left_join(data_20 %>% rename_with(~paste0(., "_past3"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past3, -Age_past3) %>%
  select(-Stuff_Plus_past2, -Stuff_Plus_past3, -Location_Plus_past2, -Location_Plus_past3, -Pitching_Plus_past2, -Pitching_Plus_past3,
         -botStf_past2, -botStf_past3, -botCmd_past2, -botCmd_past3, -botOvr_past2, -botOvr_past3,
         -vFA_past2, -vFA_past3, -vSI_past2, -vSI_past3, -vFC_past2, -vFC_past3)

predict_2022 <- data_22 %>%
  rename_with(~paste0(., "_future"), -(1:2)) %>% select(-contains("_future"), Season_future, Start_IP_future, ERA_Minus_future, Age_future) %>%
  left_join(data_21 %>% rename_with(~paste0(., "_past1"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past1, -Age_past1) %>%
  left_join(data_20 %>% rename_with(~paste0(., "_past2"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past2, -Age_past2) %>%
  left_join(data_19 %>% rename_with(~paste0(., "_past3"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past3, -Age_past3) %>%
  select(-Stuff_Plus_past2, -Stuff_Plus_past3, -Location_Plus_past2, -Location_Plus_past3, -Pitching_Plus_past2, -Pitching_Plus_past3,
         -botStf_past2, -botStf_past3, -botCmd_past2, -botCmd_past3, -botOvr_past2, -botOvr_past3,
         -vFA_past2, -vFA_past3, -vSI_past2, -vSI_past3, -vFC_past2, -vFC_past3)

predict_2021 <- data_21 %>%
  rename_with(~paste0(., "_future"), -(1:2)) %>% select(-contains("_future"), Season_future, Start_IP_future, ERA_Minus_future, Age_future) %>%
  left_join(data_20 %>% rename_with(~paste0(., "_past1"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past1, -Age_past1) %>%
  left_join(data_19 %>% rename_with(~paste0(., "_past2"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past2, -Age_past2) %>%
  left_join(data_18 %>% rename_with(~paste0(., "_past3"), -(1:2)), by = c("PlayerID", "Name")) %>% select(-Season_past3, -Age_past3) %>%
  select(-Stuff_Plus_past2, -Stuff_Plus_past3, -Location_Plus_past2, -Location_Plus_past3, -Pitching_Plus_past2, -Pitching_Plus_past3,
         -botStf_past2, -botStf_past3, -botCmd_past2, -botCmd_past3, -botOvr_past2, -botOvr_past3,
         -vFA_past2, -vFA_past3, -vSI_past2, -vSI_past3, -vFC_past2, -vFC_past3)

predict_future <- rbind(predict_2023, predict_2022, predict_2021)
predict_future <- predict_future %>% rename(Age = Age_future)



# Feature Engineering ---------------------------------------------------------------------
# Determine average velocity of fastest pitch
predict_future_final <- predict_future %>%
  mutate(vFA_past1 = ifelse(is.na(vFA_past1), 0, vFA_past1),
         vSI_past1 = ifelse(is.na(vSI_past1), 0, vSI_past1),
         vFC_past1 = ifelse(is.na(vFC_past1), 0, vFC_past1),
         vFB_past1 = pmax(vFA_past1, vSI_past1, vFC_past1, na.rm = TRUE)) %>%
  select(-vFA_past1, -vSI_past1, -vFC_past1)

# Calculate total past starting IP
predict_future_final <- predict_future_final %>%
  mutate(Start_IP_future = floor(Start_IP_future) + (Start_IP_future %% 1) / 10 * 3,
         Start_IP_past1 = ifelse(is.na(Start_IP_past1), 0, Start_IP_past1),
         Start_IP_past1 = floor(Start_IP_past1) + (Start_IP_past1 %% 1) / 10 * 3,
         Start_IP_past2 = ifelse(is.na(Start_IP_past2), 0, Start_IP_past1),
         Start_IP_past2 = floor(Start_IP_past2) + (Start_IP_past2 %% 1) / 10 * 3,
         Start_IP_past3 = ifelse(is.na(Start_IP_past3), 0, Start_IP_past3),
         Start_IP_past3 = floor(Start_IP_past3) + (Start_IP_past3 %% 1) / 10 * 3,
         Start_IP_past = Start_IP_past1 + Start_IP_past2 + Start_IP_past3)

# Convert SIERA and xERA to Minus scale


# Get average of all past metrics
calculate_weighted_average <- function(metric1, metric2, metric3, ip1, ip2, ip3) {
  # Replace NA in metrics with 0 when corresponding IP is 0
  metric1[is.na(metric1) & ip1 == 0] <- 0
  metric2[is.na(metric2) & ip2 == 0] <- 0
  metric3[is.na(metric3) & ip3 == 0] <- 0
  
  # Calculate weighted sum of metrics
  weighted_sum <- (metric1 * ip1 + metric2 * ip2 + metric3 * ip3)
  
  # Calculate total innings pitched
  total_ip <- ip1 + ip2 + ip3
  
  # Calculate weighted average, handling division by zero
  weighted_average <- ifelse(total_ip > 0, weighted_sum / total_ip, NA)
  
  return(weighted_average)
}

predict_future_final <- predict_future_final %>%
  mutate(ERA_past = calculate_weighted_average(ERA_past1, ERA_past2, ERA_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         ERA_Minus_past = calculate_weighted_average(ERA_Minus_past1, ERA_Minus_past2, ERA_Minus_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         FIP_past = calculate_weighted_average(FIP_past1, FIP_past2, FIP_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         FIP_Minus_past = calculate_weighted_average(FIP_Minus_past1, FIP_Minus_past2, FIP_Minus_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         xFIP_past = calculate_weighted_average(xFIP_past1, xFIP_past2, xFIP_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         xFIP_Minus_past = calculate_weighted_average(xFIP_Minus_past1, xFIP_Minus_past2, xFIP_Minus_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         SIERA_past = calculate_weighted_average(SIERA_past1, SIERA_past2, SIERA_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         K_Rate_past = calculate_weighted_average(K_Rate_past1, K_Rate_past2, K_Rate_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         K_Rate_Plus_past = calculate_weighted_average(K_Rate_Plus_past1, K_Rate_Plus_past2, K_Rate_Plus_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         BB_Rate_past = calculate_weighted_average(BB_Rate_past1, BB_Rate_past2, BB_Rate_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         BB_Rate_Plus_past = calculate_weighted_average(BB_Rate_Plus_past1, BB_Rate_Plus_past2, BB_Rate_Plus_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         K_BB_Rate_past = calculate_weighted_average(K_BB_Rate_past1, K_BB_Rate_past2, K_BB_Rate_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         WHIP_past = calculate_weighted_average(WHIP_past1, WHIP_past2, WHIP_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         WHIP_Plus_past = calculate_weighted_average(WHIP_Plus_past1, WHIP_Plus_past2, WHIP_Plus_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         GB_Rate_past = calculate_weighted_average(GB_Rate_past1, GB_Rate_past2, GB_Rate_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         GB_Rate_Plus_past = calculate_weighted_average(GB_Rate_Plus_past1, GB_Rate_Plus_past2, GB_Rate_Plus_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         Zone_Rate_past = calculate_weighted_average(Zone_Rate_past1, Zone_Rate_past2, Zone_Rate_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         F_Strike_Rate_past = calculate_weighted_average(F_Strike_Rate_past1, F_Strike_Rate_past2, F_Strike_Rate_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3),
         CSW_Rate_past = calculate_weighted_average(CSW_Rate_past1, CSW_Rate_past2, CSW_Rate_past3, Start_IP_past1, Start_IP_past2, Start_IP_past3))

# PCA for all past features


# Save data ---------------------------------------------------------------------

# Write data for EDA and modeling
write.csv(predict_future_final, "project/volume/data/processed/processed.csv", row.names = FALSE)
