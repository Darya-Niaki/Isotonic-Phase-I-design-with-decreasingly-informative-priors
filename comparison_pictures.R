# Target DLT rate of 0.3

# Load necessary library
library(ggplot2)
library(reshape2)
library(patchwork)

# Data for target DLT  = 0.3
scenarios <- 1:10
DIP_mean <- c(58.23, 65.82, 61.61, 60.78, 58, 60.16, 56.4, 48.77, 76.47, 60.39)
CDP <- c(48.76, 60.87, 53.81, 57.56, 51.38, 58.24, 55.69, 45.4, 84.19, 71.46)
BOIN <- c(40.1, 52, 46.7, 52.3, 43.2, 52.3, 53.6, 39.1, 81.3, 71.3)
DIP_mode <- c(43.83, 57.38, 48.3, 54.82, 48.8, 56.19, 56.86, 48.63, 83.19, 74.63)


DIP_mean_aboveMTD <- c(8.8384, 7.4048, 6.1879, 4.9245, 5.5634, 4.1566, 2.954, 3.274, 0, 0)
CDP_aboveMTD <- c(14.2285, 11.651, 11.404, 9.2549, 10.8574, 8.0642, 6.4672, 7.6648, 0, 0)
BOIN_aboveMTD <- c(14.0715, 11.5018, 12.15, 9.95, 11.97, 9.45, 7.95, 5.76, 0, 0)
DIP_mode_aboveMTD <- c(15.4378, 12.6066, 12.7166, 10.1569, 12.0044, 9.0744, 7.3722, 8.262, 0, 0)


# Create dataframe
df <- data.frame(
  Scenario = scenarios,
  DIP_mean = DIP_mean,
  CDP = CDP,
  BOIN = BOIN
)

# Convert to long format for ggplot
df_long <- melt(df, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")

# Plot for line trend
p1 <- ggplot(df_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Correct Selection (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(16, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 100))  # Force y-axis to start at 0



df2 <- data.frame(
  Scenario = scenarios,
  DIP_mode = DIP_mode,
  CDP = CDP,
  BOIN = BOIN
)


df2_long <- melt(df2, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")

p2<- ggplot(df2_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Correct Selection (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(17, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 100))  # Force y-axis to start at 0


############patient above MTD
#############################

df3 <- data.frame(
  Scenario = scenarios,
  DIP_mean = DIP_mean_aboveMTD,
  CDP = CDP_aboveMTD,
  BOIN = BOIN_aboveMTD
)


df3_long <- melt(df3, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")



p3<- ggplot(df3_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Patients treated above MTD (#)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(16, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 16))  # Force y-axis to start at 0



df4 <- data.frame(
  Scenario = scenarios,
  DIP_mode = DIP_mode_aboveMTD,
  CDP = CDP_aboveMTD,
  BOIN = BOIN_aboveMTD
)


df4_long <- melt(df4, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")



p4<- ggplot(df4_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Patients treated above MTD (#)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(17, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 16))  # Force y-axis to start at 0




plot_0.3 = (p1 | p2) / (p3 | p4) + 
  plot_annotation(title = "Target DLT rate of 0.3") & 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



#save the image
setwd("C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code/final_results_01232025/results_pictures")
ggsave("0.3plots08132025.png",plot = plot_0.3, width = 8,height = 8,dpi = 600)



###################################################################################
###################################################################################
##################Target DLT rate = 0.2############################################
rm(list = ls())

#N = 20 in these calculations
scenarios = seq(1:10)

DIP_mean <- c(66.21, 68.75, 47.74, 53.52, 53.4, 43.74, 28.62, 25.98, 23.6, 33.46)
CDP <- c(56.66, 62.35, 40.72, 54.67, 59.81, 43.08, 55.57, 43.73, 63.72, 74.72)
BOIN <- c(41.5, 46.4, 34, 47.8, 52.7, 36.2, 52, 37.5, 67.7, 75.2)
DIP_mode <- c(49.97, 54.86, 38.12, 51.98, 55.49, 41.37, 56.43, 45.46, 68.05, 74.83)



DIP_mean_aboveMTD <- c(5.0924, 4.6607, 2.4241, 2.189, 1.2527, 1.2398, 0.4245, 0.4979, 0, 0)
CDP_aboveMTD <- c(14.4014, 13.4731, 12.8679, 9.8399, 8.0605, 10.6873, 6.2017, 8.394, 0, 0)
BOIN_aboveMTD <- c(14.43, 13.42, 13.378, 10.952, 9.32, 11.76, 7.7, 9.5, 0, 0)
DIP_mode_aboveMTD <- c(14.8984, 13.9607, 13.5325, 10.4915, 8.7286, 11.2089, 6.7508, 8.7869, 0, 0)



# Create dataframe
df <- data.frame(
  Scenario = scenarios,
  DIP_mean = DIP_mean,
  CDP = CDP,
  BOIN = BOIN
)

# Convert to long format for ggplot
df_long <- melt(df, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")

# Plot for line trend
p1 <- ggplot(df_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Correct Selection (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(16, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 100))  # Force y-axis to start at 0



df2 <- data.frame(
  Scenario = scenarios,
  DIP_mode = DIP_mode,
  CDP = CDP,
  BOIN = BOIN
)


df2_long <- melt(df2, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")

p2<- ggplot(df2_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Correct Selection (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(17, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 100))  # Force y-axis to start at 0


############patient above MTD
#############################

df3 <- data.frame(
  Scenario = scenarios,
  DIP_mean = DIP_mean_aboveMTD,
  CDP = CDP_aboveMTD,
  BOIN = BOIN_aboveMTD
)


df3_long <- melt(df3, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")



p3<- ggplot(df3_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Patients treated above MTD (#)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(16, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 16))  # Force y-axis to start at 0



df4 <- data.frame(
  Scenario = scenarios,
  DIP_mode = DIP_mode_aboveMTD,
  CDP = CDP_aboveMTD,
  BOIN = BOIN_aboveMTD
)


df4_long <- melt(df4, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")



p4<- ggplot(df4_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Patients treated above MTD (#)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(17, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 16))  # Force y-axis to start at 0




plot_0.2 = (p1 | p2) / (p3 | p4) + 
  plot_annotation(title = "Target DLT rate of 0.2") & 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



#save the image
setwd("C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code/final_results_01232025/results_pictures")
ggsave("0.2plots08132025.png",plot = plot_0.2, width = 8,height = 8,dpi = 600)







###################################################################################
###################################################################################
##################Target DLT rate = 0.25############################################
rm(list = ls())

#N = 30 in these calculations
scenarios = seq(1:10)

DIP_mean <- c(63.13, 70.73, 37.6, 54.02, 24.89, 43.95, 18.27, 38.2, 15.41, 37.62)

CDP <- c(51.09, 68.09, 31.45, 50.35, 29.64, 48.22, 28.8, 49.03, 40.69, 59.44)

BOIN <- c(41.2, 56.7, 27.6, 46.5, 26, 45.4, 28.2, 47.9, 47.2, 63.6)

DIP_mode <- c(46.15, 61.16, 32.92, 50.87, 30.79, 49.47, 31.7, 48.49, 47.73, 65.5)
#verified up to here



DIP_mean_aboveMTD <- c(7.0939, 5.6138, 3.4591, 3.155, 1.8157, 2.0685, 0.8683, 1.3816, 0, 0)
CDP_aboveMTD <-  c(14.8163, 11.2108, 11.6902, 8.9725, 9.0386, 7.5779, 6.2723, 5.6778, 0, 0)
BOIN_aboveMTD <- c(14.418,11.0088,11.9564,9.5776,9.6652, 8.5813, 7.3062,6.93,0,0)

DIP_mode_aboveMTD <- c(15.2719, 11.9889, 12.6239, 9.7318, 9.9568, 8.4153, 6.8305, 6.4958, 0, 0)



# Create dataframe
df <- data.frame(
  Scenario = scenarios,
  DIP_mean = DIP_mean,
  CDP = CDP,
  BOIN = BOIN
)

# Convert to long format for ggplot
df_long <- melt(df, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")

# Plot for line trend
p1 <- ggplot(df_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Correct Selection (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(16, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 100))  # Force y-axis to start at 0



df2 <- data.frame(
  Scenario = scenarios,
  DIP_mode = DIP_mode,
  CDP = CDP,
  BOIN = BOIN
)


df2_long <- melt(df2, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")

p2<- ggplot(df2_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Correct Selection (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(17, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 100))  # Force y-axis to start at 0


############patient above MTD
#############################

df3 <- data.frame(
  Scenario = scenarios,
  DIP_mean = DIP_mean_aboveMTD,
  CDP = CDP_aboveMTD,
  BOIN = BOIN_aboveMTD
)


df3_long <- melt(df3, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")



p3<- ggplot(df3_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Patients treated above MTD (#)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(16, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 16))  # Force y-axis to start at 0



df4 <- data.frame(
  Scenario = scenarios,
  DIP_mode = DIP_mode_aboveMTD,
  CDP = CDP_aboveMTD,
  BOIN = BOIN_aboveMTD
)


df4_long <- melt(df4, id.vars = "Scenario", variable.name = "Method", value.name = "PCS")



p4<- ggplot(df4_long, aes(x = as.factor(Scenario), y = PCS, linetype = Method, shape = Method)) +
  geom_line(size = 0.3, aes(group = Method)) +  # Ensure each method has its own line
  geom_point(size = 2) +  # Add distinct markers
  labs(
    x = "Scenario",
    y = "Patients treated above MTD (#)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_shape_manual(values = c(17, 18, 1)) +  # Different point shapes
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +  # Different line types
  scale_x_discrete(breaks = 1:10) +  # Ensure x-axis has values from 1 to 10
  scale_y_continuous(limits = c(0, 16))  # Force y-axis to start at 0



library(bayesrules)
library(tidyverse)
library(patchwork)
n6 = plot_beta_binomial(alpha = 1.8, beta = 4.2, y = 2, n = 6)+ggtitle("N6")
n6 <- n6 + ggtitle("Assumed sample size = 6") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
n6

n30 = plot_beta_binomial(alpha = 9, beta = 21, y = 2, n = 6)
n30 <- n30 + ggtitle("Assumed sample size = 30") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


p <- (n6 | n30) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
p
ggsave("n630_prior_posterior2.png", plot =p, width = 8, height = 6, dpi = 600)



plot_0.25 = (p1 | p2) / (p3 | p4) + 
  plot_annotation(title = "Target DLT rate of 0.25") & 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



#save the image
setwd("C:/Users/shokouhinid/sstp_2023_Wages/Dr. wages_code/final_results_01232025/results_pictures")
ggsave("0.25plots08132025.png",plot = plot_0.25, width = 8,height = 8,dpi = 600)




#################################################
####DIP Methods##################################
#we will use DIP with target DLT rate of 0.3 

N30 <- c(57.61, 65.91, 61.43, 62.44, 60.01, 61.41, 56.48, 48.93, 75.59, 59.84)
N20 <- c(58.23, 65.82, 61.61, 60.78, 58.00, 60.16, 56.40, 48.77, 76.47, 60.39)
N15 <- c(57.82, 64.51, 61.74, 59.34, 56.43, 57.75, 54.14, 46.52, 76.38, 59.47)
N10 <- c(57.79, 65.12, 61.36, 57.99, 56.13, 55.11, 48.32, 40.73, 69.38, 53.64)
N6  <- c(58.83, 65.36, 58.54, 54.01, 51.32, 49.39, 41.82, 35.62, 57.00, 43.24)

# Create a data frame in wide format
df_wide <- data.frame(
  Index = 1:10,  # Assuming these are ordered measurements
  N30 = N30,
  N20 = N20,
  N15 = N15,
  N10 = N10,
  N6 = N6
)

# Convert to long format
df_long_dip <- melt(df_wide, id.vars = "Index", variable.name = "Group", value.name = "Value")

# Plot using ggplot with different line types, point shapes, and dashes
# Convert to long format
df_long <- melt(df_wide, id.vars = "Index", variable.name = "Method", value.name = "Value")

# Plot using ggplot, mimicking your preferred format
dip1 = ggplot(df_long, aes(x = as.factor(Index), y = Value, linetype = Method, shape = Method,color = Method)) +
  geom_line(linewidth = 0.3, aes(group = Method)) +  
  geom_point(size = 1.5) +  
  labs(
    x = "Scenario",
    y = "Correct MTD selection (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),  
    panel.background = element_rect(fill = "white", color = NA)  
  ) +
  scale_shape_manual(
    name = "Maximum effective sample size",  # Change legend title
    values = c(16, 2, 4, 1, 8)
  ) +
  scale_linetype_manual(
    name = "Maximum effective sample size",  # Change legend title
    values = c("solid", "dashed", "dotted", "dotdash", "twodash")
  ) +
  scale_color_manual(
    values = rep("black", 5),
    name = "Maximum effective sample size"
  ) +
  scale_x_discrete(breaks = 1:10) +  
  scale_y_continuous(limits = c(0, 100))


#########for patients above the MTD##################
DIP30 <- c(8.9632, 7.424, 6.3385, 5.002, 5.5651, 4.1855, 2.9797, 3.2709, 0, 0)  
DIP20 <- c(8.8384, 7.4048, 6.1879, 4.9245, 5.5634, 4.1566, 2.954, 3.274, 0, 0)  
DIP15 <- c(8.6253, 7.209, 6.1403, 4.7578, 5.4385, 4.1007, 2.9118, 3.3032, 0, 0)  
DIP10 <- c(8.1456, 6.6623, 5.8244, 4.4732, 5.2256, 3.8056, 2.7386, 3.1076, 0, 0)  
DIP6  <- c(7.2918, 5.9382, 5.0626, 3.8649, 4.6297, 3.3208, 2.3251, 2.6415, 0, 0)
df_wide_mtd <- data.frame(
  Index = 1:10,  # Assuming these are ordered measurements
  DIP30 = DIP30,
  DIP20 = DIP20,
  DIP15 = DIP15,
  DIP10 = DIP10,
  DIP6 = DIP6
)

# Plot using ggplot with different line types, point shapes, and dashes
# Convert to long format
df_long_MTD <- melt(df_wide_mtd, id.vars = "Index", variable.name = "Method", value.name = "Value")

# Plot using ggplot
MTD = ggplot(df_long_MTD, aes(x = as.factor(Index), y = Value, linetype = Method, shape = Method)) +
  geom_line(linewidth = 0.3, aes(group = Method)) +  
  geom_point(size = 1.5) +  
  labs(
    x = "Scenario",
    y = "Patients treated above MTD (#)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),  
    panel.background = element_rect(fill = "white", color = NA)  
  ) +
  scale_shape_manual(
    name = "Maximum effective sample size",  # Change legend title
    values = c(16, 2, 4, 1, 8)
  ) +  
  scale_linetype_manual(
    name = "Maximum effective sample size",  # Change legend title
    values = c("solid", "dashed", "dotted", "dotdash", "twodash")
  ) +
  scale_color_manual(
    values = rep("black", 5),
    name = "Maximum effective sample size"
  ) +
  scale_x_discrete(breaks = 1:10) +  
  scale_y_continuous(limits = c(0, 15))

# We need to have only one legend for both figures!!

# Helper: extract legend from a ggplot
get_legend <- function(p) {
  g <- ggplotGrob(p)
  g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
}

# 1) Plots without legends
dip1 <- dip1 + theme(legend.position = "none")
MTD  <- MTD + theme(legend.position = "none")

# 2) Legend-only plot (with minimal margins)
legend_plot <- ggplot(df_long, aes(
  x = as.factor(Index), y = Value,
  linetype = Method, shape = Method
)) +
  geom_line(aes(group = Method), color = "black") +
  geom_point(color = "black") +
  scale_shape_manual(name = "Maximum effective sample size",
                     values = c(16, 2, 4, 1, 8)) +
  scale_linetype_manual(name = "Maximum effective sample size",
                        values = c("solid", "dashed", "dotted", "dotdash", "twodash")) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),  # Remove extra space
    legend.box.margin = margin(0, 0, 0, 0)
  )

legend <- get_legend(legend_plot)

# 3) Combine plots, compress legend row
final_plot <- (dip1 | MTD) /
  wrap_elements(legend) +
  plot_layout(heights = c(10, 1)) +  # reduce legend height
  plot_annotation(title = "Comparing DIP_mean Under Different Maximum Effective Sample Sizes") &
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

final_plot
ggsave("dipplot08132025.png", plot = final_plot, width = 12, height = 8, dpi = 600)





library(bayesrules)
library(ggplot2)
library(patchwork)

n6 <- plot_beta_binomial(alpha = 1.8, beta = 4.2, y = 2, n = 6) +
  ggtitle("Assumed sample size = 6") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

n30 <- plot_beta_binomial(alpha = 9, beta = 21, y = 2, n = 30) +
  ggtitle("Assumed sample size = 30") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Combine plots and collect legends
p <- (n6 | n30) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

p

n6 <- plot_beta_binomial(alpha = 1.8, beta = 4.2, y = 2, n = 6) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +  # Different line types
  scale_shape_manual(values = c(16, 17, 15)) +  # Different point shapes
  scale_color_manual(values = c("black", "gray30", "gray60")) +  # Different shades of gray
  ggtitle("Assumed sample size = 6") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


######################
#change the plot from colors to no colors
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)

# Function to plot prior, likelihood, and posterior distributions
plot_beta_binomial_bw <- function(p0,y, n,N,title) {
  
  # Define the range of π values
  pi_vals <- seq(0, 1, length.out = 1000)
  alpha = 1 + p0*(N)
  beta = 1 + (1-p0)*N
  
  # Compute prior density
  prior_density <- dbeta(pi_vals, alpha, beta)
  
  # Compute likelihood (scaled for visualization)
  likelihood_density <- dbinom(y, n, pi_vals) / max(dbinom(y, n, pi_vals))
  
  # Compute posterior density
  alpha_post <- 1 + p0*(N -n) + y
  beta_post <- 1 + (1 - p0)*(N -n) +n- y
  posterior_density <- dbeta(pi_vals, alpha_post, beta_post)
  
  # Create data frame for plotting
  data <- data.frame(
    pi = rep(pi_vals, 3),
    density = c(prior_density, likelihood_density, posterior_density),
    distribution = rep(c("Prior", "Likelihood (scaled)", "Posterior"), each = length(pi_vals))
  )
  # Means of prior and posterior
  prior_mean <- alpha / (alpha + beta)
  posterior_mean <- alpha_post / (alpha_post + beta_post)
  
  # Approximate y-values (densities) at mean locations
  prior_peak <- dbeta(prior_mean, alpha, beta)
  posterior_peak <- dbeta(posterior_mean, alpha_post, beta_post)
  
  # Generate the plot
  ggplot(data, aes(x = pi, y = density, color = distribution, linetype = distribution)) +
    geom_line(size = 1) +
    geom_segment(aes(x = prior_mean, xend = prior_mean, y = 0, yend = prior_peak),
                 color = "blue", linetype = "dotted") +
    geom_segment(aes(x = posterior_mean, xend = posterior_mean, y = 0, yend = posterior_peak),
                 color = "black", linetype = "solid") +
    scale_linetype_manual(values = c("Prior" = "dotted", "Likelihood (scaled)" = "dotdash", "Posterior" = "solid")) +
    scale_color_manual(values = c("Prior" = "blue", "Likelihood (scaled)" = "darkgray", "Posterior" = "black")) +
    scale_x_continuous(breaks = c(0, 0.3, 0.35,0.4, 0.45, 0.5, 0.75, 1))+
    labs(title = title, x = expression(pi), y = "Density") +
    theme_minimal() +
    theme(legend.title = element_blank(), text = element_text(size = 14), legend.position = "bottom")
}


# Generate plots for two different assumed sample sizes
plot1 <- plot_beta_binomial_bw(p0 = 0.3, y = 2, n = 5,N = 6, title = "Maximum effective sample size = 6")
plot2 <- plot_beta_binomial_bw(p0 = 0.3, y = 2, n = 5,N = 30, title = "Maximum effective sample size = 30")

# Arrange plots side by side
combined_plot <- plot1 + plot2 + plot_layout(ncol = 1,guides = "collect")&
  theme(legend.position = "bottom")


ggsave("explanation plot_08132025.png", plot = combined_plot, width = 12, height = 12, dpi = 600)




