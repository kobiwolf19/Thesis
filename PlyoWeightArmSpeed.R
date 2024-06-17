install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lmerTest")
install.packages("texreg")
install.packages("afex")
install.packages("plyr")
install.packages("robustlmm")
install.packages("Matrix")
install.packages("mgcv")
install.packages("splines")
install.packages("gamlss")
install.packages("ggeffects")


library (lmerTest) # Mixed model package by Douglas Bates, comes w/ pvalues! 
library (texreg) #Helps us make tables of the mixed models
library (afex) # Easy ANOVA package to compare model fits
library (plyr) # Data manipulator package
library (ggplot2) # GGplot package for visualizing data
library(Matrix)
library(robustlmm)
library(dplyr)
library(mgcv)
library(splines)
library(gamlss)
library(ggeffects)
library(datawizard)

stats <- read.csv("/Users/kobi/Desktop/My Thesis/AllPlyoSessionCounters.csv")

stats_filtered <- subset(stats, stats[["peak_varus_torque"]] >= 10 & stats[["peak_varus_torque"]] <= 150)

plot(stats_filtered$ball_weight, stats_filtered$peak_varus_torque)
plot(stats_filtered$peak_arm_velo, stats_filtered$peak_varus_torque)
plot(stats_filtered$session_counter, stats_filtered$peak_varus_torque)

ggplot(data = stats_filtered,aes(x = session_counter,y = peak_arm_velo)) + geom_point() + facet_wrap(vars(name))

unique_names <- unique(stats$name)
unique_names

name_df <- stats_filtered[stats_filtered$name == "Zach Johnston", ]
max_value <- max(name_df$session_counter)




stats_filtered %>%
  filter(peak_varus_torque > 125) %>%
  select(name, peak_varus_torque, peak_arm_velo, session_counter, ball_weight) %>% View()

tolerance <- 1e-4
rows_to_remove <- which((stats_filtered$name == "Charlie Jones" & abs(stats_filtered$peak_varus_torque - 139.3866) < tolerance)
                        | (stats_filtered$name == "Joe Ariola" & abs(stats_filtered$peak_varus_torque - 126.7876) < tolerance)
                        | stats_filtered$name == "Joe Ariola" & abs(stats_filtered$peak_varus_torque - 130.8952) < tolerance)

stats_filtered[rows_to_remove,]

stats_filtered <- stats_filtered[-rows_to_remove, ]


specific_values <- c(4.4, 3.5, 18)  # Define the specific values to exclude
stats_without_smalldata <- stats_filtered[!(stats_filtered$ball_weight %in% specific_values), ]

  
# REML = TRUE
# linear model (use this one)
# remove 4.4, 3.5 and 18 for sensitivity
model <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + ball_weight + (1 | name), data = stats_filtered)
summary(model)
plot(model)
confint(model)



predicted_values_ballweight <- ggpredict(model, term = "ball_weight")
BW_plot <- plot(predicted_values_ballweight) +
  geom_line(color = "blue")

BW_plot + 
  labs(title = "Predicted Effect of Ball Weight on Peak Varus Torque", 
       x = "Ball Weight (oz)", 
       y = "Peak Varus Torque (Nm)")

my_vector <- seq(1, 46, by = 15)
my_vector

predicted_values_ballweight <- ggpredict(model, c("peak_arm_velo","session_counter [my_vector]"))
predicted_values_ballweight
interaction_plot <- plot(predicted_values_ballweight) 

interaction_plot + 
  labs(title = "Predicted Association of the Interaction Between Session and Arm Speed on Varus Torque", 
       x = "Arm Speed (mph)", 
       y = "Peak Varus Torque (Nm)",
       color = "Session Counter")


# stats_filtered$predicted_ballweight <- predicted_values_ballweight

# ggplot(data = stats_filtered,aes(x = ball_weight,y = peak_varus_torque)) + geom_point()

# predicted_values_armspeed <- ggpredict(model, term = "peak_arm_velo")
# plot(predicted_values_armspeed) +
  # geom_line(color = "red")


model_sensitive <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + ball_weight + (1 | name), data = stats_without_smalldata)
summary(model_sensitive)
plot(model_sensitive)


predicted_values_ballweight_sens <- ggpredict(model_sensitive, term = "ball_weight")
BW_plot_sens <- plot(predicted_values_ballweight_sens)

BW_plot_sens + 
  labs(title = "Sensitivity Analysis for Predicted Effect of Ball Weight on Peak Varus Torque", 
       x = "Ball Weight (oz)", 
       y = "Peak Varus Torque (Nm)")


predicted_values_ballweight_sens <- ggpredict(model_sensitive, c("peak_arm_velo","session_counter [my_vector]"))
interaction_plot_sens <- plot(predicted_values_ballweight_sens)

interaction_plot_sens + 
  labs(title = "Sensitivity Analysis for Predicted Association of Interaction Terms on Varus Torque", 
       x = "Arm Speed (mph)", 
       y = "Peak Varus Torque (Nm)",
       color = "Session Counter")


-0.228376+1.96*0.015695

1.229521-1.96*0.021020


# robust_model2 <- rlmer(peak_varus_torque ~ ns(peak_arm_velo, df = 3) + ball_weight + peak_arm_velo*session_counter + (1 | name), data = stats_filtered)
# summary(robust_model2)
# plot(robust_model2)

# robust_model <- rlmer(peak_varus_torque ~ ns(peak_arm_velo, df = 3) + ns(ball_weight, df = 3) + peak_arm_velo*session_counter + (1 | name), data = stats_filtered)
# summary(robust_model)
# plot(robust_model)
# summary(robust_model)$information_criterion

plyo_3.5 <- subset(stats_filtered, ball_weight == 3.5)
plyo_4.4 <- subset(stats_filtered, ball_weight == 4.4)
plyo_5.25 <- subset(stats_filtered, ball_weight == 5.25)
plyo_8 <- subset(stats_filtered, ball_weight == 8)
plyo_12 <- subset(stats_filtered, ball_weight == 12)
plyo_16 <- subset(stats_filtered, ball_weight == 16)
plyo_18 <- subset(stats_filtered, ball_weight == 18)
plyo_35 <- subset(stats_filtered, ball_weight == 35)
plyo_53 <- subset(stats_filtered, ball_weight == 53)
plyo_70.5 <- subset(stats_filtered, ball_weight == 70.5)

plot(plyo_53$peak_arm_velo, plyo_53$peak_varus_torque)
plot(plyo_35$peak_arm_velo, plyo_35$peak_varus_torque)


ggplot(plyo_53, aes(x = peak_arm_velo, y = peak_varus_torque, color = name)) +
  geom_point() +
  labs(title = "Peak arm speed by Peak varus torque by Name",
       x = "Arm Speed",
       y = "Varus Torque",
       color = "Pitcher")


unique_names <- unique(unlist(plyo_53$name))
unique_names
unique_names_70.5 <- unique(unlist(plyo_70.5$name))
unique_names_70.5

plyo3.5_armVeloMean <- mean(plyo_3.5$peak_arm_velo)
plyo3.5_armVeloSD <- sd(plyo_3.5$peak_arm_velo)
plyo3.5_armTorqueMean <- mean(plyo_3.5$peak_varus_torque)
plyo3.5_armTorqueoSD <- sd(plyo_3.5$peak_varus_torque)


plyo5.25_armVeloMean <- mean(plyo_5.25$peak_arm_velo)
plyo5.25_armVeloSD <- sd(plyo_5.25$peak_arm_velo)
plyo5.25_armTorqueMean <- mean(plyo_5.25$peak_varus_torque)
plyo5.25_armTorqueSD <- sd(plyo_5.25$peak_varus_torque)

plyo8_armVeloMean <- mean(plyo_8$peak_arm_velo)
plyo8_armVeloSD <- sd(plyo_8$peak_arm_velo)
plyo8_armTorqueMean <- mean(plyo_8$peak_varus_torque)
plyo8_armTorqueSD <- sd(plyo_8$peak_varus_torque)

plyo12_armVeloMean <- mean(plyo_12$peak_arm_velo)
plyo12_armVeloSD <- sd(plyo_12$peak_arm_velo)
plyo12_armTorqueMean <- mean(plyo_12$peak_varus_torque)
plyo12_armTorqueSD <- sd(plyo_12$peak_varus_torque)

plyo16_armVeloMean <- mean(plyo_16$peak_arm_velo)
plyo16_armVeloSD <- sd(plyo_16$peak_arm_velo)
plyo16_armTorqueMean <- mean(plyo_16$peak_varus_torque)
plyo16_armTorqueSD <- sd(plyo_16$peak_varus_torque)

plyo35_armVeloMean <- mean(plyo_35$peak_arm_velo)
plyo35_armVeloSD <- sd(plyo_35$peak_arm_velo)
plyo35_armTorqueMean <- mean(plyo_35$peak_varus_torque)
plyo35_armTorqueSD <- sd(plyo_35$peak_varus_torque)

plyo53_armVeloMean <- mean(plyo_53$peak_arm_velo)
plyo53_armVeloSD <- sd(plyo_53$peak_arm_velo)
plyo53_armTorqueMean <- mean(plyo_53$peak_varus_torque)
plyo53_armTorqueSD <- sd(plyo_53$peak_varus_torque)

plyo70.5_armVeloMean <- mean(plyo_70.5$peak_arm_velo)
plyo70.5_armVeloSD <- sd(plyo_70.5$peak_arm_velo)
plyo70.5_armTorqueMean <- mean(plyo_70.5$peak_varus_torque)
plyo70.5_armTorqueSD <- sd(plyo_70.5$peak_varus_torque)

hist(plyo_3.5$peak_varus_torque,breaks = 20)
hist(plyo_8$peak_varus_torque,breaks = 20)
hist(plyo_18$peak_varus_torque,breaks = 20)
hist(plyo_53$peak_varus_torque,breaks = 40)


# overlapping histograms

alldata<-list()

robust_model_3.5 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_3.5)
summary(robust_model_3.5)
plot(robust_model_3.5)
-0.01122+1.96*0.02779

robust_model_4.4 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_4.4)
summary(robust_model_4.4)
plot(robust_model_4.4)

robust_model_5.25 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_5.25)
summary(robust_model_5.25)
plot(robust_model_5.25)
0.024238+1.96*0.006926


robust_model_8 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_8)
summary(robust_model_8)
plot(robust_model_8)
hist(stats_filtered$peak_varus_torque, breaks = 50)
0.003665+1.96*0.001945

robust_model_12 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_12)
summary(robust_model_12)
plot(robust_model_12)
-0.02076+1.96*0.01645

robust_model_16 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_16)
summary(robust_model_16)
plot(robust_model_16)
0.017221+1.96*0.005982


robust_model_18 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_18)
summary(robust_model_18)
plot(robust_model_18)
-0.11299+1.96*0.05198

robust_model_35 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_35)
summary(robust_model_35)
plot(robust_model_35)
0.008161+1.96*0.004536

robust_model_53 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_53)
summary(robust_model_53)
plot(robust_model_53)
ggplot(plyo_53, aes(x = peak_arm_velo, y = peak_varus_torque, color = as.factor(session_counter))) +
  geom_point()

0.09109+1.96*0.01281

robust_model_70.5 <- rlmer(peak_varus_torque ~ peak_arm_velo*session_counter + (1 | name), data = plyo_70.5)
summary(robust_model_70.5)
plot(robust_model_70.5)
-0.01074+1.96*0.01019

# GAMs (is ball_weight/arm_velo linear/nonlinear)
# stats_filtered$name <- as.factor(stats_filtered$name)
# gamlss_model <- gamlss(peak_varus_torque ~ ns(peak_arm_velo, df = 3) + ball_weight + random(name), data = stats_filtered, family = "BCCG")

# plot(gamlss_model)


predicted_values_3.5 <- ggpredict(robust_model_3.5, term = "peak_arm_velo")
PV_3.5 <- plot(predicted_values_3.5) +
  geom_line(color = "blue")

predicted_values_5.25 <- ggpredict(robust_model_5.25, term = "peak_arm_velo")
PV_5.25 <- plot(predicted_values_5.25) +
  geom_line(aes(color = "5.25"), show.legend = TRUE)

predicted_values_8 <- ggpredict(robust_model_8, term = "peak_arm_velo")
PV_8 <- plot(predicted_values_8) +
  geom_line(aes(color = "8 oz"), show.legend = TRUE)

predicted_values_12 <- ggpredict(robust_model_12, term = "peak_arm_velo")
PV_12 <- plot(predicted_values_12) +
  geom_line(color = "orange")

predicted_values_16 <- ggpredict(robust_model_16, term = "peak_arm_velo")
PV_16 <- plot(predicted_values_16) +
  geom_line(color = "purple")

predicted_values_35 <- ggpredict(robust_model_35, term = "peak_arm_velo")
PV_35 <- plot(predicted_values_35) +
  geom_line(color = "brown")

predicted_values_53 <- ggpredict(robust_model_53, term = "peak_arm_velo")
PV_53 <- plot(predicted_values_53) +
  geom_line(color = "pink")

predicted_values_70.5 <- ggpredict(robust_model_70.5, term = "peak_arm_velo")
PV_70.5 <- plot(predicted_values_70.5) +
  geom_line(color = "grey")


combined_plot <- PV_8 + 
  geom_line(data = predicted_values_5.25, aes(x = x, y = predicted), color = "red") +
  geom_line(data = predicted_values_3.5, aes(x = x, y = predicted), color = "green") +
  geom_line(data = predicted_values_12, aes(x = x, y = predicted), color = "orange") +
  geom_line(data = predicted_values_16, aes(x = x, y = predicted), color = "purple") +
  geom_line(data = predicted_values_35, aes(x = x, y = predicted), color = "brown") +
  geom_line(data = predicted_values_53, aes(x = x, y = predicted), color = "pink") +
  geom_line(data = predicted_values_70.5, aes(x = x, y = predicted), color = "grey")
  
combined_plot  


ggplot(combined_plot, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  facet_wrap(~facet)




combined_plot_legend <- combined_plot +
  scale_color_manual(values = c("3.5" = "green", "5.25" = "red", "12" = "orange", "16" = "purple", "35" = "brown", "53" = "pink", "70.5" = "grey", "8" = "blue"),
                     labels = c("3.5" = "3.5", "5.25" = "5.25", "12" = "12", "16" = "16", "35" = "35", "53" = "53", "70.5" = "70.5", "8" = "8"))

print(combined_plot)

geom_line(data = predicted_values_5.25, aes(x = x, y = predicted), color = "red") +
  PV_3.5
PV_5.25
  
  #labs(title = "Predicted Effect of Ball Weight on Peak Varus Torque", 
   #    x = "Ball Weight (oz)", 
    #   y = "Peak Varus Torque (Nm)")





unique_values <- unique(unlist(stats_filtered$name))
unique_values


stats_filtered$unique_pair <- paste(stats_filtered$name, stats_filtered$session_counter, sep = "_")

# Find the unique combinations of subject names and counter values
unique_pairs <- unique(stats_filtered$unique_pair)

# Count the number of unique pairs
num_unique_pairs <- length(unique_pairs)


pitcher_demographics <- read.csv("/Users/kobi/Desktop/My Thesis/WakeDemographics_Plyos.csv")

BMI_mean <- mean(pitcher_demographics$BMI)
BMI_sd <- sd(pitcher_demographics$BMI)

Age_mean <- mean(pitcher_demographics$Age)
Age_sd <- sd(pitcher_demographics$Age)



stats_filtered$session_counter <- factor(stats_filtered$session_counter)

arm_velo <- aov(peak_arm_velo ~ session_counter, data = stats_filtered)
summary(arm_velo)
tukey_armspeed <- TukeyHSD(arm_velo)

p_values <- tukey_armspeed$`session_counter`[, 4]
significant_rows <- tukey_armspeed$`session_counter`[p_values < 0.05, ]
significant_rows


arm_torque <- aov(peak_varus_torque ~ session_counter, data = stats_filtered)
tukey_torque <- TukeyHSD(arm_torque)

p_values_torque <- tukey_torque$`session_counter`[, 4]
significant_rows_torque <- tukey_torque$`session_counter`[p_values_torque < 0.05, ]
significant_rows_torque


significant_results <- tukey_result[p_values < 0.05, ]

