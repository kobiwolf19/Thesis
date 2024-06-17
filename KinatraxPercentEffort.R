install.packages("lsmeans")
install.packages("emmeans")
library(emmeans)
library(lsmeans)
library(lme4)
library(Matrix)
library(dplyr)
library(ggplot2)

stats <- read.csv("/Users/kobi/Desktop/My Thesis/KinatraxPercentEffortAvg.csv")
unique_names <- unique(stats$Subject)
unique_names

stats$Percent.Effort <- factor(stats$Percent.Effort)

# shoulderRotation_model <- aov(Shoulder.Rotation.at.MER ~ Percent.Effort + Error(Subject/Percent.Effort), data = stats)
# elbowFlexion_model <- aov(Elbow.Flexion.at.MER ~ Percent.Effort + Error(Subject/Percent.Effort), data = stats)
# shoulderForce_model <- aov(Max.Resultant.Shoulder.Force ~ Percent.Effort + Error(Subject/Percent.Effort), data = stats)
# elbowForce_model <- aov(Max.Elbow.Varus.Torque ~ Percent.Effort + Error(Subject/Percent.Effort), data = stats)


shoulderRotation_model <- aov(Shoulder.Rotation.at.MER ~ Percent.Effort, data = stats)
elbowFlexion_model <- aov(Elbow.Flexion.at.MER ~ Percent.Effort, data = stats)
shoulderForce_model <- aov(Max.Resultant.Shoulder.Force ~ Percent.Effort, data = stats)
varusTorque_model <- aov(Max.Elbow.Varus.Torque ~ Percent.Effort, data = stats)

pelvisRotation_model <- aov(Max.Pelvis.Rotation.Velo ~ Percent.Effort, data = stats)
trunkRotation_model <- aov(Max.Trunk.Rotation.Velo ~ Percent.Effort, data = stats)
IRshoulderVelo_model <- aov(Max.IR.Shoulder.Velo ~ Percent.Effort, data = stats)
elbowExtensionVelo_model <- aov(Max.Elbow.Extension.Velo ~ Percent.Effort, data = stats)
shoulderABatMER_model <- aov(Shoulder.Abduction.at.MER ~ Percent.Effort, data = stats)
shoulderHorizontalAB_model <- aov(Max.Shoulder.Horizontal.Abduction ~ Percent.Effort, data = stats)

handVelo_model <- aov(Max.Hand.Velo ~ Percent.Effort, data = stats)


summary(shoulderRotation_model)
summary(elbowFlexion_model)
summary(shoulderForce_model)
summary(varusTorque_model)

summary(pelvisRotation_model)
summary(trunkRotation_model)
summary(IRshoulderVelo_model)
summary(elbowExtensionVelo_model)
summary(shoulderABatMER_model)
summary(shoulderHorizontalAB_model)

summary(handVelo_model)
tukey_handVelo <- TukeyHSD(handVelo_model)
print(tukey_handVelo)


tukey_varusTorque <- TukeyHSD(varusTorque_model)
conf_intervals <- tukey_varusTorque$`Percent.Effort`[,2:3]
print(conf_intervals)
print(tukey_varusTorque)

tukey_shoulderIR <- TukeyHSD(IRshoulderVelo_model)
print(tukey_shoulderIR)

tukey_elbowExtension <- TukeyHSD(elbowExtensionVelo_model)
print(tukey_elbowExtension)

tukey_trunkRotation <- TukeyHSD(trunkRotation_model)
print(tukey_trunkRotation)

tukey_shoudlerForce <- TukeyHSD(shoulderForce_model)
print(tukey_shoudlerForce)

# running tukey from mixed model
# mixed_shoulderRotation <- lmer(Shoulder.Rotation.at.MER ~ Percent.Effort + (1|Subject), data = stats)
# posthoc_shoulderRotation <- lsmeans(mixed_shoulderRotation, pairwise ~ Percent.Effort)
# print(posthoc_shoulderRotation)


bp_SR <- ggplot(stats, aes(y=Shoulder.Rotation.at.MER, x=Percent.Effort)) + 
  geom_boxplot(fill="gray") + 
  labs(title="Shoulder Rotation at Maximum External Rotation at Varied Effort Levels",x="Percent Effort", y = expression(paste("Shoulder Rotation at MER (",degree,")")))+
  theme_classic()

bp_SR

bp_EF <- ggplot(stats, aes(y=Elbow.Flexion.at.MER, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Elbow Flexion at Maximum External Rotation at Varied Effort Levels",x="Percent Effort", y = expression(paste("Elbow Flexion at MER (",degree,")")))+
  theme_classic()

bp_EF

bp_SF <- ggplot(stats, aes(y=Max.Resultant.Shoulder.Force, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Maximum Resultant Shoudler Force at Varied Effort Levels",x="Percent Effort", y = "Max Resultant Shoulder Force (N)" ) +
  theme_classic()

bp_SF

bp_EVT <- ggplot(stats, aes(y=Max.Elbow.Varus.Torque, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Maximum Elbow Varus Torque at Varied Effort Levels",x="Percent Effort", y = "Max Elbow Varus Torque (Nm)" ) +
  theme_classic()

bp_EVT

bp_PRV <- ggplot(stats, aes(y=Max.Pelvis.Rotation.Velo, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Maximum Pelvis Rotation Velocity at Varied Effort Levels",x="Percent Effort", y = expression(paste("Max Pelvis Rotation Velocity (",degree,"/s)")))+
  theme_classic()

bp_PRV

bp_TRV <- ggplot(stats, aes(y=Max.Trunk.Rotation.Velo, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Maximum Trunk Rotation Velocity at Varied Effort Levels",x="Percent Effort", y = expression(paste("Max Trunk Rotation Velocity (",degree,"/s)")))+
  theme_classic()

bp_TRV

bp_IRSV <- ggplot(stats, aes(y=Max.IR.Shoulder.Velo, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Maximum Shoulder Internal Rotation Velocity at Varied Effort Levels",x="Percent Effort", y = expression(paste("Max Shoulder Internal Rotation Velocity (",degree,"/s)")))+
  theme_classic()

bp_IRSV

bp_MEEV <- ggplot(stats, aes(y=Max.Elbow.Extension.Velo, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Maximum Elbow Extension Velocity at Varied Effort Levels",x="Percent Effort", y = expression(paste("Max Elbow Extension Velocity (",degree,"/s)")))+
  theme_classic()

bp_MEEV

bp_SAMER <- ggplot(stats, aes(y=Shoulder.Abduction.at.MER, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Shoulder Abduction at Maximum External Rotation at Varied Effort Levels",x="Percent Effort", y = expression(paste("Shoulder Abduction at MER (",degree,")")))+
  theme_classic()

bp_SAMER

bp_MSHA <- ggplot(stats, aes(y=Max.Shoulder.Horizontal.Abduction, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Maximum Shoulder Horizontal Abduction at Varied Effort Levels",x="Percent Effort", y = expression(paste("Max Shoulder Horizontal Abduction (",degree,")")))+
  theme_classic()

bp_MSHA

bp_MHV <- ggplot(stats, aes(y=Max.Hand.Velo, x=Percent.Effort)) + 
  geom_boxplot(fill = "gray") + 
  labs(title="Maximum Hand Velocity at Varied Effort Levels",x="Percent Effort", y = expression(paste("Max Hand Velocity (m/s)")))+
  theme_classic()

bp_MHV

# descriptive statistics
descriptive_shoudlerRotation <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Shoulder.Rotation.at.MER),
    sd = sd(Shoulder.Rotation.at.MER),
    min = min(Shoulder.Rotation.at.MER),
    median = median(Shoulder.Rotation.at.MER),
    max = max(Shoulder.Rotation.at.MER)
  )

print(descriptive_shoudlerRotation)


descriptive_varusTorque <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Max.Elbow.Varus.Torque),
    sd = sd(Max.Elbow.Varus.Torque),
    min = min(Max.Elbow.Varus.Torque),
    median = median(Max.Elbow.Varus.Torque),
    max = max(Max.Elbow.Varus.Torque)
  )

print(descriptive_varusTorque)


descriptive_elbowFlexion <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Elbow.Flexion.at.MER),
    sd = sd(Elbow.Flexion.at.MER),
    min = min(Elbow.Flexion.at.MER),
    median = median(Elbow.Flexion.at.MER),
    max = max(Elbow.Flexion.at.MER)
  )

print(descriptive_elbowFlexion)


  descriptive_shoulderForce <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Max.Resultant.Shoulder.Force),
    sd = sd(Max.Resultant.Shoulder.Force),
    min = min(Max.Resultant.Shoulder.Force),
    median = median(Max.Resultant.Shoulder.Force),
    max = max(Max.Resultant.Shoulder.Force)
  )

print(descriptive_shoulderForce)

descriptive_MPRV <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Max.Pelvis.Rotation.Velo),
    sd = sd(Max.Pelvis.Rotation.Velo),
    min = min(Max.Pelvis.Rotation.Velo),
    median = median(Max.Pelvis.Rotation.Velo),
    max = max(Max.Pelvis.Rotation.Velo)
  )

print(descriptive_MPRV)

descriptive_MTRV <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Max.Trunk.Rotation.Velo),
    sd = sd(Max.Trunk.Rotation.Velo),
    min = min(Max.Trunk.Rotation.Velo),
    median = median(Max.Trunk.Rotation.Velo),
    max = max(Max.Trunk.Rotation.Velo)
  )

print(descriptive_MTRV)

descriptive_MIRSV <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Max.IR.Shoulder.Velo),
    sd = sd(Max.IR.Shoulder.Velo),
    min = min(Max.IR.Shoulder.Velo),
    median = median(Max.IR.Shoulder.Velo),
    max = max(Max.IR.Shoulder.Velo)
  )

print(descriptive_MIRSV)

descriptive_MEEV <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Max.Elbow.Extension.Velo),
    sd = sd(Max.Elbow.Extension.Velo),
    min = min(Max.Elbow.Extension.Velo),
    median = median(Max.Elbow.Extension.Velo),
    max = max(Max.Elbow.Extension.Velo)
  )

print(descriptive_MEEV)

descriptive_SAMER <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Shoulder.Abduction.at.MER),
    sd = sd(Shoulder.Abduction.at.MER),
    min = min(Shoulder.Abduction.at.MER),
    median = median(Shoulder.Abduction.at.MER),
    max = max(Shoulder.Abduction.at.MER)
  )

print(descriptive_SAMER)

descriptive_MSHA <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Max.Shoulder.Horizontal.Abduction),
    sd = sd(Max.Shoulder.Horizontal.Abduction),
    min = min(Max.Shoulder.Horizontal.Abduction),
    median = median(Max.Shoulder.Horizontal.Abduction),
    max = max(Max.Shoulder.Horizontal.Abduction)
  )

print(descriptive_MSHA)

descriptive_MHV <- stats %>%
  group_by(Percent.Effort) %>%
  summarize(
    mean = mean(Max.Hand.Velo),
    sd = sd(Max.Hand.Velo),
    min = min(Max.Hand.Velo),
    median = median(Max.Hand.Velo),
    max = max(Max.Hand.Velo)
  )

print(descriptive_MHV)

pitcher_demographics <- read.csv("/Users/kobi/Desktop/My Thesis/WakeDemographics.csv")

BMI_mean <- mean(pitcher_demographics$BMI)
BMI_sd <- sd(pitcher_demographics$BMI)

Age_mean <- mean(pitcher_demographics$Age)
Age_sd <- sd(pitcher_demographics$Age)
