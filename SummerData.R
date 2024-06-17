install.packages("tidyverse")
install.packages("ggplot2")

library(ggplot2)

# Method 1: Manually select data
stats <- read.csv(file.choose())
stats
summary(stats)

Player <- na.omit(stats[stats$Pitcher == "Simpson, Max" & (stats$TaggedPitchType == "Fastball" | stats$TaggedPitchType == "Sinker") & stats$Date == "2023-06-08", ])

Spin <- mean(Player$SpinRate)
Velo <- mean(Player$RelSpeed)

Jump <- stats$Jump
Velo <- stats$Velo
Spin <- stats$Spin

TorqueStats <- stats[stats$Torque !=0,]
Torque <- TorqueStats$Torque
TorqueSpin <- TorqueStats$Spin
TorqueVelo <- TorqueStats$Velo
TorqueJump <- TorqueStats$Jump


JumptoSpin <- lm(log(Jump) ~ log(Spin), stats = data)
JumptoVelo <- lm(log(Jump) ~ log(Velo), stats = data)
TorquetoSpin <- lm(Torque ~ TorqueSpin, stats = data)
TorquetoVelo <- lm(Torque ~ TorqueVelo, stats = data)
TorquetoJump <- lm(Torque ~ TorqueJump, stats = data)

# make scatter plots with best fit lines and equations
# in word doc for each regression, have beta coefficient, R squared, 95th CI and p values

# QQ Plot
plot(JumptoSpin)
plot(JumptoVelo)
plot(TorquetoSpin)
plot(TorquetoJump)
plot(TorquetoVelo)

plot(log(stats$Jump), log(stats$Velo), xlab = "Jump Height", ylab = "Velo")
abline(lm(log(stats$Velo) ~ log(stats$Jump)), col = "red")
summary(lm(log(stats$Velo) ~ log(stats$Jump)))
confint(JumptoVelo, level=0.90)

plot(log(stats$Jump), log(stats$Spin), xlab = "Jump Height", ylab = "Spin")
abline(lm(log(stats$Spin) ~ log(stats$Jump)), col = "red")
summary(lm(log(stats$Spin) ~ log(stats$Jump)))
confint(JumptoSpin, level=0.90)

plot(log(Torque), log(TorqueSpin), xlab = "Torque", ylab = "Spin")
abline(lm(log(TorqueSpin) ~ log(Torque)), col = "red")
summary(lm(log(TorqueSpin) ~ log(Torque)))
confint(TorquetoSpin, level=0.90)

plot(log(Torque), log(TorqueVelo), xlab = "Torque", ylab = "Velo")
abline(lm(log(TorqueVelo) ~ log(Torque)), col = "red")
summary(lm(log(TorqueVelo) ~ log(Torque)))
confint(TorquetoVelo, level=0.90)

plot(log(Torque), log(TorqueJump), xlab = "Torque", ylab = "Jump Height")
abline(lm(log(TorqueJump) ~ log(Torque)), col = "red")
summary(lm(log(TorqueJump) ~ log(Torque)))
confint(TorquetoJump, level=0.90)



plot(log(stats$Jump), log(stats$Spin))
plot(TorqueStats$Torque, TorqueStats$Spin)


# convert kg to Newtons

stats <- read.csv("/Users/kobi/Desktop/Nextiles/Appalachian League Data.csv")
stats <- stats[c(1:20),]
stats$BWHNm <- stats$BW.H*9.81


write.csv(stats, "/Users/kobi/Desktop/Nextiles/Appalachian League Data.csv", row.names=FALSE)

torqueData <- read.csv("/Users/kobi/Desktop/Nextiles/TorqueData.csv")

torqueData$NextileNorm <- torqueData$Nextile.Torque/torqueData$BWH.Nm
torqueData$NextileNorm <- torqueData$NextileNorm*100

torqueData$KinatraxNorm <- torqueData$Kinatrax.Torque/torqueData$BWH.Nm
torqueData$KinatraxNorm <- torqueData$KinatraxNorm*100


write.csv(torqueData, "/Users/kobi/Desktop/Nextiles/TorqueData.csv", row.names=FALSE)

