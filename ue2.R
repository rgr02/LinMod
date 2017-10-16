rm(list = ls())

mietpreise <-
  read.table(
    "http://www.uni-goettingen.de/de/document/download/ba9a7b9c1e3a4a7d50a370e3bce92303.raw/mietspiegel99.raw",
    header = T)

str(mietpreise)
mietpreise$bjahr <- as.integer(mietpreise$bjahr)

source("multiplot.R")

##################################################################################
# Aufgabe 1                                                                      #
##################################################################################

library(stargazer)
library(ggplot2)


mietpreise$flaeche.reziprok <- 1/mietpreise$flaeche

model.linear <- lm(mieteqm ~ flaeche, data = mietpreise)
model.reziprok <- lm(mieteqm ~ flaeche.reziprok, data = mietpreise)
stargazer(model.linear, model.reziprok, type = "text")



ggplot() +
  geom_point(aes(mietpreise$flaeche, mietpreise$mieteqm),
             alpha = 0.3) +
  geom_line(aes(mietpreise$flaeche, model.linear$fitted.values), colour = "blue",
            size = 1) +
  geom_line(aes(mietpreise$flaeche, model.reziprok$fitted.values), colour = "red",
            size = 1) +
  xlab("Flaeche") +
  ylab("Miete / m2") +
  theme_bw()

res.1 <- 
  ggplot() +
  geom_point(aes(mietpreise$flaeche, model.linear$residuals),
             alpha = 0.3) +
  geom_abline(intercept = 0, slope = 0, size = 1,
              colour = "blue") +
  xlab("Flaeche") +
  ylab("Residual") +
  ggtitle("Residual vs. Flaeche - Linear Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

res.2 <-
  ggplot() +
  geom_point(aes(model.linear$fitted.values, model.linear$residuals),
             alpha = 0.3) +
  geom_abline(intercept = 0, slope = 0, size = 1,
              colour = "blue") +
  xlab("Fitted Values") +
  ylab("Residual") +
  ggtitle("Residual vs. Fitted - Linear Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

res.3 <- 
  ggplot() +
  geom_point(aes(mietpreise$flaeche, model.reziprok$residuals),
             alpha = 0.3) +
  geom_abline(intercept = 0, slope = 0, size = 1,
              colour = "green") +
  xlab("Flaeche") +
  ylab("Residual") +
  ggtitle("Residual vs. Flaeche - Inverse Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

res.4 <-
  ggplot() +
  geom_point(aes(model.reziprok$fitted.values, model.reziprok$residuals),
             alpha = 0.3) +
  geom_abline(intercept = 0, slope = 0, size = 1,
              colour = "green") +
  xlab("Fitted Values") +
  ylab("Residual") +
  ggtitle("Residual vs. Fitted - Inverse Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(res.1, res.2, res.3, res.4, cols = 2)

qq.linear <- 
  ggplot() +
  stat_qq(aes(sample = model.linear$residuals)) +
  theme_bw() +
  ggtitle("QQ-Plot Residuals Linear") +
  theme(plot.title = element_text(hjust = 0.5))

qq.reziprok <-
  ggplot() +
  stat_qq(aes(sample = model.reziprok$residuals)) +
  theme_bw() +
  ggtitle("QQ-Plot Residuals Reziprok") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(qq.linear, qq.reziprok, cols = 2)

##################################################################################
# Aufgabe 2                                                                      #
##################################################################################
library(dplyr)
library(stargazer)

# Wohnungen ab 1966
modern.flats <- filter(mietpreise, bjahr >= 1966)

# Modelle
normale.lage <- lm(miete ~ flaeche, data = filter(modern.flats, lage == 1))
gute.lage <- lm(miete ~ flaeche, data = filter(modern.flats, lage == 2))
stargazer(normale.lage, gute.lage, type = "text")

# Plot
plot.A21 <- 
  ggplot() +
  geom_point(aes(modern.flats$flaeche, modern.flats$miete),
             alpha = 0.3) +
  geom_line(aes(normale.lage$model$flaeche, normale.lage$fitted.values),
            colour = "red", alpha = 0.7, size = 1) +
  geom_line(aes(gute.lage$model$flaeche, gute.lage$fitted.values),
            colour = "blue", alpha = 0.7, size = 1) +
  xlab("Flaeche") +
  ylab("Miete") +
  ggtitle("Mietpreise ohne Kontrolle für Lage") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Subsetting normale und gute Lage
normal.gut <- filter(modern.flats, lage == 1 || lage == 2) %>%
  mutate(lage = ifelse(lage == 2,1,0))

# Next Model
norm.gut.model <- lm(miete ~ flaeche + lage, data = normal.gut)

plot.A22 <- 
  ggplot() +
  geom_point(aes(norm.gut.model$model$flaeche, norm.gut.model$model$miete),
             alpha = 0.3) +
  geom_abline(intercept =
                norm.gut.model$coefficients[[1]], 
              slope = norm.gut.model$coefficients[[2]],
              size = 1,
              colour = "red") +
  geom_abline(intercept =
                norm.gut.model$coefficients[[1]] +
                norm.gut.model$coefficients[[3]],
              slope = norm.gut.model$coefficients[[2]],
              size = 1,
              colour = "blue") +
  xlab("Fläche") +
  ylab("Miete") + 
  ggtitle("Mietpreise in normaler und guter Lage") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Table 3 Models
stargazer(normale.lage, gute.lage, norm.gut.model, type = "text")

# Plot 3 Models
multiplot(plot.A21, plot.A22, cols = 2)

##################################################################################
# Aufgabe 3                                                                      #
##################################################################################

# Modelle
miete.quadratic <- lm(mieteqm ~ flaeche + I(flaeche^2), data = mietpreise)
miete.cubic <- lm(mieteqm ~ flaeche + I(flaeche^2) + I(flaeche^3), data = mietpreise)

# RESET Test
miete.linear <- lm(mieteqm ~ flaeche, data = mietpreise)
reset.test <- lm(mieteqm ~ flaeche +
                   I(miete.linear$fitted.values^2) +
                   I(miete.linear$fitted.values^3), data = mietpreise)
summary(reset.test)

ggplot() +
  geom_point(aes(mietpreise$flaeche, mietpreise$mieteqm),
             alpha = 0.3) +
  geom_line(aes(miete.quadratic$model$flaeche, miete.quadratic$fitted.values),
            size = 1, 
            colour = "red",
            alpha = 0.7) +
  geom_line(aes(miete.cubic$model$flaeche, miete.cubic$fitted.values),
            size = 1,
            colour = "blue",
            alpha = 0.7) +
  ylim(0,30) +
  xlab("Fläche") +
  ylab("Miete") +
  ggtitle("E[Miete|Fläche] Polynominal") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

stargazer(miete.quadratic, miete.cubic, type = "text")


# Residual Plot
res.1 <- 
  ggplot() +
  geom_point(aes(mietpreise$flaeche, miete.quadratic$residuals),
             alpha = 0.3) +
  geom_abline(intercept = 0, slope = 0, size = 1,
              colour = "blue") +
  xlab("Flaeche") +
  ylab("Residual") +
  ggtitle("Residual vs. Flaeche - Quadratic Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

res.2 <-
  ggplot() +
  geom_point(aes(miete.quadratic$fitted.values, miete.quadratic$residuals),
             alpha = 0.3) +
  geom_abline(intercept = 0, slope = 0, size = 1,
              colour = "blue") +
  xlab("Fitted Values") +
  ylab("Residual") +
  ggtitle("Residual vs. Fitted - Quadratic Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

res.3 <- 
  ggplot() +
  geom_point(aes(mietpreise$flaeche, miete.cubic$residuals),
             alpha = 0.3) +
  geom_abline(intercept = 0, slope = 0, size = 1,
              colour = "green") +
  xlab("Flaeche") +
  ylab("Residual") +
  ggtitle("Residual vs. Flaeche - Cubic Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

res.4 <-
  ggplot() +
  geom_point(aes(miete.cubic$fitted.values, miete.cubic$residuals),
             alpha = 0.3) +
  geom_abline(intercept = 0, slope = 0, size = 1,
              colour = "green") +
  xlab("Fitted Values") +
  ylab("Residual") +
  ggtitle("Residual vs. Fitted - Cubic Model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(res.1, res.2, res.3, res.4, cols = 2)

##################################################################################
# Aufgabe 4                                                                      #
##################################################################################

str(mietpreise)

mietpreise$lage <- factor(mietpreise$lage, 1:3, c("normal", "gut", "beste"))

model <- lm(miete ~ lage, data = mietpreise)
summary(model)

round(model$coefficients[[1]],2)                            # normale Lage
round(model$coefficients[[1]] + model$coefficients[[2]],2)  # gute Lage
round(model$coefficients[[1]] + model$coefficients[[3]],2)  # beste Lage
