rm(list = ls())

mietpreise <-
  read.table(
    "http://www.uni-goettingen.de/de/document/download/ba9a7b9c1e3a4a7d50a370e3bce92303.raw/mietspiegel99.raw",
    header = T)

##################################################################################
# Aufgabe 1                                                                      #
##################################################################################

library(stargazer)
library(ggplot2)

mietpreise$flaeche.reziprok <- 1/mietpreise$flaeche

model.1 <- lm(mieteqm ~ flaeche, data = mietpreise)
model.2 <- lm(mieteqm ~ flaeche.reziprok, data = mietpreise)

stargazer(model.1, model.2, type = "text")


ggplot() +
  geom_point(aes(mietpreise$flaeche, mietpreise$mieteqm)) +
  geom_abline(slope = model.1$coefficients[[1]], intercept = model.1$coefficients[[0]])
