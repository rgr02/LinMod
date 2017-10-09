rm(list = ls())

# A.1 #########################################################################
library(dplyr)

mietpreise <-
  read.table(
  "http://www.uni-goettingen.de/de/document/download/ba9a7b9c1e3a4a7d50a370e3bce92303.raw/mietspiegel99.raw",
  header = T)

str(mietpreise)
head(mietpreise)


# check missings
any(is.na(mietpreise))

# Converting Factors
mietpreise$lage <- factor(mietpreise$lage, levels = 1:3,
                          labels = c("Normale Lage", "Gute Lage", "Beste Lage"))
mietpreise$bad <- factor(mietpreise$bad, levels = 0:1,
                         labels = c("Normal", "Gehoben"))
mietpreise$kueche <- factor(mietpreise$kueche, levels = 0:1,
                            labels = c("Normal", "Gehoben"))
mietpreise$zh <- factor(mietpreise$zh, levels = 0:1,
                        labels = c("ohne ZH", "mit ZH"))
mietpreise$bezv <- factor(mietpreise$bezv)


# Vectors for Table

Variable <- c("Miete",
              "Mieteqm",
              "Flaeche",
              "Baujahr",
              "Lage",
              "",
              "",
              "",
              "Bad",
              "",
              "",
              "Kueche",
              "",
              "",
              "Zentralheizung",
              "",
              "",
              "Bezirksviertel")

Beschreibung <- c("Nettomiete pro Monat (in DM)",
                  "Nettomiete pro Monat und qm (in DM)",
                  "Wohnflaeche in qm",
                  "Baujahr (in Jahren)",
                  "Lagekategorie gemaess Einschaetzung durch Gutachter",
                  "1 = normale Lage",
                  "2 = gute Lage",
                  "3 = beste Lage",
                  "Ausstattung des Bades",
                  "0 = normal",
                  "1 = gehoben",
                  "Ausstattung der Kueche",
                  "0 = normal",
                  "1 = gehoben",
                  "Zentralheizung",
                  "0 = ohne Zentralheizung",
                  "1 = mit Zentralheizung",
                  "Bezirksviertel in München")

tabelle <- cbind(Variable, Beschreibung)

Mittelwert <- c(round(mean(mietpreise$miete),2),
                round(mean(mietpreise$mieteqm),2),
                round(mean(mietpreise$flaeche),2),
                round(mean(mietpreise$bjahr),2),
                rep(NA, 14))

tabelle <- cbind(tabelle, Mittelwert)

Haeufigkeit <- c(rep(NA,5),
                 round(length(which(mietpreise$lage == "Normale Lage")) / length(mietpreise$lage),2),
                 round(length(which(mietpreise$lage == "Gute Lage")) / length(mietpreise$lage),2),
                 round(length(which(mietpreise$lage == "Beste Lage")) / length(mietpreise$lage),2),
                 NA,
                 round(length(which(mietpreise$bad == "Normal")) / length(mietpreise$bad),2),
                 round(length(which(mietpreise$bad == "Gehoben")) / length(mietpreise$bad),2),
                 NA,
                 round(length(which(mietpreise$kueche == "Normal")) / length(mietpreise$kueche),2),
                 round(length(which(mietpreise$kueche == "Gehoben")) / length(mietpreise$kueche),2),
                 NA,
                 round(length(which(mietpreise$zh == "ohne ZH")) / length(mietpreise$zh),2),
                 round(length(which(mietpreise$zh == "mit ZH")) / length(mietpreise$zh),2),
                 NA)

tabelle <- cbind(tabelle, Haeufigkeit)

SD <- c(round(sd(mietpreise$miete),2),
        round(sd(mietpreise$mieteqm),2),
        round(sd(mietpreise$flaeche),2),
        round(sd(mietpreise$bjahr),2),
        rep(NA, 14))

tabelle <- cbind(tabelle, SD)


MIN <- c(round(min(mietpreise$miete),2),
         round(min(mietpreise$mieteqm),2),
         round(min(mietpreise$flaeche),2),
         round(min(mietpreise$bjahr),2),
         rep(NA,14))

tabelle <- cbind(tabelle, MIN)

MAX <- c(round(max(mietpreise$miete),2),
         round(max(mietpreise$mieteqm),2),
         round(max(mietpreise$flaeche),2),
         round(max(mietpreise$bjahr),2),
         rep(NA, 14))

tabelle <- as.data.frame(cbind(tabelle, MAX))
View(tabelle)

# Replication of Abbildung 1.3 / 1.7

source("multiplot.R")
library(ggplot2)

p1 <- ggplot(data = mietpreise) +
  geom_histogram(aes(x = mietpreise$miete, y = ..density..),
                 colour="black", fill="white") +
  xlab("Nettomiete in DM") +
  ylab("geschätzte Dichte") +
  xlim(0,3600) +
  ylim(0,0.0015) +
  geom_density(aes(x = mietpreise$miete, y = ..density..),
               fill = "red", alpha = 0.15) +
  ggtitle("Histogramm und Kerndichteschätzer: Nettomiete") +
  geom_vline(aes(xintercept=mean(mietpreise$miete)),
             linetype="dashed", size=1, colour="black") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = mietpreise) +
  geom_histogram(aes(x = mietpreise$mieteqm, y = ..density..),
                 colour="black", fill="white") +
  xlab("Nettomiete pro qm in DM") +
  ylab("geschätzte Dichte") +
  xlim(0,35) +
  ylim(0,0.08) +
  geom_density(aes(x = mietpreise$mieteqm, y = ..density..),
               fill = "blue", alpha = 0.15) +
  ggtitle("Histogramm und Kerndichteschätzer: Nettomiete pro qm") +
  geom_vline(aes(xintercept=mean(mietpreise$mieteqm)),
             linetype="dashed", size=1, colour="black") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

p3 <- ggplot(data = mietpreise) +
  geom_histogram(aes(x = mietpreise$flaeche, y = ..density..),
                 colour="black", fill="white") +
  xlab("Wohnfläche in qm") +
  ylab("geschätzte Dichte") +
  xlim(0,160) +
  ylim(0,0.02) +
  geom_density(aes(x = mietpreise$flaeche, y = ..density..),
               fill = "green", alpha = 0.15) +
  ggtitle("Histogramm und Kerndichteschätzer: Wohnflaeche")+
  geom_vline(aes(xintercept=mean(mietpreise$flaeche)),
             linetype="dashed", size=1, colour="black") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data = mietpreise) +
  geom_histogram(aes(x = mietpreise$bjahr, y = ..density..),
                 colour="black", fill="white") +
  xlab("Wohnfläche in qm") +
  ylab("geschätzte Dichte") +
  xlim(1910,2000) +
  ylim(0,0.08) +
  geom_density(aes(x = mietpreise$bjahr, y = ..density..),
               fill = "yellow", alpha = 0.15)  +
  ggtitle("Histogramm und Kerndichteschätzer: Baujahr") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1, p2, p3, p4, cols = 2)


h1 <- ggplot(data = mietpreise) +
  geom_jitter(aes(mietpreise$lage, mietpreise$mieteqm)) +
  geom_boxplot(aes(mietpreise$lage, mietpreise$mieteqm,
                   fill = mietpreise$lage),alpha = 0.4) +
  theme(legend.position = "none") +
  ylab("Nettomiete pro qm") +
  xlab("")

h2 <- ggplot() +
  geom_density(aes(mietpreise$mieteqm, colour = mietpreise$lage),
               size = 1) +
  theme_bw() +
  ylab("Geschätzte Dichte") +
  xlab("Nettomiete pro qm") +
  theme(legend.position = "none")
  theme(legend.title = element_blank())

multiplot(h1, h2, cols = 2)

# Aufgabe 3 ##################################################################
  
x <- c(2.8, 0.8, 3.4, 8.3,5.7, 9.5)
y <- c(5.1, 2.7, 5.8, 4.6, 3.1, 7.3)

data <- as.data.frame(cbind(x,y))

b1 <- cov(data$x,data$y)/var(data$x)
b0 <- mean(data$y) - b1*mean(data$x)
data$yi <- b0 + b1*x
data$residual <- data$y - data$yi

ggplot() +
  geom_point(aes(data$x, data$y)) +
  geom_abline(intercept = b0, slope = b1, colour = "red") +
  geom_point(aes(data$x, data$residual, col = "green")) +
  geom_abline(intercept = 0, slope = 0) +
  ylab("Y - Werte") +
  xlab("X - Werte") +
  theme(legend)
  theme_bw()

  # Aufgabe 4 ##################################################################
  
  library(stargazer)
  
  # non nested alternative
  
  model.level <- lm(miete ~ flaeche, data = mietpreise)
  model.log <- lm(log(miete) ~ flaeche, data = mietpreise)
  
  stargazer(model.level, model.log, type = "text")
  
m1.plot <- ggplot() +
  geom_point(aes(mietpreise$flaeche, mietpreise$miete), alpha = 0.1) +
  geom_smooth(aes(mietpreise$flaeche, mietpreise$miete),method = "lm") +
  ylab("Mietpreise") +
  xlab("Fläche") +
  theme_bw()

m2.plot <- ggplot() +
  geom_point(aes(mietpreise$flaeche, log(mietpreise$miete)), alpha = 0.1) +
  geom_smooth(aes(mietpreise$flaeche, log(mietpreise$miete)), method = "lm") +
  ylab("Log(Mietpreise)") +
  xlab("Fläche") +
  theme_bw()

multiplot(m1.plot, m2.plot)