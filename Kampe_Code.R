# Hausarbeit | Data Analytics II - Empirische Wirtschaftsforschung
# Niklas Leander Kampe | 16-611-618

# Die Berechnungen und Ergebnisse dieser Detai beziehen sich auf die beigelegte PDF-Datei "Kampe_Hausarbeit.pdf", in der näher auf die 
# Ergebnisse eingegangen wird, Interpretationen der Ergebnisse als auch die Beantwortung weiterer Teilaufgaben erfolgen.

# Teil 1: Oregon Health Experiment

# Data definition:
  # winner: Dummyvariable - 1 = Lotteriegewinner/in; 0 = kein/e Lotteriegewinn/in
  # insured: Dummyvariable - 1 = krankenbersichert; 0 = nicht krankenversichert
  # good-health: Dummyvariable - 1 = gute Gesundheit; 0 = keine gute Gesundheit
  # female: Dummyvariable - 1 = weiblich; 0 = männlich
  # hhinc_pct f pl: Haushaltseinkommen in % der Armutsgrenze
  # race: Krategorievariable - Zugehörigkeit zu ethnischer Gruppe

# Vorbereitung

# Set working directory to source file location (local folder "Hausarbeit")
setwd("~/Desktop/02 HSG/01 Data Analytics II - Empirische Wirtschaftsforschung/Hausarbeit")

# Arbeitsumgebung leeren
rm(list=ls())

# Datensatz "OHIE_data.RData" in Arbeitsumgebung laden
load("OHIE_data.RData")

# Benötigte Packages "AER" (with all dependencies), "ggplot2" und "dplyr" installieren bzw. updaten, da bereits zuvor installiert
install.packages("AER", dependencies = T)
install.packages(c("dplyr", "ggplot2"))

# Benötigte Packages "AER", "ggplot2" und "dplyr" laden
library(AER)
library(dplyr)
library(ggplot2)

# Datensatz "OHIE_data.RData" zur Veranschaulichung zusammenfassen
summary(data)

# Datensatz anpassen hinsichtlich unvollständiger Teildatensätze
data <- na.omit(data)

# Aufgabe 1: Univariate Regression

# Regression: Dummyvariable "good_health" auf Dummyvariable "insured"
ols1 <- lm(good_health ~ insured, data = data)
# Zusammenfassung der Regressionsergebnisse
summary(ols1)

# Aufgabe 3: Kreuztabelle

# Kreuztabelle: Dummyvariable "winner" x Dummyvariable "insured"
kreuztabelle <- mutate(group_by(count(data, winner, insured)), percentage = (n/sum(n))*100)
print(kreuztabelle)

# Aufgabe 4: Kausaler Effekt

# TSLS Schritt 1: OLS-Regression zur Isolierung der exogenen Variation von insured, die mit v korreliert
tsls1.1 <- lm(insured ~ winner, data = data)
# Zusammenfassung der Regressionsergebnisse
summary(tsls1.1)
# Speicherung der Fitted Values aus der OLS-Regression
tsls1.1_fittedvalues <- tsls1.1$fitted.values

# TSLS Schritt 2: Einsetzen des Schätzers aus tsls1.1 in ursprüngliche Regression
tsls1.2 <- lm(good_health ~ tsls1.1_fittedvalues, data = data)
# Zusammenfassung der Regressionsergebnisse
summary(tsls1.2)

# Aufgabe 5: Zusätzliche Berücksichtigung der Kontrollvariablen

# Übersicht über Kontrollvariablen female, hhinc_pctfpl und race
table(data$female)
summary(data$hhinc_pctfpl)
summary(data$race)
# Untersuchung unter Hinzunahme der Kontrollvariablen
ols_control <- lm(good_health ~ insured + female + hhinc_pctfpl + race, data = data)
summary(ols_control)


# Teil 2: Simulation IV


# Benötigtes Package "MASS" installieren bzw. updaten, da bereits zuvor installiert
install.packages("MASS")

# Benötigtes Package "MASS" laden
library(MASS)

# Vorbereitung
# Seed setzen für Replikation der zufälligen Stichprobe
set.seed(412354329)
# Anzahl der Beobachtungen auf n = 1000 setzen
n = 1000
# Zwei korrelierte Fehlerterme u und v aus multivariaten Normalverteilung ziehen
mu = c(0,0)
Sigma = matrix(c(100,-7,-7,1),2,2)
err = mvrnorm(n, mu, Sigma)
u = err[,1]
v = err[,2]
# Instrumentalvariable als Dummyvariable z mit z ∼ Binomial(1, 0.5) ziehen
z = rbinom(n,1,0.5)
# Endogene erklä̈rende Variable Krankenversicherung KV in zwei Schritten generieren
# Schritt 1: Temporäre Variable temp = −1 + 1 · z + v generieren 
pi0 = -1
pi1 = 1
KV = pi0 + pi1 * z + v
KV = as.numeric(KV > 0)
# Abhä̈ngige Variable G = 50 + 10 · KV + u generieren (G = Gesundheitsindex mit Werten zwischen 0 und 100)
b0 = 50
b1 = 10
G = b0 + b1 * KV + u


# Aufgabe 6


# Aufgabe 6a: Geeignete deskriptive Statistiken

# Deskriptive Statistik für Gesundheitsindex G
summary(G)
# Deskriptive Statistik Variable Krankenversicherung KV
table(KV)
# Deskriptive Statistik für endogene Dummyvariable z
table(z)

# Aufgabe 6b: Kovarianz & Verletzung von MLR.4

# Kovarianz zwischen KV und u
cov_KV_u <- cov(KV,u)
print(cov_KV_u)

# Aufgabe 6c: OLS Regression I

# OLS-Gression mit KV als abhängige und z als erklärende Variable
ols2 <- lm(KV ~ z)
summary(ols2)
# Kovarianz zwischen KV und z berechnen
cov(KV,z)

# Aufgabe 6d: OLS Regression II

ols3 <- lm(G ~ KV)
summary(ols3)
ols3$coefficients[1] - b0
ols3$coefficients[2] - b1

# Aufgabe 6e: IV Regression

# TSLS Schritt 1: OLS-Regression zur Isolierung der exogenen Variation von KV, die mit v korreliert
tsls2.1 <- lm(KV ~ z)
# Zusammenfassung der Regressionsergebnisse
summary(tsls2.1)
# Speicherung der Fitted Values aus der OLS-Regression
tsls2.1_fittedvalues <- tsls2.1$fitted.values

# TSLS Schritt 2: Einsetzen des Schätzers aus tsls1.1 in ursprüngliche Regression
tsls2.2 <- lm(G ~ tsls2.1_fittedvalues)
# Zusammenfassung der Regressionsergebnisse
summary(tsls2.2)
# Abweichungen des IV-Schötzers vom wahren Wert Beta(1)
tsls2.2$coefficients[1] - b0
tsls2.2$coefficients[2] - b1


# Aufgabe 7


# Vorbereitungen

# Benötigte Packages "tidyverse", "broom" und "ggthemes" installieren bzw. updaten, da bereits zuvor installiert
install.packages(c("tidyverse", "broom", "ggthemes"))

# Benötigte Pckages laden
library(tidyverse)
library(broom)
library(ggthemes)

# Zugewiesene Stichprobengrösse (sp = Stichprobengrösse)
n_spg = 775

# Anzahl Stichproben
n_sp = 1000

# Für das angenehmere Fitting werden die Daten in einem Data Frame gespeichert
tbl_data <- tibble(G = G, KV = KV, z = z)

# Aufgabe 7a: OLS & IV Regression

# Matrix zur Speicherung der Schätzergebnisse der OLS- und IV-Regression
sim_schätzer = matrix(NA,n_sp,2)
colnames(sim_schätzer) = c("OLS","IV")

# Seed setzen
set.seed(41235432)

# For-Loop zur Berechnung der OLS- und IV-Schätzer 
for (i in 1:n_sp) {
  # Stichprobe ziehen
  sim_sample <- sample_n(tbl_data, size = n_spg)
  
  # OLS-Schätzer für Beta(1)
  ols4 <- lm(G ~ KV, data = sim_sample)
  sim_schätzer[i,1] = ols4$coefficients[2]
    
  # IV-Schätzer für Beta(1)
  tsls3.1 <- lm(KV ~ z, data = sim_sample)
  tsls3.1_fittedvalues <- tsls3.1$fitted.values
  tsls3.2 <- lm(G ~ tsls3.1_fittedvalues, data = sim_sample)
  sim_schätzer[i,2] = tsls3.2$coefficients[2]
}
# Anzeigen der Matrix mit den Schätzergebnissen
print(sim_schätzer)
# Durchschnitt der Beta(1)-OLS-Schätzer
mean(sim_schätzer[,1])
# Durchschnitt der Beta(1)-IV-Schätzer
mean(sim_schätzer[,2])
# Matrix sim_schätzer als Data Frame speichern
df = as.data.frame(sim_schätzer)

# Aufgabe 7b: Grafische Darstellung

# Histogramm OLS-Schätzer als Variable erstellen
hist_OLS <- ggplot(df,aes(x=OLS)) + 
              geom_histogram(bins=50) + 
              geom_vline(xintercept = c(b1,mean(df$OLS)),linetype=c('solid','dashed')) + 
              xlab("OLS-Schätzer Beta(1)") +
              ylab("Anzahl Schätzwerte") +
              xlim(-2, 14) +
              theme_classic()
# Histrogramm OLS-Schätzer anzeigen
print(hist_OLS)

# Histogramm IV-Schätzer mit ggplot als Variable erstellen
hist_IV <- ggplot(df,aes(x=IV)) + 
              geom_histogram(bins=50) + 
              geom_vline(xintercept = c(b1,mean(df$IV)),linetype=c('solid','dashed')) + 
              xlab("IV-Schätzer Beta(1)") +
              ylab("Anzahl Schätzwerte") +
              xlim(-2, 14) +
              theme_classic()
# Histrogramm IV-Schätzer anzeigen
print(hist_IV)

# Aufgabe 7c: Verzerrung der Schätzer

# Verzerrung des IV-Schätzers als Differenz vom wahren Wert Beta(1) und dem Durchschnitts-OLS-Schätzer
mean_b1_OLS <- mean(sim_schätzer[,1])
verz_OLS <- mean_b1_OLS - b1
print(paste("Verzerrung OLS-Schätzer:", verz_OLS))

# Verzerrung des IV-Schätzers als Differenz vom wahren Wert Beta(1) und dem Durchschnitts-IV-Schätzer
mean_b1_IV <- mean(sim_schätzer[,2])
verz_IV <- mean_b1_IV - b1
print(paste("Verzerrung IV-Schätzer:", verz_IV))


# Aufgabe 8


# Neuen Seed setzen
set.seed(41235432)

# Replikation Aufgabe 7a: OLS & IV Regression
z_2 = rbinom(n,1,0.5)
pi1_2 = 0.1
KV_2 = pi0 + pi1_2 * z + v
KV_2 = as.numeric(KV_2 > 0)
G_2 = b0 + b1 * KV_2 + u

# Für das angenehmere Fitting werden die Daten in einem Data Frame gespeichert
tbl_data_2 <- tibble(G_2 = G_2, KV_2 = KV_2, z_2 = z_2)

# Matrix zur Speicherung der Schätzergebnisse der OLS- und IV-Regression
sim_schätzer_2 = matrix(NA,n_sp,2)
colnames(sim_schätzer_2) = c("OLS","IV")

# For-Loop zur Berechnung der OLS- und IV-Schätzer 
for (i in 1:n_sp) {
  # Stichprobe ziehen
  sim_sample_2 <- sample_n(tbl_data_2, size = n_spg)
  
  # OLS-Schätzer für Beta(1)
  ols5 <- lm(G_2 ~ KV_2, data = sim_sample_2)
  sim_schätzer_2[i,1] = ols5$coefficients[2]
  
  # IV-Schätzer für Beta(1)
  tsls4.1 <- lm(KV_2 ~ z_2, data = sim_sample_2)
  tsls4.1_fittedvalues <- tsls4.1$fitted.values
  tsls4.2 <- lm(G_2 ~ tsls4.1_fittedvalues, data = sim_sample_2)
  sim_schätzer_2[i,2] = tsls4.2$coefficients[2]
}
print(sim_schätzer_2)
# Durchschnitt der Beta(1)-OLS-Schätzer
mean(sim_schätzer_2[,1])
# Durchschnitt der Beta(1)-IV-Schätzer
mean(sim_schätzer_2[,2])
# Matrix sim_schätzer als Data Frame speichern
df_2 = as.data.frame(sim_schätzer_2)

# Replikation Aufgabe 7b: Grafische Darstellung

# Histogramm OLS-Schätzer als Variable erstellen
hist_OLS_2 <- ggplot(df_2,aes(x=OLS)) + 
  geom_histogram(bins=50) + 
  geom_vline(xintercept = c(b1,mean(df_2$OLS)),linetype=c('solid','dashed')) + 
  xlab("OLS-Schätzer Beta(1)") +
  ylab("Anzahl Schätzwerte") +
  theme_classic()
# Histrogramm OLS-Schätzer anzeigen
print(hist_OLS_2)

# Histogramm IV-Schätzer mit ggplot als Variable erstellen
hist_IV_2 <- ggplot(df_2,aes(x=IV)) + 
  geom_histogram(bins=50) + 
  geom_vline(xintercept = c(b1,mean(df_2$IV)),linetype=c('solid','dashed')) + 
  xlab("IV-Schätzer Beta(1)") +
  ylab("Anzahl Schätzwerte") +
  xlim(-100, 100) +
  theme_classic()
# Histrogramm IV-Schätzer anzeigen
print(hist_IV_2)

# Replikation Aufgabe 7c: Verzerrung der Schätzer
# Verzerrung OLS-Schätzer
mean_b1_OLS_2 <- mean(sim_schätzer_2[,1])
verz_OLS_2 <- mean_b1_OLS_2 - b1
print(paste("Verzerrung OLS-Schätzer mit schwachem Instrument:", verz_OLS_2))
# Verzerrung OLS-Schätzer
mean_b1_IV_2 <- mean(sim_schätzer_2[,2])
verz_IV_2 <- mean_b1_IV_2 - b1
print(paste("Verzerrung IV-Schätzer mit schwachem Instrument:", verz_IV_2))

