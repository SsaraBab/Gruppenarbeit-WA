## R-Skript 4

#  Hier sollte mit Hilfe der in (3.) in Funktionen-R-Skript 1 erstellten Funktionen den 
#  Datensatz aus (2.) analysiert werden (Deskription und Visualisierung). Hierbei Sollte
#  dies R-Skript in Minimum jede der Funktionen (a) bis (f) aus Funktionen-R-Skript 1 
#  einmal anwenden.

# Implementierung der Funktionen

# Import Dateien

# Install R Packages
library(tidyverse)
library(readr)

Datensatz <- read_csv("Datensatz.csv")
# Umwandlung in Faktoren
Datensatz$Studienfach <- as.factor(Datensatz$Studienfach) 
Datensatz$Mathe_LK <- as.factor(Datensatz$Mathe_LK)

# Nutzen von Dateien
dplyr::glimpse(Datensatz)

# Variablen aus Datensatz zur Nutzung in Funktionen
ID <- Datensatz$ID
Alter <- Datensatz$Alter
Fach <- Datensatz$Studienfach
I_Mathe <- Datensatz$Interesse_Mathe
I_Program <- Datensatz$Interesse_Programmieren
Mathe_LK <- Datensatz$Mathe_LK

# R-Skript1 einzulesen
source("Funktionen-R-Skript1.R")

## (Funktion a - function(metrisch)) Eine Funktion, die verschiedene geeignete deskriptive Statistiken fuer metrische
## Variablen berechnet und ausgibt

# function a aufzurufen
metrisch(Alter)        # Durchschnittsalter: 25
metrisch(I_Mathe)      # Mittelwert: 4,29
metrisch(I_Program)    # Mittelwert: 4,05

## (Funktion b - function(kategoriell)) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
## fuer kategoriale Variablen berechnet und ausgibt

# function b aufzurufen
kategoriell(Alter)     # Starke Haeufung 23-27
kategoriell(I_Mathe)   # recht Gleichverteilt, Spitze bei 7
kategoriell(I_Program) # Haeufung bei Extremwerten
kategoriell(Fach)      # Mathe geringe Haeufigkeit
kategoriell(Mathe_LK)  # sehr gleich (50/50)

# hist(kategoriell(Alter)[,1]) ### evtl. Histogramm(e) fuer relative Haeufigkeit hinzufuegen

## (Funktion c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammenhang zwischen zwei kategorialen Variablen
## berechnet ausgibt

# funktion c aufzurufen
bi.kategoriell(I_Mathe, Mathe_LK)    # Ja Spike bei 7
bi.kategoriell(I_Program, Mathe_LK)  # Mathe_LK Ja Ertremwertverteilt
bi.kategoriell(I_Mathe, I_Program)   # zu geringe Samplesize
bi.kategoriell(Fach, Mathe_LK)       # Statistik einziges Fach weniger Mathe_LK ja als nein

## (Funktion d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammengang zwischen einer metrischen und einer
## dichotomen Variablen berechnet und ausgibt

# funktion d aufzurufen
m <-c(I_Mathe)
l <-c(Mathe_LK)

metrisch.dichotom(l,m)

## (Funktion e) Eine Funktion, die eine mindestens ordinal skalierte Variable
## quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")
# Funktion a aufzurufen

create.quantil(I_Program)
create.quantil(I_Program,0,1/2)

## (Funktion f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
## kategorialen Variablen erstellt.

# funktion f aufzurufen
I_Prog_Kat <- create.quantil(I_Program)
I_Mathe_Kat <- create.quantil(I_Mathe)

visual.multi.kategoriell3(Fach, I_Prog_Kat, I_Mathe_Kat, main = "Balkendiagramm von Faechern, Programmierung und Mathe")

data <- data.frame (cat1 = c(Fach))
visualisierung(data)