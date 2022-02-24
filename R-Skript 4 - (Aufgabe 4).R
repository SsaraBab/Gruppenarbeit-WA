## R-Skript 4

#  Hier sollte mit Hilfe der in (3.) in Funktionen-R-Skript 1 erstellten Funktionen den 
#  Datensatz aus (2.) analysiert werden (Deskription und Visualisierung). Hierbei Sollte
#  dies R-Skript in Minimum jede der Funktionen (a) bis (f) aus Funktionen-R-Skript 1 
#  einmal anwenden.


# Import Dateien

# Install R Packages "Tidyverse"

library(tidyverse)

library(readr)

Datensatz <- read_csv("Datensatz.csv")
## Umwandlung in Faktoren
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

# Implementierung der Funktionen

# R-Skript1 einzulesen
source("Funktionen-R-Skript1.R")

## (Funktion a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken fuer metrische
## Variablen berechnet und ausgibt

## Funktion a
#function(metrisch)
  
metrisch(Alter)
metrisch(I_Mathe)
metrisch(I_Program)

## (Funktion b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
## fuer kategoriale Variablen berechnet und ausgibt

# Funktion a aufzurufen
function(kategoriell)
  
  Datensatz$Studienfach <- c(rep(c("Statistic", "Data Science", "informatik", "Mathe"), c(31,37,25,7)))
kategoriell(Datensatz$Studienfach)

## Funktion b
#function(kategoriell)

kategoriell(Alter)
kategoriell(I_Mathe)
kategoriell(I_Program)
kategoriell(Fach)
kategoriell(Mathe_LK)


# hist(kategoriell(Alter)[,1]) ### evtl. Histogramm(e) fuer relative Haeufigkeit hinzufuegen

## (Funktion c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammenhang zwischen zwei kategorialen Variablen
## berechnet ausgibt
# Funktion a aufzurufen



## (Funktion d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammengang zwischen einer metrischen und einer
## dichotomen Variablen berechnet und ausgibt
# Funktion a aufzurufen
function(metrisch.dichotom)
m <- c(Datensatz$Interesse_Programmieren)
l <- 
  
metrisch.dichotom(l, m)


## (Funktion e) Eine Funktion, die eine mindestens ordinal skalierte Variable
## quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")
# Funktion a aufzurufen
function (create.quantil)
q <- Datensatz$Interesse_Programmieren
create.quantil(q)
create.quantil(q,0,1/2)



## (Funktion f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
## kategorialen Variablen erstellt.

# Funktion a aufzurufen
function(visualisierung)
  
data <- data.frame ()
visualisierung(data)
