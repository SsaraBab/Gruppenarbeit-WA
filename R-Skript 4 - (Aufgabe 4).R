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

# Nutzen von Dateien

dplyr::glimpse(Datensatz)

# Implementierung der Funktionen

## (Funktion a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken fuer metrische
## Variablen berechnet und ausgibt

# R-Skript1 einzulesen
source("Funktionen-R-Skript1.R") 

# Funktion a aufzurufen
function(metrisch)
x <- Datensatz$ID
metrisch(x)
