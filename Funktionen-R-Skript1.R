## Funktionen-R-Skript zur Analyse des Datensatzes

##setwd("~/Documents/GitHub/Gruppenarbeit-WA") ## hier jede*r bitte seinen eigenen Pfad eingeben
## oder weiss jemand, ob man das bei GitHub irgendwie allgemein einstellen kann?
source("Funktionen-R-Skript2.R") ## ganz zu Anfang die Datei mit den Hilfsfunktionen einlesen

## (a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken fuer metrische Variablen
## berechnet und ausgibt

metrisch <- function(x){ ## Funktion fuer metrische Variablen
  if(is.factor(x) == TRUE | is.character(x) == TRUE) return("Die Variable muss metrisch sein.") 
  ## Funktion soll zunaechst pruefen, dass x kein factor oder character ist -- 
  ## andere ungeeignete Datentypen bitte hier ergaenzen!
  else {x <- x} ## weiss nicht, wie ich R sonst sagen soll, dass R einfach weitermacht
  m <- mean(x) ## berechne das arithmetische Mittel und weise ihm einen Namen zu
  stabw <- sd(x) ## berechne die Standardabweichung und weise ihr einen Namen zu
  Ergebnis <- paste("Das arithmetische Mittel ist ",m," mit einer Standardabweichung von ",stabw,".")
  ## Gebe die errechneten Werte aus
return(Ergebnis) ## letzten Abstand noch korrigieren!
}