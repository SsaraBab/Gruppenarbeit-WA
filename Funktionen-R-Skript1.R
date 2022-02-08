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
  med <- median(x) ## berechne den Median und weise ihm einen Namen zu
  Ergebnis <- paste("Das arithmetische Mittel ist ",m," mit einer Standardabweichung von ",stabw,"und der Median ist",med,".")
  ## Gebe die errechneten Werte aus
return(Ergebnis) ## letzten Abstand noch korrigieren!
}

## Beispiel zum Ausprobieren:
metrisch(c(1:100))




## (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
## für kategoriale Variablen berechnet und ausgibt

kategoriell <- function(x){ ## Funktion fuer kategorielle Variablen
  t <- table(x) ##Erstellung einer Tabelle der absoluten Haeufigkeiten
  h <- prop.table(t) ##Erstellung einer Tabelle der relaiven Haeufigkeiten
  h <- round(h, digits = 2) ##Rundung der relativen Haeufigkeiten auf 2 Nachkommastellen
  Ergebnis <- paste("")
  ## Gebe die errechneten Werte aus
  return(Ergebnis) ## letzten Abstand noch korrigieren!
}

y <- c(rep(c("eins", "zwei", "drei", "vier"), c(1,2,4,1)))
kategoriell(y)


