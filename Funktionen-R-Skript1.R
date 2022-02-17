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
  m <- mean(x) ## berechne das arithmetische Mittel und weise ihm einen Namen zu
  stabw <- sd(x) ## berechne die Standardabweichung und weise ihr einen Namen zu
  med <- median(x) ## berechne den Median und weise ihm einen Namen zu
  Ergebnis <- paste("Das arithmetische Mittel ist ",m," mit einer Standardabweichung von ",stabw," und der Median ist ",med,".", sep = "")
  ## Gebe die errechneten Werte aus
  return(Ergebnis) 
}

## Beispiel zum Ausprobieren:
x <- c(1:100)
metrisch(x)




## (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
## fuer kategoriale Variablen berechnet und ausgibt

kategoriell <- function(x){ ## Funktion fuer kategorielle Variablen
  t <- table(x) ##Erstellung einer Tabelle der absoluten Haeufigkeiten
  h <- prop.table(t) ##Erstellung einer Tabelle der relaiven Haeufigkeiten
  h <- round(h, digits = 4) ##Rundung der relativen Haeufigkeiten auf 4 Nachkommastellen
  Ergebnis <- h
  ## Gebe die errechneten Werte aus
  return(Ergebnis) ## letzten Abstand noch korrigieren!
}

## Beispiel zum Ausprobieren:
y <- c(rep(c("eins", "zwei", "drei", "vier"), c(1,2,4,1)))
kategoriell(y)



## (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammenhang zwischen zwei kategorialen Variablen
## berechnet ausgibt

bi.kategoriell <- function(x, y){ ## Funktion fuer zwei kategorielle Variablen
  t <- table(x, y) ##Erzeugung von KOntingenztafel
  h <- prop.table(t, 1) ##Tafel fuer die relativen Haeufigkeiten
  h <- round(h, digits = 4) ##Rundung der relativen Haeufigkeiten auf 4 Nachkommastellen
  Ergebnis <- h
  ## Gebe die errechneten Werte aus
  return(Ergebnis) ## letzten Abstand noch korrigieren!
}

## Beispiel zum Ausprobieren:
z <- c(rep(c("male", "female", "nb"), c(2,3,3)))
bi.kategoriell(y, z)




## (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammengang zwischen einer metrischen und einer
## dichotomen Variablen berechnet und ausgibt

metrisch.dichotom <- function(x, y){ ## Funktion fuer eine merische und eine dichotome Variable 
                                     ## x dichotom, y numerisch
  if(length(x) != length(y)) 
    return("Die Variablen muessen gleicher Laenge sein")
  if(!is.numeric(x))
    x <- as.numeric(factor(x))
    
  e <- cor.test(x,y)    ## berechnet Punktbiseriale Korrelation
  Ergebnis <- e
  ## Gebe die errechneten Werte aus
  return(Ergebnis) ## letzten Abstand noch korrigieren!
}

## Beispiel zum Ausprobieren:
m <- c(1,3,2,4,2,7,12,0)
l <- c(rep(c("male", "female"), 4))
metrisch.dichotom(l, m)



## (e) Eine Funktion, die eine mindestens ordinal skalierte Variable
## quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")

create.quantil <- function(x, l = 1/3, m = 2/3){  ## l = niedriges Quantil, m = mittleres
  
  if(!is.numeric(x)|!is.numeric(l)|!is.numeric(m))
    return("Alle Werte müssen numeric sein.")
  low <- quantile(x,l) 
  medium <- quantile(x,m)   ## Berechnung der quantile von x
  high <- quantile(x,1)
  
  complete <- ifelse(x <= quantile(x,l), "low", 
         ifelse(x <= quantile(x,m), "medium","high")) ## Klassifizierung der Werte nach Quantilen, braucht evtl. noch Anpassung um Sinnvoller zu sein.
  
  return(c(low,medium,high))
  # return(complete)           ## fuer Gewuenschte Version auskommentierung aendern
  
}

## Beispiele zum Ausprobieren:
q <- 1:100
create.quantil(q)
create.quantil(q,0,1/2)



## (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
## kategorialen Variablen erstellt






