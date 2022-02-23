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
  Mod <- getmode(x) ##Berechne den Modus mit der Hilfsfunktion aus Funktionen-R-Skript2.R
  Ergebnis <- paste("Das arithmetische Mittel ist ",m," mit einer Standardabweichung von ",stabw, ",der Modus liegt bei",Mod,"und der Median ist ",med,".", sep = "")
  ## Gebe die errechneten Werte aus
  
  return(Ergebnis) 
}

## Beispiel zum Ausprobieren:
x <- c(1:100)
metrisch(x)




## (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
## fuer kategoriale Variablen berechnet und ausgibt

kategoriell <- function(x){ ## Funktion fuer kategorielle Variablen
  H <- table(x) ##Erzeugung von KOntingenztafel (absolute Haeufigkeiten)
  h <- H/length(x) ##Tafel fuer die relativen Haeufigkeiten
  h <- round(h, digits = 4) ##Rundung der relativen Haeufigkeiten auf 4 Nachkommastellen
  Ergebnis <- cbind("abs. Haeufigk." = H, "rel. Haeufigk." = h) ## relative & absolute Haeufigkeiten in einem Frame
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
  H <- table(x, y) ##Erzeugung von KOntingenztafel (absolute Haeufigkeiten)
  H <- addmargins(table(x,y)) ## Zeilen- und Spaltensummen hinzufuegen
  h <- prop.table(H, 1) ##Tafel fuer die relativen Haeufigkeiten
  h <- round(h, digits = 4) ##Rundung der relativen Haeufigkeiten auf 4 Nachkommastellen
  h <- addmargins(prop.table(table(x, y))) ## Zeilen- und SPaltensummen hinzufuegen
  Ergebnis <- cbind("abs. Haeufigk." = H, "rel. Haeufigk." = h) ## rel. und abs. Haeufigkeitstabellen zusammenfuegen
  ## Gebe die errechneten Werte aus
  return(Ergebnis) ## letzten Abstand noch korrigieren!
}

## Beispiel zum Ausprobieren:
z <- c(rep(c("male", "female", "nb"), c(2,3,3)))
bi.kategoriell(y, z)




## (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammengang zwischen einer metrischen und einer
## dichotomen Variablen berechnet und ausgibt

metrisch.dichotom <- function(x, y){ ## Funktion fuer eine metrische und eine dichotome Variable 
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
    return("Alle Werte mussen numeric sein.")
  low <- quantile(x,l) 
  medium <- quantile(x,m)   ## Berechnung der quantile von x
  high <- quantile(x,1)
  
  complete <- ifelse(x <= quantile(x,l), "low", 
                     ifelse(x <= quantile(x,m), "medium","high")) ## Klassifizierung der Werte nach Quantilen, braucht evtl. noch Anpassung um Sinnvoller zu sein.
  
  # return(c(low,medium,high))
  return(complete)           ## fuer Gewuenschte Version auskommentierung aendern
  
}

## Beispiele zum Ausprobieren:
q <- 1:100
create.quantil(q)
create.quantil(q,0,1/2)



## (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier
## kategorialen Variablen erstellt

## install.packages("vcd") ## falls noch nicht installiert, Paket vcd hier installieren
library(vcd) ## Paket vcd laden
library(grid) ## wegen Warnmeldung auch Paket grid laden, ist aber in base R

visual.multi.kategoriell <- function(x, y, z,...){ 
  ## habe erstmal versucht, die Funktion fuer 3 Variablen zu schreiben
  ## ... also mit weiteren optionalen Argumenten
  ## Kontrolle der Argumente entfaellt, da mit allen Variablen moeglich
  if(length(x)!=length(y) | length(x)!= length(z)) ## sicherstellen, dass Variablen
    ## gleiche Laenge haben, sonst Fehlermeldung
    return("Die Variablen muessen gleicher Laenge sein.")
  daten <- data.frame(x, y, z) ## data.frame unserer Variablen erstellen
  tab <- table(daten) ## Kontingenztabelle erstellen fuer mosaicplot Funktion
  mosaicplot(tab, shade = TRUE, ...) ## Mosaicplot erstellen mit optionalen weiteren Argumenten
}

## Beispiel zum Ausprobieren
a <- c(rep(c("gruen", "rot"), times = 3), rep("blau", times = 6))
b <- c(rep("tief", times = 6), rep(c("hoch", "mittel"), times = 3))
c <- c(rep(c("laut"), times = 3), rep("maessig", times = 3), rep("leise", times = 6))

visual.multi.kategoriell(a, b, c, main = "Mosaicplot von a, b und c",
                         xlab = "a", ylab = "b")

## Ich bin mir nicht sicher, ob die Unklarheit des Plots in meiner Funktion liegt oder
## daran, dass ich mein Beispiel nicht gut gewaehlt habe.

## Hier ein Beispiel aus dem Internet, mit mosaicplot
mosaicplot( ~ Admit + Gender + Dept, data = UCBAdmissions, shade = TRUE)

## Hier noch eine Alternative

## install.packages("ggplot2") ## ggf vorher installieren: ggplot2

## install.packages("ggpubr") ## ggf vorher installieren: ggpubr


visual.multi.kategoriell2 <- function(x, y, z,...){
  ## zunaechst Kontrolle, dass alle Variablen die gleiche Laenge haben
  if(length(x)!= length(y) | length(x) != length(z))
    return("Die Variablen muessen die gleiche Laenge haben.")
  library(ggplot2) ## lade noetige Pakete 
  library(ggpubr)
  theme_set(theme_pubr())
  df <- data.frame(x, y, z) ## erstelle data.frame aus Variablen
  Ergebnis <- ggplot(df, aes(x = x, y = y),...)+ ## plotte die Grafik mit ggplot
    geom_bar( ## Balkendiagramme proportional zur Anzahl in Gruppe
      aes(fill = z), stat = "identity", color = "white", ##aesthetic mappings
      position = position_dodge(0.9)
    )+
    facet_wrap(~z) + ## 1-dimensionale Abfolge von Panels in 2-d verwandeln
    fill_palette("jco") ## Palette fuer das Ausfuellen waehlen
  return(Ergebnis) ## Ergebnis ausgeben
}

visual.multi.kategoriell2(a, b, c, main = "Balkendiagramm von a, b und c")
## sieht schoener aus als der Mosaicplot, Problem: ich kriege keinen Titel rein :(

## (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt.

#   
visualisierung <- function(data) { ## Funktion zum Visualisieren und Zeichnen von Diagrammen
  library(ggplot2) ## Hinzufügen einer Bibliothek zum Zeichnen eines Diagramms
  ggplot(data, aes(x=reorder(cat1, cat1, function(x)-length(x)))) + geom_bar(fill='red') +  labs(x='cat1') ## Verwendung von ggplot, um ein Diagramm nach Kategorie mit roter Farbe und dem Titel "cat1" zu zeichnen
}

## Beispiele zum Ausprobieren:
data <- data.frame(cat1 = c("eins", "zwei", "drei", "vier","eins","funf","eins", "funf"))
visualisierung(data)

## freiwillig: weitere zur Deskription und Visualisierung geeignete Funktionen

## Boxplots fuer eine metrische Variable nach einer dichotomen Variable

grafik.metrisch.dichotom <- function(x, y,...){ ## Funktion fuer 1 metrische, 1 dichotome Variable
  ## hierbei soll x dichotom sein, y numerisch, weitere Argumente koennen uebergeben werden
  ## z. b. fuer Titel, Achsenbeschriftung
  if(!is.numeric(y)) return("Die Variable y muss numeric sein.")
  box <- boxplot(y ~ x,...) ##erstelle Boxplot fuer x(metrisch), getrennt nach y(dichotom)
  return(box) ## Ergebnis ausgeben
}

## Beispiel zum Ausprobieren, kopiert aus d)
m <- c(1,3,2,4,2,7,12,0)
l <- c(rep(c("male", "female"), 4))
grafik.metrisch.dichotom(l,m, main = "Boxplots von m, getrennt nach l")
