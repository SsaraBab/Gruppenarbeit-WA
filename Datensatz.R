## Part 1 der Aufgabe

## ID fuer ID-Spalte anlegen
ID <- c(1:100)

## Wert fuer Zufallsgenerator setzen, um Ziehung von Zufallszahlen reproduzierbar zu machen
set.seed(2022)

## Alter simulieren
Alter.Normalverteilt <- rnorm(n=100,mean=25,sd=2) #Normalverteiltes Alter (Erwartungswert 25, Stand.abw. 2) mit Nachkommastellen 
Alter <- signif(Alter.Normalverteilt, digits = 2) #Rundung des Alters

## Also ist 'Alter' letztendlich Vektor f?r die Spalte.

Daten <- data.frame(ID = ID, Alter = Alter) ## erster Dataframe mit den Infos zu ID und Alter.




## Studienfach simulieren

Faecher <- c("Statistik", "Data Science", "Mathe", "Informatik") # Vektor der STudienfaecher erzeugt
Studienfach <- sample(Faecher, size = 100, replace = TRUE, prob = c(0.3,0.3,0.1,0.2))
## Studienfaecher zufaellig ausgewahlt aus dem Vektor, wobei Statistik & Data Science Wkeit von 0.3 haben,
## Mathe von 0.1 und Informatik von 0.2

## Also ist 'Studienfach' letztendlich Vektor f?r die Spalte.

Daten <- data.frame(ID = ID, Alter = Alter, Studienfach = Studienfach) ## Dataframe mit ergaenzten Daten zum Studienfach.




## Interesse an Mathematik simulieren

Interesse_Mathe0 <- sample(1:7, size = 100, replace = TRUE) ## zunaechst Vektor aus diskreter Gleichvert. erzeugen
## dabei noch kein Zusammenhang mit Studienfach

Daten <- data.frame(ID = ID, Alter = Alter, Studienfach = Studienfach, Interesse_Mathe = Interesse_Mathe0)
## Vektor ohne Zusammenhang zun?chst zum Daraframe hinzufuegen, um ihn dort zu bearbeiten.

Daten$Interesse_Mathe <- ifelse(Daten$Studienfach == "Mathe", Daten$Interesse_Mathe + 2, Daten$Interesse_Mathe)
## Interesse plus 2 falls das Studienfach Mathe
Daten$Interesse_Mathe <- ifelse(Daten$Studienfach == "Statistik", Daten$Interesse_Mathe + 1, Daten$Interesse_Mathe)
Daten$Interesse_Mathe <- ifelse(Daten$Studienfach == "Data Science", Daten$Interesse_Mathe + 1, Daten$Interesse_Mathe)
## Interesse plus 1 falls das Studienfach Statistik oder Data Science.

abrunden <- function(x){## Funktion zum Abrunden: falls Addition zu Punktzahl hoeher als 7 
  ##gefuehrt hat, Wert fuer Interesse auf 7 reduzieren
  ifelse(x > 7, x == 7, x)
}
Daten$Interesse_Mathe <- abrunden(Daten$Interesse_Mathe) ## abrunden durchfuehren

any(Daten$Interesse_Mathe > 7) ## Kontrolle, ob Rundung korrekt durchgefuehrt wurde

## Somit ist die SPalte fuer Interesse an Mathe erledigt.

## Interesse an Programmieren 

Interesse_Programmieren <- sample(1:7, size = 100, replace = TRUE) ## zunaechst Vektor aus diskreter 
## Gleichverteilung ziehen, noch ohne Zusammenhang mit Studienfach

Daten <- data.frame(ID = ID, Alter = Alter, Studienfach = Studienfach, Interesse_Mathe = Interesse_Mathe0,
                    Interesse_Programmieren = Interesse_Programmieren)
## Spalte fuer Interesse am Programmieren hinzufuegen, um direkt im Data Frame weiterzuarbeiten.

Daten$Interesse_Programmieren <- ifelse(Daten$Studienfach == "Informatik", 
                                        Daten$Interesse_Programmieren*1.5, Daten$Interesse_Programmieren)
## Interesse Mal 1.5 falls das Studienfach Informatik.
Daten$Interesse_Programmieren <- ifelse(Daten$Studienfach == "Data Science", 
                                        Daten$Interesse_Programmieren*1.2, Daten$Interesse_Programmieren)
## Interesse Mal 1.2 falls das Studienfach Data Science.

Daten$Interesse_Programmieren <- signif(Daten$Interesse_Programmieren, digits = 1) # Vektor auf 1 Ziffer runden

Daten$Interesse_Programmieren <- abrunden(Daten$Interesse_Programmieren)
# Eintraege groesser als 7 auf 7 abrunden

any(Daten$Interesse_Programmieren > 7) ## Kontrolle, ob Funktion korrekt die Eintraege auf 7 begrenzt hat

## Versuch, den Interessenvektor Programmieren anders abzurunden, gleiches Problem: 
## erst Werte groesser 7 auf 6.5 setzen, dann die signif Funktion

Daten$Interesse_Programmieren <- ifelse(Daten$Interesse_Programmieren > 7, 
                                        Daten$Interesse_Programmieren == 6.5, Daten$Interesse_Programmieren)
Daten$Interesse_Programmieren <- signif(Daten$Interesse_Programmieren, digits = 1) # Vektor auf 1 Ziffer runden

## Mir ist der Fehler ein absolutes Raetsel, auch googeln half nicht. Daher nun ein dirty 
## workaround, weil ich mir nicht anders zu helfen weiss: da wir ja wissen, dass entweder die 
## Funktion abrunden oder das ifelse fuer Werte groesser 7 ausschliesslich die betreffenden Werte auf 0 reduziert
## haben, ersetzen wir nun alle Nullen durch "7": 
Daten$Interesse_Mathe <- ifelse(Daten$Interesse_Mathe == 0, 
                                Daten$Interesse_Mathe == 7, Daten$Interesse_Mathe)
Daten$Interesse_Programmieren <- ifelse(Daten$Interesse_Programmieren == 0, 
                                        Daten$Interesse_Programmieren == 7,
                                        Daten$Interesse_Programmieren)
## Bei Mathe funktioniert es, bei Programmieren leider nicht. 
## Obwohl die Zeilen fuer mich identisch aussehen!

Daten

## Fuer Variable Mathe_LK: 
## aus 0 und 1 ziehen mit Wahrscheinlichkeiten basierend auf dem Studienfach, funktioniert nicht:

Daten$Mathe_LK <- sample(c(0,1), size = 100, replace = TRUE, 
                         prob = ifelse(Daten$Studienfach == "Statistik", 0.6, 
                                       ## Wkeit fuer Mathe LK bei Fach Statistik ist 0.6
                                            ifelse(Daten$Studienfach == "Data Science", 0.5, 
                                                   ## Wkeit bei Data Science 0.5
                                                      ifelse(Daten$Studienfach == "Mathe", 0.8, 0.4))))
## Wkeit fuer Mathe LK bei Fach Mathe ist 0.8, bei allem anderen, also Informatik, bei 0.4

##Fehler in sample.int(length(x), size, replace, prob) : 
##falsche Anzahl von Wahrscheinlichkeiten

## Falls wir es nicht ans Laufen kriegen dann eben ohne Zusammenhang:

Daten$Mathe_LK <- sample(c("nein","ja"), size = 100, replace = TRUE)

Daten

## Ich dachte, man sollte das noch als factor kodieren, kriege es aber nicht ans Laufen:
Daten$Mathe_LK <- as.factor(Daten$Mathe_LK, levels = c("nein","ja"), labels = c("nein", "ja"))
##Fehler in as.factor(Daten$Mathe_LK, levels = c("nein", "ja"), labels = c("nein",  : 
##unbenutzte Argumente (levels = c("nein", "ja"), labels = c("nein", "ja"))
                                                                        





  