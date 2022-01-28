## Part 1 der Aufgabe

## ID fuer ID-Spalte anlegen
ID <- c(1:100)

## Wert fuer Zufallsgenerator setzen, um Ziehung von Zufallszahlen reproduzierbar zu machen
set.seed(2022)

## Alter simulieren
Alter.Normalverteilt <- rnorm(n=100,mean=25,sd=2) #Normalverteiltes Alter (Erwartungswert 25, Stand.abw. 2) mit Nachkommastellen 
Alter <- signif(Alter.Normalverteilt, digits = 2) #Rundung des Alters

## Also ist 'Alter' letztendlich Vektor fuer die Spalte.

Daten <- data.frame(ID = ID, Alter = Alter) ## erster Dataframe mit den Infos zu ID und Alter.




## Studienfach simulieren

Faecher <- c("Statistik", "Data Science", "Mathe", "Informatik") # Vektor der STudienfaecher erzeugt
Studienfach <- sample(Faecher, size = 100, replace = TRUE, prob = c(0.3,0.3,0.1,0.2))
## Studienfaecher zufaellig ausgewahlt aus dem Vektor, wobei Statistik & Data Science Wkeit von 0.3 haben,
## Mathe von 0.1 und Informatik von 0.2

## Also ist 'Studienfach' letztendlich Vektor fuer die Spalte.

Daten <- data.frame(ID = ID, Alter = Alter, Studienfach = Studienfach) ## Dataframe mit ergaenzten Daten zum Studienfach.




## Interesse an Mathematik simulieren

Interesse_Mathe0 <- sample(1:7, size = 100, replace = TRUE) ## zunaechst Vektor aus diskreter Gleichvert. erzeugen
## dabei noch kein Zusammenhang mit Studienfach

Daten <- data.frame(ID = ID, Alter = Alter, Studienfach = Studienfach, Interesse_Mathe = Interesse_Mathe0)
## Vektor ohne Zusammenhang zunaechst zum Daraframe hinzufuegen, um ihn dort zu bearbeiten.

Daten$Interesse_Mathe <- ifelse(Daten$Studienfach == "Mathe", Daten$Interesse_Mathe + 2, Daten$Interesse_Mathe)
## Interesse plus 2 falls das Studienfach Mathe
Daten$Interesse_Mathe <- ifelse(Daten$Studienfach == "Statistik", Daten$Interesse_Mathe + 1, Daten$Interesse_Mathe)
Daten$Interesse_Mathe <- ifelse(Daten$Studienfach == "Data Science", Daten$Interesse_Mathe + 1, Daten$Interesse_Mathe)
## Interesse plus 1 falls das Studienfach Statistik oder Data Science.

abrunden <- function(x){## Funktion zum Abrunden: falls Addition zu Punktzahl hoeher als 7 
  ##gefuehrt hat, Wert fuer Interesse auf 7 reduzieren
  i <- 1
  while(i <= length(x)){
    if(x[i] > 7) x[i] <- 7
    else x[i] <- x[i]
    i <- i + 1
  }
  x
}

Daten$Interesse_Mathe <- abrunden(Daten$Interesse_Mathe) ## abrunden durchfuehren

any(Daten$Interesse_Mathe > 7) ## Kontrolle, ob Rundung korrekt durchgefuehrt wurde
any(Daten$Interesse_Mathe <= 0) ## Kontrolle, ob Funktion korrekt die Eintraege auf groesser 0 begrenzt hat

## Somit ist die Spalte fuer Interesse an Mathe erledigt.

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
any(Daten$Interesse_Programmieren <= 0) ## Kontrolle, ob Funktion korrekt die Eintraege auf groesser 0 begrenzt hat

## Fuer Variable Mathe_LK: 
## Falls wir es nicht ans Laufen kriegen, dann eben ohne Zusammenhang:

Daten$Mathe_LK <- sample(c("nein","ja"), size = 100, replace = TRUE)

Daten

## Daten als factor codieren:
Daten$Mathe_LK <- factor(Daten$Mathe_LK, levels = c("nein", "ja"), labels = c("Nein", "Ja"))

Daten$Mathe_LK ## Ueberpruefung ob Codierung als factor geklappt hat
str(Daten) ## Ueberpruefung ob Codierung als factor geklappt hat 

## Zuletzt erstellen Datensatz als csv Datei speichern:
write.csv(Daten, file = "Datensatz.csv", row.names = FALSE) ## Hier bewusst keine weiteren Optionen wie header = TRUE,
## col.names = TRUE o. ae. gesetzt, da es sonst zur Fehlermeldung kommt:

## weil eben eine Fehlermeldung kam, aber eine csv-Datei erstellt wurde:
# Warnmeldung:
#   In write.csv(Daten, file = "Datensatz.csv", col.names = TRUE, row.names = FALSE) :
#   Versuch ignoriert 'col.names' zu setzen
## Warnmeldung kommt auch, wenn ich zusaetzlich row.names = TRUE setze. 
## row.names = FALSE scheint irgendwie nicht zu wirken, bei Kontrolle unten stehen welche.

## Einlesen der erstellten csv-Datei zur Kontrolle, ob alles korrekt gespeichert wurde,

Daten2 <- read.csv(file = "Datensatz.csv")

Daten2                                
str(Daten2)
## Sieht alles aus wie beabsichtigt, bis auf die linke Spalte mit den Zeilennummern, aber die 
## habe ich durch row.names = FALSE oben bei write.csv leider auch nicht weggekriegt.