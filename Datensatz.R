## Part 1 der Aufgabe


## Alter simulieren
Alter.Normalverteilt <- rnorm(n=100,mean=25,sd=2) #Normalverteiltes Alter (Erwartungswert 25, Stand.abw. 2) mit Nachkommastellen 
Alter <- signif(Alter.Normalverteilt, digits = 2) #Rundung des ALters

## Studienfach simulieren

Faecher <- c("Statistik", "Data Science", "Mathe", "Informatik") # Vektor der STudienfaecher erzeugt
Studienfach <- sample(Faecher, size = 100, replace = TRUE, prob = c(0.3,0.3,0.1,0.2))
## Studienfaecher zufaellig ausgewahlt aus dem Vektor, wobei Statistik & Data Science Wkeit von 0.3 haben,
## Mathe von 0.1 und Informatik von 0.2

## Interesse an Mathematik simulieren

Interesse_Mathe0 <- sample(1:7, size = 100, replace = TRUE) ## zunaechst Vektor aus diskreter Gleichvert. erzeugen
## dabei noch kein Zusammenhang mit Studienfach
Interesse_Mathe_Zushang <- function(x){ ## Funktion, um das Interesse an Mathematik mit dem Studienfach in 
  ## Zusammenhang zu bringen
  ifelse(Studienfach == "Mathe", x + 2, x) # bei Studienfach Mathe
  ## 2 Punkte addieren
  ifelse(Studienfach == "Statistik"| 
           Studienfach == "Data Science", x + 1, x)
  ## bei Studienfach Statistik oder Data Science 1 Punkt addieren, sonst gleich lassen
}
Interesse_Mathe1 <- Interesse_Mathe_Zushang(Interesse_Mathe0) # Funktion aufrufen, um Zusammenhang
## zu Studienfach zu erzeugen
abrunden <- function(x){## Funktion zum Abrunden: falls Addition zu Punktzahl hoeher als 7 
  ##gefuehrt hat, Wert fuer Interesse auf 7 reduzieren
  ifelse(x > 7, x == 7, x)
}
Interesse_Mathe <- abrunden(Interesse_Mathe1) ## abrunden durchfuehren

any(Interesse_Mathe > 7) ## Kontrolle, ob Rundung korrekt durchgefuehrt wurde

## ID für ID-Spalte anlegen
ID <- c(1:100)

## Interesse an Programmieren 

Interesse_Programmieren0 <- sample(1:7, size = 100, replace = TRUE) ## zunaechst Vektor aus diskreter 
## Gleichverteilung ziehen, noch ohne Zusammenhang mit Studienfach
Interesse_Programmieren_Zushang <- function(x){ # Funktion, um Interessenvektor in Abhaengigkeit vom
  ## Studienfach zu vervielfaeltigen
  ifelse(Studienfach == "Informatik", x*1.5, x)
  ifelse(Studienfach == "Data Science", x*1.2, x)
}

Interesse_Programmieren1 <- Interesse_Programmieren_Zushang(Interesse_Programmieren0) # Funktion anwenden
Interesse_Programmieren2 <- signif(Interesse_Programmieren1, digits = 1) # Vektor auf 1 Ziffer runden
Interesse_Programmieren <- abrunden(Interesse_Programmieren2) # Eintraege groesser als 7 auf 7 abrunden

any(Interesse_Programmieren > 7) ## Kontrolle, ob Funktion korrekt die Eintraege auf 7 begrenzt hat