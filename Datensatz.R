## Part 1 der Aufgabe


## Alter simulieren
Alter.Normalverteilt <- rnorm(n=100,mean=25,sd=2) #Normalverteiltes Alter (Erwartungswert 25, Stand.abw. 2) mit Nachkommastellen 
Alter <- signif(Alter.Normalverteilt, digits = 2) #Rundung des ALters

## Studienfach simulieren

Faecher <- c("Statistik", "Data Science", "Mathe", "Informatik") # Vektor der STudienfaecher erzeugt
Studienfach <- sample(Faecher, size = 100, replace = TRUE, prob = c(0.3,0.3,0.1,0.2))
## Studienfaecher zufaellig ausgewahlt aus dem Vektor, wobei Statistik & Data Science Wkeit von 0.3 haben,
## Mathe von 0.1 und Informatik von 0.2
