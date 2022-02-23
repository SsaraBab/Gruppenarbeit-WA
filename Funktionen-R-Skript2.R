## Datei fuer die Hilfsfunktionen/interne Funktionen
## hier mindestens eine Funktion einfuegen


# Funktion für den Modus:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Beispiele
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
getmode(v)

charv <- c("o","it","the","it","it")
getmode(charv)
