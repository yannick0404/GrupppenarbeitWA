### Simulation des Datensatzes

# Spalte Alter
  set.seed(1)
  alter <- rnorm(100, mean = 25, sd = 2)
  daten <- data.frame(alter)
  
  # Alter auf zwei Nachkommastellen runden
  daten$alter <- round(daten$alter, digits = 1)
  
# Spalte Studienfach
  studienfaecher <- as.factor( c("Statistik", "Data Science", "Mathe",
                               "Informatik") )
  set.seed(12)
  studienfach <- sample(studienfaecher, size = 100, replace = TRUE,
                      prob = c(6/17, 6/17, 4/17, 1/17))
  daten$studienfach <- studienfach

# Spalte Interesse an Mathematik
  interesse <- ordered(1:7)
  sim_interessem <- function(i){
    if(daten$studienfach[i] == "Mathe" ){
      return(sample(interesse, size = 1, 
                  prob = c(1/30, 1/30, 2/30, 5/30, 5/30, 9/30, 7/30)))
    }
    if(daten$studienfach[i] == "Statistik" ){
      return(sample(interesse, size = 1,
                    prob = c(1/30, 2/30, 4/30, 6/30, 7/30, 6/30, 4/30)))
    }
    if(daten$studienfach[i] == "Data Science" ){
      return(sample(interesse, size = 1,
                    prob = c(1/30, 3/30, 4/30, 7/30, 8/30, 5/30, 2/30)))
    }
    if(daten$studienfach[i] == "Informatik" ){
      return(sample(interesse, size = 1,
                    prob = c(2/30, 5/30, 6/30, 7/30, 6/30, 3/30, 1/30)))
    }
  }
  
  set.seed(13)
  daten$interesse_mathe <- sapply( 1:100, sim_interessem )

# Spalte Interesse an Informatik
  sim_interessei <- function(i){
    if(daten$studienfach[i] == "Mathe" ){
      return(sample(interesse, size = 1, 
                    prob = c(3/30, 4/30, 6/30, 7/30, 5/30, 3/30, 2/30)))
    }
    if(daten$studienfach[i] == "Statistik" ){
      return(sample(interesse, size = 1,
                    prob = c(1/30, 2/30, 5/30, 6/30, 7/30, 6/30, 3/30)))
    }
    if(daten$studienfach[i] == "Data Science" ){
      return(sample(interesse, size = 1,
                    prob = c(1/30, 2/30, 4/30, 6/30, 8/30, 6/30, 3/30)))
    }
    if(daten$studienfach[i] == "Informatik" ){
      return(sample(interesse, size = 1,
                    prob = c(1/30, 1/30, 1/30, 4/30, 9/30, 8/30, 6/30)))
    }
  }
  
  set.seed(16)
  daten$interesse_info <- sapply( 1:100, sim_interessei )
  
# Spalte Mathe-LK (ja/nein)
  jaNein <- c("ja","nein")
  
  sim_MatheLK <- function(i){
      return(sample(jaNein, size = 1, 
                    prob = c( as.numeric( daten$interesse_mathe[i] )/10, (10 - as.numeric( daten$interesse_mathe[i] ))/10)))
  }
  
  set.seed(16)
  daten$matheLK <- sapply( 1:100, sim_MatheLK )
  
# id
  daten <- cbind(id = 1:100, daten)

# CSV Datei erstellen
  write.csv2(daten, "datensatzWA.csv", row.names = F)
  