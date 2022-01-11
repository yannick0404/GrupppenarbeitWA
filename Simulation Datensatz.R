### Simulation des Datensatzes
  
  # Spalte mit ids
    id <- 1:100
    daten <- data.frame(id)
  
  # Spalte Alter
    set.seed(2022)
    alter <- rnorm(100, mean = 25, sd = 2)
    daten <- cbind(daten, alter)
  
  # Spalte Studienfach
    studienfaecher <- c("Statistik", "Data Science", "Mathe", "Informatik")
    set.seed(12)
    studienfach <- sample(studienfaecher, size = 100, replace = TRUE, prob = c(6/17, 6/17, 4/17, 1/17))
    daten <- cbind(daten, as.factor(studienfach) )

    
