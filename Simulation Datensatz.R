### Simulation des Datensatzes

  # Spalte mit ids
  id <- 1:100
  daten <- data.frame(id)
  
  # Spalte Alter
  set.seed(1)
  alter <- rnorm(100, mean = 25, sd = 2)
  daten$alter <- alter
  
  # Spalte Studienfach
  studienfaecher <- as.factor( c("Statistik", "Data Science", "Mathe",
                                 "Informatik") )
  set.seed(12)
  studienfach <- sample(studienfaecher, size = 100, replace = TRUE,
                        prob = c(6/17, 6/17, 4/17, 1/17))
  daten$studienfach <- studienfach
  
  # Spalte Interesse an Mathematik
  interesse <- ordered(1:7)
  sim_interesse <- function(i){
    if(daten$studienfach[i] == "Mathe" ){
      # Poisson Verteilt simulieren
      return(sample(interesse, size = 1, prob = c(ppois(1,6), dpois(2,6),
                                                  dpois(3,6), dpois(4,6),
                                                  dpois(5,6), dpois(6,6), 
                                                  dpois(7,6)+ 1-ppois(7,6)), 
                      replace = TRUE))
      }
    if(daten$studienfach[i] == "Statistik" ){
      return(sample(interesse, size = 1,
                    prob = c(1/30, 2/30, 4/30, 6/30, 7/30, 6/30, 4/30), 
                    replace = TRUE))
    }
    if(daten$studienfach[i] == "Data Science" ){
      return(sample(interesse, size = 1,
                    prob = c(1/30, 3/30, 4/30, 7/30, 8/30, 5/30, 2/30), 
                    replace = TRUE))
    }
    if(daten$studienfach[i] == "Informatik" ){
      return(sample(interesse, size = 1,
                    prob = c(2/30, 5/30, 6/30, 7/30, 6/30, 3/30, 1/30), 
                    replace = TRUE))
    }
  }
  
  set.seed(13)
  daten$interesse_mathe <- sapply( 1:100, sim_interesse )
  

    
  