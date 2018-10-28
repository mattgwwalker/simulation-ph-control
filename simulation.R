library(dequer)

# Christchurch water as at 2018-10-28 as per https://www.ccc.govt.nz/services/water-and-drainage/water-supply/quality-and-monitoring/water-quality-and-monitoring/
christchurchWater <- c(water=1000, hcl=0, naoh=0, nacl=0, h2co3=1, hco3=52) # (litres, grams, grams, grams)

# Modified as per commissioning experience
christchurchWater <- c(water=1000, hcl=0, naoh=0, nacl=0, h2co3=0.5/10, hco3=52/10) # (litres, grams, grams, grams)

tankContents <- c(water=10000, hcl=0, naoh=0, nacl=0) # (litres, grams, grams, grams)
bleedRate <- 1.5 # litres / second
recirculationRate = 124000/60/60 # litres / second 

# Simulates the recirculation pump; returns the tank contents after pumping as well 
# as the pumping contents
recirc <- function(tankContents, recircRate, timeDelta) {
  recircTotalVolume <- recirculationRate * timeDelta
  recircContents <- recircTotalVolume/tankContents["water"] * tankContents
  tankContents <- tankContents - recircContents
  return(list(tankContents=tankContents, recircContents=recircContents))
}


# Returns the liquid contents after bleeding
bleed <- function(liquidContents, recircRate, timeDelta) {
  bleedTotalVolume <- bleedRate * timeDelta
  water <- liquidContents["water"]
  if (water == 0) return(liquidContents)
  bleedVolumes <- bleedTotalVolume/water * liquidContents 
  liquidContents <- liquidContents - bleedVolumes
  return(liquidContents)
}


# Returns the recirculation contents to the tank
returnRecirc <- function(tankContents, recircContents) {
  stopifnot(length(tankContents) == length(recircContents))
  tankContents <- tankContents + recircContents
  return(tankContents)
}


# Simulate the delay of liquid as it does down the pipe
delay <- function(liquidContents) {
  q <- attr(delay, "queue")
  pushback(q, liquidContents)
  pop(q)
}

init_delay <- function(delayTime, timeDelta) {
  q <- queue()
  
  numPlaces <- delayTime / timeDelta
  if (numPlaces < 1) numPlaces <- 1

  emptyContents <- c(water=0, hcl=0, naoh=0, nacl=0) # (litres, grams, grams, grams)  
  for (i in (1:numPlaces)) {
    pushback(q, emptyContents)
  }
  
  attr(delay, "queue") <<- q
}

test_delay <- function() {
  init_delay(2,1)
  tankContents <- c(water=10000, hcl=0, naoh=0, nacl=0) # (litres, grams, grams, grams)
  emptyContents <- c(water=0, hcl=0, naoh=0, nacl=0) # (litres, grams, grams, grams)  
  
  result <- delay(tankContents)
  stopifnot(result == emptyContents)  
  result <- delay(tankContents)
  stopifnot(result == emptyContents)  
  result <- delay(tankContents)
  stopifnot(result == tankContents)  
}


# Simulates make-up water
makeupWaterRate <- 6000 / 60 / 60 # litres / second
makeupWaterHigh <- 10000 # litres
makeupWaterLow <- 8000   # litres
makeupWater <- function(tankContents, makeupWaterRate, onVolume, offVolume, timeDelta) {
  # Turn make up water on if necessary
  if (tankContents["water"] <= onVolume) {
    # Turn makeup water on
    attr(makeupWater, "state") <<- TRUE
  }
  
  # Turn make up water off if necessary
  if (tankContents["water"] >= offVolume) {
    # Turn makeup water off
    attr(makeupWater, "state") <<- FALSE
  }
  
  # Get current value of makeupWaterOn
  makeupWaterOn <- attr(makeupWater, "state")
  if (is.null(makeupWaterOn)) makeupWaterOn <- FALSE
  
  if (makeupWaterOn) {
    makeupWaterVolume <- makeupWaterRate * timeDelta
    tankContents["water"] <- tankContents["water"] + makeupWaterVolume
  }
    
  return(tankContents)
}

init_makeupWater <- function() {
  attr(makeupWater, "state") <<- FALSE
}

test_makeupWater <- function() {
  init_makeupWater()
  tankContents <- c(water=0, hcl=0, naoh=0) # (litres, grams, grams)
  result <- makeupWater(tankContents, 
                        makeupWaterRate = 1, 
                        onVolume = 0,
                        offVolume = 1,
                        timeDelta = 1)
  stopifnot(result["water"] == 1)
  stopifnot(result["hcl"] == 0)
  stopifnot(result["naoh"] == 0)
  
  # Make up water should now be off
  result <- makeupWater(result, 
                        makeupWaterRate = 1, 
                        onVolume = 0,
                        offVolume = 1,
                        timeDelta = 1)
  stopifnot(result["water"] == 1)
  stopifnot(result["hcl"] == 0)
  stopifnot(result["naoh"] == 0)  
  
  # Make up water should stay off
  result <- makeupWater(result, 
                        makeupWaterRate = 1, 
                        onVolume = 0,
                        offVolume = 1,
                        timeDelta = 1)
  stopifnot(result["water"] == 1)
  stopifnot(result["hcl"] == 0)
  stopifnot(result["naoh"] == 0)  
}

# Simulates the measurement of pH
measurePh <- function(liquidContents) {
  hcl <- unname(liquidContents["hcl"])
  naoh <- unname(liquidContents["naoh"])
  h2co3 <- unname(liquidContents["h2co3"])  # weak acid
  hco3 <- unname(liquidContents["hco3"])    # conjugate base
  water <- unname(liquidContents["water"])
  
  if (is.na(hcl)) hcl <- 0
  if (is.na(naoh)) naoh <- 0
  if (is.na(h2co3)) h2co3 <- 0
  if (is.na(hco3)) hco3 <- 0
  if (water<=0) {
    return(7)
  }

  gramsPerMolHcl <- 36.46
  gramsPerMolNaoh <- 39.99
  gramsPerMolH2co3 <- 62.02
  gramsPerMolHco3 <- 61.02
  
  molHcl <- hcl / gramsPerMolHcl
  molNaoh <- naoh / gramsPerMolNaoh
  molH2co3 <- h2co3 / gramsPerMolH2co3
  molHco3 <- hco3 / gramsPerMolHco3  
  
  # Check that neuralisation has already occurred
  stopifnot(hcl==0 || naoh==0)
  


  
  
  
    

  
  
  

  if (h2co3!=0 && hco3!=0) {
    # The buffer isn't broken
    pKa <- 6.1 # for H2CO3
    ka <- 10^(-pKa)
    
    # Equation 3 from "The Henderson–Hasselbalch Equation: Its History and Limitations" see https://pdfs.semanticscholar.org/5275/451edb70a6a86c232cf7605cc251277a829b.pdf
    f <- function(ph, ka, molOfAcid, molOfSalt, volume) {
      h <- 10^(-ph)
      ma <- molOfAcid / volume
      mb <- molOfSalt / volume
      oh <- 10^-14 / h
      
      numerator <- ma - h + oh
      denominator <- mb + h - oh
      
      ka * numerator / denominator - h
    }
    
    f2 <- function(h) {
      log(abs(f(h, ka, molH2co3, molHco3, water)))
    }
    
    #ph1 <- optimize(f2, lower=0, upper=14)$minimum
    #ph2 <- optim(0, f2, lower=0, upper=14, method="Brent")$par
    #ph3 <- optim(0, f2, lower=0, upper=14, method="SANN")$par
    #ph4 <- optim(0, f2, method="CG")$par

    multisearch_optimize <- function(f, lower, upper, divisions=10) {
      # Calulate ranges
      stepSize = (upper - lower)/divisions
      lowers <- seq(lower, upper-stepSize, stepSize)
      uppers <- seq(lower+stepSize, upper, stepSize)
      
      minSoFar = NA
      result = NA
      for (i in 1:divisions) {
        thisResult <- optimize(f, lower=lowers[i], upper=uppers[i])
        thisMin <- thisResult$objective
        if (is.na(minSoFar) || thisMin < minSoFar) {
          minSoFar <- thisMin
          result <- thisResult
        }
      }
      
      return(result)
    }
    
    ph <- multisearch_optimize(f2, 0, 14, 14)$minimum    
    
        
    if(ph>13) {
      cat(ph)
    }
    
    return(ph)
    
  }
  
  # Test - quadratic solution
  c1 <- molH2co3 / water # weak acid concentration
  c2 <- molHcl / water # strong acid concentration
  pKa <- 6.1 # for H2CO3
  ka <- 10^(-pKa)
  ph <- -log10((c2+sqrt(c2^2 + 4*ka*c1))/2)
  return(ph)
  
  
  
  
  # The bufer's broken; just ignore the weak acid and its conjugate base
  if (hcl > 0) {
    # pH calculation if there is HCl
    molHclPerLitre <- molHcl / water
    ph <- -log10(molHclPerLitre)
    if (ph>7) ph <- 7 # The amount of HCl is so small this equation is no longer correct
  } else if (naoh > 0) {
    # pH calculation if there is NaOH
    molNaohPerLitre <- molNaoh / water
    poh <- -log10(molNaohPerLitre)
    ph <- 14 - poh
    if (ph<7) ph <- 7 # The amount of NaOH is so small this equation is no longer correct
  } else {
    # pH if there is no HCl nor NaOH
    ph <- 7
  }

  stopifnot(ph > 0 && ph < 14)
  
  ph
}

linearisePh <- function(ph) {
  10^(8-ph)
}


test_measurePh <- function() {
  tankContents <- c(water=10000, hcl=1000, naoh=0) # (litres, grams, grams)
  result <- measurePh(tankContents)
  stopifnot(abs(result - 2.56) < 0.01)
  
  tankContents <- c(water=10000, hcl=1, naoh=0, nacl=0, h2co3=0, hco3=0) # (litres, grams, grams)
  result <- measurePh(tankContents)
  stopifnot(abs(result - 5.56) < 0.01)
  
  tankContents <- c(water=1000, hcl=0, naoh=0, nacl=0, h2co3=0.5, hco3=52) # (litres, grams, grams)
  result <- measurePh(tankContents)
  #stopifnot(abs(result - 8.12) < 0.01) # As per the Henderson–Hasselbalch Equation
  stopifnot(abs(result - 8.07) < 0.01) # As per the Equation 3 of "The Henderson–Hasselbalch Equation: Its History and Limitations"
  
}


# Simulate a slower-to-respond pH probe
measurePhWithRollingAverage <- function(liquidContents, noise=0.1) {
  previousValues <- attr(measurePhWithRollingAverage, "queue")
  count <- length(previousValues)
  desiredSamples <- attr(measurePhWithRollingAverage, "desiredSamples")
  total <- attr(measurePhWithRollingAverage, "total")
  
  measuredPh <- measurePh(liquidContents)
  if (is.na(measuredPh)) return(NA)
  
  # Add noise
  measuredPh <- rnorm(1, mean=measuredPh, sd=abs(noise*measuredPh))
  
  pushback(previousValues, measuredPh)

  total <- total + measuredPh
  count <- count + 1
  
  if (count > desiredSamples) {
    total <- total - pop(previousValues)
    count <- count - 1
  }
  

  attr(measurePhWithRollingAverage, "total") <<- total

  stopifnot(count>0)  
  result <- total / count
  stopifnot(result >= 0 && result <= 14)
  result
}

init_measurePhWithRollingAverage <- function(duration, timeDelta) {
  desiredSamples <- duration / timeDelta
  if (desiredSamples < 1) desiredSamples <- 1
  attr(measurePhWithRollingAverage, "queue") <<- queue()
  attr(measurePhWithRollingAverage, "desiredSamples") <<- desiredSamples
  attr(measurePhWithRollingAverage, "total") <<- 0
}


test_measurePhWithRollingAverage <- function() {
  init_measurePhWithRollingAverage(1, 0.1)
  phs <- NULL
  tankContents <- c(water=10000, hcl=100, naoh=0, nacl=0) # (litres, grams, grams, grams)
  phs <- c(phs, measurePhWithRollingAverage(tankContents))
  
  tankContents <- c(water=10000, hcl=10, naoh=0, nacl=0) # (litres, grams, grams, grams)
  phs <- c(phs, measurePhWithRollingAverage(tankContents))
  
  tankContents <- c(water=10000, hcl=10, naoh=0, nacl=0) # (litres, grams, grams, grams)
  phs <- c(phs, measurePhWithRollingAverage(tankContents))
  
  # errors:
  tankContents <- c(water=10000, hcl=10, naoh=0, nacl=0) # (litres, grams, grams, grams)
  phs <- c(phs, measurePhWithRollingAverage(tankContents))
  
  tankContents <- c(water=10000, hcl=10, naoh=0, nacl=0) # (litres, grams, grams, grams)
  phs <- c(phs, measurePhWithRollingAverage(tankContents))
  
  tankContents <- c(water=10000, hcl=10, naoh=0, nacl=0) # (litres, grams, grams, grams)
  phs <- c(phs, measurePhWithRollingAverage(tankContents))
  
  phs
}



# Instantanously adds an acid dose
instantaneousAcidDose <- function(tankContents, doseAmount) {
  tankContents["hcl"] <- tankContents["hcl"] + doseAmount
  return(tankContents)
}

test_instantaneousAcidDose <- function() {
  tankContents <- c(water=10000, hcl=0, naoh=0) # (litres, grams, grams)
  result <- instantaneousAcidDose(tankContents, 1000)
  stopifnot(result["hcl"] == 1000)
}


# Pour acid over time
doseAcidAtFixedRate <- function(tankContents, doseAmount, doseDuration, timeDelta) {
  dosedSoFar <- attr(doseAcidAtFixedRate, "dosedSoFar")
  if (is.null(dosedSoFar)) {
    dosedSoFar <- 0
  }
  
  thisDoseAmount <- doseAmount / doseDuration * timeDelta

  if (dosedSoFar < doseAmount) {
    tankContents["hcl"] <- tankContents["hcl"] + thisDoseAmount
    dosedSoFar <- dosedSoFar + thisDoseAmount
    attr(doseAcidAtFixedRate, "dosedSoFar") <<- dosedSoFar
  }  

  return(tankContents)
}

# Initialise static local variable
init_doseAcidAtFixedRate <- function() {
  attr(doseAcidAtFixedRate, "dosedSoFar") <<- 0
}

test_doseAcidAtFixedRate <- function() {
  init_doseAcidAtFixedRate()
  tankContents <- c(water=10000, hcl=0, naoh=0) # (litres, grams, grams)
  result <- doseAcidAtFixedRate(tankContents, 1000, 5*60, timeDelta=60)
  stopifnot(result["hcl"] == 200)
  result <- doseAcidAtFixedRate(result, 1000, 5*60, timeDelta=60)
  stopifnot(result["hcl"] == 400)
}


doseCausticAtFixedRate <- function(liquidContents, doseRate, timeDelta) {
  liquidContents["naoh"] <- liquidContents["naoh"] + doseRate * timeDelta
  return(liquidContents)
}

maxPumpRate <- 60/60/60 # litres/second
pumpCausticSolution <- function(liquidContents, pumpPercentage, maxPumpRate, timeDelta) {
  pumpRate <- maxPumpRate * pumpPercentage / 100 # litres / second
  pumpVolume <- pumpRate * timeDelta # litres
  densityOfCausticSolution <- 1220 # grams / litre
  pumpWeight <- pumpVolume * densityOfCausticSolution # grams
  proportionOfCausticByWeight <- 0.20 
  causticDose <- pumpWeight * proportionOfCausticByWeight # grams
  liquidContents["naoh"] <- liquidContents["naoh"] + causticDose
  return(liquidContents)
}

pumpCausticSolutionWithRollingAverage <- function(liquidContents, pumpPercentage, maxPumpRate, timeDelta) {
  previousValues <- attr(pumpCausticSolutionWithRollingAverage, "queue")
  count <- length(previousValues)
  desiredCommands <- attr(pumpCausticSolutionWithRollingAverage, "desiredCommands")
  total <- attr(pumpCausticSolutionWithRollingAverage, "total")
  
  pushback(previousValues, pumpPercentage)
  
  total <- total + pumpPercentage
  count <- count + 1
  
  if (count > desiredCommands) {
    total <- total - pop(previousValues)
    count <- count - 1
  }
  
  attr(pumpCausticSolutionWithRollingAverage, "total") <<- total
  
  stopifnot(count!=0)
  result <- total / count
  
  # Add noise
  result <- rnorm(1, mean=result, sd=abs(0.05*result))
  if (result > 100) result <- 100
  if (result < 0) result <- 0
  
  pumpCausticSolution(liquidContents, result, maxPumpRate, timeDelta)
}  

init_pumpCausticSolutionWithRollingAverage <- function(duration, timeDelta) {
  desiredCommands <- duration / timeDelta
  if (desiredCommands < 1) desiredCommands <- 1
  attr(pumpCausticSolutionWithRollingAverage, "queue") <<- queue()
  attr(pumpCausticSolutionWithRollingAverage, "desiredCommands") <<- desiredCommands
  attr(pumpCausticSolutionWithRollingAverage, "total") <<- 0
}



chemicalReaction <- function(liquidContents) {
  # Neutralise the HCl and NaOH, produce NaCl
  
  hcl <- unname(liquidContents["hcl"])
  naoh <- unname(liquidContents["naoh"])
  nacl <- unname(liquidContents["nacl"])
  h2co3 <- unname(liquidContents["h2co3"])  # weak acid
  hco3 <- unname(liquidContents["hco3"])    # conjugate base
  
  if (is.na(hcl)) hcl <- 0
  if (is.na(naoh)) naoh <- 0
  if (is.na(nacl)) nacl <- 0
  if (is.na(h2co3)) h2co3 <- 0
  if (is.na(hco3)) hco3 <- 0
  
  gramsPerMolHcl <- 36.46
  gramsPerMolNaoh <- 39.99
  gramsPerMolNacl <- 58.44
  gramsPerMolH2co3 <- 62.02
  gramsPerMolHco3 <- 61.02
  
  molHcl <- hcl / gramsPerMolHcl
  molNaoh <- naoh / gramsPerMolNaoh
  molH2co3 <- h2co3 / gramsPerMolH2co3
  molHco3 <- hco3 / gramsPerMolHco3
  
  # The HCl and NaOH neutralise each other
  if (molHcl > molNaoh) {
    molNacl <- molNaoh
    molHcl <- molHcl - molNaoh
    molNaoh <- 0
  } else {
    molNacl <- molHcl
    molNaoh <- molNaoh - molHcl
    molHcl <- 0
  }
  
  # Remaining HCl or NaOH will have (first) reacted with the buffer
  if (molHcl > 0) {
    # HCl will react with conjugate base to form weak acid
    # HCl + HCO3- -> H2CO3 + Cl-
    if (molHco3 > molHcl) {
      # There's more buffer than strong acid
      molH2co3 <- molH2co3 + molHcl
      molHco3 <- molHco3 - molHcl
      molHcl <- 0
    } else {
      # There is more strong acid than buffer
      molH2co3 <- molH2co3 + molHco3
      molHcl <- molHcl - molHco3
      molHco3 <- 0
    }
  } else {
    # NaOH will react with weak acid to form conjugate base
    # NaOH + H2CO3 -> HCO3- + Na+ + H2O
    if (molH2co3 > molNaoh) {
      # There is more buffer than strong base
      molHco3 <- molHco3 + molNaoh
      molH2co3 <- molH2co3 - molNaoh
      molNaoh <- 0
    } else {
      # There is more strong base than buffer
      molHco3 <- molHco3 + molH2co3
      molNaoh <- molNaoh - molH2co3
      molH2co3 <- 0
    }
  }
  
  # Convert back to grams
  hcl <- molHcl * gramsPerMolHcl
  naoh <- molNaoh * gramsPerMolNaoh
  nacl <- nacl + molNacl * gramsPerMolNacl
  h2co3 <- molH2co3 * gramsPerMolH2co3
  hco3 <- molHco3 * gramsPerMolHco3
  
  liquidContents["hcl"] <- hcl
  liquidContents["naoh"] <- naoh
  liquidContents["nacl"] <- nacl
  liquidContents["h2co3"] <- h2co3
  liquidContents["hco3"] <- hco3
  
  return(liquidContents)
}

test_chemicalReaction <- function() {
  # Test that NaOH and HCl neutralise
  start <- c(water=10000, hcl=1000, naoh=1000) # (litres, grams, grams)
  result <- chemicalReaction(start)
  stopifnot( abs(result["hcl"] - 88.2) < 0.1 )
  stopifnot( abs(result["naoh"] - 0) < 0.1 )
  stopifnot( abs(result["nacl"] - 1461.4) < 0.1 )
  stopifnot( abs(result["h2co3"] - 0) < 0.1 )
  stopifnot( abs(result["hco3"] - 0) < 0.1 )

  # Test HCl addition where buffer isn't broken
  start <- c(water=10000, hcl=1, naoh=0, nacl=0, h2co3=100, hco3=100) # (litres, grams, grams)
  result <- chemicalReaction(start)
  stopifnot( abs(result["hcl"] - 0) < 0.1 )
  stopifnot( abs(result["naoh"] - 0) < 0.1 )
  stopifnot( abs(result["nacl"] - 0) < 0.1 )
  stopifnot( abs(result["h2co3"] - 101.7) < 0.1 )
  stopifnot( abs(result["hco3"] - 98.3) < 0.1 )

  # Test HCl addition where buffer breaks
  start <- c(water=10000, hcl=100, naoh=0, nacl=0, h2co3=10, hco3=10) # (litres, grams, grams)
  result <- chemicalReaction(start)
  stopifnot( abs(result["hcl"] - 94.0) < 0.1 )
  stopifnot( abs(result["naoh"] - 0) < 0.1 )
  stopifnot( abs(result["nacl"] - 0) < 0.1 )
  stopifnot( abs(result["h2co3"] - 20.2) < 0.1 )
  stopifnot( abs(result["hco3"] - 0) < 0.1 )
  
  # Test NaOH addition where buffer isn't broken
  start <- c(water=10000, hcl=0, naoh=1, nacl=0, h2co3=100, hco3=100) # (litres, grams, grams)
  result <- chemicalReaction(start)
  return(result)
  stopifnot( abs(result["hcl"] - 0) < 0.1 )
  stopifnot( abs(result["naoh"] - 0) < 0.1 )
  stopifnot( abs(result["nacl"] - 0) < 0.1 )
  stopifnot( abs(result["h2co3"] - 98.4) < 0.1 )
  stopifnot( abs(result["hco3"] - 101.5) < 0.1 )
  
  # Test NaOH addition where buffer breaks
  start <- c(water=10000, hcl=0, naoh=100, nacl=0, h2co3=10, hco3=10) # (litres, grams, grams)
  result <- chemicalReaction(start)
  stopifnot( abs(result["hcl"] - 0) < 0.1 )
  stopifnot( abs(result["naoh"] - 93.6) < 0.1 )
  stopifnot( abs(result["nacl"] - 0) < 0.1 )
  stopifnot( abs(result["h2co3"] - 0) < 0.1 )
  stopifnot( abs(result["hco3"] - 19.8) < 0.1 )
}

proportionalPumpControl <- function(pv, sp, p) {
  err <- sp - pv
  result <- err * p
  
  # Clamp output to 0 to 100%
  if (result > 100) result <- 100
  if (result < 0) result <- 0
  
  names(result) <- "pump.output"
  return(result)
}

test_proportionalPumpControl <- function() {
  sp <- 8
  pv <- 6
  proportionalPumpControl(pv, sp, 100)
}

# Simulates the system through a single time step
phSetpoint <- 8
update <- function(tankContents, recircRate, time, timeDelta) {
  # Add acid
  if (time>60) {
    tankContents <- doseAcidAtFixedRate(tankContents, 1000, 10*60, timeDelta)
  }
  
  # Add makup water
#  tankContents <- makeupWater(tankContents, makeupWaterRate, 
#                              makeupWaterLow, makeupWaterHigh, timeDelta=timeDelta)
  
  # Recirc the water
  recircResult <- recirc(tankContents, recircRate, timeDelta)
  tankContents <- recircResult$tankContents
  recircContents <- recircResult$recircContents
  
  # Dose caustic
#  previousPh <- attr(update, "ph.recirc")
#  if (is.null(previousPh)) previousPh <- 7
  pumpPercentage <- NA
#  pumpPercentage <- proportionalPumpControl(previousPh,
#                                            phSetpoint,
#                                            2)
  #recircContents <- pumpCausticSolution(recircContents, pumpPercentage, maxPumpRate, timeDelta)
#  recircContents <- pumpCausticSolutionWithRollingAverage(recircContents, pumpPercentage, maxPumpRate, timeDelta)
  
  # Delay the contents of the pipe
  recircContents <- delay(recircContents)
  
  # Measure the pH in the recirc line
  recircContents <- chemicalReaction(recircContents)
  phRecirc <- measurePhWithRollingAverage(recircContents)
  linearPhRecirc <- linearisePh(phRecirc)
  names(phRecirc) <- "ph.recirc"
  names(linearPhRecirc) <- "linear_ph.recirc"
  attr(update, "ph.recirc") <<- phRecirc
  
  # Bleed some of the recirc water
#  recircContents <- bleed(recircContents, recircRate, timeDelta)

  # Return the remaining recirc water to the tank
  tankContents <- returnRecirc(tankContents, recircContents)
  
  # Measure the pH in the tank
  tankContents <- chemicalReaction(tankContents)
  #phTank <- measurePhWithRollingAverage(tankContents, noise=0)
  phTank <- measurePh(tankContents)
  names(phTank) <- "ph.tank"

  return(list(tankContents=tankContents, data=c(phRecirc, linearPhRecirc, phTank, pumpPercentage)))
}

init_update <- function() {
  attr(update, "ph.recirc") <<- 7
}

test_update <- function() {
  init_update()
  tankContents <- c(water=10000, hcl=1000, naoh=0) # (litres, grams, grams)
  update(tankContents, recirculationRate, 1, timeDelta=1)
  # No test defined
}

# Simulates the system through multiple time steps
run <- function(tankContents, recircRate, duration, timeDelta=0.2) {
  init_update()
  init_delay(1, timeDelta)
  init_makeupWater()
  init_doseAcidAtFixedRate()
  init_measurePhWithRollingAverage(100, timeDelta)
  init_pumpCausticSolutionWithRollingAverage(2, timeDelta)
  time <- 0
  results <- NULL
  
  while (time < duration) {
    result <- update(tankContents, recircRate, time, timeDelta)
    tankContents <- result$tankContents
    time <- time + timeDelta
    results <- rbind(results, c(time=time, result$tankContents, result$data))
  }
  
  return(data.frame(results))
}

results <- run(christchurchWater*10, recirculationRate, duration=15*60, timeDelta=0.1)

plot(results$time/60, results$water, type="l",
     main="Volume in Tank", xlab="Time (mins)", ylab="Volume (litres)")

plot(results$time/60, results$ph.tank, type="l",
     main="pH in Tank", xlab="Time (mins)", ylab="pH",
     ylim=c(0,14))
abline(h=phSetpoint, col="grey")
abline(h=3.5, col="grey")

plot(results$time/60, results$ph.recirc, type="l",
     main="pH in Recirc Line (black)\nand Pump Speed (red)", xlab="Time (mins)", ylab="pH",
     ylim=c(0,14), xlim=c(30,60))
abline(h=phSetpoint, col="grey")
par(new = T)
plot(results$time/60, results$pump.output, type="l", col="red",
     ylim=c(0,100), axes=F, xlab=NA, ylab=NA,
     xlim=c(30,60))
abline(h=10, col="grey")

plot(results$time/60, results$nacl/(results$water*1000)*100, type="l",
     main="Proportion of Salt", xlab="Time (mins)", ylab="Salt percentage by weight (%)")


#plot(results$time/60, results$linear_ph.recirc, type="l")

exit()


# Testing equation 3 from "The Henderson–Hasselbalch Equation: Its History and Limitations" see https://pdfs.semanticscholar.org/5275/451edb70a6a86c232cf7605cc251277a829b.pdf

f <- function(ph, ka, molOfAcid, molOfSalt, volume) {
  h <- 10^(-ph)
  ma <- molOfAcid / volume
  mb <- molOfSalt / volume
  oh <- 10^-14 / h
  
  numerator <- ma - h + oh
  denominator <- mb + h - oh
  
  ka * numerator / denominator - h
}

ka <- 1.8 * 10^-5
molOfAcid <- 0.45
molOfSalt <- 0.94
volume <- 1

ka <- 1.8 * 10^-5
molOfAcid <- 0.009
molOfSalt <- 0.13
volume <- 1


ka <- 10^(-7)
molOfAcid <- 0.0009
molOfSalt <- 0.0001
volume <- 0.1


ka <- 7.94 * 10^-7
molOfAcid <- 0.42
molOfSalt <- 0.436
volume <- 9969

ka <- 7.94 * 10^-7
molOfAcid <- 0.8562
molOfSalt <- 0.001318
volume <- 9969


f2 <- function(h) {
  log(abs(f(h, ka, molOfAcid, molOfSalt, volume)))
}

#optimize(f2, lower=0, upper=14)
#optim(7, f2, lower=0, upper=14, method="Brent")$par
#library(pracma)
#steep_descent(0, f2)

x <- seq(0,14, length=1000)
plot(x, f2(x))


multisearch_optimize <- function(f, lower, upper, divisions=10) {
  # Calulate ranges
  stepSize = (upper - lower)/divisions
  lowers <- seq(lower, upper-stepSize, stepSize)
  uppers <- seq(lower+stepSize, upper, stepSize)
  
  minSoFar = NA
  result = NA
  for (i in 1:divisions) {
    thisResult <- optimize(f, lower=lowers[i], upper=uppers[i])
    thisMin <- thisResult$objective
    if (is.na(minSoFar) || thisMin < minSoFar) {
      minSoFar <- thisMin
      result <- thisResult
    }
  }
  
  return(result)
}

multisearch_optimize(f2, 0, 14, 14)

