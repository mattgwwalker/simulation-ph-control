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
bleed <- function(liquidContents, bleedRate, timeDelta) {
  bleedTotalVolume <- bleedRate * timeDelta
  water <- liquidContents["water"]
  if (water == 0) return(liquidContents)
  bleedVolumes <- bleedTotalVolume/water * liquidContents 
  liquidContents <- liquidContents - bleedVolumes
  return(liquidContents)
}


conditionalBleed <- function(liquidContents, bleedRate, ph, timeDelta) {
  minAcceptablePh <- attr(conditionalBleed, "minAcceptablePh")
  maxAcceptablePh <- attr(conditionalBleed, "maxAcceptablePh")
  waitTime <- attr(conditionalBleed, "waitTime")
  thisWait <- attr(conditionalBleed, "thisWait")
  
  if (is.na(ph)) {
    # pH isn't known, don't bleed
    return(liquidContents)
  }
  if (ph>=minAcceptablePh && ph<=maxAcceptablePh){
    # pH is acceptable to bleed
    if (!is.na(thisWait)) {
      if (thisWait>0) thisWait <- thisWait - timeDelta
      if (thisWait<0) thisWait <- NA
    } else {
      # pH is acceptable and we do not have to wait
      return(bleed(liquidContents, bleedRate, timeDelta))
    }
  } else {
    # pH is not acceptable, reset timer
    thisWait <- waitTime
  }    
  
  attr(conditionalBleed, "thisWait") <<- thisWait
  return(liquidContents)
}

init_conditionalBleed <- function(minAcceptablePh, maxAcceptablePh, waitTime) {
  attr(conditionalBleed, "minAcceptablePh") <<- minAcceptablePh
  attr(conditionalBleed, "maxAcceptablePh") <<- maxAcceptablePh
  attr(conditionalBleed, "waitTime") <<- waitTime
  attr(conditionalBleed, "thisWait") <<- NA
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
    makeupWater <- attr(makeupWater, "makeupWater")
    stopifnot(length(makeupWater) == length(tankContents))
    makeupWaterVolume <- makeupWaterRate * timeDelta
    tankContents <- tankContents + makeupWaterVolume*makeupWater
  }
    
  return(tankContents)
}

init_makeupWater <- function(makeupWater) {
  attr(makeupWater, "state") <<- FALSE
  attr(makeupWater, "makeupWater") <<- makeupWater/makeupWater["water"]
}

test_makeupWater <- function() {
  distilledWater <- c(water=0, hcl=0, naoh=0)
  init_makeupWater(distilledWater)
  
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
# A site detailing a similar algorithm: http://science.widener.edu/svb/pset/acidbase.html
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
    return(NA)
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
    # The solution is a buffer
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

    return(ph)
  }
  
  if (hcl > 0) {
    # Calculate the pH of a strong acid mixed with a weak acid (see https://chemistry.stackexchange.com/questions/69307/calculation-of-the-ph-of-a-mixture-of-a-strong-acid-and-weak-acid)
    c1 <- molH2co3 / water # weak acid concentration
    c2 <- molHcl / water # strong acid concentration
    pKa <- 6.1 # for H2CO3
    ka <- 10^(-pKa)
    ph <- -log10((c2+sqrt(c2^2 + 4*ka*c1))/2)
    return(ph)
  }
  
  if (naoh > 0) {
    # Calculate the pH of a strong base mixed with a weak base (guess at formula based on above)
    c1 <- molHco3 / water # weak base concentration
    c2 <- molNaoh / water # strong base concentration
    pKb <- 7.7 # for HCO3
    kb <- 10^(-pKb)
    poh <- -log10((c2+sqrt(c2^2 + 4*kb*c1))/2)
    ph <- 14 - poh
    return(ph)
  }
  
  stop("It should be impossible to get here")
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
measurePhWithRollingAverage <- function(liquidContents, noise=0.0) {
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





measurePhWithRollingAverage2 <- function(liquidContents, noise=0.0) {
  previousValues <- attr(measurePhWithRollingAverage2, "queue")
  count <- length(previousValues)
  desiredSamples <- attr(measurePhWithRollingAverage2, "desiredSamples")
  total <- attr(measurePhWithRollingAverage2, "total")
  
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
  
  
  attr(measurePhWithRollingAverage2, "total") <<- total
  
  stopifnot(count>0)  
  result <- total / count
  stopifnot(result >= 0 && result <= 14)
  result
}

init_measurePhWithRollingAverage2 <- function(duration, timeDelta) {
  desiredSamples <- duration / timeDelta
  if (desiredSamples < 1) desiredSamples <- 1
  attr(measurePhWithRollingAverage2, "queue") <<- queue()
  attr(measurePhWithRollingAverage2, "desiredSamples") <<- desiredSamples
  attr(measurePhWithRollingAverage2, "total") <<- 0
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
  noise <- attr(pumpCausticSolutionWithRollingAverage, "noise")
  
  # Pump didn't start unless it recieved a signal above 0.25%
  if (pumpPercentage < 0.25) pumpPercentage <- 0
  
  # PCL-to-pump signal is analogue and the sender and receiver have resolutions
  # which can be simulated as a number of steps from 0% to 100%.
  steps <- 1024 * 10
  pumpPercentage <- round(pumpPercentage/100 * steps)/steps * 100
  
    
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
  result <- rnorm(1, mean=result, sd=abs(noise*result))
  
  # Clamp results between 0% and 100%
  if (result > 100) result <- 100
  if (result < 0) result <- 0
  
  pumpCausticSolution(liquidContents, result, maxPumpRate, timeDelta)
}  

init_pumpCausticSolutionWithRollingAverage <- function(duration, timeDelta, noise=0) {
  desiredCommands <- duration / timeDelta
  if (desiredCommands < 1) desiredCommands <- 1
  attr(pumpCausticSolutionWithRollingAverage, "queue") <<- queue()
  attr(pumpCausticSolutionWithRollingAverage, "desiredCommands") <<- desiredCommands
  attr(pumpCausticSolutionWithRollingAverage, "total") <<- 0
  attr(pumpCausticSolutionWithRollingAverage, "noise") <<- noise
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

proportionalPumpControl <- function(pv, sp, p, directionOption=TRUE) {
  if (directionOption) {
    err <- sp - pv
  } else{
    err <- pv - sp
  }
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

setPointRamp <- function(timeDelta) {
  duration <- attr(setPointRamp, "duration")
  rate <- attr(setPointRamp, "rate")
  current <- attr(setPointRamp, "current")
  
  if (!is.na(duration)) {
    time <- min(timeDelta, duration)
    new <- current + rate*time
    duration <- duration - time
    if (duration<=0) duration <- NA
    attr(setPointRamp, "current") <<- new
    attr(setPointRamp, "duration") <<- duration
    return(new)
  } else{
    return(current)
  }
}

command_setPointRamp <- function(current, desired, duration) {
  attr(setPointRamp, "duration") <<- duration
  attr(setPointRamp, "rate") <<- (desired-current)/duration
  attr(setPointRamp, "current") <<- current
}

test_setPointRamp <- function() {
  command_setPointRamp(0, 10, 5)
  result <- setPointRamp(1)
  stopifnot(abs(result - 2) < 0.01)

  result <- setPointRamp(1)
  stopifnot(abs(result - 4) < 0.01)
  
  command_setPointRamp(0, 10, 5)
  result <- setPointRamp(6)
  stopifnot(abs(result - 10) < 0.01)
}

# Simulates the system through a single time step
phSetpoint <- NA
previousPh <- NA
update <- function(tankContents, recircRate, time, timeDelta) {
  # Sanity check
  if (tankContents["water"] <= 0) stop("There's no water in the tank!")
  
  # Update setpoint ramp
  phSetpoint <- setPointRamp(timeDelta)

  # Add acid
  if (time>10*60) {
    tankContents <- doseAcidAtFixedRate(tankContents, 1000, 5*60, timeDelta)
  }
  
  # Add makup water
  tankContents <- makeupWater(tankContents, makeupWaterRate, 
                              makeupWaterLow, makeupWaterHigh, timeDelta=timeDelta)
  
  # Recirc the water
  recircResult <- recirc(tankContents, recircRate, timeDelta)
  tankContents <- recircResult$tankContents
  recircContents <- recircResult$recircContents
  
  # Dose caustic
  previousPh <- attr(update, "ph.recirc")
  if (is.null(previousPh) || is.na(previousPh)) pumpPercentage <- NA
  else {
    #pumpPercentage <- proportionalPumpControl(previousPh, phSetpoint, p=3)
    pumpPercentage <- proportionalPumpControl(10^(8-previousPh),
                                              10^(8-phSetpoint),
                                              p=1000, FALSE)
    #recircContents <- pumpCausticSolution(recircContents, pumpPercentage, maxPumpRate, timeDelta)
    recircContents <- pumpCausticSolutionWithRollingAverage(recircContents, pumpPercentage, maxPumpRate, timeDelta)
  }
  
  # Delay the contents of the pipe
  recircContents <- delay(recircContents)
  
  # Measure the pH in the recirc line
  recircContents <- chemicalReaction(recircContents)
  phRecirc <- measurePhWithRollingAverage(recircContents)
  phRecirc2 <- measurePhWithRollingAverage2(recircContents)
  
  linearPhRecirc <- linearisePh(phRecirc)
  names(phRecirc) <- "ph.recirc"
  names(linearPhRecirc) <- "linear_ph.recirc"
  attr(update, "ph.recirc") <<- phRecirc
  
  # Bleed some of the recirc water
  #recircContents <- bleed(recircContents, bleedRate, timeDelta)
  recircContents <- conditionalBleed(recircContents, bleedRate, phRecirc, timeDelta)

  # Return the remaining recirc water to the tank
  tankContents <- returnRecirc(tankContents, recircContents)
  
  # Measure the pH in the tank
  tankContents <- chemicalReaction(tankContents)
  #phTank <- measurePhWithRollingAverage(tankContents, noise=0)
  phTank <- measurePh(tankContents)
  names(phTank) <- "ph.tank"

  return(list(tankContents=tankContents, data=c(setpoint=phSetpoint, phRecirc, ph.recirc2=phRecirc2, linearPhRecirc, phTank, pumpPercentage)))
}

init_update <- function(tankContents) {
  tankContents <- chemicalReaction(tankContents)
  phTank <- measurePh(tankContents)
  attr(update, "ph.recirc") <<- phTank
  phSetpoint <<- phTank
  command_setPointRamp(phTank, 8.7, 5*60)
}

test_update <- function() {
  init_update()
  tankContents <- c(water=10000, hcl=1000, naoh=0) # (litres, grams, grams)
  update(tankContents, recirculationRate, 1, timeDelta=1)
  # No test defined
}

# Simulates the system through multiple time steps
run <- function(tankContents, recircRate, duration, timeDelta=0.2) {
  init_update(tankContents)
  init_delay(2.5, timeDelta)
  init_makeupWater(christchurchWater)
  init_conditionalBleed(6, 10, 30)
  init_doseAcidAtFixedRate()
  init_measurePhWithRollingAverage(100, timeDelta) # used for proportional control
  init_measurePhWithRollingAverage2(10, timeDelta) # used for fair comparison
  init_pumpCausticSolutionWithRollingAverage(1, timeDelta)
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

results <- run(christchurchWater*10, recirculationRate, duration=30*60, timeDelta=0.2)

plot(results$time/60, results$water, type="l",
     main="Volume in Tank", xlab="Time (mins)", ylab="Volume (litres)")

plot(results$time/60, results$ph.tank, type="l",
     main="pH in Tank", xlab="Time (mins)", ylab="pH",
     ylim=c(0,14))
lines(results$time/60, results$setpoint, col="grey")

plot(results$time/60, results$ph.recirc, type="l",
     main="pH in Recirc Line (black)\nand Pump Speed (red)", xlab="Time (mins)", ylab="pH",
     ylim=c(0,14))
lines(results$time/60, results$setpoint, col="grey")
par(new = T)
plot(results$time/60, results$pump.output, type="l", col="red",
     ylim=c(0,100), axes=F, xlab=NA, ylab=NA)
axis(4)

plot(results$time/60, results$nacl/(results$water*1000)*100, type="l",
     main="Proportion of Salt", xlab="Time (mins)", ylab="Salt percentage by weight (%)")


# Save two set of results as results.10s and results.100s
if (FALSE) { # don't run this normally
  par(mfrow = c(2,1))
  plot(results.100s$time/60, results.100s$ph.recirc2, 
       type="l", col="red", lwd=2,
       ylim=c(0,14),
       main="pH Measurement in Recirculation Line\nwith H+ Control Loop",
       ylab="pH",
       xlab="time (mins)")
  lines(results.100s$time/60, results.100s$ph.recirc)
  abline(h=6, col="grey")
  abline(h=10, col="grey")
  legend("bottomright",c("10s probe with 100s control (passive)","100s probe with 100s control"),
         col=c("red","black"),
         lwd=c(2,1))
  
  plot(results.100s$time/60, results.100s$ph.recirc2, 
       type="l", col="red", lwd=2,
       ylim=c(0,14),
       main="pH Measurement in Recirculation Line\nwith H+ Control Loop",
       ylab="pH",
       xlab="time (mins)")
  lines(results.10s$time/60, results.10s$ph.recirc)
  abline(h=6, col="grey")
  abline(h=10, col="grey")
  legend("bottomright",c("10s probe with 100s control (passive)","10s probe with 10s control"),
         col=c("red","black"),
         lwd=c(2,1))
  par(mfrow=c(1,1))
}
#plot(results$time/60, results$linear_ph.recirc, type="l")

