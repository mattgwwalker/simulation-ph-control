tankContents <- c(water=10000, hcl=0, naoh=0, nacl=0) # (litres, grams, grams, grams)
bleedRate <- 1.5 # litres / second
recirculationRate = 124000/60/60 # litres / second 

# Simulates the recirculation pump; returns the tank contents after pumping as well 
# as the pumping contents
recirc <- function(tankContents, recircRate, timeDelta) {
  recircTotalVolume <- recirculationRate * timeDelta
  recircContents <- recircTotalVolume/sum(tankContents) * tankContents
  tankContents <- tankContents - recircContents
  return(list(tankContents=tankContents, recircContents=recircContents))
}


# Returns the liquid contents after bleeding
bleed <- function(liquidContents, recircRate, timeDelta) {
  bleedTotalVolume <- bleedRate * timeDelta
  bleedVolumes <- bleedTotalVolume/sum(liquidContents) * liquidContents 
  liquidContents <- liquidContents - bleedVolumes
  return(liquidContents)
}


# Returns the recirculation contents to the tank
returnRecirc <- function(tankContents, recircContents) {
  tankContents <- tankContents + recircContents
  return(tankContents)
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
  water <- unname(liquidContents["water"])
  
  # Check that neuralisation has already occurred
  stopifnot(hcl==0 | naoh==0)
  
  if (hcl > 0) {
    # pH calculation if there is HCl
    gramsPerMol <- 36
    molOfHcl <- hcl / gramsPerMol
    volume <- water # Assumes that chemical additions do not change volume
    molPerLitre <- molOfHcl / volume
    ph <- -log10(molPerLitre)
    if (ph>7) ph <- 7 # The amount of HCl is so small this equation is no longer correct
  } else if (naoh > 0) {
    # pH calculation if there is NaOH
    gramsPerMol <- 40
    molOfNaoh <- naoh / gramsPerMol
    volume <- water # Assumes that chemical additions do not change volume
    molPerLitre <- molOfNaoh / volume
    poh <- -log10(molPerLitre)
    ph <- 14 - poh
    if (ph<7) ph <- 7 # The amount of NaOH is so small this equation is no longer correct
  } else {
    # pH if there is no HCl nor NaOH
    ph <- 7
  }
  
  # Linearised pH
  linear_ph <- 10^(8-ph)
  
  return(c(ph=ph, linear_ph=linear_ph))
}

test_measurePh <- function() {
  tankContents <- c(water=10000, hcl=1000, naoh=0) # (litres, grams, grams)
  result <- measurePh(tankContents)
  stopifnot(abs(result - 2.56) < 0.01)
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

chemicalReaction <- function(liquidContents) {
  hcl <- unname(liquidContents["hcl"])
  naoh <- unname(liquidContents["naoh"])
  
  gramsPerMolHcl <- 36
  gramsPerMolNaoh <- 40
  molHcl <- hcl / gramsPerMolHcl
  molNaoh <- naoh / gramsPerMolNaoh
  
  if (molHcl > molNaoh) {
    molHcl <- molHcl - molNaoh
    molNaoh <- 0
  } else {
    molNaoh <- molNaoh - molHcl
    molHcl <- 0
  }
  
  # Convert back to grams
  hcl <- molHcl * gramsPerMolHcl
  naoh <- molNaoh * gramsPerMolNaoh
  
  liquidContents["hcl"] <- hcl
  liquidContents["naoh"] <- naoh
  
  return(liquidContents)
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
update <- function(tankContents, recircRate, time, timeDelta) {
  # Add acid
  if (time>60) {
    tankContents <- doseAcidAtFixedRate(tankContents, 1000, 5*60, timeDelta)
  }
  # Add makup water
  tankContents <- makeupWater(tankContents, makeupWaterRate, makeupWaterLow, makeupWaterHigh, timeDelta=timeDelta)
  # Recirc the water
  recircResult <- recirc(tankContents, recircRate, timeDelta)
  tankContents <- recircResult$tankContents
  recircContents <- recircResult$recircContents
  # Dose caustic
  phSetpoint <- 8
  previousPh <- attr(update, "ph.recirc")
  if (is.null(previousPh)) previousPh <- 7
  pumpPercentage <- proportionalPumpControl(previousPh,
                                            phSetpoint,
                                            2)
  recircContents <- pumpCausticSolution(recircContents, pumpPercentage, maxPumpRate, timeDelta)
  # Measure the pH in the recirc line
  recircContents <- chemicalReaction(recircContents)
  measuredPhRecirc <- measurePh(recircContents)
  names(measuredPhRecirc) <- c("ph.recirc", "linear_ph.recirc")
  attr(update, "ph.recirc") <<- measuredPhRecirc["ph.recirc"]
  # Bleed some of the recirc water
  recircContents <- bleed(recircContents, recircRate, timeDelta)
  # Return the remaining recirc water to the tank
  tankContents <- returnRecirc(tankContents, recircContents)
  # Measure the pH in the tank
  tankContents <- chemicalReaction(tankContents)
  measuredPhTank <- measurePh(tankContents)
  names(measuredPhTank) <- c("ph.tank", "linear_ph.tank")
  
  return(list(tankContents=tankContents, data=c(measuredPhRecirc, measuredPhTank, pumpPercentage)))
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
  init_makeupWater()
  init_doseAcidAtFixedRate()
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

results <- run(tankContents, recirculationRate, duration=50*60, timeDelta=1)

plot(results$time/60, results$water, type="l")
plot(results$time/60, results$ph.tank, type="l")
plot(results$time/60, results$ph.recirc, type="l")
plot(results$time/60, results$linear_ph.recirc, type="l")


