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
makeupWater <- function(tankContents, makeupWaterRate, onVolume=8000, offVolume=10000, timeDelta) {
  # Initial value of makeupWaterOn
  makeupWaterOn <- attr(makeupWater, "state")
  if (is.null(makeupWaterOn)) makeupWaterOn <- FALSE

  if (tankContents["water"] <= onVolume) {
    # Turn makeup water on
    attr(makeupWater, "state") <<- TRUE
  }
  if (tankContents["water"] > offVolume) {
    # Turn makeup water off
    attr(makeupWater, "state") <<- FALSE
  }
  
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

# Simulates the system through a single time step
update <- function(tankContents, recircRate, time, timeDelta) {
  # Add acid
  if (time>10) {
    tankContents <- doseAcidAtFixedRate(tankContents, 1000, 5*60, timeDelta)
  }
  # Add makup water
  tankContents <- makeupWater(tankContents, makeupWaterRate, timeDelta=timeDelta)
  # Recirc the water
  recircResult <- recirc(tankContents, recircRate, timeDelta)
  tankContents <- recircResult$tankContents
  recircContents <- recircResult$recircContents
  # Measure the pH
  measuredPh <- measurePh(recircContents)
  # Bleed some of the recirc water
  recircContents <- bleed(recircContents, recircRate, timeDelta)
  # Return the remaining recirc water to the tank
  tankContents <- returnRecirc(tankContents, recircContents)
  
  return(list(tankContents=tankContents, ph=measuredPh))
}

test_update <- function() {
  tankContents <- c(water=10000, hcl=1000, naoh=0) # (litres, grams, grams)
  update(tankContents, recirculationRate, 1, timeDelta=1)
  # No test defined
}

# Simulates the system through multiple time steps
run <- function(tankContents, recircRate, duration, timeDelta=0.2) {
  init_makeupWater()
  init_doseAcidAtFixedRate()
  time <- 0
  results <- NULL
  
  while (time < duration) {
    result <- update(tankContents, recircRate, time, timeDelta)
    tankContents <- result$tankContents
    time <- time + timeDelta
    results <- rbind(results, c(time=time, result$tankContents, result$ph))
  }
  
  return(data.frame(results))
}

results <- run(tankContents, recirculationRate, duration=20*60, timeDelta=0.2)

plot(results$time/60, results$ph, type="l")
plot(results$time/60, results$water, type="l")
plot(results$time/60, results$hcl/results$water, type="l")
plot(results$time/60, results$linear_ph, type="l")


