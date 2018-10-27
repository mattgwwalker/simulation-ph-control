tankContents <- c(water=10000, hcl=1000, naoh=0, nacl=0) # (litres, grams, grams, grams)
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



test_makeupWater <- function() {
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
ph <- function(liquidContents) {
  hcl <- unname(liquidContents["hcl"])
  naoh <- unname(liquidContents["naoh"])
  water <- unname(liquidContents["water"])
  
  # Check that neuralisation has already occurred
  stopifnot(hcl==0 | naoh==0)
  
  # pH calculation if there is HCl:
  if (hcl > 0) {
    gramsPerMol <- 36
    molOfHcl <- hcl / gramsPerMol
    volume <- water # Assumes that chemical additions do not change volume
    molPerLitre <- molOfHcl / volume
    ph <- -log10(molPerLitre)
    if (ph>7) ph <- 7 # The amount of HCl is so small this equation is no longer correct
  }
  
  # Linearised pH
  linear_ph <- 10^(8-ph)
  
  return(c(ph=ph, linear_ph=linear_ph))
}

test_ph <- function() {
  tankContents <- c(water=10000, hcl=1000, naoh=0) # (litres, grams, grams)
  result <- ph(tankContents)
  stopifnot(abs(result - 2.56) < 0.01)
}


# Simulates the system through a single time step
update <- function(tankContents, recircRate, timeDelta = 0.2) {
  # Add makup water
  tankContents <- makeupWater(tankContents, makeupWaterRate, timeDelta=timeDelta)
  # Recirc the water
  recircResult <- recirc(tankContents, recircRate, timeDelta)
  tankContents <- recircResult$tankContents
  recircContents <- recircResult$recircContents
  # Measure the pH
  measuredPh <- ph(recircContents)
  # Bleed some of the recirc water
  recircContents <- bleed(recircContents, recircRate, timeDelta)
  # Return the remaining recirc water to the tank
  tankContents <- returnRecirc(tankContents, recircContents)
  
  return(list(tankContents=tankContents, ph=measuredPh))
}

test_update <- function() {
  tankContents <- c(water=10000, hcl=1000, naoh=0) # (litres, grams, grams)
  update(tankContents, recirculationRate, 1)
}

# Simulates the system through multiple time steps
run <- function(tankContents, recircRate, duration, timeDelta=0.2) {
  time <- 0
  results <- NULL
  
  while (time < duration) {
    result <- update(tankContents, recircRate, timeDelta)
    tankContents <- result$tankContents
    time <- time + timeDelta
    results <- rbind(results, c(time=time, result$tankContents, result$ph))
  }
  
  return(data.frame(results))
}

results <- run(tankContents, recirculationRate, duration=5*60*60, timeDelta=10)

plot(results$time/60/60, results$ph, type="l")
plot(results$time/60/60, results$water, type="l")
plot(results$time/60/60, results$hcl/result$water, type="l")
plot(results$time/60/60, results$linear_ph, type="l")


