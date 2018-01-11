#Simulate data for dynamic occupancy model
#Brent Pease @BrentPease1

# Option to set random number generator to get the same data
set.seed(74)


# Set sample sizes
nsites <- 100                                             # Number of sites to be sampled
nsurveys <- 4                                          # Number of surveys (visits)
nyears <- 4                                               # Number of years in study


# Define parameter values
psi_init <- 0.4                                           # Initial probability of occupancy
p <- 0.6                                                  # Probability of detection
phi <- 0.7                                                # Probability of survival (1 - extinction)
gamma <- 0.3                                              # Probability of colonization


# Prepare vector and arrays to hold true and observed values
psi <- rep(NA, nyears)                                    # Probability of occupancy across years
muZ <- z <- array(dim = c(nsites, nyears))                # Expected and perfectly observed occurrence    
y <- array(NA, dim = c(nsites, nsurveys, nyears))         # Detection / non-detection data
psi[1] <- psi_init                                        # Assign initial occurrence value to first slot of psi vector


# generate presence/absence data (the truth)
# First year
z[,1] <- rbinom(n = nsites, size = 1, prob = psi[1])      # Initial occupancy state
# Following years
for(i in 1:nsites){                                       # Loop over sites
  for(t in 2:nyears){                                     # Loop over years
    muZ[i,t] <- z[i, t-1]*phi + (1-z[i, t-1])*gamma
    z[i,t] <- rbinom(n = 1,size = 1,prob = muZ[i,t])
  }
}

# Generate detection/non-detection data
for(i in 1:nsites){                                       # Loop over sites
  for(t in 1:nyears){                                     # Loop over years
    mu <- z[i,t] * p                                      # Observation process                            
    for(j in 1:nsurveys){                                 # Loop over surveys
      y[i,j,t] <- rbinom(1, 1, mu)                        # Populate y array with detection/non-detection data
    }
  }
}

# Compute annual population occupancy
for (k in 2:nyears){                                      # Loop over remaining years
  psi[k] <- psi[k-1]*phi + (1-psi[k-1])*gamma             # Compute occupancy for each year
}

# `y` is the final simulated detection/non-detection data
print(y[1:6,,1])                                          # First 6 sites of year 1
