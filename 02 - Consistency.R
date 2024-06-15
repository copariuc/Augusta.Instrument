# Installing and loading libraries
if(!require(psych)) install.packages("psych")
if(!require(dplyr)) install.packages("dplyr")

library(psych); library(dplyr)

# Loading dataset
load(file = "Data.Rdata"); names(ds)

# Setting up correction key ####
key <- list(
  ## BERQ ####
  SEEK.DIS = c("BERQ.1", "BERQ.6", "BERQ.11", "BERQ.16"),
  WITHDRAW = c("BERQ.2", "BERQ.7", "BERQ.12", "BERQ.17"),
  ACTI.APP = c("BERQ.3", "BERQ.8", "BERQ.13", "BERQ.18"),
  SEEK.SOC = c("BERQ.4", "BERQ.9", "BERQ.14", "BERQ.19"),
  IGNORING = c("BERQ.5", "BERQ.10", "BERQ.15", "BERQ.20")
  
  ## CERQ ####
)

# Assessing internal consistency ####
## Seeking distraction (SEEK.DIS) ####
t <- "Seeking distraction (SEEK.DIS)"
a <- psych::alpha(x = ds %>% dplyr::select(key$SEEK.DIS), 
                  cumulative = T, title = t, check.keys = T); a
a.SEEK.DIS <- c(as.numeric(round(a$total[1], 2)),        # Alpha Value
                as.numeric(round(a$feldt$lower.ci, 2)),  # Lower bound (CI 95%)
                as.numeric(round(a$feldt$upper.ci, 2)))  # Upper bound (CI 95%)

o <- psych::omega(m = ds %>% dplyr::select(key$SEEK.DIS), 
                  fm = "minres", rotate = "oblimin", plot = F,
                  nfactors = 1, poly = F, n.iter = 10, flip = T, p = .05,
                  title = t)
o.SEEK.DIS <- c(as.numeric(round(o$om$omega.tot, 2)))  # Omega value

## Withdrawal (WITHDRAW) ####
t <- "Withdrawal (WITHDRAW)"
a <- psych::alpha(x = ds %>% dplyr::select(key$WITHDRAW), 
                  cumulative = T, title = t, check.keys = T); a
a.WITHDRAW <- c(as.numeric(round(a$total[1], 2)),        # Alpha Value
                as.numeric(round(a$feldt$lower.ci, 2)),  # Lower bound (CI 95%)
                as.numeric(round(a$feldt$upper.ci, 2)))  # Upper bound (CI 95%)

o <- psych::omega(m = ds %>% dplyr::select(key$WITHDRAW), 
                  fm = "minres", rotate = "oblimin", plot = F,
                  nfactors = 1, poly = F, n.iter = 10, flip = T, p = .05,
                  title = t)
o.WITHDRAW <- c(as.numeric(round(o$om$omega.tot, 2)))  # Omega value

## Actively approaching (ACTI.APP) ####
t <- "Actively approaching (ACTI.APP)"
a <- psych::alpha(x = ds %>% dplyr::select(key$ACTI.APP), 
                  cumulative = T, title = t, check.keys = T); a
a.ACTI.APP <- c(as.numeric(round(a$total[1], 2)),        # Alpha Value
                as.numeric(round(a$feldt$lower.ci, 2)),  # Lower bound (CI 95%)
                as.numeric(round(a$feldt$upper.ci, 2)))  # Upper bound (CI 95%)

o <- psych::omega(m = ds %>% dplyr::select(key$ACTI.APP), 
                  fm = "minres", rotate = "oblimin", plot = F,
                  nfactors = 1, poly = F, n.iter = 10, flip = T, p = .05,
                  title = t)
o.ACTI.APP <- c(as.numeric(round(o$om$omega.tot, 2)))  # Omega value

## Seeking social support (SEEK.SOC) ####
t <- "Seeking social support (SEEK.SOC)"
a <- psych::alpha(x = ds %>% dplyr::select(key$SEEK.SOC), 
                  cumulative = T, title = t, check.keys = T); a
a.SEEK.SOC <- c(as.numeric(round(a$total[1], 2)),        # Alpha Value
                as.numeric(round(a$feldt$lower.ci, 2)),  # Lower bound (CI 95%)
                as.numeric(round(a$feldt$upper.ci, 2)))  # Upper bound (CI 95%)

o <- psych::omega(m = ds %>% dplyr::select(key$SEEK.SOC), 
                  fm = "minres", rotate = "oblimin", plot = F,
                  nfactors = 1, poly = F, n.iter = 10, flip = T, p = .05,
                  title = t)
o.SEEK.SOC <- c(as.numeric(round(o$om$omega.tot, 2)))  # Omega value

## Ignoring (IGNORING) ####
t <- "Ignoring (IGNORING)"
a <- psych::alpha(x = ds %>% dplyr::select(key$IGNORING), 
                  cumulative = T, title = t, check.keys = T); a
a.IGNORING <- c(as.numeric(round(a$total[1], 2)),        # Alpha Value
                as.numeric(round(a$feldt$lower.ci, 2)),  # Lower bound (CI 95%)
                as.numeric(round(a$feldt$upper.ci, 2)))  # Upper bound (CI 95%)

o <- psych::omega(m = ds %>% dplyr::select(key$ACTI.APP), 
                  fm = "minres", rotate = "oblimin", plot = F,
                  nfactors = 1, poly = F, n.iter = 10, flip = T, p = .05,
                  title = t)
o.IGNORING <- c(as.numeric(round(o$om$omega.tot, 2)))  # Omega value



# Compute total score ####
scores <- psych::scoreItems(keys = key, items = ds, 
                            totals = T, missing = T, 
                            impute = "median")
ds <- cbind(ds, scores$scores); head(ds)

# Saving dataset
save(ds, file = "Totals.RData")
rm(a, t, o)
