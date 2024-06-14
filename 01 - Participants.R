# Installing and loading libraries


# Loading dataset
load(file = "Data.RData"); names(ds)

# IV. Participants' characteristics ####
n <- nrow(ds)

## Age analysis ####
age.min <- min(ds$child_age, na.rm = T); age.max <- max(ds$child_age, na.rm = T)
age.m <- round(mean(ds$child_age, na.rm = T), 2); age.sd <- round(sd(ds$child_age, na.rm = T), 2)

## Gender analysis ####
gen <- table(ds$gender); gen
gen.p <- as.numeric(round(gen / n * 100, 2)); gen.p

## Education analysis ####
edu <- table(ds$edu_lev); edu
edu.p <- as.numeric(round(edu / n * 100, 2)); edu.p

## Marital status ####
mar <- table(ds$mar_stat); mar
mar.p <- as.numeric(round(mar / n * 100, 2)); mar.p

