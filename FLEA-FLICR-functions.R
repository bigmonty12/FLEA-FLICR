# FLEA-FLICR Functions

#====Pre-processing Functions====

# Used to change FLEA sampling rate
# Aggregates every n rows and finds mean
BinMean <- function (vec, every, na.rm = FALSE) {
  n <- length(vec)
  x <- .colMeans(vec, every, n %/% every, na.rm)
  r <- n %% every
  if (r) x <- c(x, mean.default(vec[(n - r + 1):n], na.rm = na.rm))
  x
}

# Functions to find baseline

# Running median
baselineRunMed <- function(x){
  n <- length(x[[1]])
  k <- (1 + 2 * min((n-1)%/% 2, ceiling(0.1*n))) # Turlach default for k
  m <- min(k, 24001)
  # Use running median to find baseline for each well
  baselines <- lapply(x, runmed, k=m)
  baselines
}

# Beads method 
baselineBeads <- function(x, name_fun){
  source("beads.R")
  baselines1 <- baselineRunMed(x)
  baselines2 <- lapply(x - baselines1, beads, 1, 0.05, 6, 0.6*0.5, 0.6*5, 0.6*4)
  baselines3 <- rlist::list.map(baselines2, as.list(.[1]))
  baselines4 = unlist(baselines3, recursive = F)
  baselines5 <- lapply(baselines4, function(x) as.data.frame(as.matrix(x)))
  baselines6 <- as.data.frame(baselines5)
  names(baselines6) < name_fun
  baselines <- x - baselines6
  rm(baselines1, baselines2, baselines3, baselines4, baselines5, baselines6)
  baselines
}

# Plotting Functions
plotData <- function(x, y, z, col){
  plot(x, y=y, main=z, col=col, type="l",
       xlab="Time [min]", ylab="Intensity [au]")
}

plotDataAndLine <- function(x, y, z, col, baseline){
  plot(x, y=y, main=z, col=col, type="l",
       xlab="Time [min]", ylab="Intensity [au]")
  lines(x=x, y=baseline, lwd=2, col="blue")
}

#====Analysis Functions====

# Convert data to none, leg, or proboscis event
editEvents <- function(x) {
  runs <- rle(as.integer(x))
  
  for (i in seq_along(runs$values)) {
    # leg events sandwiching a proboscis event are converted to proboscis events
    if (runs$values[i] == 2) {
      if (runs$values[i-1] == 1) runs$values[i-1] = 2
      if (runs$values[i+1] == 1) runs$values[i+1] = 2
    }
  }
  new_runs <- rle(inverse.rle(runs))
  for (i in seq_along(new_runs$values)) {
    if (new_runs$values[i] == 1) {
      # if "leg event" is longer than 4 sec, change to none event
      if (new_runs$lengths[i] > 4*5) new_runs$values[i] = 0
    }
    if (new_runs$values[i] == 2) {
      # if "proboscis event" is longer than 40 sec, change to none event
      if (new_runs$lengths[i] > 40*5) new_runs$values[i] = 0
    }
  }
  return(inverse.rle(new_runs))
}

# Remove data until first event of the arena
removeUntilFirstEvent <- function(x, length, wells, name_fun){
  rows <- length * 60 * 5
  rowNum <- wells * 2 - 1
  df <- list()
  for (i in seq(1, rowNum, 2)) {
    # Time of first event in arena
    m <- min(rle(x[[i]])$lengths[1], rle(x[[i+1]])$lengths[1])
    # Length of analysis min(30 minutes and remain length after removal until first event)
    l <- min(rows, length(tail(x[[i]], -m)))
    
    df[[i]] <- head(tail(x[[i]], -m), l)
    df[[i+1]] <- head(tail(x[[i+1]], -m), l)
  }
  df <- data.frame(lapply(df, "length<-", max(lengths(df))))
  df <- data.frame(df)
  colnames(df) <- name_fun
  return(df)
}

# Find time analyzed (useful if time analyzed is less than the desired time to analyze)
timeAnalyzed <- function(x){
  return(length(x[[1]]) / 300)
}

# Find number of events, total time of each type of event, average length of each type of event
# and % of events which are leg events
analyzeEvents <- function(x) {
  legEventIndex <- which(rle(x)$values == 1)
  legEventNums <- length(legEventIndex)
  legEventSecs <- sum(rle(x)$lengths[legEventIndex]) / 5
  probEventIndex <- which(rle(x)$values == 2)
  probEventNums <- length(probEventIndex)
  probEventSecs <- sum(rle(x)$lengths[probEventIndex]) / 5
  totalEventNums <- legEventNums + probEventNums
  legAveEventSecs <- legEventSecs / legEventNums
  probAveEventSecs <- probEventSecs / probEventNums
  legEventPercentage <- legEventNums / totalEventNums
  eventsAnalyzed <- list("Leg.Events" = legEventNums,
                         "Proboscis.Events" = probEventNums,
                         "Total.Events" = totalEventNums,
                         "Leg.Percentage" = legEventPercentage,
                         "Leg.Seconds" = legEventSecs,
                         "Leg.Average.Seconds" = legAveEventSecs,
                         "Proboscis.Seconds" = probEventSecs,
                         "Proboscis.Average.Seconds" = probAveEventSecs
  )
  return(eventsAnalyzed)
}

# Find preferences (proboscis, leg, total, and event) from analyzedEvents
analyzePreference <- function(x, wells) {
  totalPreferences <- list()
  end <- wells * 2 - 1
  z = 1
  for (i in seq(1, end, 2)) {
    prob1 <- as.integer(x[i,]$Proboscis.Seconds)
    prob2 <- as.integer(x[i+1,]$Proboscis.Seconds)
    probPreference <- (prob2 - prob1) / (prob2 + prob1)
    leg1 <- as.integer(x[i,]$Leg.Seconds)
    leg2 <- as.integer(x[i+1,]$Leg.Seconds)
    legPreference <- (leg2 - leg1) / (leg2 + leg1)
    totalPreference <- ((prob2 + leg2) - (prob1 + leg1)) / ((prob2 + leg2) + (prob1 + leg1))
    events1 <- as.integer(x[i,]$Total.Events)
    events2 <- as.integer(x[i+1,]$Total.Events)
    totalEventPreference <- (events2 - events1) / (events2 + events1)
    preferences <- list("Proboscis.Preference" = probPreference,
                        "Leg.Preference" = legPreference,
                        "Total.Preference" = totalPreference,
                        "Event.Preference" = totalEventPreference)
    totalPreferences[[z]] <- preferences
    z <- z + 1
  }
  return(totalPreferences)
} 

#====Results Functions====

# Find duration of each individual event
bouts <- function(df, type){
  if(type == 'Leg'){
    x <- 1
  }
  if(type == 'Proboscis'){
    x <- 2
  }
  r <- rle(df)
  return(r$lengths[which(r$values == x)])
}

# Tidy up individual event duration into nice table
tidy_bouts <- function(df, type){
  x <- lapply(df, bouts, type)
  cleanData <- t(plyr::ldply(x, rbind, .id=NULL))
  return(cleanData / 5)
}