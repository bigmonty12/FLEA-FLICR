library(tidyverse)

fin <- read.csv("012920_DFM_5.csv")

# Create dataframe with metadata
metadata <- fin[1:4]
# Create dataframe with only well readings
wells <- fin[5:16]
# Use running median to find baseline for each well
n <- length(wells$W1)
k <- (1 + 2 * min((n-1)%/% 2, ceiling(0.1*n))) # Turlach default for k
m <- min(k, 24001)
baselines <- lapply(wells, runmed, k=m)
# Subtract baseline from each well and change any negative values to 0
subtractBaselines <- wells - baselines
subtractBaselines[subtractBaselines < 0] <- 0
time <- seq_len(length(wells[[1]])) / 300
time_df <- data.frame(a=time, b=time, c=time, d=time, e=time, f=time, g=time, h=time, i=time, j=time, k=time, l=time)
dev.off()
par(mfcol = c(2, 6))
Map(function(x,y) plot(x, main =y, col="red", type="l"), wells, names(wells))
Map(function(x,y) plot(x, main =y, col="blue", type="l"), subtractBaselines, names(subtractBaselines))
Map(function(x,y,z) plot(x, y=y, main =z, col="blue", type="l"), time_df, subtractBaselines, names(subtractBaselines))

plot(subtractBaselines$W5, col="red", type="l")
plot(subtractBaselines, col="blue", type="l")

# Convert raw value to none (0), leg (1), or proboscis (2) event
labeledEvents <- subtractBaselines
labeledEvents[labeledEvents < 10] <- 0
labeledEvents[labeledEvents > 0 & labeledEvents < 100] <- 1
labeledEvents[labeledEvents >= 100] <- 2

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

editedEvents <- as.data.frame(lapply(labeledEvents, editEvents))

#==== Remove data until first event of the arena ==== 
removeUntilFirstEvent <- function(x){
  df <- data.frame(matrix(0, nrow = 9000, ncol = 12))
  for (i in seq(1, 11, 2)) {
    # Time of first event in arena
    m <- min(rle(x[[i]])$lengths[1], rle(x[[i+1]])$lengths[1])
    # Length of analysis min(30 minutes and remain length after removal until first event)
    l <- min(9000, length(tail(x[[i]], -m)))
    
    df[[i]] <- head(tail(x[[i]], -m), l)
    df[[i+1]] <- head(tail(x[[i+1]], -m), l)
  }
  colnames(df) <- c("W1", "W2", "W3", "W4", "W5", "W6",
                    "W7", "W8", "W9", "W10", "W11", "W12")
  return(df)
}

#==== Find events and total times ====
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

removedUntilFirstEvent <- removeUntilFirstEvent(editedEvents)
analyzedEvents <- lapply(removedUntilFirstEvent, analyzeEvents)
analyzedEvents <- as.data.frame(do.call(rbind, analyzedEvents))

analyzePreference <- function(x) {
  totalPreferences <- list()
  x = 1
  for (i in seq(1, 11, 2)) {
    prob1 <- as.integer(analyzedEvents[i,]$Proboscis.Seconds)
    prob2 <- as.integer(analyzedEvents[i+1,]$Proboscis.Seconds)
    probPreference <- (prob2 - prob1) / (prob2 + prob1)
    leg1 <- as.integer(analyzedEvents[i,]$Leg.Seconds)
    leg2 <- as.integer(analyzedEvents[i+1,]$Leg.Seconds)
    legPreference <- (leg2 - leg1) / (leg2 + leg1)
    totalPreference <- ((prob2 + leg2) - (prob1 + leg1)) / ((prob2 + leg2) + (prob1 + leg1))
    events1 <- as.integer(analyzedEvents[i,]$Leg.Seconds)
    events2 <- as.integer(analyzedEvents[i+1,]$Leg.Seconds)
    totalEventPreference <- (events2 - events1) / (events2 + events1)
    preferences <- list("Proboscis.Preference" = probPreference,
                        "Leg.Preference" = legPreference,
                        "Total.Preference" = totalPreference,
                        "Event.Preference" = totalEventPreference)
    totalPreferences[[x]] <- preferences
    x <- x + 1
  }
  return(totalPreferences)
} 

preferences <- analyzePreference(analyzedEvents)
preferences <- as.data.frame(do.call(rbind, preferences))