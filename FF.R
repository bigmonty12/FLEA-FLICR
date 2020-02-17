library(dplyr)

#fin <- read.csv("012920_DFM_5.csv")
#fin <- read.csv("../../../FLIC-FLEA/191003 DFM_14.csv")
fin <- readxl::read_excel("020620_1_Analysis.xlsx", sheet = 2)

BinMean <- function (vec, every, na.rm = FALSE) {
  n <- length(vec)
  x <- .colMeans(vec, every, n %/% every, na.rm)
  r <- n %% every
  if (r) x <- c(x, mean.default(vec[(n - r + 1):n], na.rm = na.rm))
  x
}
b_mean <- BinMean(fin$Dev1_Voltage__0, every = 100)

conversion <- as.data.frame(lapply(fin[2:9], BinMean, every=100)) * 310

# Prob cutoff values for FLEA
# 3.3 MOhm -> 40
# 10 MOhm -> 100
# 20 MOhm -> 155
# 33 MOhm -> 190 
# Create dataframe with metadata
metadata <- fin[1:4]
# Create dataframe with only well readings
wells <- fin[5:16]
# Use running median to find baseline for each well
n <- length(wells[[1]])
k <- (1 + 2 * min((n-1)%/% 2, ceiling(0.1*n))) # Turlach default for k
m <- min(k, 24001)
# Use running median to find baseline for each well
# Runtime for 23 hour FLIC (About 8 minutes total); 30 minutes
memMed <- function(x, k){
  y <- runmed(x, k)
  print(pryr::mem_used())
  return(y)
}
baselines1 <- lapply(wells, memMed, k=m) # 50 seconds; < 1 sec
baselines2 <- lapply(wells - baselines1, beads, 1, 0.05, 6, 0.6*0.5, 0.6*5, 0.6*4) # 7 minutes; 13 seconds
baselines3 <- rlist::list.map(baselines2, as.list(.[1]))
baselines4 = unlist(baselines3, recursive = F)
baselines5 <- lapply(baselines4, function(x) as.data.frame(as.matrix(x)))
baselines6 <- as.data.frame(baselines5)
baselines <- wells - baselines6

subtractBaselines <- wells - baselines1
subtractBaselines[subtractBaselines < 0] <- 0
time <- seq_len(length(wells[[1]])) / 300
time_df <- data.frame(a=time, b=time, c=time, d=time, e=time, f=time, g=time, h=time, i=time, j=time, k=time, l=time)
dev.off()
par(mfcol = c(2, 6))
plot_baselines <- function(x, y, z, baseline){
  plot(x, y=y, main=z, col="red", type="l", xlab="Time [min]", ylab="Intensity [au]")
  print(length(x))
  lines(x = x, y = baseline, lwd=2, col="blue")
}
Map(plot_baselines, time_df, wells, names(wells), smoothBaselines)
Map(plot_baselines, time_df, wells, names(wells), baselines)
Map(function(x,y) plot(x, main =y, col="red", type="l"), wells, names(wells))
Map(function(x) lines(x, lwd=2, col="blue"), baselines)
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
