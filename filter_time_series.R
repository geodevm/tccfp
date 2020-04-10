# Filter out bursts
for (i in 1:as.numeric(dim(collars))) {
  if (collars$animal_id[i + 1] != collars$animal_id[i] | as.numeric(dim(collars)) == i) {
    collars$difftime[i] <- NA
  } else if (collars$animal_id[i + 1] == collars$animal_id[i]) {
    collars$difftime[i] <- difftime(collars$acquisition_time[i + 1], collars$acquisition_time[i], units = "hours")
  }
}
for (i in 1:as.numeric(dim(collars))) {
  if (collars$difftime[i] < 1.5) {
    j = i
    while (collars$difftime[j+1] < 1 | is.na(collars$difftime[j+1])) {
      collars <- collars[-(j+1),]
    }
  }
}
if (collars$difftime[1] < 1) {
  collars <- collars[!1,]
}
collars$difftime <- NULL

# Filter in bursts
for (i in 1:as.numeric(dim(collars))) {
  if (collars$animal_id[i + 1] != collars$animal_id[i] | as.numeric(dim(collars)) == i) {
    collars$difftime[i] <- NA
  } else if (collars$animal_id[i + 1] == collars$animal_id[i]) {
    collars$difftime[i] <- difftime(collars$acquisition_time[i + 1], collars$acquisition_time[i], units = "hours")
  }
}
k = 0
for (i in 1:as.numeric(dim(collars))) {
  if (is.na(collars$difftime[i])) {
    if (i == as.numeric(dim(collars))){
      collars$burst_id[i] <- k
    } else if (collars$difftime[i+1] > 1) {
      j = i  
      while (collars$difftime[j+1] > 1 | is.na(collars$difftime[j+1])) {
        collars <- collars[-(j+1),]
      }
      collars$burst_id[j] <- k
      k = k + 1
    } else {
      collars$burst_id[i] <- k
    }
  } else if (collars$difftime[i] > 1) {
    j = i
    while (collars$difftime[j+1] > 1 | is.na(collars$difftime[j+1])) {
      collars <- collars[-(j+1),]
    }
    collars$burst_id[j] <- k
    k = k + 1
  } else {
    collars$burst_id[i] <- k
  }
}
if (collars$difftime[1] > 1) {
  collars <- collars[-1,]
}
collars$difftime <- NULL
