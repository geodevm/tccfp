for (i in 1:dim(collars)) {
  collars$difftime[i] <- difftime(collars$acquisition_time[i + 1], collars$acquisition_time[i], units = "hours")
  if (collars$difftime[i-1] < 1 & collars$difftime[i-1]) {
    collars$difftime[i] == collars$difftime[i-1]
  } else if (is.na(collars$difftime[i-1]) | is.na(collars$difftime[i])) {
    collars$difftime[i] == collars$difftime[i]
  }
}

# For filtering out fine scale
oppo <- collars

for (i in 1:dim(oppo)) {
  if (oppo$difftime[i:(i+14)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 12]) {
    oppo <- oppo[-((i+1):(i+14)),]
  } else if (oppo$difftime[i:(i+12)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 13]) {
    oppo <- oppo[-((i+1):(i+13)),]
  } else if (oppo$difftime[i:(i+11)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 12]) {
    oppo <- oppo[-((i+1):(i+12)),]
  } else if (oppo$difftime[i:(i+10)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 11]) {
    oppo <- oppo[-((i+1):(i+11)),]
  } else if (oppo$difftime[i:(i+9)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 10]) {
    oppo <- oppo[-((i+1):(i+10)),]
  } else if (oppo$difftime[i:(i+8)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 9]) {
    oppo <- oppo[-((i+1):(i+9)),]
  } else if (oppo$difftime[i:(i+7)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 8]) {
    oppo <- oppo[-((i+1):(i+8)),]
  } else if (oppo$difftime[i:(i+6)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 7]) {
    oppo <- oppo[-((i+1):(i+7)),]
  } else if (oppo$difftime[i:(i+5)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 6]) {
    oppo <- oppo[-((i+1):(i+6)),]
  } else if (oppo$difftime[i:(i+4)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 5]) {
    oppo <- oppo[-((i+1):(i+5)),]
  } else if (oppo$difftime[i:(i+3)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 4]) {
    oppo <- oppo[-((i+1):(i+4)),]    
  } else if (oppo$difftime[i:(i+2)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 3]) {
    oppo <- oppo[-((i+1):(i+3)),]
  } else if (oppo$difftime[i:(i+1)] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 2]) {
    oppo <- oppo[-((i+1):(i+2)),]
  } else if (oppo$difftime[i] < 4 & oppo$animal_id[i] == oppo$animal_id[i + 1]) {
    oppo <- oppo[-(i+1),]
  }
}

collars

filt <- as.numeric(rownames(oppo))

collars <- collars[-filt,]

list
# For filtering out course
cull <- c()
for (i in 1:dim(collars)) {
  if (collars$difftime[i:(i+14)] < 1 & collars$animal_id[i] == collars$animal_id[i + 14]) {
    cull <- c(cull, i:(i+14))
  } else if (collars$difftime[i:(i+12)] < 1 & collars$animal_id[i] == collars$animal_id[i + 13]) {
    cull <- c(cull, i:(i+13))
  } else if (collars$difftime[i:(i+11)] < 1 & collars$animal_id[i] == collars$animal_id[i + 12]) {
    cull <- c(cull, i:(i+12))
  } else if (collars$difftime[i:(i+10)] < 1 & collars$animal_id[i] == collars$animal_id[i + 11]) {
    cull <- c(cull, i:(i+11))
  } else if (collars$difftime[i:(i+9)] < 1 & collars$animal_id[i] == collars$animal_id[i + 10]) {
    cull <- c(cull, i:(i+10))
  } else if (collars$difftime[i:(i+8)] < 1 & collars$animal_id[i] == collars$animal_id[i + 9]) {
    cull <- c(cull, i:(i+9))
  } else if (collars$difftime[i:(i+7)] < 1 & collars$animal_id[i] == collars$animal_id[i + 8]) {
    cull <- c(cull, i:(i+8))
  } else if (collars$difftime[i:(i+6)] < 1 & collars$animal_id[i] == collars$animal_id[i + 7]) {
    cull <- c(cull, i:(i+7))
  } else if (collars$difftime[i:(i+5)] < 1 & collars$animal_id[i] == collars$animal_id[i + 6]) {
    cull <- c(cull, i:(i+6))
  } else if (collars$difftime[i:(i+4)] < 1 & collars$animal_id[i] == collars$animal_id[i + 5]) {
    cull <- c(cull, i:(i+5))
  } else if (collars$difftime[i:(i+3)] < 1 & collars$animal_id[i] == collars$animal_id[i + 4]) {
    cull <- c(cull, i:(i+4))
  } else if (collars$difftime[i:(i+2)] < 1 & collars$animal_id[i] == collars$animal_id[i + 3]) {
    cull <- c(cull, i:(i+3))
  } else if (collars$difftime[i:(i+1)] < 1 & collars$animal_id[i] == collars$animal_id[i + 2]) {
    cull <- c(cull, i:(i+2))
  } else if (collars$difftime[i] < 1 & collars$animal_id[i] == collars$animal_id[i + 1]) {
    cull <- c(cull, i:(i+1))
  } else if (collars$difftime[i] < 1 & collars$difftime[i] > 0) {
    cull <- c(cull, i)
  } else if (is.na(collars$difftime[i]) | is.na(collars$difftime[i+1]))
    collar[i] <- collar[i]
}

cull <- unique(cull)

collars <- collars[cull,]









cull <- c()
for (i in 1:dim(collars)) {
  if () {
    cull <- c(cull, i)
  } else if (collars$difftime[i+1] > 1 & collars$difftime[i] > 0 & collars$difftime[i] < 1) {
    cull <- c(cull, i:(i+1))
  }
}

collars <- collars[cull,]

collars <- collars[-collars$difftime < 1,]
max(collars$difftime, na.rm = T)
