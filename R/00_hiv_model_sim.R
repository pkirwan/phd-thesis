########################################################## RITA MODEL 1 #######################################################
#### Need to have new progression matrix
Q_mat <- function(t, d1, d2, d3, d4, d5, q, nlatst) {
  out <- matrix(0, nr = nlatst, nc = nlatst)

  ### States definition
  ## State 1: CD4 > 500, inf in the last three months
  ## State 2: CD4 > 500, inf btwn 3 and 6 months ago
  ## State 3: 500 < CD4 < 350, inf btwn 3 and 6 months ago
  ## State 4

  ### No prog, No diag
  out[1, 1] <- 0 ## (1-d0[t])*(1-q[1]) equal to 0 cannot stay latent in state 1 (3 month period)
  out[2, 2] <- 0 ## equal to 0 cannot stay latent in state 2
  out[3, 3] <- 0 ## equal to 0 cannot stay latent in state 2
  out[4, 4] <- (1 - d2[t]) * (1 - q[1])
  out[5, 5] <- (1 - d3[t]) * (1 - q[2])
  out[6, 6] <- (1 - d4[t]) * (1 - q[3])
  out[7, 7] <- (1 - d5[t]) * (1 - q[4])

  ### No diag, prog
  out[1, 2] <- (1 - d1[t]) * (1 - q[1])
  out[1, 3] <- (1 - d1[t]) * q[1]
  out[2, 4] <- (1 - d1[t]) * (1 - q[1])
  out[2, 5] <- (1 - d1[t]) * q[1]

  out[3, 5] <- (1 - d1[t]) * (1 - q[2])
  out[3, 6] <- (1 - d1[t]) * q[2] # added line to allow progression to lower CD4 state

  out[4, 5] <- (1 - d2[t]) * q[1]
  out[5, 6] <- (1 - d3[t]) * q[2]
  out[6, 7] <- (1 - d4[t]) * q[3]

  return(out)
}

D_mat <- function(t, d1, d2, d3, d4, d5, q, nlatst, ndiagst) {
  out <- matrix(0, nr = nlatst, nc = ndiagst)
  # second diagnosis matrix for case with no rita state
  out[1, 2] <- d1[t]
  out[2, 2] <- d1[t]
  out[3, 3] <- d1[t]

  out[4, 2] <- d2[t]
  out[5, 3] <- d3[t]
  out[6, 4] <- d4[t]
  out[7, 5] <- d5[t]

  out[7, 6] <- (1 - d5[t]) * q[4]

  return(out)
}

epi_mat <- function(infs, Q_l, D_l, nlatst, ndiagst, nquar) {
  ### Epidemics loop
  lat <- matrix(NA, nr = nquar, nc = nlatst)
  diags <- matrix(NA, nr = nquar, nc = ndiagst)

  ### First iteration - subdivide the infections in the first quarter
  lat[1, ] <- c(
    infs[1],
    infs[1] * 0.8,
    infs[1] * 0.1,
    infs[1] * 4,
    infs[1] * 2,
    infs[1],
    infs[1] * 0.25
  )
  diags[1, ] <- t(D_l[[1]]) %*% lat[1, ]

  for (t in 2:nquar) {
    h <- c(infs[t], rep(0, nlatst - 1))
    lat[t, ] <- (t(Q_l[[t]]) %*% lat[t - 1, ]) + h
    diags[t, ] <- t(D_l[[t]]) %*% lat[t - 1, ]
  }

  out <- list("lat" = lat, "diags" = diags)
  return(out)
}
