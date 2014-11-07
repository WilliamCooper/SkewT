require(Ranadu)
require(ggplot2)
require(nleqslv)
TZERO <- StandardConstant ("Tzero")
pBot = 1000
pTop = 100
logPrange <- log10 (pBot) - log10 (pTop)
tBot <- -100
tTop <- 40
tMin <- -40
CP <- SpecificHeats ()
RoverCP <- CP[3] / CP[1]

Xplot <- function (.T, .p) {
  return (.T / (tTop - tMin) + 0.5 - (log10 (.p) - log10(pBot))/ logPrange)
}

pLevels <- seq (pTop, pBot, by=50)
tLevels <- seq (tBot, tTop, by=5)

plot (c(0., 1., 1., 0., 0.), c(pBot, pBot, pTop, pTop, pBot), 
      ylim=c(1000., 100.), log='y', type='l')
for (p in pLevels) {
  lines (c(0., 1.), c(p, p))
}
for (t in tLevels) {
  x1 <- Xplot (t, pBot)
  y1 <- pBot
  x2 <- x1 + 1
  y2 <- pTop
  lines (c(x1, x2), c(y1, y2), col='blue')  
}
Theta <- seq (TZERO - 100, TZERO + 300, by=10)
for (theta in Theta) {
  x1 <- Xplot(theta / ((1000/pBot)^RoverCP) - TZERO, pBot)
  y1 <- pBot
  x2 <- Xplot(theta / ((1000/pTop)^RoverCP) - TZERO, pTop)
  y2 <- pTop
  lines (c(x1, x2), c(y1, y2), col='orange')
}
ThetaE <- seq (TZERO-100, TZERO+150, by=10)
TfromEPT <- function (.T, .thetaE, .P) {
  e <- MurphyKoop (.T, .P)
  r <- MixingRatio (e / .P)
  lhv <- 2.501e6 - 2370. * .T    # latent heat of vaporization, temp-dependent
  expn=lhv * r / (CP[1] * (TZERO + .T))
  return (.thetaE - (TZERO+.T)*(1000/(.P-e))^RoverCP * exp(expn))
}
tt <- pLevels  # shortut to define new vector
for (thetaE in ThetaE) {
  for (i in 1:length(pLevels)) {
    tt[i] <- nleqslv::nleqslv (10., TfromEPT, jac=NULL, thetaE, pLevels[i])$x
  }
  lines (Xplot (tt, pLevels), pLevels, col='red', lty=4)
}
rMix <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1., 3., 5, seq (5, 30, by=5)) * 0.001
TfromRmix <- function (.T, .rMix, .P) {
  e <- MurphyKoop (.T, .P)
  return (.rMix - MixingRatio (e / .P))
}
for (rmix in rMix) {
  for (i in 1:length (pLevels)) {
    tt[i] <- nleqslv::nleqslv (10., TfromRmix, jac=NULL, rmix, pLevels[i])$x
  }
  lines (Xplot (tt, pLevels), pLevels, col='darkgreen', lty=2)
}
## now add Davies-Jones form
DJTfromEPT <- function (.T, .thetaE, .P) {
  L0 <- 2.56313e6
  L1 <- 1754.
  K2 <- 1.137e6
  TK <- .T + TZERO
  e <- MurphyKoop (.T, .P)
  r <- MixingRatio (e/.P) 
  CP <- SpecificHeats(0.)     # need dry-air value, don't need vector
  TL = 2840./(3.5*log(TK)-log(e)-4.805)+55.
  TDL <- TK * (1000./(.P-e))**0.2854*(TK/TL)**(0.28e-3*r)
  THETAP <- TDL * exp (r*(L0-L1*(TL-TZERO)+K2*r)/(CP[1]*TL))
  return (.thetaE - THETAP)
}
for (thetaE in ThetaE) {
  for (i in 1:length(pLevels)) {
    tt[i] <- nleqslv::nleqslv (10., DJTfromEPT, jac=NULL, thetaE, pLevels[i])$x
  }
  lines (Xplot (tt, pLevels), pLevels, col='red', lty=2, lwd=3)
}
