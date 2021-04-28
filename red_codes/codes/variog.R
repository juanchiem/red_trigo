gridDim <- 60
xy <- expand.grid(x=1:gridDim, y=1:gridDim)

pacman::p_load(gstat)
# Variogram model, with defined sill and range
varioMod <- vgm(psill=0.05, range=10, model='Exp')

# Set up an additional variable from simple kriging
zDummy <- gstat(formula=z~1, locations = ~x+y, dummy=TRUE, 
                beta=1, model=varioMod, nmax=20)

# Generate 2 randomly autocorrelated predictor data fields
set.seed(3)
xyz <- predict(zDummy, newdata=xy, nsim=2)

# Generate an autocorrelated response variable:
# Deterministic on predictors, with normal error
e <- rnorm(nrow(xyz), sd=.5)
xyz$resp <- .7*xyz$sim1 + 1.4*xyz$sim2 + e

What does this SAC look like?
  
library(sp)
test <- xyz
gridded(test) = ~x+y
spplot(test[1])

head(xyz)
spdf <- SpatialPointsDataFrame(xy, data=xyz)

vario <- variogram(resp~1, data=xyz, locations= ~x+y, 
                   cutoff=0.5*gridDim)

plot(vario$dist, vario$gamma)

rng <- 15
mxPosition <- floor(gridDim/rng)
keepPosition <- rng*(1:mxPosition)
keepX <- which(xy$x %in% keepPosition)
keepY <- which(xy$y %in% keepPosition)
bigGrain <- xyz[intersect(keepX, keepY),]

fmla <- as.formula(resp ~ sim1 + sim2)

lmBig <- lm(fmla, data = bigGrain)
summary(lmBig)

lmFine <- lm(fmla, data = xyz)
summary(lmFine)

cornrPosition <- 1:length(keepPosition)
keepX <- which(xy$x %in% cornrPosition)
keepY <- which(xy$y %in% cornrPosition)
cornrFine <- xyz[intersect(keepX, keepY),]
