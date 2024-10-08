library(sf)
library(survival)
library(fmesher)
library(INLA)
library(ggplot2)
library(gridExtra)

## log(\lambda_i) = \eta_i
##  \eta_i  : linear predictor for observation i
##    \eta_i = Z_i \beta + A_i u(s_0)
##      Z_i \beta   : fixed effects
##      A_i u(s_0)  : spatial effect (mapped from s_0 to loc. i)

## Leukemia dataset 
data(Leuk)

## Set survival time as year
Leuk$time <- Leuk$time / 365.25

### D E S C R I P T I V E

## Data summary
round(sapply(Leuk[, c(1, 2, 5:8)], summary), 2)

## a visualization of the data locations and boundary
plot(nwEngland)
with(Leuk,
     points(xcoord, ycoord,
            pch = 8,
            col = cens + 1,
            cex = 0.5 + log(time)))

plot(nwEngland)
with(Leuk,
     points(xcoord, ycoord, pch = 19, cex = 0.5))

## plot the Kaplan-Mayer estimator 
km <- survfit(Surv(time, cens) ~ sex, Leuk) 
par(mar = c(2.5, 2.5, 0.5, 0.5),
    mgp = c(1.5, 0.5, 0), las = 1)
plot(km, conf.int = TRUE,
     col = 2:1, bty = "n") 
legend('topright', c('female', 'male'),
       lty = 1, col = 2:1,
  bty = "n") 

### M O D E L I N G

## model formulae without u()
form0 <- inla.surv(time, cens) ~
    sex + age + wbc + tpi 

## fitting the model without u()
fit0 <- inla(
    formula = form0,
    family = "weibullsurv",
    data = Leuk)

fit0$summary.fixed
fit0$summary.hyperpar

### SPDE model setup

## 1: build a mesh: for u(s_0), s_0 are mesh nodes

## not a very good one for this problem:
mesh0 <- fm_mesh_2d(
    nwEngland, ## where
    max.edge = c(0.05, 0.2) ## (max) triangle sizes
)

## show it
plot(mesh0, asp = 1)
plot(nwEngland, add = TRUE, lwd = 2)

## better: non-convex boundaries around
bnd1 <- fm_nonconvex_hull(
    x = nwEngland,
    convex = 0.03,
    concave = 0.1,
    resol = 50)
bnd2 <- fm_nonconvex_hull(
    x = nwEngland,
    convex = 0.25)

## build a mesh accounting for boundaries
mesh <- fm_mesh_2d(
    boundary = list(bnd1, bnd2),
    max.edge = c(0.05, 0.2),
    cutoff = 0.02)

## visualize it
plot(mesh)
plot(nwEngland, add = TRUE, lwd = 2)

## 2. build a projector matrix 
data.proj <- fm_evaluator(
    mesh = mesh,
    loc = cbind(Leuk$xcoord,
                Leuk$ycoord)
)
str(data.proj)

## 3. build the SPDE model object 
spde <- inla.spde2.pcmatern(
    mesh = mesh, ## where to build on
    prior.range = c(0.05, 0.01), ## P(range < 0.05) = 0.01
    prior.sigma = c(1, 0.01)) ## P(sigma > 1) = 0.01


## model formulae adding u(): add f() model term

##  index needed for f(): NA's as we will use A.local
Leuk$spatial <- rep(NA, nrow(Leuk))

## projector matrix (from mesh nodes to obs. locations)
form1 <- update(
    form0,
    . ~ . + f(spatial, model = spde,
              A.local = data.proj$proj$A))

## mode fit with u()
fit1 <- inla(
    formula = form1,
    family = "weibullsurv",
    data = Leuk
)

fit1$summary.fix
fit1$summary.hyperpar

## build a projector for a grid
(bbnw <- bbox(nwEngland))
r0 <- diff(range(bbnw[1, ])) / diff(range(bbnw[2, ]))
grid.proj <- fm_evaluator(
    mesh = mesh,
    xlmim = bbnw[1, ], 
    ylim = bbnw[2, ],
    dims = c(200 * r0, 200)
)

str(grid.proj)

## evaluate the field 
spat.m <- fm_evaluate(
    grid.proj,
    field = fit1$summary.random$spatial$mean)

str(spat.m)

image(grid.proj$x,
      grid.proj$y,
      spat.m,
      asp = 1)
plot(nwEngland, add = TRUE)

## improve the plot: set as NA outside the map
ov <- over(SpatialPoints(grid.proj$lattice$loc),
           nwEngland)
spat.m[is.na(ov)] <- NA

image(grid.proj$x,
      grid.proj$y,
      spat.m,
      asp = 1)
plot(nwEngland, add = TRUE)

## improve the plot
i.ok <- which(!is.na(spat.m))
xyz <- data.frame(
    x = grid.proj$lattice$loc[i.ok, 1],
    y = grid.proj$lattice$loc[i.ok, 2],
    z = as.vector(spat.m)[i.ok]
)

gg0 <- ggplot() + theme_minimal() 

nwEngland_geom <- list()
for (i in 1:length(nwEngland@polygons[[1]]@Polygons)){
  nwEngland_geom <- c(nwEngland_geom, list(nwEngland@polygons[[1]]@Polygons[[i]]@coords))
  
}

gg.m <- gg0 +
    geom_contour_filled(
        data = xyz, 
        aes(x = x, y = y, z = z)) + 
        geom_sf(data = st_as_sf(st_sfc(st_polygon(nwEngland_geom))),
          fill = 'transparent', lwd = 0.3, col = "black") 
gg.m

xyz$sd <- as.vector(
    fm_evaluate(
        grid.proj,
        field = fit1$summary.random$spatial$sd
    ))[i.ok]

summary(xyz)

gg.sd <- gg0 + 
    geom_contour_filled(
        data = xyz, 
        aes(x = x, y = y, z = sd)) + 
  geom_sf(data = st_as_sf(st_sfc(st_polygon(nwEngland_geom))),
          fill = 'transparent', lwd = 0.3, col = "black") 

## visualize the spatial effect
grid.arrange(
    gg.m,
    gg.sd,
    nrow = 1
)
    


## Survival case 2:
cph.fit <- coxph(
    formula = Surv(time, cens) ~ sex + age + wbc + tpi, 
    data = Leuk
)
cph.fit


## using inla now
cph.inla <- inla(
    formula = form0,
    family = 'coxph', 
    data = Leuk[, c(1,2, 5:8)]
)

## expand the data (internally done in inla)
data.expanded <- inla.coxph(
    formula = form0,
    data = Leuk, 
    control.hazard = list(n.intervals = 25))
str(data.expanded)

## projector for the expanded data 
cph.A <- fm_evaluator(
    mesh = mesh,
    loc = cbind(data.expanded$data$xcoord,
                data.expanded$data$ycoord)
)

## update formula for the expanded data to add u()
cph.formula <- update(
    data.expanded$formula,
    . ~ . + f(spatial, model = spde,
              A.local = cph.A$proj$A)
)

## fit the Cox PH model with spatial effect 
cph.res <- inla(
    cph.formula,
    family = 'Poisson',
    data = c(data.expanded$data,
             data.expanded$data.list),
    E = data.expanded$E
)

cph.res$summary.hy

## fixed effect comparison
list(
    surv = coef(summary(fit0))[, c(1,3)], 
    r0 = cph.inla$summary.fixed[-1, 1:2], 
    r1 = cph.res$summary.fixed[-1, 1:2]
)


## spatial effect: similar as for the Weibull
cor(
    fit1$summary.random$spatial[c("mean", "sd")],
    cph.res$summary.random$spatial[c("mean", "sd")]
)

