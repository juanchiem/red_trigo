"https://bwk.kuleuven.be/geostatistics/Module3.R"
"https://gwenantell.com/exploring-spatial-autocorrelation-in-r/"

layout <- data.frame(
  plot = c(1L, 3L, 5L, 7L, 8L, 11L, 12L, 15L, 16L, 17L, 18L, 22L, 23L,
           26L, 28L, 30L, 31L, 32L, 35L, 36L, 37L, 39L, 40L, 42L),
  level = c(0L, 10L, 1L, 4L, 10L, 0L, 4L, 10L, 0L, 4L, 0L, 1L, 0L, 10L,
            1L, 10L, 4L, 4L, 1L, 1L, 1L, 0L, 10L, 4L),
  response = c(5.93, 5.16, 5.42, 5.11, 5.46, 5.44, 5.78, 5.44, 5.15, 5.16,
               5.17, 5.82, 5.75, 4.48, 5.25, 5.49, 4.74, 4.09, 5.93, 5.91,
               5.15, 4.5, 4.82, 5.84),
  x = c(0, 0, 0, 3, 3, 3, 3, 6, 6, 6, 6, 9, 9, 12, 12, 12, 15, 15, 15,
        15, 18, 18, 18, 18),
  y = c(0, 10, 20, 0, 5, 20, 25, 10, 15, 20, 25, 15, 20, 0, 15, 25, 0,
        5, 20, 25, 0, 10, 20, 25))

layout
library(nlme)

fmla <- as.formula(response ~ level)
lmBig <- lm(fmla, data = bigGrain)
summary(lmBig)


m0 <- gls(response ~ level, data = layout)  
plot(Variogram(m0, form=~x+y))
m1 <- update(m0, corr=corSpher(c(15, 0.25), form=~x+y, nugget=TRUE))
anova(m0, m1)
model <- lm(response ~ level, data = layout)      
model2 <- update(model, .~.+x*y)

m0b <- update(m0, method="ML")
m1b <- update(m1, method="ML")
m2b <- update(m0b, .~x*y)
anova(m0b, m1b, m2b, test=FALSE)
