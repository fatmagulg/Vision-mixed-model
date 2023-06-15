library(lme4)
library(lattice) # for xyplot
library(faraway)

data(vision)

head(vision, 9)
unique(vision$subject)
# There are seven subjects and 4 power levels. Each subject had alll power levels applied to them twice
# So there are 8 observations per subject

# Create a numerical power variable to visualize how the acuity changes with increasing power:
vision$npower <- rep(1:4,14)

head(vision)

xyplot(acuity~npower|subject, data=vision, 
       type="l",groups=eye, lty=1:2,layout=c(4,2))
# The plot shows no apparent trend between left and right eyes.
# Individual #6 has large difference between the two eyes. The third measurement on the left eye of 
# individual #6 (solid line) looks like it could be an error.

# Fit a mixed model to the data with power as a factor and interaction between subject and eye
m1 <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), data = vision, REML = TRUE)
summary(m1)


# Correlation between measurements on the same subject:
4.64^2/(4.64^2+3.2^2+4.07^2)
# [1] 0.4454292
# Correlation between measurements on the same eye:
(4.64^2+3.21^2)/(4.64^2+3.2^2+4.07^2)
# [1] 0.6586124
# Stronger correlation between observations on the same eye
# than between the left and right eyes of the same subject.

# Diagnostic plots
# Residuals vs fitted:
plot(resid(m1)~fitted(m1),xlab="Fitted",ylab="Residuals")
abline(h=0)
# another way to plot residuals vs fitted

# QQ plot of the random effects for eyes
qqnorm(ranef(m1)$"subject:eye"[[1]],main="")
qqline(ranef(m1)$"subject:eye"[[1]])
# Outlier for subject #6
