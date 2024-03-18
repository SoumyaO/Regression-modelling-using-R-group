# Import ----
library(MASS)
library(glmnet)
library(mgcv)
# Read data ----
data = read.table('Visits.txt', header=TRUE)

# EDA ----
hist(data$visits)

# Modelling ----

# Model 1 - Basic Poisson ----
poisson_all = glm('visits ~ .', data=data, family='poisson')
summary(poisson_all) 

# Residual deviance compares our model with the saturated model. Null deviance
# with the null model. Only used for poisson because it doesn't estimate the
# dispersion
pchisq(4380, 5178, lower.tail=F)
# Pvalue < 0.05: reject the null hypothesis
# the null hypothesis is the model I'm fitting is a reasonable model.
# alternative hypothesis: my model is not a good model
# Pvalue > 0.05: cannot reject the null hypothesis
# -> our model is good.

# -> null deviance should always be > than residual deviance and its pvalue
# should be <0.05 reject the null --> The model I'm fitting (the null model)
# is a bad model

# The residual dev should be < than the null deviance because we want our model
# to be better than the null model

# It would seem to indicate that almost all regressors are highly significant.
# Let’s also check the residual analysis:
png('./plots/Linh_model1/summary.png', width=500, height=400)
par(mfrow=c(2,2))
plot(poisson_all)
dev.off()

# We want the residuals to be scattered around 0 (normally distributed). Which
# is not the case --> there is still patterns to be captured. Same for scale
# location plot.
# Normal QQ plot: normality assumption does not hold. so it means the
# distribution I'm using is not a reasonable one.
# contour plot: if the value is outside of the countour plot -> potential
# inlfuential points. We can consider remove them.

par(mfrow=c(1,1))
plot(residuals(poisson_all) - fitted(poisson_all))
abline(0, 0, lty=2)

# model 2 - quasi poisson ----
quasipoisson_all <- glm('visits ~ .', data=data, family='quasipoisson')
summary(quasipoisson_all)

par(mfrow=c(2,2))
plot(quasipoisson_all)

# which variables have now become insignificant?
anova(quasipoisson_all, test='Chisq')
# terms added sequentially --> adding age to the model containing gender
# significantly improved the model

# (Dispersion parameter for quasipoisson family taken to be 1.327571
# overdispersion

# model 3 - NB ----
nb_all <- glm.nb('visits ~ .', data=data)
summary(nb_all)

par(mfrow=c(2,2))
plot(nb_all)

# Theta = 0.93 --> slight underdispersion

# (Dispersion parameter for Negative Binomial(0.9302) family taken to be 1)
# Close to 1. slightly underdispersion. but fulfill. variance increase by 0.93
# for every unit increase in the mean

# Model 4 - Add factors to NB ----
nb_all_f <- update(nb_all, '~ . + as.factor(illness) - illness', data=data)
summary(nb_all_f)
anova(nb_all, nb_all_f, test='Chisq')
# Theta = 1.02 --> No over dispersion

pchisq(3023.5, 5174, lower.tail=F)
# Model is a good model (pvalue > 0.05)

par(mfrow=c(2,2))
plot(nb_all_f)

anova(nb_all, nb_all_f, test='Chisq')

# NB tuned
nb_tuned_f <- update(nb_all_f, '~ . - income - private - freerepat - nchronic - lchronic', data=data)
summary(nb_tuned_f)
plot(nb_tuned_f)

AIC(nb_tuned_f)

# Still not good. Lack of fit.
# --> Can change the distribution

# Model 3B - Add factors to poisson ----

# Adding health factor does not improve the model significantly
quasi_poisson_f = update(quasipoisson_all, '~ . + as.factor(health) - health')
summary(quasi_poisson_f)
anova(quasipoisson_all, quasi_poisson_f, test='Chisq')

# Adding age factor does not improve the model significantly
quasi_poisson_f = update(quasipoisson_all, '~ . + as.factor(age) - age')
summary(quasi_poisson_f)
anova(quasipoisson_all, quasi_poisson_f, test='Chisq')

# Adding illness factor does improve the model significantly
quasi_poisson_f = update(quasipoisson_all, '~ . + as.factor(illness) - illness')
summary(quasi_poisson_f)

# Coefficient gendermale = -0.139713
# (exp(-0.139713) - 1) * 100 = -13.03922
# --> male, then the avg number of visits are 13% lower than female

anova(quasipoisson_all, quasi_poisson_f, test='Chisq')
plot(quasi_poisson_f)

par(mfrow=c(1,1))
plot(residuals(quasi_poisson_f) - fitted(quasi_poisson_f))
abline(0, 0, lty=2)

# Model 5 - TPRS ----

model_gam = gam(visits ~ private + freepoor + freerepat + nchronic + lchronic + gender
    + s(illness, bs='tp', k=4)
    + s(age, bs='tp', k=4)
    + s(income, bs='tp', k=4)
    + s(reduced, bs='tp', k=4)
    + s(health, bs='tp', k=4),
    data=data,
    family='nb',
)

summary(model_gam)
gam.check(model_gam)
AIC(model_gam)
model_gam$sp

model_gam$null.deviance
model_gam$deviance

plot(model_gam, seWithMean = TRUE, shade = TRUE, scale = 0, pages=1)

# Model 6 - TPRS tuned ----

model_gam2 = gam(visits ~ freepoor + gender
                + s(illness, bs='tp', k=4)
                + age
                + s(income, bs='tp', k=4)
                + s(reduced, bs='tp', k=4)
                + health,
                data=data,
                family='nb',
)

summary(model_gam2)
AIC(model_gam2)
gam.check(model_gam2)
model_gam2$sp

anova(model_gam, model_gam2, test='Chisq')

plot(model_gam2, seWithMean = TRUE, shade = TRUE, scale = 0, pages=1)


# As for the interpretation, we can see that, for example, the ratio of the averages number of trips is 1.6
# for one unit increase in quality ranking, holding constant the values of the other explanatory variables
# in the model. Also, the expected number of trips for the individual engaged in water-skiing is 1.5
# times the expected number of trips for the individual not engaged in water-skiing, holding constant
# the values of the other explanatory variables in the model. This means that the predicted number
# of trips for individuals engaged in water-skiing is 50% more than that for individuals not engaged in
# water-skiing.

