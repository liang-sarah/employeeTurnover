library(dplyr)
library(tidyverse)
library(ggplot2)
library(survival)
library(survminer)
library(janitor)
library(formatR)
library(knitr)

opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

turnover = turnover %>%
clean_names(parsing_option = 0)
turnover$profession[which(turnover$profession == "Finan\xf1e")] = "Finance"

turnover = turnover %>%
mutate(ageentry = age - stag/12) %>%
select(-age)

# survival object, plot
turnover.km <- survfit(Surv(stag, event) ~ 1, data = turnover)
plot(turnover.km, xlab = "Time Spent Working", ylab = "Survival Rate",
main = "Survival Function of Employee Turnover")
ggsurvplot(survfit(Surv(stag, event) ~ gender, data = turnover))

model <- coxph(Surv(stag, event) ~ gender, data = turnover)

turnover.null <- coxph(Surv(stag, event) ~ 1, data = turnover)
turnover.full <- coxph(Surv(stag, event) ~ ., data = turnover)
# iterate for optimal model that minimizes AIC
step(turnover.null, scope = list(lower = turnover.null, upper = turnover.full),
direction = "forward")


model.1 <- coxph(formula = Surv(stag, event) ~ ageentry + industry +
extraversion + greywage + traffic + coach + way + profession +
anxiety, data = turnover)
anova(model.1)

AIC(model.1)
cox.zph(model.1)

cloglog <- function(x) {
log(-log(x))
}
ggsurvplot(survfit(Surv(stag, event) ~ profession, data = turnover),
fun = "cloglog", xlab = "time", ylab = "log(-log(S))", main = "C-loglog Plot for Profession
Variable",
pch = rep(19, 2))

ggsurvplot(survfit(Surv(stag, event) ~ anxiety, data = turnover),
fun = "cloglog", xlab = "time", ylab = "log(-log(S))", main = "C-loglog Plot for Anxiety
Variable",
pch = rep(19, 2))


ggsurvplot(survfit(Surv(stag, event) ~ traffic, data = turnover),
fun = "cloglog", xlab = "time", ylab = "log(-log(S))", main = "C-loglog Plot for Traffic
Variable",
pch = rep(19, 2))


model.2 = coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + extraversion + greywage + traffic +
coach + way + anxiety, data = turnover)
anova(model.2)
AIC(model.2)

summary(model.2)$conf.int




turnover_new = turnover %>%
mutate(Great_personality = (extraversion + independ + selfcontrol +
novator) > 25) %>%
mutate(Great_personality = as.numeric(Great_personality)) %>%
mutate(anxiety_square = (anxiety - 5)ˆ2) %>%
mutate(anxiety_indicator = as.numeric(anxiety > 7)) %>%
mutate(selfcontrol_square = (selfcontrol - 5)ˆ2) %>%
mutate(selfcontrol_indicator = as.numeric(selfcontrol > 7)) %>%
mutate(extraversion_indicator = as.numeric(extraversion >
7)) %>%
mutate(extraversion_square = (extraversion - 5)ˆ2) %>%
mutate(independ_indicator = as.numeric(independ > 7)) %>%
mutate(independ_square = (independ - 5)ˆ2) %>%
mutate(novator_indicator = as.numeric(novator > 7)) %>%
mutate(novator_square = (novator - 5)ˆ2)
turnover.null <- coxph(Surv(stag, event) ~ 1, data = turnover_new)
turnover.full <- coxph(Surv(stag, event) ~ ., data = turnover_new)
step(turnover.null, scope = list(lower = turnover.null, upper = turnover.full),
direction = "forward")
model.innovation = coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + extraversion + greywage + traffic +
coach + way + anxiety_indicator + independ_indicator + selfcontrol_square,
data = turnover_new)


AIC(model.2) 
AIC(model.innovation) 

innovation.summary = summary(model.innovation)
anova(model.innovation)
innovation.summary$conf.int




model.test1 = coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + extraversion + greywage + traffic +
coach + way + independ_indicator + selfcontrol_square + anxiety,
data = turnover_new)
anova(model.test1)

model.innovation1 = coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + extraversion + greywage + traffic +
coach + way + independ_indicator + selfcontrol_square + anxiety_indicator,
data = turnover_new)
anova(model.innovation1)

model.test2 = coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + extraversion + greywage + traffic +
coach + way + selfcontrol_square + anxiety_indicator + independ,
data = turnover_new)
anova(model.test2)

model.innovation2 = coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + extraversion + greywage + traffic +
coach + way + selfcontrol_square + anxiety_indicator + independ_indicator,
data = turnover_new)
anova(model.innovation2)

model.test3 = coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + extraversion + greywage + traffic +
coach + way + anxiety_indicator + independ_indicator + selfcontrol,
data = turnover_new)
anova(model.test3)

model.innovation3 = coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + extraversion + greywage + traffic +
coach + way + anxiety_indicator + independ_indicator + selfcontrol_square,
data = turnover_new)
anova(model.innovation3)


model.int1 <- coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + greywage + traffic + coach + way +
extraversion * anxiety_indicator * independ_indicator * selfcontrol_square,
data = turnover_new)
anova(model.int1)


model.int2 <- coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + greywage + traffic + coach + way +
extraversion + anxiety_indicator + independ_indicator + selfcontrol_square +
anxiety_indicator:independ_indicator + extraversion:anxiety_indicator:independ_indicator +
anxiety_indicator:independ_indicator:selfcontrol_square +
extraversion:anxiety_indicator:independ_indicator:selfcontrol_square,
data = turnover_new)
anova(model.int1, model.int2)

anova(model.int2)


model.int3 <- coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + greywage + traffic + coach + way +
extraversion + anxiety_indicator + independ_indicator + selfcontrol_square +
21
extraversion:anxiety_indicator:independ_indicator + extraversion:anxiety_indicator:independ_indicator, data = turnover_new)
anova(model.int3, model.int1)
anova(model.int3)

model.int4 <- coxph(formula = Surv(stag, event) ~ strata(profession) +
ageentry + industry + greywage + traffic + coach + way +
extraversion + anxiety_indicator + independ_indicator + selfcontrol_square +
extraversion:anxiety_indicator:independ_indicator:selfcontrol_square,
data = turnover_new)
anova(model.int4) # all significant

anova(model.int1, model.int4) # significant model




model.int5 <- coxph(formula = Surv(stag, event) ~ strata(profession) +
greywage + ageentry * way + industry + traffic + coach +
extraversion + anxiety_indicator + independ_indicator + selfcontrol_square +
extraversion:anxiety_indicator:independ_indicator:selfcontrol_square,
data = turnover_new)
anova(model.int5)

anova(model.int4, model.int5)











