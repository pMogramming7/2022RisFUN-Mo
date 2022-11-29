xaringanExtra::use_clipboard()







library(Hmisc)
library(corrplot)
library(MASS)
library(car)
library(interactions)
library(yarrr)
library(tidyr)
library(readr)
library(lme4)
library (lmerTest)
library(nlme)
library(gvlma)
library(simpleboot)
library(DAAG)
library(bootstrap)
library(MBESS)
library(leaps)
library(sandwich)
library(dplyr)

students<-read.table('D:/R_WD/Git linked/2022RisFUN-Mo/Statistics/Data-statistic/students.txt',header=T, sep="\t", dec='.') 
lm.cat<-lm(shoesize~gender, data=students)
anova(lm.cat)

iris.lm<-lm(Petal.Width ~ Species, data=iris)
summary(iris.lm)
anova(iris.lm)

pirateplot(formula = time ~ cleaner + type,
           data = poopdeck,
           ylim = c(0, 150),
           xlab = "Cleaner",
           ylab = "Cleaning Time (minutes)",
           main = "poopdeck data",
           back.col = gray(.97), 
           cap.beans = TRUE, 
           theme = 2)

## # Step 1: Create an aov object
## mod.aov <- aov(formula = y ~ x1 + x2 + ..., data = data)

## # Step 2: Look at a summary of the aov object
## summary(mod.aov)

## # Step 3: Calculate post-hoc tests
## TukeyHSD(mod.aov)

## # Step 4: Look at coefficients
## mod.lm <- lm(formula = y ~ x1 + x2 + ..., data = data)
## summary(mod.lm)

pirateplot(time ~ cleaner, 
                  data = poopdeck, 
                  theme = 2, 
                  cap.beans = TRUE,
                  main = "formula = time ~ cleaner")

# Step 1: aov object with time as DV and cleaner as IV
cleaner.aov <- aov(formula = time ~ cleaner, data = poopdeck)

# Step 2: Look at the summary of the anova object
summary(cleaner.aov)

# Step 3: Conduct post-hoc tests
TukeyHSD(cleaner.aov)

# Step 4: Create a regression object
cleaner.lm <- lm(formula = time ~ cleaner, data = poopdeck)
summary(cleaner.lm)

# Step 1: aov object with time as DV and cleaner and type as IV
cleaner.type.aov <- aov(formula = time ~ cleaner + type, data = poopdeck)

# Step 2: Get ANOVA table with summary()
summary(cleaner.type.aov)

# Step 3: Conduct post-hoc tests
TukeyHSD(cleaner.type.aov)

# Step 4: Look at regression coefficients
cleaner.type.lm <- lm(formula = time ~ cleaner + type, data = poopdeck)
summary(cleaner.type.lm)

# Step 1: Create ANOVA object with interactions
cleaner.type.int.aov <- aov(formula = time ~ cleaner * type, data = poopdeck)
# Step 2: Look at summary table
summary(cleaner.type.int.aov)

# Step 4: Calculate regression coefficients
cleaner.type.int.lm <- lm(formula = time ~ cleaner * type, data = poopdeck)
summary(cleaner.type.int.lm)

cat_plot(cleaner.type.int.lm, pred = cleaner, modx = type, interval = TRUE)

# Step 1: Calculate regression object with lm()
time.lm <- lm(formula = time ~ type + cleaner, data = poopdeck)

# Type I ANOVA - aov()
time.I.aov <- aov(time.lm)
# or anova(time.lm)

# Type II ANOVA - Anova(type = 2)
time.II.aov <- car::Anova(time.lm, type = 2)

# Type III ANOVA - Anova(type = 3)
time.III.aov <- car::Anova(time.lm, type = 3)

# Are observations in the poopdeck data balanced?
with(poopdeck, table(cleaner, type)) 

names(cleaner.type.int.aov)

poopdeck$int.fit <- cleaner.type.int.aov$fitted.values
poopdeck$me.fit <- cleaner.type.aov$fitted.values

mean(abs(poopdeck$int.fit - poopdeck$time))
mean(abs(poopdeck$me.fit - poopdeck$time))

shapiro.test(cleaner.type.int.aov$residuals) # test our residuals vs a normal distribution
bartlett.test(cleaner.type.int.aov$residuals ~ interaction(cleaner, type), data = poopdeck) # test variance of our residuals in the different groups.

AWARD <- read.csv("D:/R_WD/Git linked/2022RisFUN-Mo/Statistics/Data-statistic/poisson_sim.csv")
AWARD <- within(AWARD,{
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic","Vocational"))
  id <- factor(id)
})
summary(AWARD)

AWARD.hist <- ggplot(AWARD, aes(num_awards, fill=prog)) +
  geom_histogram(binwidth = .5,  position="dodge") 

AWARD.hist

summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=AWARD))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
"Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
LL = coef(m1) - 1.96 * std.err,
UL = coef(m1) + 1.96 * std.err)
r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
  AWARD = pchisq(deviance, df.residual, lower.tail=FALSE)))

## calculate and store predicted values
AWARD$phat <- predict(m1, type="response")

## order by program and then by math
AWARD <- AWARD[with(AWARD, order(prog, math)), ]

## create the plot
ggplot(AWARD, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")

Weevil_damage <- read.csv("D:/R_WD/Git linked/2022RisFUN-Mo/Statistics/Data-statistic/Weevil_damage.csv")
Weevil_damage$block <- as.factor(Weevil_damage$block) # Making block a factor
weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage)
summary(weevil.m)

rairuoho<-read.table('D:/R_WD/Git linked/2022RisFUN-Mo/Statistics/Data-statistic/rairuoho.txt',header=T, sep="\t", dec=".")
rairuoho$ID<-rownames(rairuoho)
rai<-rairuoho %>% pivot_longer(day3:day8, names_to = "day", values_to = "length")
rai$day<-parse_number(rai$day)
rai


ggplot(rai, aes(x = day, y = length)) +
  geom_point() +
  scale_x_continuous(breaks = 0:9)+
  facet_wrap(~ID)

lmer.rai<-lmer(length ~ day + (1|ID), data=rai, REML=TRUE) # REML default
summary(lmer.rai)


newdata <- crossing(ID = rai %>% pull(ID),
  day = 3:8)

head(newdata)

newdata2 <- newdata %>%
  mutate(length = predict(lmer.rai, newdata))

# plot
ggplot(rai, aes(x = day, y = length)) +
  geom_line(data = newdata2,
            color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~ID) +
  labs(y = "Length", x = "Days")

anova(lmer.rai)

rand(lmer.rai)

lme.rai = lme(length ~ day, random=~1|ID, data=rai)
summary(lme.rai)

lm.rai<-lm(length ~ day, data= rai)
anova (lme.rai, lm.rai)

lmer.rai<-lmer(length ~ day + (day|ID), data=rai, REML=TRUE) # REML default
summary(lmer.rai)


newdata <- crossing(ID = rai %>% pull(ID),
  day = 3:8)

head(newdata)

newdata2 <- newdata %>%
  mutate(length = predict(lmer.rai, newdata))

# plot
ggplot(rai, aes(x = day, y = length)) +
  geom_line(data = newdata2,
            color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~ID) +
  labs(y = "Length", x = "Days")
