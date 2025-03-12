## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(VizTest)
library(ggplot2)
# Datasets from
library(wooldridge)
library(carData)

# For wrangling and analysis
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(lme4)
library(marginaleffects)

## ----reg-obj, echo = TRUE, out.width = "100%", fig.cap = "Regular objects --- default output"----
#### 4.1 Regular object ####
model1 <- lm(colGPA~skipped+alcohol+PC+male+car+job20,data=gpa1)

# Parsing to viztest
viztestObj <- viztest(model1, test_level = 0.05, range_levels = c(0.25,0.99),
    level_increment = 0.01)

# Print
print(viztestObj)

## ----reg-obj-plot, echo = TRUE, fig.show="hide", fig.height=4, fig.width=6, out.width = "100%", fig.cap = "Regular objects --- default output"----
f1 <- plot(viztestObj)

## ----reg-obj-plot2, echo = TRUE, fig.height=4, fig.width=6, out.width = "100%", fig.cap = "Inferential (black) and 95% (gray) Confidence Intervals", fig.show="hide"----
reg_plot_data <- plot(viztestObj, make_plot=FALSE) %>% 
  mutate(lwr95 = est - qnorm(.975)*se, 
         upr95 = est + qnorm(.975)*se)

f2 <- ggplot(reg_plot_data, aes(y = label, x=est)) + 
  geom_linerange(aes(xmin=lwr95, xmax=upr95), color="gray75") + 
  geom_linerange(aes(xmin=lwr, xmax=upr), color="black", linewidth=3) + 
  geom_point(color="white", size=.75) + 
  geom_vline(xintercept=0, linetype=3) + 
  theme_classic() + 
  labs(x="Regression Coefficient", y="")

## ----reg-plot-both, out.width="100%", echo=FALSE, fig.cap = "Plots of Regression Output with Inferential Confidence Intervals", fig.height=3.5, fig.width=7----
(f1 + ggtitle("A) Default plot() Output") + 
    theme(plot.title.position="plot")) + 
(f2 + ggtitle("B) plot() Output with 95% CI Included") +
   theme(plot.title.position="plot"))

## ----mltlvl-obj, echo = TRUE, out.width = "100%", fig.cap = "Multilevel regression objects --- default output with all reference lines"----
#### 4.2 Multilevel regression objects ####
data(WVS, package='carData')

# Poverty variable as a scale from -1 to 1
NewWVS <- WVS %>% 
  mutate(povertyNum = as.numeric(poverty)-2) 

# The model
model2 <- lmer(povertyNum ~ age + religion + 
                 degree + gender + (1 | country), NewWVS)

# Creating custom object
## Extracting fixed effect coefficients and naming them
named_coef_vec <- fixef(model2)
coefNames <- c("(Intercept)","Age","Religious","Has a degree","Male")
names(named_coef_vec) <- coefNames

## Extracting vcov matrix
#### As matrix necessary to overwrite special object from lme4
vcov <- as.matrix(vcov(model2))

eff_vt <- make_vt_data(named_coef_vec, vcov)

# Parsing to viztest
viztestObj <- viztest(eff_vt,test_level = 0.05, 
                       range_levels = c(0.25,0.99),level_increment = 0.01)

# Print
print(viztestObj)

## ----mltlvl-obj-plot, echo = TRUE, out.width = "65%", fig.height=3.5, fig.width=5, fig.align="center", fig.cap = "Multilevel regression objects --- default output with all reference lines"----
# Plotting
plot(viztestObj,ref_lines = "all", level = "ce") +
  labs(y="", x="Fixed Effects")

## ----predprob-obj, echo = TRUE, out.width = "100%", fig.cap = "Predicted probabilities --- default output with ambiguous reference lines"----
#### 4.3 Predicted probabilities ####
data(TitanicSurvival, package="carData")
NewTitanicSurvival <- TitanicSurvival %>% 
  mutate(ageCat = case_when(age <= 10 ~ "0-10",
                            age > 10 & age <=18 ~ "11-18",
                            age > 18 & age <=30 ~ "19-30",
                            age > 30 & age <=40 ~ "31-40",
                            age > 40 & age <=50 ~ "41-50",
                            age >50 ~ "51+"))
# The model
model3 <- glm(survived~sex*ageCat+passengerClass,
              data=NewTitanicSurvival,family = binomial(link="logit")) 

# Predicted values
mes <- avg_predictions(model3, 
                variables = list(ageCat = levels(NewTitanicSurvival$ageCat),
                                 sex=levels(NewTitanicSurvival$sex)))

## ----"print-titanic-intervals-norm"-------------------------------------------
vt_titanic <- viztest(mes, 
        test_level = 0.05, 
        range_levels = c(0.25,0.99),
        level_increment = 0.01, 
        include_zero=FALSE)

vt_titanic

## ----avt-preds-plot, echo = TRUE, out.width = "65%", fig.height=3.5, fig.width=5, fig.align="center",  fig.cap = "Average Predicted Probabilities --- Plot with Normal Theory Inferential CIs"----
mes <- mes %>% 
  mutate(lwr76 = estimate - qnorm(.88)*std.error, 
         upr76 = estimate + qnorm(.88)*std.error)

ggplot(mes, aes(y = ageCat, x=estimate, xmin = lwr76, 
              xmax=upr76, colour=sex)) + 
  geom_pointrange(position = position_dodge(width=.5)) + 
  scale_colour_manual("Sex",values=c("gray50", "black")) + 
  theme_bw() + 
  labs(x="Predicted Pr(Survival)\nInferential Confidence Intervals (76%)", 
       y="Age Category")

## ----eval=TRUE,echo=T---------------------------------------------------------
ap_sim <- avg_predictions(model3, 
                variables = list(ageCat = levels(NewTitanicSurvival$ageCat),
                                 sex=levels(NewTitanicSurvival$sex))) %>% 
  inferences(method = "simulation", R=2500)

## ----echo=T-------------------------------------------------------------------
post <- t(attr(ap_sim, "posterior"))
colnames(post) <- paste0("b", 1:ncol(post))
post_vt <- make_vt_data(post, type="sim")
vt_sim <- viztest(post_vt, cifun="hdi", include_zero=FALSE)
vt_sim

## ----ave-preds-sim-plot, echo = TRUE, out.width = "65%", fig.height=3.5, fig.width=5, fig.align="center",  fig.cap = "Average Predicted Probabilities --- Plot with Simulation-based Inferential HDIs"----
hdis <- apply(post, 2, \(x)hdi(x, 0.79))
mes <- mes %>% 
  arrange(ageCat) %>% 
  mutate(lwr79 =hdis[1,], 
         upr79 = hdis[2,])

ggplot(mes, aes(y = ageCat, x=estimate, xmin = lwr79, 
              xmax=upr79, colour=sex)) + 
  geom_pointrange(position = position_dodge(width=.5)) + 
  scale_colour_manual("Sex",values=c("gray50", "black")) + 
  theme_bw() + 
  labs(x="Predicted Pr(Survival)\nInferential Highest Density Regions (79%)", 
       y="Age Category")

## ----echo=T-------------------------------------------------------------------
#### 4.4 Descriptive quantities ####
data(CES11, package="carData")
NewCES11 <- CES11 %>% 
  mutate(rel_imp=(as.numeric(importance)-1)/3) %>% 
  group_by(province) %>% 
  summarise(mean=mean(rel_imp),
            samp_var=var(rel_imp)/n())

# Creating a vtcustom object
## Vector for the means
means <- NewCES11$mean
names(means) <- NewCES11$province

vt_ces_data <- make_vt_data(means, NewCES11$samp_var)

# Passing to viztest
viztestCES <- viztest(vt_ces_data,
                      test_level = 0.05, 
                      range_levels = c(0.25,0.99),
                      level_increment = 0.01,
                      include_zero=FALSE)

# Print
viztestCES

## ----descr-obj, echo = TRUE, out.width = "50%", fig.height=3.5, fig.width=3.5, fig.align="center",  fig.cap = "Descriptive quantities --- default output with ambiguous reference lines"----
# plotting
plot(viztestCES, level = "ce", trans = \(x)x*100, ref_lines = "ambiguous")+
  labs(y="Provinces", x="Average importance given to religion") + 
  theme_bw()

