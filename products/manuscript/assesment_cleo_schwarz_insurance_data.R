# Assessment 

# Packages 
install.packages("medicaldata")
install.packages("tidyverse")
install.packages("gtsummary")
library(medicaldata)
library(tidyverse)
library(gtsummary)
library(ggplot2)

#data 
dat <- read_csv("insurance_with_date.csv")
str(dat)

insurance <- dat |> 
  select(X, age, sex, bmi, smoker, charges) |> 
  mutate(sex = factor(sex, levels=c("male", "female"), labels=c("m","f")), 
         smoker = factor(smoker))

#descriptive statistics -> continous variable -> mean median... 
summarysmoke <- insurance |>
  tbl_summary(
    by = smoker, 
    type = all_continuous() ~ "continuous2", 
    include =c("charges","bmi", "age"),
    statistic = all_continuous() ~ c("{mean} ({sd})",
                                  "{median} ({p25}, {p75})",
                                  "{min}, {max}")
  ) 
  
#QQplot - test for normality --> only within groups! und Histogramm -> log machen 
# test um zu schauen ob t test möglich ist 
insurance %>%
  ggplot(aes(sample = charges, fill = smoker, colour = smoker)) + 
  geom_qq_line(distribution = stats::qnorm) +
  geom_qq(distribution = stats::qnorm) + 
  xlab("theoretical") +
  ylab("sample") +
  theme_classic() +
  facet_wrap("sex")

#QQplot 
insurance %>%
  ggplot(aes(sample = charges_log, fill = smoker, colour = smoker)) + 
  geom_qq_line(distribution = stats::qnorm) +
  geom_qq(distribution = stats::qnorm) + 
  xlab("theoretical") +
  ylab("sample") +
  theme_classic() +
  facet_wrap("smoker")

shapiro.test(insurance$charges)

insurance |> 
  group_by(smoker) |> 
  shapiro.test(charges_log)

#histogram 
ggplot(insurance, aes(x=charges)) +
  geom_histogram(bins=40) +
  theme_classic()

# Fazit: nicht normalverteilt -> trotzdem t-test weil viele proben! 

#log t-test 
insurance$charges_log <- log(insurance$charges)

t.test(charges_log ~ smoker, data = insurance)

ggplot(insurance, aes(x=charges_log, fill = smoker)) +
  geom_histogram(bins=40) +
  theme_classic() 

# ttest charges smoker vs no smokers -> wilcoxon -> beeinflusst rauchen kosten? (wilcoxon weil nicht normalverteilt) 
wilcox.test(charges ~ smoker, data = insurance,  paired = FALSE)

# pairwise wilcoxon bei charges nicht log transormiert 

# boxplot 1 smokers vs charges
ggplot(insurance , aes(x = smoker, y = charges, fill = smoker )) + 
  geom_boxplot() +
  ggtitle(label = "Smokers vs. Charges") +
  xlab(label = "smoker") +
  ylab(label = "charges")+
  theme_classic() + theme(legend.position="bottom")

#boxplot 2 sex vs. charges 
ggplot(insurance , aes(x = sex, y = charges )) + 
  geom_boxplot() +
  theme_classic()

#boxplot smoker vs bmi (nicht nötig) 
ggplot(insurance , aes(x = smoker, y = bmi )) + 
  geom_boxplot() 



#bmi (continious) vs charges 
ggplot(insurance , aes(x = bmi, y = charges, color = smoker )) +
  geom_point(alpha = 0.7, shape = 22, size = 1.5, stroke = 1.5)

#H0 There is no difference between smokers and charges 
ind1<-insurance$smoker=yes # wie mache ich dass nur smoker?
t.test(charges~smoker, data=insurance)

#H0 there is no difference between high bmi > 25 (definition obesity) and charges 
ind<-insurance$bmi=>25 # wie mache ich bmi grösser als 25? 
t.test(charges~bmi, data=insurance, subset=ind)

#tabl summary results t-test 

lm(formula = charges ~ bmi, data = insurance)
