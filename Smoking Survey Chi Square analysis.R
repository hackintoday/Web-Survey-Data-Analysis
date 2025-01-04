library(tidyverse)
library(writexl)
library(readxl)
library(survey)
library(srvyr)

data <- read_excel("Smoker Survey R.xlsx")

unique(data$Marriage)
unique(data$Income)

data <- data |> 
  mutate(age_cat = case_when(
    data$Age >= 19 & data$Age <= 29  ~ "19-29",
    data$Age >= 30 & data$Age <= 39  ~ "30-39",
    data$Age >= 40 & data$Age <= 49  ~ "40-49",
    data$Age >= 50 & data$Age <= 59  ~ "50-59",
    data$Age >= 60   ~ "60-"))

data <- data |>
  mutate(marr_cat = case_when(
    data$Marriage == "기혼"|data$Marriage == "사별"|data$Marriage == "동거"|data$Marriage == "사실혼"|data$Marriage == "별거" ~ "결혼함",
    data$Marriage == "미혼"|data$Marriage == "이혼"|data$Marriage == "기타" ~ "결혼안함",
    data$Marriage == 99 | data$Marriage == 98 ~ NA
  ))

data <- data |>
  mutate(inc_cat = case_when(
    data$Income == "소득 없음" ~ "None",
    data$Income == "100만 원 미만" ~ "< 100",
    data$Income == "100만 원 이상 200만 원 미만" ~ "100-200",
    data$Income == "200만 원 이상 300만 원 미만" ~ "200-300",
    data$Income == "300만 원 이상 400만 원 미만" ~ "300-400",
    data$Income == "400만 원 이상 500만 원 미만" ~ "400-500",
    data$Income == "500만 원 이상 600만 원 미만" ~ "500-600",
    data$Income == "600만 원 이상 700만 원 미만" ~ "600-700",
    data$Income == "700만 원 이상" ~ ">= 700",
    data$Income == 99 | data$Income == 98 ~ NA
  ))

data <- data |>
  mutate(inc_cat = case_when(
    data$Income == "소득 없음" ~ 0,
    data$Income == "100만 원 미만" ~ 1,
    data$Income == "100만 원 이상 200만 원 미만" ~ 2,
    data$Income == "200만 원 이상 300만 원 미만" ~ 3,
    data$Income == "300만 원 이상 400만 원 미만" ~ 4,
    data$Income == "400만 원 이상 500만 원 미만" ~ 5,
    data$Income == "500만 원 이상 600만 원 미만" ~ 6,
    data$Income == "600만 원 이상 700만 원 미만" ~ 7,
    data$Income == "700만 원 이상" ~ 8,
    data$Income == 99 | data$Income == 98 ~ NA
  ))


marr <- data[!is.na(data$marr_cat),]
inc <- data[!is.na(data$inc_cat),]

#survey design object
des <- data |> 
  as_survey_design(id = 1, weights = Weights)

marr_des <- marr |> 
  as_survey_design(id = 1, weights = Weights)

inc_des <- inc |>
  as_survey_design(id = 1, weights= Weights)


des |> group_by(age_cat, Smoker) |>
  summarize(proportion = survey_mean(), total = survey_total())


design <- svydesign(id = ~1, weights = ~Weights, data = data)

svytable(~Smoker+age_cat,des)
prop.table(svytable(~age_cat+Smoker,des), margin=1)

#Regular proportion
prop.table(table(data$age_cat,data$Smoker), margin=1)
table(marr$marr_cat,marr$Smoker)
prop.table(table(marr$marr_cat,marr$Smoker), margin=1)
table(inc$inc_cat,inc$Smoker)
prop.table(table(inc$inc_cat,inc$Smoker), margin=1)

#Regular chi-squared
chisq.test(table(data$age_cat, data$Smoker))
chisq.test(table(marr$marr_cat,marr$Smoker))
chisq.test(table(inc$inc_cat,inc$Smoker))


svyby(~age_cat, ~Smoker, des, svytotal)
svyby(~Smoker, ~age_cat, des, svymean)
svychisq(~Smoker+age_cat, des)

#Marriage
svyby(~marr_cat, ~Smoker, marr_des, svytotal)
svyby(~Smoker, ~marr_cat, marr_des, svymean)
svychisq(~Smoker+marr_cat, marr_des)

#income
svyby(~inc_cat, ~Smoker, inc_des, svytotal)
svyby(~Smoker, ~inc_cat, inc_des, svymean)
svychisq(~Smoker+inc_cat, inc_des)



svytable(~Electronic+age_cat,des)
svychisq(~Electronic+age_cat, des)



#Daily Smokers plotted against age 
smokerdata = data[data$Dailysmoker == 1,]
#Smoker below 40
smokerdata = data[data$Dailysmoker == 1 & data$Age <= 40,]
#Smoker above 40
smokerdata = data[data$Dailysmoker == 1 & data$Age > 40,]

model <- lm(CigNum~Age, data=smokerdata)
summary(model)
anova(model)
plot(smokerdata$Age, smokerdata$CigNum, xlab = "age", ylab = "No. of Cigarettes")

n = length(smokerdata$CigNum)
plot(100*(1:n)/(n), sort(smokerdata$CigNum),
     main = "Visualizing Percentiles",
     xlab = "Percentile",
     ylab = "No. of Cigarettes")

res <- resid(model)
plot(fitted(model), res, xlab = "x_1", ylab = "residuals")
abline(0,0)
qqnorm(res)
qqline(res)
plot(density(res))
#weighted linear regression
smoker_des <- smokerdata |> as_survey_design(id = 1, weights = Weights)
fit <- svyglm(CigNum~Age, family=gaussian(), design=smoker_des)
round(
  cbind(
  summary(fit, df.resid = degf(fit$survey.design))$coef,
  confint(fit, ddf      = degf(fit$survey.design))
  )
,4)
car::Anova(fit, type = 3, test.statistic = "F", 
           error.df = degf(fit$survey.design))
svyplot(CigNum~Age, smoker_des, xlab= "age", ylab = "No. of cigs per day")
abline(svyglm(CigNum~Age, design=smoker_des), col = 'red', lwd = 2)
legend("topright", "Regression line",
       title = "Weighted estimate",
       lty = 1, col = 'red', bty = "n", seg.len = 5)


#cigarettes
smokerdata = data[data$Dailysmoker == 1 & !is.na(data$inc_cat),]
plot(smokerdata$inc_cat, smokerdata$CigNum)

smoker_des <- smokerdata |> as_survey_design(id = 1, weights = Weights)
fit <- svyglm(CigNum~inc_cat, family=gaussian(), design=smoker_des)
round(
  cbind(
    summary(fit, df.resid = degf(fit$survey.design))$coef,
    confint(fit, ddf      = degf(fit$survey.design))
  )
  ,4)

svyplot(CigNum~inc_cat, smoker_des,  basecol=function(df){c("red","blue")[ifelse(df$Sex=="남성", 1, 2)]},
        main = "weighted bubble plot and regression", xlab= "Income Category", ylab = "No. of cigarettes")
abline(svyglm(CigNum~inc_cat, design=smoker_des), col = 'red', lwd = 2)
legend("topleft",col=c("red","blue"), pch=19, legend=c("남성", "여성"))
legend("topright", "Regression line",
       title = "Weighted estimate",
       lty = 1, col = 'red', bty = "n", seg.len = 5)



