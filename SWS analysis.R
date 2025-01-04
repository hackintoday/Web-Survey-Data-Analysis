library(tidyverse)
library(writexl)
library(readxl)
library(survey)
library(srvyr)

data <- read_excel("C:/Users/aquil/Desktop/SWS data.xlsx", sheet = "데이터(1532명)")
data$post_B_final_wt

#Climate change response
data <- data |>
  mutate(above70 = case_when(
    data$Q32 >= 70 ~ 1,
    data$Q32 < 70 ~ 0
  ))

data <- data |>
  mutate(above80 = case_when(
    data$Q32 >= 80 ~ 1,
    data$Q32 < 80 ~ 0
  ))


data <- data |>
  mutate(at100 = case_when(
    data$Q32 == 100 ~ 1,
    data$Q32 < 100 ~ 0
  ))


data <- data |>
  mutate(at0 = case_when(
    data$Q32 == 0 ~ 1,
    data$Q32 > 0 ~ 0
  ))


des <- data |> 
  as_survey_design(id = 1, weights = post_B_final_wt)

mean(data$Q32)
prop.table(table(data$above70))
prop.table(table(data$above80))
prop.table(table(data$at100))
prop.table(table(data$at0))


des <- data |> 
  as_survey_design(id = 1, weights = post_B_final_wt)

svymean(~Q32, des)
svymean(~above70, des)
svymean(~above80, des)
svymean(~at100, des)
svymean(~at0, des)
subdes <- subset(des, above70 == 1)
svymean(~Q32, subdes)
subdes <- subset(des, above80 == 1)
svymean(~Q32, subdes)



#tabacco smoker indicator
data <- data |>
  mutate(Q22 = case_when(
    data$Q22 == "예" ~ 1,
    data$Q22 == "아니오" ~ 0))

#Telephone phobia questions conversions
data <- data |>
  mutate(ContactP = as.factor(case_when(
    data$`Q25_1)` == "모르는 전화번호이어도 일단 온 전화는 받는다." ~ "Likely to contact",
    is.na(data$`Q25_1)`) & data$`Q25-1`=="예"~ "Highly unlikely to contact",
    is.na(data$`Q25_1)`) & data$`Q25-1`=="아니오"~ "Unlikely to contact"
      )))


data <- data |> 
  mutate(Q28_cat = case_when(
    data$Q28 == "미혼으로 배우자가 없다"~ "Single",
    data$Q28 == unique(data$Q28)[2]| data$Q28 == unique(data$Q28)[4]  ~ "Married",
    data$Q28 == unique(data$Q28)[3] ~ "Divorced",
    data$Q28 == unique(data$Q28)[5]  ~ "Widowed"
  ))

unique(data$Q28_cat)
unique(data$Q28)

#Q12 피운 담배량. Q12 is filter for Q12-1, which does not include non smokers
data <- data |> 
  mutate(Q12_fix = if_else(is.na(`Q12-1`), Q12, `Q12-1`))
data$Q12_fix


#Q15 음주. Q15 is filter for Q15-1, which does not include non drinkers
data <- data |> 
  mutate(Q15_fix = if_else(is.na(`Q15-1`), "살아오면서 1잔 이상", `Q15-1`))
data$Q15_fix


data <- data |> 
  mutate(age_cat = case_when(
    data$Q4 >= 19 & data$Q4 <= 29  ~ "19-29",
    data$Q4 >= 30 & data$Q4 <= 39  ~ "30-39",
    data$Q4 >= 40 & data$Q4 <= 49  ~ "40-49",
    data$Q4 >= 50 & data$Q4 <= 59  ~ "50-59",
    data$Q4 >= 60  ~ "60-"   
    ))

unique(data$Q29)
data <- data |> 
  mutate(Q26_cat = case_when(
    data$Q26 == "무학" | data$Q26 == "서당/한학" | data$Q26 == "중학교" | data$Q26 == "초등학교" | data$Q26 == "고등학교"  ~ "고졸 이하",
    data$Q26 == "2년/3년제 대학"  ~ "2년/3년제 대학",
    data$Q26 == "4년제 대학"  ~ "4년제 대학",
    data$Q26 == "대학원 이상"  ~ "대학원 이상"
  ))

data <- data |> 
  mutate(Q26_cat = case_when(
    data$Q26 == "무학" | data$Q26 == "서당/한학" | data$Q26 == "중학교" | data$Q26 == "초등학교" | data$Q26 == "고등학교"  ~ "고졸 이하",
    data$Q26 == "2년/3년제 대학"  ~ "2년/3년제 대학",
    data$Q26 == "4년제 대학"  ~ "4년제 대학",
    data$Q26 == "대학원 이상"  ~ "대학원 이상"
  ))

#Check for sum
sum(data$ContactP == "Highly unlikely to contact")
sum(data$ContactP == "Unlikely to contact")
sum(data$ContactP == "Likely to contact")

#division works row-wise not column-wise as required here
t(t(as.matrix(table(data$age_cat, data$ContactP)))/colSums(table(data$age_cat, data$ContactP)))
#alternatively A2 / colSums(A2)[col(A2)] * 100
#col(A2) returns a matrix representation with each element's column number
colSums(table(data$age_cat, data$ContactP))

#Regular chi-squared
chisq.test(table(data$age_cat, data$ContactP))

svyby(~age_cat, ~ContactP, des, svytotal)
svyby(~age_cat, ~ContactP, des, svymean)
svychisq(~age_cat+ContactP, des)


des <- data |> 
  as_survey_design(id = 1, weights = post_B_final_wt)

prop.table(table(data$ContactP))
sqrt(prop.table(table(data$ContactP))/(1-prop.table(table(data$ContactP)))/1532)

#Filter by contact likelihood
HC <- data[data$ContactP == "Highly unlikely to contact",]
UC <- data[data$ContactP == "Unlikely to contact",]
LC <- data[data$ContactP == "Likely to contact",]
prop.table(table(HC$age_cat))
sqrt(prop.table(table(HC$age_cat))/(1-prop.table(table(HC$age_cat)))/157)

prop.table(table(UC$age_cat))
sqrt(prop.table(table(UC$age_cat))/(1-prop.table(table(UC$age_cat)))/659)

prop.table(table(LC$age_cat))
sqrt(prop.table(table(LC$age_cat))/(1-prop.table(table(LC$age_cat)))/716)

des <- HC |> 
  as_survey_design(id = 1, weights = post_B_final_wt)
svymean(~age_cat, des)

des <- UC |> 
  as_survey_design(id = 1, weights = post_B_final_wt)
svymean(~age_cat, des)

des <- LC |> 
  as_survey_design(id = 1, weights = post_B_final_wt)
svymean(~age_cat, des)


#Filter chisquare
data <- data |>
  mutate(ContactP = as_factor(case_when(
    is.na(data$`Q25_1)`) & data$`Q25-1`=="예"~ "Highly unlikely to contact",
    is.na(data$`Q25_1)`) & data$`Q25-1`=="아니오"~ "Unlikely to contact"
  )))
data <- data |>
  mutate(ContactP = as.factor(case_when(
    data$`Q25_1)` == "모르는 전화번호이어도 일단 온 전화는 받는다." ~ "Likely to contact",
    is.na(data$`Q25_1)`) & data$`Q25-1`=="예"~ "Highly unlikely to contact"
    )))
data <- data |>
  mutate(ContactP = as.factor(case_when(
    data$`Q25_1)` == "모르는 전화번호이어도 일단 온 전화는 받는다." ~ "Likely to contact",
    is.na(data$`Q25_1)`) & data$`Q25-1`=="아니오"~ "Unlikely to contact"
  )))
Filtered <- data[data$ContactP == "Highly unlikely to contact"|data$ContactP == "Unlikely to contact",]
Filtered <- data[data$ContactP == "Highly unlikely to contact"|data$ContactP == "Likely to contact",]
Filtered <- data[data$ContactP == "Unlikely to contact"|data$ContactP == "Likely to contact",]
Filtered<-Filtered[!is.na(Filtered$post_B_final_wt),]
t(t(as.matrix(table(Filtered$age_cat, Filtered$ContactP)))/colSums(table(Filtered$age_cat, Filtered$ContactP)))
chisq.test(table(Filtered$age_cat, Filtered$ContactP))
des <- Filtered |> 
  as_survey_design(id = 1, weights = post_B_final_wt)
svyby(~age_cat, ~ContactP, des, svytotal)
svyby(~age_cat, ~ContactP, des, svymean)
svychisq(~age_cat+ContactP, des)

#amalgamated filter
data <- data |>
  mutate(ContactP = as.factor(case_when(
    data$`Q25_1)` == "모르는 전화번호이어도 일단 온 전화는 받는다." ~ "Likely to contact",
    is.na(data$`Q25_1)`) & data$`Q25-1`=="예"~ "Highly unlikely/Unlikely to contact",
    is.na(data$`Q25_1)`) & data$`Q25-1`=="아니오"~ "Highly unlikely/Unlikely to contact"
  )))

t(t(as.matrix(table(data$Q28_cat, data$ContactP)))/colSums(table(data$Q28_cat, data$ContactP)))
colSums(table(data$Q28_cat, data$ContactP))
chisq.test(table(data$Q28_cat, data$ContactP))

des <- data |> 
  as_survey_design(id = 1, weights = post_B_final_wt)

svyby(~Q28_cat, ~ContactP, des, svytotal)
svyby(~Q28_cat, ~ContactP, des, svymean)
svychisq(~Q28_cat+ContactP, des)



#tapply(data$ContactP, function(x){prop <- prop.table(table(x))
#SE <- sqrt(prop*(1-prop)/length(x))})
#list(prop=prop, SE=SE)


design <- as_(id = ~1, weights = ~post_B_final_wt, data = data)

svymean(~ContactP, des)
confint(svymean(~ContactP, des))

#Rest of work
table(data$Q15_fix, data$ContactP)
colSums(table(data$Q15_fix, data$ContactP))
t(t(as.matrix(table(data$Q15_fix , data$ContactP)))/colSums(table(data$Q15_fix, data$ContactP)))
chisq.test(table(data$Q15_fix, data$ContactP))

des <- data |> 
  as_survey_design(id = 1, weights = post_B_final_wt)


svyby(~Q15_fix, ~ContactP, des, svytotal)
svyby(~Q15_fix, ~ContactP, des, svymean)
svychisq(~Q15_fix+ContactP, des)

#generated Likely to contact 716 obs after mutating data with 
#ContactP, Q26_cat(Education) and age_cat
#filtered does not come with final weights!!! Load 20231126 Likely to Contact.xlsx
filtered <- data[data$ContactP == "Likely to contact",]
filtered <- filtered |>
  mutate(LTC_person_sws_wt = 2324)
filtered$LTC_person_sws_wt

#Counting post strata
for (x in c("19-29", "30-39", "40-49", "50-59", "60-")) {
  print(sum(filtered$Q26_cat == "고졸 이하" & filtered$age_cat == x))}
for (x in c("19-29", "30-39", "40-49", "50-59", "60-")) {
  print(sum(filtered$Q26_cat == "2년/3년제 대학" & filtered$age_cat == x))}
for (x in c("19-29", "30-39", "40-49", "50-59", "60-")) {
  print(sum(filtered$Q26_cat == "4년제 대학" & filtered$age_cat == x))}
for (x in c("19-29", "30-39", "40-49", "50-59", "60-")) {
  print(sum(filtered$Q26_cat == "대학원 이상" & filtered$age_cat == x))}
for (y in c("남성","여성")){
  for (x in c("19-29", "30-39", "40-49", "50-59", "60-")) {
  print(sum(filtered$Q26_cat == "고졸 이하" & filtered$age_cat == x & filtered$Q6 == y))}
}
for (y in c("남성","여성")){
  for (x in c("19-29", "30-39", "40-49", "50-59", "60-")) {
    print(sum(filtered$Q26_cat == "2년/3년제 대학" & filtered$age_cat == x & filtered$Q6 == y))}
}
for (y in c("남성","여성")){
  for (x in c("19-29", "30-39", "40-49", "50-59", "60-")) {
    print(sum(filtered$Q26_cat == "4년제 대학" & filtered$age_cat == x & filtered$Q6 == y))}
}
for (y in c("남성","여성")){
  for (x in c("19-29", "30-39", "40-49", "50-59", "60-")) {
    print(sum(filtered$Q26_cat == "대학원 이상" & filtered$age_cat == x & filtered$Q6 == y))}
}

filtered$LTC_post_sws_adj = NA
filtered$LTC_final_sws_wt = NA

# Men #################################################
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "19-29" 
               & filtered$Q6 == "남성")
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "30-39" 
               & filtered$Q6 == "남성")
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "40-49" 
               & filtered$Q6 == "남성")
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "50-59" 
               & filtered$Q6 == "남성")
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "60-" 
               & filtered$Q6 == "남성")
filtered$LTC_post_sws_adj[strata] = 257335/sum(filtered$LTC_person_sws_wt[strata])

filtered$LTC_post_sws_adj[strata]
filtered$LTC_final_sws_wt[strata] = filtered$LTC_person_sws_wt[strata] * filtered$LTC_post_sws_adj[strata] 
sum(filtered$LTC_final_sws_wt[strata])

#Women#################################################

strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "19-29" 
               & filtered$Q6 == "여성")
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "30-39" 
               & filtered$Q6 == "여성")
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "40-49" 
               & filtered$Q6 == "여성")
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "50-59"
               & filtered$Q6 == "여성")
strata <-which(filtered$Q26_cat == "대학원 이상" 
               & filtered$age_cat == "60-" 
               & filtered$Q6 == "여성")
filtered$LTC_post_sws_adj[strata] = 74395/sum(filtered$LTC_person_sws_wt[strata])


filtered$LTC_post_sws_adj[strata]
filtered$LTC_final_sws_wt[strata] = filtered$LTC_person_sws_wt[strata] * filtered$LTC_post_sws_adj[strata] 
sum(filtered$LTC_final_sws_wt[strata])

sum(filtered$LTC_final_sws_wt)
write_xlsx(filtered, "20231126 Likely to Contact.xlsx")

des <- filtered |> 
  as_survey_design(id = 1, weights = LTC_final_sws_wt)

prop.table(table(filtered$Q2))
prop.table(table(filtered$Q6))
prop.table(table(filtered$age_cat))
prop.table(table(filtered$Q26_cat))
prop.table(table(filtered$Q28_cat))


svymean(~Q2, des)
svymean(~Q6, des)
svymean(~age_cat, des)
svymean(~Q26_cat, des)


filtered <- filtered |> 
  mutate(Q28_cat = case_when(
    filtered$Q28 == "미혼으로 배우자가 없다"~ "Single",
    filtered$Q28 == unique(filtered$Q28)[2]| filtered$Q28 == unique(filtered$Q28)[4]  ~ "Married",
    filtered$Q28 == unique(filtered$Q28)[3] ~ "Divorced",
    filtered$Q28 == unique(filtered$Q28)[5]  ~ "Widowed"
  ))

svymean(~Q28_cat, des)


unique(filtered$Q28)

#strata <-which(filtered$Q26_cat == "2년/3년제 대학" 
#& (filtered$age_cat == "50-59" | filtered$age_cat == "60-")
#& filtered$Q6 == "여성")

unique(data$Q29)
subdata <- subset(data, Q29 == "부채(빚) 상환")
subdata <- subset(data, Q29 == "생활비 부족")
unique(subdata$Q29)
prop.table(table(subdata$Q6))
prop.table(table(subdata$age_cat))
prop.table(table(data$Q6))
colSums(table(data$Q29, data$Q6))
t(t(as.matrix(table(data$Q29, data$Q6)))/colSums(table(data$Q29, data$Q6)))
t(t(as.matrix(table(data$Q29, data$age_cat)))/colSums(table(data$Q29, data$age_cat)))
unique(data$Q34)
prop.table(table(data$Q34))
unique(data$Q33)
prop.table(table(data$Q33))

des <- data |> 
  as_survey_design(id = 1, weights = post_B_final_wt)

subdes <- subset(des, Q29 == "부채(빚) 상환")
subdes <- subset(des, Q29 == "생활비 부족")
svymean(~Q6, subdes)
svymean(~age_cat, subdes)

svymean(~Q29, des)
svyby(~Q29, ~Q6, des, svymean)
svyby(~Q29, ~age_cat, des, svymean)
svymean(~Q34, des)
svymean(~Q33, des)