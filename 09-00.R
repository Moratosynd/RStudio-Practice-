install.packages("foreign")  

library(foreign)             
library(dplyr)               
library(ggplot2)             
library(readxl)    

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)

welfare <- raw_welfare

head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,            
                  birth = h10_g4,          
                  marriage = h10_g10,      
                  religion = h10_g11,      
                  income = p1002_8aq1,    
                  code_job = h10_eco9,    
                  code_region = h10_reg7)  

class(welfare$sex)
table(welfare$sex)

table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)

summary(welfare$income)

welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

table(is.na(welfare$income))


sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()


class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

summary(welfare$birth)

table(is.na(welfare$birth))

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))

head(age_income)

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

welfare <- welfare %>%
  mutate(age = ifelse(age < 30, "young",
                      ifelse(age <= 59, "middle", "old")))

table(welfare$age)
qplot(welfare$age)

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))

age_income

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_col()

ggplot(data = age_income, aes(x = age, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = age, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young","middle", "old"))

ggplot(data = sex_income, aes(x = age, y = mean_income, fill = sex)) +
  geom_col(Position= "dodge") +
  scale_x_discrete(limits = c("young","middle", "old"))

sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))

head(sex_age)

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()


class(welfare$code_job)
table(welfare$code_job)

library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
  head(10)

job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))

head(job_income)

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)

top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)

bottom10

ggplot(data = bottom10, aes(x = reorder(job, -mean_income), 
                            y = mean_income)) +
  geom_col() + 
  coord_flip() + 
  ylim(0, 850)

job_male <- welfare %>%
  filter(!is.na(job) & sex == "male") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

job_male

# 여성 직업 빈도 상위 10개 추출
job_female <- welfare %>%
  filter(!is.na(job) & sex == "female") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

job_female

# 남성 직업 빈도 상위 10개 직업
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

# 여성 직업 빈도 상위 10개 직업
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()


class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)

class(welfare$marriage)
table(welfare$marriage)


welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))

table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)


religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))

religion_marriage

religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))


divorce <- religion_marriage %>%
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)

divorce

ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()


age_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(age, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))

age_marriage

age_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(age, group_marriage) %>% 
  group_by(age) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

age_divorce <- age_marriage %>% 
  filter(age != "young" & group_marriage == "divorce") %>% 
  select(age, pct)

age_divorce


ggplot(data = age_divorce, aes(x = age, y = pct)) + geom_col()

age_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & age != "young") %>%
  group_by(age, religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))

age_religion_marriage

age_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & age != "young") %>%
  count(age, religion, group_marriage) %>%
  group_by(age, religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

df_divorce <- age_religion_marriage %>%
  filter(group_marriage == "divorce") %>% 
  select(age, religion, pct)

df_divorce

ggplot(data = df_divorce, aes(x = age, y = pct, fill = religion )) +
  geom_col(position = "dodge")

class(welfare$code_region)
table(welfare$code_region)

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region

welfare <- left_join(welfare, list_region, id = "code_region")

welfare 
select(code_region, region) 
head

region_age <- welfare 
group_by(region, age) 
summarise(n = n()) 
mutate(tot_group = sum(n)) 
mutate(pct = round(n/tot_group*100, 2))

head(region_age)

region_age <- welfare 
count(region, age) 
group_by(region) 
mutate(pct = round(n/sum(n)*100, 2))

ggplot(data = region_age, aes(x = region, y = pct, fill = age)) +
  geom_col() +
  coord_flip()

list_order_old <- region_age 
filter(age == "old") 
arrange(pct)

list_order_old

order <- list_order_old$region
order

ggplot(data = region_age, aes(x = region,  y = pct, fill = age)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)


class(region_age$age)
levels(region_age$age)

region_age$age <- factor(region_age$age,
                         level = c("old", "middle", "young"))
class(region_age$age)
levels(region_age$age)

ggplot(data = region_age, aes(x = region,  y = pct, fill = age)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)