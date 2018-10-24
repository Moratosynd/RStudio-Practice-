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