exam %>% select(math)

exam %>% select(english)

exam %>% select(class, math, english)

exam %>% select(-math)

exam %>% select(-math, -english)

exam %>% filter(class == 1) %>%  select(english)

exam %>% filter(class == 1) %>% 
  select(english)

exam %>% 
  select(id, math) %>% 
  head

exam %>%
  select(id, math) %>%
  head(10)
