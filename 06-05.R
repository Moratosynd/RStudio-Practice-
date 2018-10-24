exam %>% 
  mutate(total = math + english + science) %>% 
  head

exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>% 
  head()

exam %>% 
  mutate(test = ifelse(science >= 60, "pass","fail")) %>% 
  head

exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(total) %>% 
  head