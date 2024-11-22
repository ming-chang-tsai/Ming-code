library(pacman)
p_load(tidyverse,here,conflicted,readxl,lubridate)
conflict_prefer("filter","dplyr")

master <- read_xlsx("../../MasterSheet.xlsx",sheet = "Athlete_Info", skip = 1) %>% 
  rename(fullname=1,bday=Date) %>% 
  mutate(bday=ymd(bday),
         id=paste(fullname,bday))

ski <- read_xlsx("2023-24 BC Ski Team for IST.xlsx") %>% 
  select(Name,Gender,BirthDate) %>% 
  rename(fullname=1,Sex=2,bday=3) %>% 
  separate(fullname, into = c("fullname", "team"), sep = "[\\(\\)]") %>% 
  mutate(fullname=trimws(fullname, whitespace = "[\\h\\v]"),
         bday=ymd(bday),
         id=paste(fullname,bday)) %>% 
  select(-team) %>% 
  filter(!(id %in% master$id), !is.na(bday))



dev <- read_xlsx("2023-24 BC Dev Squad for IST.xlsx") %>% 
  select(Name,Gender,BirthDate) %>% 
  rename(fullname=1,Sex=2,bday=3) %>% 
  separate(fullname, into = c("fullname", "team"), sep = "[\\(\\)]") %>% 
  mutate(fullname=trimws(fullname, whitespace = "[\\h\\v]"),
         bday=ymd(bday),
         id=paste(fullname,bday)) %>% 
  select(-team) %>% 
  filter(!(id %in% master$id), !is.na(bday))

final <- bind_rows(ski,dev) %>% 
  select(-id) %>% 
  mutate(day=day(bday),
         month=month(bday),
         year=year(bday), .before=bday)
  



