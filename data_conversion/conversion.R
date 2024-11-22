library(pacman)
p_load(tidyverse,conflicted,rvest,lubridate)
conflict_prefer("filter","dplyr")

html_file <- read_html("BC Team DP Test 2023.html")

table <- html_file %>% html_nodes('table') %>% html_table() %>% 
  .[[1]]

colnames(table) <- table[1,]
table <- table[-1,] %>% 
  mutate(across(c("Pl","Bib"),as.numeric))





