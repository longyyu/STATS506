## R Script for Week 6 Activity
##
## 0. Data Source: Stats506_F20 Github Repo:/problem_sets/data/
##   documentation: https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_i.htm
## 
## Author: Rithu Uppalapati, Yawen Hu, 
##         Yanyu Long, Chen Shang
## Updated: October 13, 2020

# 79: -------------------------------------------------------------------------

setwd("E:/git/Stats506_public/activities/week6")
library(tidyverse)

demo = read_delim("./nhanes_demo.csv", delim = ',') %>%
  select(SEQN, RIDSTATR, RIAGENDR, RIDAGEYR, DMDEDUC2)

ohxden = read_delim("./nhanes_ohxden.csv", delim = ',') %>%
  select(SEQN, OHDDESTS)

demo = demo %>% 
  left_join(ohxden, by = "SEQN") %>%
  set_names("id", "exam_status", "gender", 
            "age", "college", "ohx_status") %>%
  mutate(under_20 = ifelse(age <20, 1, 0),
         college = ifelse(college %in% c(4, 5), 
                          "College Graduate/Some College", 
                          "No College/<20"),
         ohx = ifelse((ohx_status == 1) & (exam_status == 2), 
                      "complete", 
                      "missing"),
         ohx = ifelse(is.na(ohx), "missing", ohx),
         gender = c('Male', 'Female')[gender]) %>%
  filter(exam_status == 2)
  
create_label = function(percentage, value){
  return(sprintf("%s (%3.1f%%)",
                 value %>% format(big.mark = ','),
                 percentage * 100))
}

#under_20, gender, college
balance_table = demo %>%
  mutate(is.complete = ifelse(ohx == "complete", 1, 0),
         is.missing = ifelse(ohx == "missing", 1, 0)) %>%
  group_by(gender, ohx) %>%
  summarise(n=n(), .groups = 'drop') %>%
  pivot_wider(names_from = all_of('ohx'), values_from = 'n') %>%
  mutate(
    complete.new = create_label(
      complete / (complete + missing), 
      complete),
    missing.new = create_label(
      missing / (complete + missing),
      missing),
  ) %>%
  select(-missing, -complete) %>%
  rename(missing = missing.new,
         complete = complete.new)

# %>% mutate(
#     p = chisq.test()$p.value
#     )
#   )

# 79: -------------------------------------------------------------------------