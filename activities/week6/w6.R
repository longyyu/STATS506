## R Script for Week 6 Activity
##
## 0. Data Source: Stats506_F20 Github Repo:/problem_sets/data/
##   documentation: https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_i.htm
## 
## Author: Rithu Uppalapati, Yawen Hu, 
##         Yanyu Long, Chen Shang
## Updated: October 20, 2020

# 79: -------------------------------------------------------------------------

setwd("E:/git/Stats506_public/activities/week6")
library(tidyverse)
library(kableExtra)

demo = read_delim("./nhanes_demo.csv", delim = ',') %>%
  select(SEQN, RIAGENDR, RIDAGEYR, DMDEDUC2, RIDSTATR) %>% 
  left_join({
    read_delim("./nhanes_ohxden.csv", delim = ',') %>%
      select(SEQN, OHDDESTS)},
    by = "SEQN",
  ) %>%
  set_names("id", "gender", "age", "college", 
            "exam_status", "ohx_status") %>%
  filter(exam_status == 2) %>%
  mutate(
    gender = c('Male', 'Female')[gender],
    under_20 = ifelse(age < 20, "<20", "20+"),
    college = ifelse((college %in% c(4, 5)) & (under_20 == "20+"), 
                    "College Graduate/Some College", 
                    "No College/<20"),
    ohx = ifelse((ohx_status == 1), 
                "complete", 
                "missing"),
    ohx = replace_na(ohx, "missing"),
  )
  
format_val_percent = function(value, percentage){
  return(sprintf("%s (%.1f%%)",
                 value %>% format(big.mark = ','),
                 percentage * 100))
}

format_p_val = function(p_value, min = 1e-3, fmt = "%5.3f"){
  ifelse(
    p_value < min,
    sprintf(sprintf("p < %s", fmt), min),
    sprintf(sprintf("p = %s", fmt), p_value)
  )
}

construct_balance_table = function(data, var_row, var_col = "ohx"){
  bal_table = data %>%
    group_by(.data[[var_row]], .data[[var_col]]) %>%
    summarise(n=n(), .groups = 'drop') %>%
    pivot_wider(names_from = all_of(var_col), values_from = 'n')
  
  p_val = {bal_table %>% 
    select(-var_row) %>% 
    as.matrix() %>% 
    chisq.test()}$p.value %>%
    format_p_val()
  
  bal_table = bal_table %>%
    mutate(
      variable = var_row,
      complete_new = format_val_percent(
        complete,
        complete / (complete + missing)
      ),
      missing_new = format_val_percent(
        missing,
        missing / (complete + missing)
      ),
      p_value = c(p_val, rep("", n() - 1))
    ) %>%
    select(-missing, -complete) %>%
    rename(missing = missing_new,
           complete = complete_new,
           level = .data[[var_row]]) %>%
    select(variable, level, everything())
  
  return(bal_table)
}

row_vars = c("under_20", "gender", "college")
tab_list = vector(mode = "list", length = length(row_vars))
names(tab_list) = row_vars

for (var in row_vars){
  tab_list[[var]] = demo %>% construct_balance_table(var_row = var)
}

col_names = c("Group", "Complete", "Missing", "P-value")

bind_rows(tab_list) %>%
  select(-variable) %>%
  knitr::kable(
    format = 'html',
    col.names = col_names
  ) %>%
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(
    header = c(' ' = 1, 'Dentition Exam' = 2, ' ' = 1)
  ) %>%
  pack_rows("Age", 1, 2) %>%
  pack_rows("Gender", 3, 4) %>%
  pack_rows("Education", 5, 6)

# 79: -------------------------------------------------------------------------