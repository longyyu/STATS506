## Stats 506, F20
## Case Study 1 - tidyverse
## 
## Which Census Division has the highest proportion of single-family                                                                  
##   attached homes?  We'll use the 2009 RECS to answer this question. 
##
##  0. See 0-case_study1.R for data sources. 
##  1. clean data
##  2. compute point estimates of proportions by census division
##  3. construct CI's for the point estimates
##  4. make tables/graphics for presentation
##
## Author: James Henderson, jbhender@umich.edu
## Updated: September 21, 2020
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)

# directories: ----------------------------------------------------------------
path = './data'

# data: -----------------------------------------------------------------------

## 2009 RECS data
### doeid, division, typehuq, nweight
recs_file = sprintf('%s/recs2009_public.csv', path)
recs_min = sprintf('%s/recs_min.csv', path)
if ( !file.exists(recs_min) ) {
  recs = read_delim( recs_file, delim = ',' ) %>%
    select( id = DOEID, w = NWEIGHT, division = DIVISION, type = TYPEHUQ)
  write_delim(recs, path = , delim = ',')
} else {
  recs = read_delim(recs_min, delim = ',')
}

## 2009 RECS replicate weights
wt_file = sprintf('%s/recs2009_public_repweights.csv', path)
rep_weights = read_delim(wt_file, delim = ',') %>%
  rename(id = DOEID) %>%
  select(-NWEIGHT)

## codebook
cb_file = sprintf('%s/recs2009_public_codebook.xlsx', path)
codebook = readxl::read_xlsx(cb_file, skip = 1) %>% 
  select(1:4) %>%
  filter(!is.na(`Variable Name`))

# data cleaning: --------------------------------------------------------------

## variables of interest
variables = c(division = 'DIVISION', type = 'TYPEHUQ')
codes = codebook %>%
  filter(`Variable Name` %in% variables) %>%
  transmute(
    variable = `Variable Name`,
    levels = 
      stringr::str_split(`Response Codes and Labels`, pattern = '\\r\\n'),
    labels =  stringr::str_split(`...4`, pattern = '\\r\\n')
  )
  
## apply labels
decode_recs = function(x, varname, codes = codes) {
  # apply factor labels to variables using the codebook "codes"
  # Inputs: 
  #   x - input vector to be changed to factor
  #   varname - the name of the 'variable' in `codes`
  #   codes - a codebook of factor levels and labels
  # Output: x converted to a factor with levels given in codes

  with(filter(codes, variable == varname),
   factor(x, levels = as.numeric(levels[[1]]), labels = labels[[1]])
  )
}

recs = recs %>% 
  mutate(division = decode_recs(division, 'DIVISION', codes),
         type = decode_recs(type, 'TYPEHUQ', codes),
         id = as.double(id)
         )

# combine mountain subdivisions: ----------------------------------------------
levels = with(recs, levels(division))
mt_divs = c("Mountain North Sub-Division (CO, ID, MT, UT, WY)",
            "Mountain South Sub-Division (AZ, NM, NV)")
new_mt_div = "Mountain Division (AZ, CO, ID, MT, NM, NV, UT, WY)"
levels[grep('^Moun', levels)] = new_mt_div
levels = unique(levels)

recs = recs %>%
  mutate( 
    division = as.character(division),
    division = ifelse(division %in% mt_divs, new_mt_div, division),
    division = factor(division, levels = levels)
  )

# point estimates of housing type proportions by Census division: -------------
type_by_division = recs %>%
  group_by(division, type) %>%
  summarize( nhomes = sum(w), .groups = 'drop_last' ) %>%
  mutate( p_type = nhomes / sum(nhomes) )

# for CI's, make rep_weights long format: -------------------------------------
long_weights = rep_weights %>%
  pivot_longer(
    cols = starts_with('brr'),
    names_to = 'rep',
    names_prefix = 'brr_weight_',
    values_to = 'rw'
  ) %>%
  mutate( rep = as.integer(rep) )

# compute confidence intervals, using replicate weights: ----------------------

## replicate proportions
type_by_div_repl = recs %>%
  select(-w) %>%
  left_join(long_weights, by = 'id') %>%
  group_by(division, rep, type) %>%
  summarize( nhomes = sum(rw), .groups = 'drop_last' ) %>%
  mutate( p_type_repl = nhomes / sum(nhomes) ) %>%
  ungroup()

## variance of replicate proportions around the point estimate
fay = 0.5
type_by_div_var = type_by_div_repl %>%
  left_join(select(type_by_division, -nhomes), by = c('division', 'type')) %>%
  group_by(division, type) %>%
  summarize(v = mean( {p_type_repl - p_type}^2 ) / { {1 - fay}^2 }, 
            .groups = 'drop')
  
type_by_division = type_by_division %>%
  left_join(type_by_div_var, by = c('division', 'type'))

## construct CI's
m = qnorm(.975)
type_by_division = type_by_division %>%
  mutate(
   se = sqrt(v),
   lwr = pmax(p_type - m * se, 0),
   upr = pmin(p_type + m * se, 1)
  )

#filter(type_by_division, type == 'Single-Family Attached') %>%
#  arrange(desc(p_type))

# construct a plot answering the key question: --------------------------------
div_ord = {
  type_by_division %>%
  filter(type == 'Single-Family Attached') %>%
  arrange(p_type)
  }$division %>%
  as.character()

type_by_division %>%
  filter(type == 'Single-Family Attached') %>%
  mutate( across(all_of(c('p_type', 'lwr', 'upr')), 
                 .fns = function(x) 100 * x) 
  ) %>%
  mutate( division = factor( as.character(division), levels = div_ord)) %>%
  ggplot( aes(x = p_type, y = division) ) +
   geom_point() +
   geom_errorbarh( aes(xmin = lwr, xmax = upr) ) + 
   theme_bw() +
   xlab('Single-Family Attached Homes (%)') +
   ylab('Census Division') +
   xlim(c(0, 12))

# construct a plot with all available housing types: --------------------------
type_by_division %>%
  mutate( across(all_of(c('p_type', 'lwr', 'upr')), 
                 .fns = function(x) 100 * x) 
  ) %>%
  mutate( `Housing Type` = type) %>%
  ggplot( aes(x = p_type, y = division, 
              color = `Housing Type`, shape = `Housing Type`) 
  ) +
   geom_point(
     position = position_dodge2(width = 0.5)
   ) +
   geom_errorbarh( 
    aes(xmin = lwr, xmax = upr),
    position = position_dodge(width = 0.5),
    height = 0.75,
    alpha = 0.75
   ) + 
#  facet_wrap(~type, ncol = 1) +
   theme_bw() +
   xlab('% of Homes') +
   scale_color_manual( 
    values = c('darkblue', 'red3', 'darkred', 'green4', 'darkgreen')
   ) +
   scale_shape_manual( values = c(18, 16, 1, 17, 2))

# 79: -------------------------------------------------------------------------