library(magrittr)
d <- read.csv('./data/raw/crime_v2.csv')

d_processed <- d %>%
  ## step 1. Remove the NA values
  filter(is.na(county) == FALSE) %>%
  ## step 2. Remove the duplicates
  distinct(county, .keep_all = TRUE) %>%
  ## step 3. Top coding all the probability variable with the max value = 1
  mutate(
    prbarr = ifelse(prbarr > 1, 1, prbarr),
    # looks like `prbconv` is coded as a factor, I will change it to numeric
    prbconv = ifelse(as.numeric(levels(prbconv))[prbconv] > 1, 1, as.numeric(levels(prbconv))[prbconv])
  ) %>%
  ## step 4. change the `wser` variable within the proper wage range
  mutate(
    wser = ifelse(wser > 2000, wser/10, wser)
  )

## step 5. Change the variable names for better understanding
colnames(d_processed) <- c('county_id', 'year', 'crime_rate', 'pr_arrest', 'pr_conviction', 'pr_sentence', 'avg_sentence', 'police_per_capita',
                 'population_density', 'tax_per_capita', 'is_west', 'is_central', 'is_urban', 'pct_minority',
                 'w_construction', 'w_tuc', 'w_sales', 'w_fin', 'w_service', 'w_manu', 'w_fed', 'w_state', 'w_local_gov',
                 'offense_mix', 'pct_young_male')
colnames(d_processed)


write.csv(d_processed, './data/processed/crime_v2_test.csv')
