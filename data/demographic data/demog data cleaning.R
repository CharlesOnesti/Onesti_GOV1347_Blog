## shocks demographic data cleaning
# pivot wide

# sex
sex_2009_2020 <- read_csv("~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/Shocks/demographic data/sex_2009_2020.csv")

sex_df_wide <- sex_2009_2020 %>%
  select(-moe) %>%
  pivot_wider(names_from = sex, values_from = estimate) # beautiful, done

# age
age_2009_2020 <- read_csv("~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/Shocks/demographic data/age_2009_2020.csv")

age_df_wide <- age_2009_2020 %>%
  select(-moe) %>%
  pivot_wider(names_from = age, values_from = estimate)

# race
race_2009_2020 <- read_csv("~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/Shocks/demographic data/race_2009_2020.csv")

race_df_wide <- race_2009_2020 %>%
  select(-moe) %>%
  pivot_wider(names_from = race, values_from = estimate)

# now merge all three on GEOID and year
demog_df <- sex_df_wide %>%
  inner_join(age_df_wide, by = c("GEOID", "district", "state", "year")) %>%
  inner_join(race_df_wide, by = c("GEOID", "district", "state", "year")) %>%
  dplyr::rename("st_cd_fips" = "GEOID")

# leading zeros for geoid
demog_df$st_cd_fips <- ifelse(nchar(demog_df$st_cd_fips) == 3, paste0("0",demog_df$st_cd_fips), 
                         demog_df$st_cd_fips)

# write csv
write.csv(demog_df, "~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/Shocks/demographic data/demographic_2009_2020.csv")

# create version with 2020 subset (for prediction)
demog_df_2020 <- demog_df %>%
  filter(year == c(2020))

# write csv
write.csv(demog_df_2020, "~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/Shocks/demographic data/demographic_2020.csv")

# create version with 2020 subsetted out (for training)
demog_df_2009_2019 <- demog_df %>%
  filter(year != c(2020))

# write csv
write.csv(demog_df_2009_2019, "~/Dropbox/ElectionAnalytics_Midterms/Lab sessions/Shocks/demographic data/demographic_2009_2019.csv")




