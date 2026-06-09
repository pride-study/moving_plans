###################################################################################################################
# Organization: The PRIDE Study - Stanford Medicine 
# Project: LGBTQIA+ Moving Thoughts, Plans, and Decisions Following 2024 U.S. Presidential Election
# Description: Analyzes LGBTQIA+ participants' plans and decisions to move following the 2024 U.S. 
#              Presidential election. Examines differences in demographic characteristics, and mental
#              health symptoms (GAD-7, PHQ-9, PCL-6) by moving groups.
#              Processes qualitative data on moving destinations and reasons. 
# Author: Nguyen K Tran
# Date created: 2025-09-29
# Date updated: 2026-06-08
###################################################################################################################

# 0. load packages -------------------------------------------------------
library(tidyverse)
library(here)
library(psych)
library(marginaleffects)
library(tableone)

# 1. import data ---------------------------------------------------------
# list data file names
aq_files <- c( 
    "FlentjeLast_RRR-002_AQ24_2025-09-20.csv",  
    "FlentjeLast_RRR-002_AQ25_2025-09-20.csv"
)
race_files <- c(
    "FlentjeLast_RRR-002_AQ24_RACE_ETHN_2025-09-20.csv",
    "FlentjeLast_RRR-002_AQ25_RACE_ETHN_2025-09-20.csv"
)

# load analytic data
df_list <- map(aq_files, ~ read_csv(here("data", .x)) |> 
    janitor::clean_names())
df_race_list <- map(race_files, ~ read_csv(here("data", .x)) |> 
    janitor::clean_names())
df_date24 <- read_csv(here("data", "Flentje_RRR-001_AQ24_StartDate_2025-07-07.csv")) |> 
    janitor::clean_names()

# location where respondents want to move coded qualitatively 
df_move_where <- read_csv(here("data", "df_move_where-2025-09-29.csv")) |> 
    janitor::clean_names()

# assign names to data frames 
names(df_list) <- c("df24", "df25")
names(df_race_list) <- c("df24", "df25")

rm(aq_files, race_files)

# 2. data wrangling ------------------------------------------------------
# left join new race/ethnicity to aq data frames 
merged_df_list <- map2(df_list, df_race_list, ~left_join(.x, .y, by = "pid"))

# format start dates
df_date24 <- df_date24 |> 
    select(pid, start_date) |> 
    mutate(start_date = as.Date(start_date, "%m/%d/%y"))

# left join start dates to aq24
merged_df_list$df24 <- left_join(merged_df_list$df24, df_date24, by = "pid") 

# filter aq data to those who finished 
merged_df_list <- imap(merged_df_list, ~ .x |> 
    filter(finished == 1) |>
    mutate(
        year_aq_start = 2000 + as.numeric(str_extract(.y, "\\d+")),
        region = as.character(region), 
        division = as.character(division)
    )
)

# filter aq24 to exclude responses on or before 11/4/24
# remove start date from aq
merged_df_list$df24 <- merged_df_list$df24 |>
    filter(start_date > as.Date("2024-11-04")) 

# bind aq24 and aq25
df <- bind_rows(merged_df_list)

# keep most recent response
df <- bind_rows(merged_df_list) |> 
  group_by(pid) |>  
  slice_max(year_aq_start, n = 1, with_ties = FALSE) |>
  ungroup()

rm(df_date24, df_list, df_race_list, merged_df_list)

# recode variables
label_1 <- c("Not at all", "Very little", "Somewhat", "Quite a bit", "A great deal")

df <- df |> 
    mutate(
        gad = rowSums(across(gad1:gad7)), 
        phq = rowSums(across(phq1:phq9)),
        pcl = rowSums(across(pcl1:pcl6)), 
        across(c(
            genderid_1:genderid_12,
            orientation_1:orientation_11,
            race_ethn_new24_1:race_ethn_new24_8
        ), ~ ifelse(is.na(.), 0, .)), 
        # Create indicator for missing race, gender, sexual orientation
        race_miss = ifelse(rowSums(across(race_ethn_new24_1:race_ethn_new24_8), na.rm = T) == 0, 1, 0), 
        gender_miss = ifelse(rowSums(across(genderid_1:genderid_12), na.rm = T) == 0, 1, 0),
        orientation_miss = ifelse(rowSums(across(orientation_1:orientation_11), na.rm = T) == 0, 1, 0),
        # Create indicator for multiple selections of race, gender, sexual orientation
        race_mul = ifelse(rowSums(across(race_ethn_new24_1:race_ethn_new24_8), na.rm = T) > 1, 1, 0),
        gender_mul = ifelse(rowSums(across(genderid_1:genderid_12), na.rm = T) > 1, 1, 0),
        orientation_mul = ifelse(rowSums(across(orientation_1:orientation_11), na.rm = T) > 1, 1, 0),
        # gender modality based on self-report 0=cis, 1=trans
        gi_bin = factor(case_when(
            selfbin_gi %in% 1:2 ~ 0, 
            selfbin_gi %in% 3:6 ~ 1, 
            T ~ NA
        ), labels = c("cis", "tgd")),
        # Education level 1=HS grad, 2=some college, 3=4-yr grad, 4=grad degree
        educ = factor(case_when(
            ed_level %in% 1:3 ~ 1,
            ed_level %in% 4:6 ~ 2,
            ed_level == 7 ~ 3,
            ed_level == 8 ~ 4, 
            ed_level %in% 9:10 ~ 5
        ), label = c("HS grad or less","Some college","4-year grad","Master degree", "Doctoral/Professional")),
        # Individual income: 1=0-20k, 2= >20k-50k, 3= >50k-100k, >100k
        income_cat = factor(case_when(
            is.na(income) ~ NA,
            income %in% 0:2 ~ 1,
            income %in% 3:5 ~ 2,
            income %in% 6:10 ~ 3,
            T ~ 4
        ), labels = c("$0-20k", ">$20k-50k", ">$50k-100k", ">$100k")), 
        # binary move intentions
        con_stmove_bin = ifelse(con_stmove == 1, 0, 1),
        con_cntrymove_bin = ifelse(con_cntrymove == 1, 0, 1), 
        # move plan
        move_plan = factor(case_when(
            move_plan_3 == 1 ~ 1, 
            move_plan_1 == 1 & is.na(move_plan_2) ~ 2, 
            is.na(move_plan_1) & move_plan_2 == 1 ~ 3, 
            move_plan_1 == 1 & move_plan_2 == 1 ~ 3, 
            con_cntrymove_bin == 0 & con_stmove_bin == 0 ~ 0, 
            T ~ NA
        ), labels = c("No thoughts","Thoughts but no plan","Planned to move",
            "Moved")), 
        across(c(con_stmove, con_cntrymove), ~ factor(.x, levels = 1:5, labels = label_1)),
        # Residence in blue states based on 2024 presidential election 
        blue_state_residence = case_when(
            state %in% c("WA","OR","CA","CO","NM","MN","IL","VA","MD","CT",
                        "DE","DC","NJ","RI","NY","MA","NH","VT","ME","HI") ~ 1,
            is.na(state) ~ NA,
            T ~ 0
        )
    )

rm(label_1)

# 3. measure of reliability ----------------------------------------------
# define scales
mh_scale <- list(
  GAD7 = c("gad1", "gad2", "gad3", "gad4", "gad5", "gad6", "gad7"),
  PHQ9 = c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9"),
  PCL6 = c("pcl1", "pcl2", "pcl3", "pcl4", "pcl5", "pcl6")
)

# calculate omega total for each scale
omega_results <- map(mh_scale, function(items) {
  tmp <- df |> select(all_of(items))
  omega_obj <- omega(tmp, plot = FALSE)
  omega_obj$omega.tot
})
omega_results

rm(mh_scale)

# 4. differences across moving plans and decision groups -----------------
v_names <- df |> 
    select(
        age, race_ethn_new24_1:race_ethn_new24_8, race_mul, race_miss,
        genderid_1:genderid_12, gender_mul, gender_miss, gi_bin, 
        orientation_1:orientation_11, orientation_mul, orientation_miss,
        educ, income_cat, blue_state_residence, gad:pcl
    ) |> 
    names()
v_fct_names <- discard(v_names, ~ .x %in% c("age","gad","phq","pcl"))

df_tab <- CreateTableOne(
  vars = v_names, 
  strata = "move_plan",
  data = df[!is.na(df$move_plan),], 
  factorVars = v_fct_names, 
  includeNA = T, 
  addOverall = T
)

tab1 <- print(
  df_tab, 
  nonnormal = "age",
  quote = F,
  noSpaces = T, 
  printToggle = F
)

# save output as csv
# write.csv(
#   tab1, 
#   here("output", paste0("FlentjeLast_Results-", Sys.Date(), ".csv"))
# )

# 5. post-hoc comparisons ------------------------------------------------
# fit models
models <- list(
  gad = lm(gad ~ move_plan, data = df),
  phq = lm(phq ~ move_plan, data = df),
  pcl = lm(pcl ~ move_plan, data = df),
  gender = glm(gi_bin ~ move_plan, data = df, family = binomial()),
  income = nnet::multinom(income_cat ~ move_plan, data = df)
)

# helper function to estimate pairwise comparison with Bonferroni correction 
process_contrasts <- function(model, outcome_name, outcome_type) {
  avg_comparisons(model, variables = list(move_plan = "pairwise")) %>%
    as.data.frame() %>%
    mutate(
      outcome = outcome_name,
      outcome_type = outcome_type,
      p.value.bonf = p.adjust(p.value, method = "bonferroni"),
      sig.bonf = case_when(
        p.value.bonf < 0.001 ~ "***",
        p.value.bonf < 0.01 ~ "**",
        p.value.bonf < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    select(outcome, outcome_type, everything())
}

# process all models to get all pairwise contrasts
contrasts <- list(
  gad = process_contrasts(models$gad, "GAD-7", "continuous"),
  phq = process_contrasts(models$phq, "PHQ-9", "continuous"),
  pcl = process_contrasts(models$pcl, "PCL-5", "continuous"),
  gender = process_contrasts(models$gender, "Gender Modality", "binary"),
  income = process_contrasts(models$income, "Income", "multinomial")
)

# or combine all into one dataframe for easy comparison
all_results <- bind_rows(contrasts)

# view significant results across all outcomes
tab_pair_comp <- all_results |> 
  select(
    outcome, outcome_type, group, contrast, 
    estimate, p.value.bonf, sig.bonf
  ) 

# save output
# write.csv(
#     tab_pair_comp, 
#     here(
#       "output", 
#       paste0("FlentjeLast_posthoc_comparisons-", Sys.Date(), ".csv")
#     ),
#     row.names = F
# )

# 6. obtain write-in responses for qualitative analysis ------------------
# moving destination among participants who moved  
df_move_where <- df |> 
  filter(move_plan_2 == 1) |> 
  select(pid, move_where)

# moving reasons among participants who planned to move and who did move 
df_move_why <- df |> 
  filter(move_plan_1 == 1 | move_plan_2 == 1) |> 
  select(pid, gi_bin, blue_state_residence, move_plan, move_why)

# save data
# write.csv(
#     df_move_where, 
#     here(
#       "output", 
#       paste0("FlentjeLast_move_where_writein-", Sys.Date(), ".csv")
#     ),
#     row.names = F
# )

# write.csv(
#     df_move_why, 
#     here(
#       "output", 
#       paste0("FlentjeLast_move_why_writein-", Sys.Date(), ".csv")
#     ),
#     row.names = F
# )