
library(tidyverse)
library(janitor)

setwd("/Users/carolinekinnen/Code/Machine_Learning/ml_project")

#####################
#### Census Data ####
#####################

acs19_data <- read_csv("raw_data/R12800737_SL860.csv")

rename_acs <- function(acs_data, year) {
  
  acs_data_return <- acs_data %>% 
    select(zip_code = Geo_ZCTA5,
           count_total_population = SE_A03001_001,
           count_white_alone_race = SE_A03001_002,
           count_black_alone_race = SE_A03001_003,
           count_native_alone_race = SE_A03001_004,
           count_asian_alone_race = SE_A03001_005,
           count_pacific_islander_alone_race = SE_A03001_006,
           count_other_alone_race = SE_A03001_007,
           count_two_more_race = SE_A03001_008,
           count_total_renter_occup = SE_A01003B_001,
           count_rent_1524_age = SE_A01003B_002,
           count_rent_2534_age = SE_A01003B_003,
           count_rent_3544_age = SE_A01003B_004,
           count_rent_4554_age = SE_A01003B_005,
           count_rent_5559_age = SE_A01003B_006,
           count_rent_6064_age = SE_A01003B_007,
           count_rent_6574_age = SE_A01003B_008,
           count_rent_7584_age = SE_A01003B_009,
           count_rent_85plus_age = SE_A01003B_010,
           count_rent_less_hs_ed = SE_A12005B_002,
           count_rent_hs_ed = SE_A12005B_003,
           count_rent_college_ed = SE_A12005B_004,
           count_rent_adv_ed = SE_A12005B_005,
           count_rent_income_less5k = SE_A14003B_002,
           count_rent_income_5k9k = SE_A14003B_003,
           count_rent_income_10k14k = SE_A14003B_004,
           count_rent_income_15k19k = SE_A14003B_005,
           count_rent_income_20k24k = SE_A14003B_006,
           count_rent_income_25k34k = SE_A14003B_007,
           count_rent_income_35k49k = SE_A14003B_008,
           count_rent_income_50k74k = SE_A14003B_009,
           count_rent_income_75k99k = SE_A14003B_010,
           count_rent_income_100k149k = SE_A14003B_011,
           count_rent_income_150kplus = SE_A14003B_012, 
           median_rent_income = SE_A14016_001,
           gini_index_income_ineq = SE_A14028_001,
           median_own_house_value = SE_A10036_001,
           count_rent_cost_less_300 = SE_A10038_002,
           count_rent_cost_300_499 = SE_A10038_003,
           count_rent_cost_500_799 = SE_A10038_004,
           count_rent_cost_800_899 = SE_A10038_005,
           count_rent_cost_1000_1499 = SE_A10038_006,
           count_rent_cost_1500_1999 = SE_A10038_007,
           count_rent_cost_2000_2499 = SE_A10038_008,
           count_rent_cost_2500_2999 = SE_A10038_009,
           count_rent_cost_3000_plus = SE_A10038_010,
           count_rent_cost_none = SE_A10038_011,
           count_rent_income_less20k_rent_less20_percent = SE_A10039B_003,
           count_rent_as_percent_income_less20k_20_29_percent = SE_A10039B_004,
           count_rent_as_percent_income_less20k_30plus_percent = SE_A10039B_005,
           count_rent_as_percent_income_20k34k_less20_percent = SE_A10039B_007,
           count_rent_as_percent_income_20k34k_20_29_percent = SE_A10039B_008,
           count_rent_as_percent_income_20k34k_30plus_percent = SE_A10039B_009,
           count_rent_as_percent_income_35k49k_less20_percent = SE_A10039B_011,
           count_rent_as_percent_income_35k49k_20_29_percent = SE_A10039B_012,
           count_rent_as_percent_income_35k49k_30plus_percent = SE_A10039B_013,
           count_rent_as_percent_income_50k74k_less20_percent = SE_A10039B_015,
           count_rent_as_percent_income_50k74k_20_29_percent = SE_A10039B_016,
           count_rent_as_percent_income_50k74k_30plus_percent = SE_A10039B_017,
           count_rent_as_percent_income_75kplus_less20_percent = SE_A10039B_019,
           count_rent_as_percent_income_75kplus_20_29_percent = SE_A10039B_020,
           count_rent_as_percent_income_75kplus_30plus_percent = SE_A10039B_021) %>%
    rename_at(vars(-(1)), paste0, "_acs", year)
  
  return(acs_data_return)
}

acs19 <- rename_acs(acs19_data, "19")
  
write_csv(acs19, "clean_data/acs19_data.csv")

#####################
#### Permit Data ####
#####################

permits_data <- read_csv("raw_data/Issued_Construction_Permits.csv")

rename_permits <- function(permits_data) {
  permits_data_return <- permits_data %>%
    janitor::clean_names() %>%
    select(permit_type,
           permit_type_desc,
           permit_num,
           permit_class_mapped,
           permit_class,
           work_class,
           description,
           applied_date,
           issued_date,
           calendar_year_issued,
           fiscal_year_issued,
           issuance_method,
           status_current,
           status_date,
           expires_date,
           completed_date,
           total_existing_bldg_sqft,
           remodel_repair_sqft,
           total_new_add_sqft,
           total_valuation_remodel,
           total_job_valuation,
           number_of_floors,
           housing_units,
           building_valuation,
           building_valuation_remodel,
           electrical_valuation,
           electrical_valuation_remodel,
           mechanical_valuation,
           mechanical_valuation_remodel,
           plumbing_valuation,
           plumbing_valuation_remodel,
           med_gas_valuation, 
           med_gas_valuation_remodel,
           original_address_1,
           original_city,
           original_state,
           original_zip,
           council_district)
  
  permits_data_return <- permits_data_return %>%
    filter(calendar_year_issued > 2000)
  
  return(permits_data_return)
}

permits <- rename_permits(permits_data)

permits_sample <- sample_n(permits, 300000)

write_csv(permits, "clean_data/austin_permits_post_2000.csv")
write_csv(permits_sample, "clean_data/austin_permits_post_2000_sample.csv")




