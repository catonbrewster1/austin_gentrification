
library(tidyverse)
library(janitor)
library(lubridate)

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


########################################
#### New Residential Unit Variables ####
########################################

new_res_data <- read_csv("raw_data/New_Residential_Units__Summary_by_Zip_Code_and_Type.csv") %>%
  clean_names()

# ID names of permit classes that are new residential units
new_res_vars <- new_res_data %>%
  select(permit_class) %>% 
  unique()

#########################
#### Demolition Data ####
#########################

demo_data <- read_csv("raw_data/Demolition_Permits_-_Summary_by_Zipcode_and_Type.csv") %>%
  clean_names()

# ID names of permit classes that are new residential units
demo_vars <- demo_data %>%
  select(permit_class) %>% 
  unique()


#######################################
#### Subdivision Case Applications ####
#######################################

subdiv_data <- read_csv("raw_data/Subdivision_Cases.csv")

rename_subdiv <- function(subdiv_data) {
  subdiv_data_return <- subdiv_data %>%
    janitor::clean_names() %>%
    select(permit_number,
           case_type,
           case_name,
           sub_type,
           work,
           status,
           proposed_land_use,
           desc_of_proposed_development,
           final_date,
           status_date,
           application_start_date,
           approval_date,
           calendar_year_folder_created,
           zip_code,
           existing_no_of_lots,
           proposed_no_of_lots)
    
    return(subdiv_data_return)
}

subdiv <- rename_subdiv(subdiv_data)

write_csv(subdiv, "clean_data/subdiv.csv")

######################
#### Tree Permits ####
######################

tree_data <- read_csv("raw_data/Issued_Tree_Permits.csv")

rename_tree <- function(tree_data) {
  
  tree_data_return <- tree_data %>%
    janitor::clean_names() %>%
    select(permit_number,
           project_id,
           permit_class,
           permit_address,
           issued_date,
           expires_date,
           application_type,
           public_tree,
           reason_for_request,
           latitude,
           longitude,
           council_district)
  
  return(tree_data_return)
}

tree <- rename_tree(tree_data)

write_csv(tree, "clean_data/tree_removal.csv")

#####################
#### Crime Data ####
#####################

crime_data <- read_csv("raw_data/Crime_Reports.csv")

rename_crime <- function(crime_data) {
  crime_data_return <- crime_data %>%
    janitor::clean_names() %>% 
    select(incident_number,
           highest_offense_description,
           family_violence,
           occurred_date,
           zip_code,
           council_district,
           census_tract,
           clearance_status,
           category_description)
  
  return(crime_data_return)
}

crime <- rename_crime(crime_data)

crime_sample <- sample_n(crime, 30000)

write_csv(crime, "clean_data/crime_incidents.csv")
write_csv(crime_sample, "clean_data/crime_incidents_sample.csv")

#####################
#### Water Data ####
#####################

water_data_commercial <- read_csv("raw_data/Austin_Water_-_Commercial_Water_Consumption.csv")
water_data_res <- read_csv("raw_data/Austin_Water_-_Residential_Water_Consumption.csv") %>% mutate(type = "residential")

water_data <- water_data_commercial %>%
  mutate(type = "commercial") %>%
  rbind(water_data_res) %>% 
  clean_names() %>%
  mutate(year = as.numeric(substr(year_month, 1, 4)),
         month = as.numeric(substr(year_month, 5, 7))) %>%
  select(-year_month)

write_csv(water_data, "clean_data/water_consumpt_data.csv")

#####################
##### 311 Data ######
#####################

three11_data <- read_csv("raw_data/311_Unified_Data_Test_Site_2019.csv")

three11 <- three11_data %>%
  clean_names() %>%
  filter(county == "TRAVIS") %>%
  select(zip_code,
         service_request_sr_number,
         sr_type_code,
         sr_description,
         created_date) %>%
  mutate(created_date_trunc = substr(created_date, 1, 10)) %>%
  mutate(created_date = mdy(created_date_trunc)) %>%
  mutate(created_year = year(created_date),
         created_month = month(created_date)) %>%
  select(-c(created_date, created_date_trunc))
  
write_csv(three11, "clean_data/311_requests.csv")

#####################
#### Home Data #####
#####################

zillow_home_val_data <- read_csv("raw_data/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv") %>%
  filter(CountyName == "Travis County") %>% 
  select(zip_code = RegionName,
         10:312)

write_csv(zillow_home_val_data, "clean_data/zillow_home_value.csv")

zillow_rent_data <- read_csv("raw_data/Zip_ZORI_AllHomesPlusMultifamily_SSA.csv") %>%
  filter(MsaName == "Austin, TX") %>%
  select(zip_code = RegionName, 
         5:91)
  
write_csv(zillow_rent_data, "clean_data/zillow_rent_index.csv")

