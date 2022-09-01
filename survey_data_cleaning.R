#####################################################
# Exploratory Data Analysis - Survey Data
# 8/26/2022   Rachael E. Blake
#####################################################

# load packages
library(tidyverse) 

# source the data download and read script
source("survey_data_download.R")
# we want to used the dataframe called "sdf" 


# setset sdf for quantitative EDA
qsdf <- sdf %>% 
        # columns to use for quantitative analysis
        select(c(12, 13:16, 17:31, 36, 40, 46, 51, 54:61, 66, 70, 74, 78, 86)) %>% 
        # rename columns
        dplyr::rename(decade_challenge = Coluna12,
                      collect_new_data_yes = Coluna14,
                      collect_new_data_no = Coluna15,
                      use_existing_data_yes = Coluna16,
                      use_existing_data_no = Coluna17,
                      data_subj_phys_oc = Coluna18,
                      data_subj_biochem_oc = Coluna19,
                      data_subj_bio_oc = Coluna20,
                      data_subj_meteor = Coluna21, 
                      data_subj_hydrog = Coluna22,
                      data_subj_geology = Coluna23,
                      data_subj_fish_aqua = Coluna24,
                      data_subj_oil_gas = Coluna25,
                      data_subj_shipping = Coluna26,
                      data_subj_mar_debris = Coluna27,
                      data_subj_pollution = Coluna28,
                      data_subj_developmt = Coluna29,
                      data_subj_society_econ = Coluna30,
                      data_subj_conserv = Coluna31,
                      data_subj_other = Coluna32,
                      create_products = Coluna38,
                      support_decision_process = Coluna42,
                      data_mgmt_plan = Coluna48,
                      no_dmp_use_existing_plan = Coluna53,
                      public_repo_no_registration = Coluna56,
                      public_repo_registration = Coluna57,
                      private_restricted_project_repo = Coluna58,
                      create_new_repo = Coluna59,
                      apply_permissive_license = Coluna60,
                      restrict_access_privacy = Coluna61,
                      restrict_access_sensitive = Coluna62,
                      want_advice_sharing = Coluna63,
                      diff_find_data = Coluna70,
                      unkn_data_availability = Coluna74,
                      lack_funding = Coluna79,
                      lack_human_capacity = Coluna84,
                      project_limits_sharing = Coluna94
                      ) %>% 
        # remove first two rows containing header info
        slice(-(1:2)) %>% 
        # consolidate 13 and 14, 15 and 16 to one column for each group
        unite("collect_new_data", collect_new_data_yes:collect_new_data_no, 
              na.rm = TRUE, remove = TRUE) %>% 
        unite("use_existing_data", use_existing_data_yes:use_existing_data_no,
              na.rm = TRUE, remove = TRUE)
### 

# data subject dataframe
ds_df <- qsdf %>% 
         # select the columns I want to transform
         select(decade_challenge, data_subj_phys_oc:data_subj_conserv) %>% 
         rowid_to_column() %>% # make a unique id column
         # make long dataframe
         pivot_longer(cols = data_subj_phys_oc:data_subj_conserv, names_to = "col_name", 
                      values_to = "data_subject") %>% 
         select(-col_name) %>%
         drop_na() # remove rows with an NA in them

### 

# data sharing dataframe
sh_df <- qsdf %>% 
         # select the columns I want to transform
         select(decade_challenge, public_repo_no_registration:want_advice_sharing) %>% 
         rowid_to_column() %>% # make a unique id column
         # make long dataframe
         pivot_longer(cols = public_repo_no_registration:want_advice_sharing, 
                      names_to = "col_name", values_to = "sharing") %>% 
         select(-col_name) %>%
         drop_na() %>% # remove rows with an NA in them
         mutate(sharing = ifelse(sharing %in% "Posting in existing, visible and publicly accessible repositories, with no user registration needed",
                                 "Posting in existing, visible and publicly accessible repositories, with no user registration needed.",
                                 sharing))

### 

# data sharing dataframe
rk_df <- qsdf %>% 
         # select the columns I want to transform
         select(decade_challenge, diff_find_data:project_limits_sharing) %>% 
         rowid_to_column() %>% # make a unique id column
         # make long dataframe
         pivot_longer(cols = diff_find_data:project_limits_sharing, 
                      names_to = "col_name", values_to = "risks") %>% 
         #select(-col_name) %>%
         drop_na() 

### 

# data advice dataframe
da_df <- sdf %>% 
         # columns to use for quantitative analysis
         select(c(1, 12, 61, 92, 93:96)) %>% 
         # rename columns
         dplyr::rename(`name_id decade_action` = Coluna1,
                       `decade challenge` = Coluna12,
                       `want_advice sharing` = Coluna63,
                       `agree to_be contacted` = Coluna101,
                       your_name = Coluna103,
                       your_email = Coluna104,
                       `name_data lead` = Coluna105,
                       `email_data lead` = Coluna106) %>% 
         # remove first two rows containing header info
         slice(-(1:2)) %>% 
         # only keep rows with a response in the `want_advice sharing` column
         filter(!is.na(`want_advice sharing`))
  
  














