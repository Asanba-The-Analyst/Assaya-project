
# Load Required Packages
library(tidyverse)
library(haven)
library(labelled)
library(mice)
library(tableone)
library(ggplot2)
library(broom)
library(gtsummary)
library(flextable)

# Load data and convert variable names to lowercase
data <- read_dta("Data/JULY232025.dta")
colnames(data) <- tolower(names(data))
# DATA PROCESSING PIPELINE ====================================================

# 1. Initial Data Conversion --------------------------------------------------
data_processed <- data %>%
  # First rename variables to meaningful names
  rename(
    pop_centre_rural     = "geodvpsz",
    marital_status       = "dhhgms",
    gender              = "gender",
    age_group           = "dhhgage",
    immigration_status  = "sdcfimm",
    years_in_canada     = "sdcgres",
    education           = "edu_05",
    perceived_mh_compared_prev_year = "gen_08c",
    perceived_life_stress          = "gen_07",
    perceived_work_stress          = "gen_09",
    self_perceived_mh              = "gen_08b",
    depression_interference_12m    = "depdint",
    suicide_thought_12m_flag       = "depfsyt",
    depression_hospitalized_life   = "dep_87",
    depression_consulted_prof_life = "dep_72",
    suicide_ideation_12m           = "sui_03",
    suicide_ideation_worst_episode = "sui_01",
    suicide_ideation_life          = "sui_02",
    consulted_ment_hlth_12m        = "sr1_004j"
  ) #%>%
  
  # Convert all labelled variables to factors
 # mutate(across(where(is.labelled), ~as_factor(.x))) %>%
  
  # 2. Binary Variable Conversion --------------------------------------------
data_processed <- data_processed %>% mutate(
  # Convert consultation variable
  # consulted_ment_hlth_12m = factor(
  #   case_when(
  #     consulted_ment_hlth_12m == "1" ~ "No",
  #     consulted_ment_hlth_12m == "2" ~ "Yes",
  #     TRUE ~ NA_character_
  #   ),
  #   levels = c("No", "Yes")
  # ),
  
  # Convert all other SR1 binary variables
  across(
    starts_with("sr1_"), 
    ~factor(
      case_when(
        .x == 1 ~ "Yes",
        .x == 2 ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    )
  )
) %>%
  
  # 3. Categorical Variable Encoding -----------------------------------------
mutate(
  pop_centre_rural = factor(
    pop_centre_rural,
    levels = c(1, 2, 3, 4),
    labels = c("Large urban", "Medium urban", "Small urban", "Rural")
  ),
  
  marital_status = factor(
    marital_status,
    levels = c(1, 2, 3, 4, 5),
    labels = c("Married", "Common-law", "Widowed/Divorced/Separated", 
               "Single", "Other")
  ),
  
  gender = factor(
    gender,
    levels = c(1, 2),
    labels = c("Male", "Female")
  ),
  
  age_group = factor(
    age_group,
    levels = c(1, 2, 3, 4, 5, 6, 7, 8),
    labels = c("12-14", "15-17", "18-19", "20-24", "25-29", 
               "30-34", "35-39", "40+")
  ),
  
  immigration_status = factor(
    immigration_status,
    levels = c(1, 2),
    labels = c("Immigrant", "Non-immigrant")
  ),
  
  years_in_canada = factor(
    years_in_canada,
    levels = c(1, 2, 3),
    labels = c("0-5 years", "6-10 years", "11+ years")
  ),
  
  education = factor(
    education,
    levels = c(1, 2, 3, 4, 5, 6, 7),
    labels = c(
      "Less than high school", 
      "High school", 
      "Trade certificate", 
      "College diploma", 
      "University certificate below bachelor", 
      "Bachelor's degree", 
      "University above bachelor"
    )
  )
)

# Verify the transformations
table(data_processed$gender)
table(data_processed$marital_status)
table(data_processed$age_group)
# SERVICE USE FLAG CREATION ==================================================

# 1. Define Service Variable Groups ------------------------------------------
service_groups <- list(
  professional = c(
    "sr1_004a", "sr1_004b", "sr1_004c", 
    "sr1_004d", "sr1_004e"
  ),
  informal = c(
    "sr1_004f", "sr1_004g", "sr1_004h", 
    "sr1_113", "sr1_116"
  ),
  text = c(
    "sr1_10ad", "sr1_20ad", "sr1_30ad", 
    "sr1_50ad", "sr1_60ad", "sr1_70ad", 
    "sr1_80ad", "sr1100ad"
  ),
  phone = c(
    "sr1_10ab", "sr1_20ab", "sr1_30ab", 
    "sr1_40ab", "sr1_50ab", "sr1_60ab", 
    "sr1_70ab", "sr1_80ab", "sr1100ab", 
    "sr1_116"
  ),
  video = c(
    "sr1_10ac", "sr1_20ac", "sr1_30ac", 
    "sr1_40ac", "sr1_50ac", "sr1_60ac", 
    "sr1_70ac", "sr1_80ac", "sr1100ac"
  ),
  inperson = c(
    "sr1_10aa", "sr1_20aa", "sr1_30aa", 
    "sr1_40aa", "sr1_50aa", "sr1_60aa", 
    "sr1_70aa", "sr1_80aa", "sr1100aa"
  )
)

# 2. Create Flag Creation Function -------------------------------------------
create_service_flag <- function(data, vars) {
  # Check which variables exist in the data
  existing_vars <- vars[vars %in% names(data)]
  
  # Return NA vector if no variables exist
  if (length(existing_vars) == 0) {
    return(rep(NA, nrow(data)))
  }
  
  # Create flag based on any "Yes" responses
  flag <- apply(
    data[, existing_vars], 
    1, 
    function(x) any(x == "Yes", na.rm = TRUE)
  )
  
  # Convert to factor with consistent levels
  factor(
    ifelse(flag, "Yes", "No"), 
    levels = c("No", "Yes")
  )
}

# 3. Apply Flag Creation -----------------------------------------------------
data_processed <- data_processed %>%
  mutate(
    professional_service_any  = create_service_flag(., service_groups$professional),
    informal_support_any      = create_service_flag(., service_groups$informal),
    any_text_sms_service_use  = create_service_flag(., service_groups$text),
    any_phone_service_use     = create_service_flag(., service_groups$phone),
    any_video_service_use     = create_service_flag(., service_groups$video),
    any_inperson_service_use  = create_service_flag(., service_groups$inperson)
  )
#3. Descriptive Analysis

# Demographic characteristics
demographic_vars <- c("pop_centre_rural", "marital_status", "gender", "age_group",
                      "immigration_status", "years_in_canada", "education")

# Create demographic characteristics table
tbl_demographics <- data_processed %>%
  select(all_of(demographic_vars)) %>%
  tbl_summary(
    # Display options
    missing = "no",
    
    # Variable labels
    label = list(
      pop_centre_rural ~ "Population Centre",
      marital_status   ~ "Marital Status",
      gender           ~ "Gender"
    )
  ) %>%
  # Add styling and caption
  modify_caption("**Table 1: Demographic Characteristics of Study Participants**")


#4. Mental Health Service Utilization Patterns

# 4.1 Professional vs. Informal Service Use Comparison
tbl_service_compare <- data_processed %>%
  select(professional_service_any, informal_support_any) %>%
  tbl_summary(
    label = list(
      professional_service_any ~ "Professional Services",
      informal_support_any ~ "Informal Support"
    ),
    missing = "no"
  ) %>%
  modify_caption("**Table 4: Professional vs. Informal Service Utilization**")

# 4.2 Detailed Professional Service Breakdown
professional_vars <- c("sr1_004a", "sr1_004b", "sr1_004c", "sr1_004d", "sr1_004e")
tbl_prof_detail <- data_processed %>%
  select(all_of(professional_vars)) %>%
  tbl_summary(
    label = list(
      sr1_004a ~ "Psychiatrist",
      sr1_004b ~ "Family Doctor",
      sr1_004c ~ "Psychologist",
      sr1_004d ~ "Nurse",
      sr1_004e ~ "Social Worker"
    ),
    missing = "no"
  ) %>%
  modify_caption("**Table 5: Detailed Professional Service Utilization**")

tbl_service_compare
tbl_prof_detail
#5. Barriers to Mental Health Services

# 5.1 Create comprehensive barrier variables
data_processed <- data_processed %>%
  mutate(
    # Cost barriers
    barrier_cost = factor(case_when(
      sr1_014e == "Yes" | sr1_024e == "Yes" | sr1_034e == "Yes" | sr1_044e == "Yes" | sr1_054e == "Yes" ~ "Yes",
      TRUE ~ "No"
    ), levels = c("No", "Yes")),
    
    # Stigma barriers
    barrier_stigma = factor(case_when(
      sr1_014j == "Yes" | sr1_024f == "Yes" | sr1_034f == "Yes" | sr1_044f == "Yes" | sr1_054f == "Yes" ~ "Yes",
      TRUE ~ "No"
    ), levels = c("No", "Yes")),
    
    # Accessibility barriers
    barrier_access = factor(case_when(
      sr1_014h == "Yes" | sr1_024h == "Yes" | sr1_034h == "Yes" | sr1_044h == "Yes" | sr1_054h == "Yes" ~ "Yes",
      TRUE ~ "No"
    ), levels = c("No", "Yes"))
  )

# 5.2 Overall barrier prevalence
tbl_barriers <- data_processed %>%
  select(starts_with("barrier_")) %>%
  tbl_summary(
    label = list(
      barrier_cost ~ "Financial Barriers",
      barrier_stigma ~ "Stigma Barriers",
      barrier_access ~ "Accessibility Barriers"
    ),
    missing = "no"
  ) %>%
  modify_caption("**Table 6: Prevalence of Service Access Barriers**")

# 5.3 Barriers by immigration characteristics
tbl_barriers_immigration <- data_processed %>%
  select(starts_with("barrier_"), immigration_status, years_in_canada) %>%
  tbl_summary(
    by = immigration_status,
    missing = "no"
  ) %>%
  add_p() %>%
  modify_caption("**Table 7: Barriers by Immigration Status**")

tbl_barriers
tbl_barriers_immigration

      
#6. Service Utilization Patterns by Modality

# Create a comprehensive service modality table
tbl_modality <- data_processed %>%
  select(any_text_sms_service_use, any_phone_service_use, 
         any_video_service_use, any_inperson_service_use) %>%
  tbl_summary(
    label = list(
      any_text_sms_service_use ~ "Text/SMS Services",
      any_phone_service_use ~ "Phone Services",
      any_video_service_use ~ "Video Services",
      any_inperson_service_use ~ "In-Person Services"
    ),
    missing = "no"
  ) %>%
  modify_caption("**Table 6: Mental Health Service Utilization by Modality**")
tbl_modality

# Compare modalities by immigration status
tbl_modality_immigration <- data_processed %>%
  select(any_text_sms_service_use, any_phone_service_use, 
         any_video_service_use, any_inperson_service_use,
         immigration_status) %>%
  tbl_summary(
    by = immigration_status,
    missing = "no"
  ) %>%
  add_p() %>%
  modify_caption("**Table 7: Service Modality Use by Immigration Status**")
tbl_modality_immigration
#7. Barriers to Service Access Analysis

# Create barrier variables (example for psychologist)
data_processed <- data_processed %>%
  mutate(
    barrier_cost = case_when(
      sr1_034e == "Yes" | sr1_054e == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    barrier_stigma = case_when(
      sr1_034f == "Yes" | sr1_054f == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    barrier_transport = case_when(
      sr1_034h == "Yes" | sr1_054h == "Yes" ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  mutate(across(starts_with("barrier_"), ~factor(.x, levels = c("No", "Yes"))))

# Analyze barriers
tbl_barriers <- data_processed %>%
  select(starts_with("barrier_")) %>%
  tbl_summary(
    label = list(
      barrier_cost ~ "Cost Barrier",
      barrier_stigma ~ "Stigma Barrier",
      barrier_transport ~ "Transportation Barrier"
    ),
    missing = "no"
  ) %>%
  modify_caption("**Table 8: Reported Barriers to Mental Health Services**")

# Barriers by immigration status
tbl_barriers_immigration <- data_processed %>%
  select(starts_with("barrier_"), immigration_status) %>%
  tbl_summary(
    by = immigration_status,
    missing = "no"
  ) %>%
  add_p() %>%
  modify_caption("**Table 9: Barriers by Immigration Status**")

tbl_barriers
tbl_barriers_immigration
#8. Suicidal Ideation Analysis

# Prevalence of suicidal ideation
tbl_suicide <- data_processed %>%
  select(suicide_ideation_life, suicide_ideation_12m) %>%
  tbl_summary(
    label = list(
      suicide_ideation_life ~ "Lifetime Suicidal Ideation",
      suicide_ideation_12m ~ "Past 12-Month Suicidal Ideation"
    ),
    missing = "no"
  ) %>%
  modify_caption("**Table 10: Prevalence of Suicidal Ideation**")

# Suicidal ideation by service use
# Alternative conversion options
data_processed <- data_processed %>%
  mutate(
    suicide_ideation_life = labelled::to_factor(suicide_ideation_life),
    suicide_ideation_12m = labelled::to_factor(suicide_ideation_12m)
  )

tbl_suicide_service <- data_processed %>%
  select(suicide_ideation_life, professional_service_any, informal_support_any) %>%
  tbl_summary(
    by = suicide_ideation_life,
    missing = "no"
  ) %>%
  add_p() %>%
  modify_caption("**Table 11: Service Use by Suicidal Ideation Status**")

tbl_suicide
tbl_suicide_service
#9. Family Support Analysis

# Family support variables
data_processed <- data_processed %>%
  mutate(
    family_support = case_when(
      sr1_60aa == "Yes" | sr1_60ab == "Yes" | 
      sr1_60ac == "Yes" | sr1_60ad == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    family_support = factor(family_support, levels = c("No", "Yes"))
  )

# Family support by suicidal ideation
tbl_family_support <- data_processed %>%
  select(family_support, suicide_ideation_life) %>%
  tbl_summary(
    by = suicide_ideation_life,
    missing = "no"
  ) %>%
  add_p() %>%
  modify_caption("**Table 12: Family Support by Suicidal Ideation Status**")

# Family support effectiveness
tbl_family_help <- data_processed %>%
  filter(family_support == "Yes") %>%
  select(sr1_062) %>%
  tbl_summary(
    label = list(sr1_062 ~ "Perceived Helpfulness of Family Support"),
    missing = "no"
  ) %>%
  modify_caption("**Table 13: Perceived Helpfulness of Family Support**")

tbl_family_support
tbl_family_help



#10. Advanced Modeling


# Check levels for all variables in the model
# Check number of unique levels for each predictor variable
check_levels <- data_processed %>%select(gender, age_group, immigration_status,years_in_canada,
         professional_service_any, informal_support_any,barrier_cost,
         barrier_stigma,family_support)%>%summarise(across(
           everything(),~length(unique(.x)))
  
  # Print the results in a clean format
  check_levels %>%
    pivot_longer(
      everything(),
      names_to = "Variable",
      values_to = "Unique_Levels"
    ) %>%
    kable(align = "l") %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE
    ) %>%
    row_spec(
      0, 
      bold = TRUE, 
      color = "white",
      background = "#2c3e50"
    )
# Multivariate model for suicidal ideation with service use and barriers
model_suicide_full <- glm(suicide_ideation_life ~ gender + age_group + 
                          immigration_status + years_in_canada +
                          professional_service_any + informal_support_any +
                          barrier_cost + barrier_stigma + family_support,
                        data = data_processed,
                        family = binomial(link = "logit"))

tbl_suicide_full <- tbl_regression(model_suicide_full, exponentiate = TRUE) %>%
  modify_caption("**Table 14: Full Model Predicting Suicidal Ideation**")

# Interaction between family support and professional service use
model_interaction <- glm(suicide_ideation_life ~ professional_service_any * family_support +
                         gender + age_group + immigration_status,
                       data = data_processed,
                       family = binomial(link = "logit"))

tbl_interaction <- tbl_regression(model_interaction, exponentiate = TRUE) %>%
  modify_caption("**Table 15: Interaction Between Professional Services and Family Support**")

tbl_suicide_full
tbl_interaction
Visualization

# Service use by age group
ggplot(data_processed, aes(x = age_group, fill = professional_service_any)) +
  geom_bar(position = "fill") +
  labs(title = "Professional Service Use by Age Group",
       x = "Age Group", y = "Proportion") +
  scale_fill_brewer(palette = "Set1", name = "Used Professional\nServices") +
  theme_minimal()

# Suicidal ideation by years in Canada
ggplot(data_processed %>% filter(!is.na(years_in_canada)), 
       aes(x = years_in_canada, fill = suicide_ideation_life)) +
  geom_bar(position = "fill") +
  labs(title = "Lifetime Suicidal Ideation by Years in Canada",
       x = "Years in Canada", y = "Proportion") +
  scale_fill_brewer(palette = "Set2", name = "Lifetime\nSuicidal Ideation") +
  theme_minimal()