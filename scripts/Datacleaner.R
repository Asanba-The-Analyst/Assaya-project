################################ Assaya project ##############################


# --- R Script for Mental Health Service Utilization and Suicidal Ideation Analysis ---
# 
install.packages("labelled")  # For working with labelled data
install.packages("mice")      # For multiple imputation
install.packages("tableone")  # For creating descriptive tables
install.packages("ggplot2")   # For enhanced plotting
install.packages("corrplot")  # For correlation plots (if needed)

library(tidyverse)
library(haven)
library(labelled)
library(mice)
library(tableone)
library(ggplot2)
library(broom) # For tidying model outputs

# --- 2. Data Loading ---

data <- read_dta("Data/JULY232025.dta")
colnames(data) <- tolower(names(data))
JULY232025 <- read_dta("Data/JULY232025.dta")


# --- 3. Data Cleaning and Pre-processing ---

# Renaming variables for clarity
data_processed <- data%>%
  rename(
    Pop_Centre_Rural = geodvpsz,
    Marital_Status = dhhgms,
    Gender = gender,
    Age_Group = dhhgage,
    Immigration_Status = sdcfimm,
    Years_in_Canada = sdcgres,
    Education = edu_05,
    Perceived_MH_Compared_Prev_Year = gen_08c,
   
    # Professional Service Use
    Nurse_Video_Talk = sr1_40ac,
    SocialWorker_Counsellor_12m = sr1_004e,
    No_Ment_Hlth_Consult_12m = sr1_004j, # Invert later for 'consulted'
    Psychologist_Phone = sr1_30ab,
    Psychologist_Video = sr1_30ac,
    Psychologist_Text = sr1_30ad,
    Nurse_InPerson = sr1_40aa,
    Nurse_Phone = sr1_40ab,
    Psychiatrist_Video = sr1_10ac,
    Psychiatrist_Phone = sr1_10ab,
    Psychiatrist_Text = sr1_10ad,
    Psychiatrist_InPerson = sr1_10aa,
    FamilyDoc_Text = sr1_20ad,
    FamilyDoc_Video = sr1_20ac,
    FamilyDoc_Phone = sr1_20ab,
    FamilyDoc_InPerson = sr1_20aa,
    SocialWorker_Text = sr1_50ad,
    SocialWorker_Phone = sr1_50ab,
    SocialWorker_Video = sr1_50ac,
    SocialWorker_InPerson = sr1_50aa,
    Consulted_Nurse_12m = sr1_004d,
    Consulted_FamilyDoc_GP_12m = sr1_004b,
    Consulted_Other_12m = sr1_004i,
    Consulted_Psychiatrist_12m = sr1_004a,
    Consulted_Psychologist_12m = sr1_004c,
    Professional_Services_Used_12m_Flag = sr1fpru,
    
    # Informal Support Use
    Internet_Discuss_People = sr1_112c,
    Internet_Other = sr1_112e,
    Consulted_CoWorker_Boss_12m = sr1_004h,
    Internet_Learn_Symptoms = sr1_112a,
    SelfHelp_Group_12m = sr1_113,
    Telephone_Helpline_12m = sr1_116,
    Family_Video = sr1_60ac,
    Internet_Info_Help_Support_12m = sr1_111,
    Friend_Text = sr1_70ad,
    Internet_Online_Therapy = sr1_112d,
    OtherPeople_Video = sr1100ac, # Note the typo in your original (sr1100ac instead of sr1_100ac) - assuming this
    Family_Phone = sr1_60ab,
    OtherPeople_Phone = sr1100ab,
    Consulted_Family_12m = sr1_004f,
    Consulted_Friend_12m = sr1_004g,
    OtherPeople_InPerson = sr1100aa,
    CoWorker_Supervisor_Phone = sr1_80ab,
    OtherPeople_Text = sr1100ad,
    CoWorker_Supervisor_Video = sr1_80ac,
    CoWorker_Supervisor_Text = sr1_80ad,
    Family_InPerson = sr1_60aa,
    Internet_Find_Help = sr1_112b,
    SelfHelp_Alcohol_Drug = sr1_114b,
    SelfHelp_Other = sr1_114c,
    Friend_InPerson = sr1_70aa,
    CoWorker_Supervisor_InPerson = sr1_80aa,
    Friend_Phone = sr1_70ab,
    Friend_Video = sr1_70ac,
    NonProfessional_Services_Used_12m_Flag = sr1fnpu,
    Perceived_Help_Family = sr1_062,
    
    # Barriers
    SW_Not_Helping = sr1_054c,
    Psychologist_Embarrassed = sr1_034f,
    SW_Embarrassed = sr1_054f,
    Psychiatrist_Other_Reason = sr1_014l,
    Dr_Problem_Better_NoHelp = sr1_024d,
    Psychologist_Transport_Childcare = sr1_034h,
    Psychiatrist_Not_Helping = sr1_014c,
    Psychologist_Discrimination = sr1_034k,
    SW_Service_Not_Available = sr1_054i,
    Dr_Not_Comfortable_Approach = sr1_024j,
    Dr_Discrimination = sr1_024k,
    Psychologist_Solve_Problem_NoHelp = sr1_034g,
    SW_Felt_Better = sr1_054a,
    Psychologist_Felt_Better = sr1_034a,
    Psychiatrist_Service_Not_Available = sr1_014i,
    Nurse_Other_Reason = sr1_044l,
    SW_Transport_Childcare = sr1_054h,
    Dr_Other_Reason = sr1_024l,
    Dr_Transport_Childcare = sr1_024h,
    SW_Not_Comfortable_Approach = sr1_054j,
    SW_Problem_Better_NoHelp = sr1_054d,
    Nurse_Transport_Childcare = sr1_044h,
    Dr_Service_Not_Available = sr1_024i,
    SW_Solve_Problem_NoHelp = sr1_054g,
    Nurse_Solve_Problem_NoHelp = sr1_044g,
    Nurse_Completed_Treatment = sr1_044b,
    Psychiatrist_Cant_Afford = sr1_014e,
    Psychiatrist_Transport_Childcare = sr1_014h,
    SW_Discrimination = sr1_054k,
    Stopped_Psychologist = sr1_033,
    Psychologist_Cant_Afford = sr1_034e,
    Nurse_Not_Comfortable_Approach = sr1_044j,
    Psychologist_Not_Comfortable_Approach = sr1_034j,
    Psychiatrist_Completed_Treatment = sr1_014b,
    Dr_Cant_Afford = sr1_024e,
    Dr_Embarrassed = sr1_024f,
    Stopped_Psychiatrist = sr1_013,
    Psychologist_Service_Not_Available = sr1_034i,
    Nurse_Problem_Better_NoHelp = sr1_044d,
    Psychiatrist_Felt_Better = sr1_014a,
    Nurse_Not_Helping = sr1_044c,
    Dr_Completed_Treatment = sr1_024b,
    Dr_Solve_Problem_NoHelp = sr1_024g,
    Psychiatrist_Solve_Problem_NoHelp = sr1_014g,
    SW_Other_Reason = sr1_054l,
    Psychiatrist_Not_Comfortable_Approach = sr1_014j,
    Nurse_Embarrassed = sr1_044f,
    Stopped_Nurse = sr1_043,
    Psychiatrist_Problem_Better_NoHelp = sr1_014d,
    Nurse_Cant_Afford = sr1_044e,
    Nurse_Felt_Better = sr1_044a,
    Psychologist_Problem_Better = sr1_034d,
    Stopped_FamilyDoc = sr1_023,
    SW_Cant_Afford = sr1_054e,
    Psychologist_Other_Reason = sr1_034l,
    SW_Completed_Treatment = sr1_054b,
    Dr_Felt_Better = sr1_024a,
    Nurse_Service_Not_Available = sr1_044i,
    Psychologist_Completed_Treatment = sr1_034b,
    Psychologist_Not_Helping = sr1_034c,
    Nurse_Discrimination = sr1_044k,
    Dr_Not_Helping = sr1_024c,
   
    # Suicidal Ideation / Depression
    Perceived_Life_Stress = gen_07,
    Perceived_Work_Stress = gen_09,
   # Perceived_Health = gendhdi,
    Self_Perceived_MH = gen_08b,
    Depression_Interference_12m = depdint,
    Suicide_Thought_12m_Flag = depfsyt,
    Depression_Hospitalized_Life = dep_87,
    Depression_Consulted_Prof_Life = dep_72,
    Suicide_Ideation_12m = sui_03,
    Suicide_Ideation_WorstEpisode = sui_01,
    Suicide_Ideation_Life = sui_02
  ) %>%
  # Convert to factors where appropriate, explicitly setting levels for order
  mutate(
    Pop_Centre_Rural = as.factor(Pop_Centre_Rural),
    Marital_Status = as.factor(Marital_Status),
    Gender = as.factor(Gender),
    Age_Group = factor(Age_Group, levels = c("18-29", "30-49", "50-64", "65+")),
    Years_in_Canada = factor(Years_in_Canada, levels = c("0-5 years", "6-10 years", "11+ years")),
    Education = factor(Education, levels = c("High School", "College", "University")),
    Perceived_MH_Compared_Prev_Year = factor(Perceived_MH_Compared_Prev_Year, ordered = TRUE), # Assuming 1-5 scale
    Perceived_Life_Stress = factor(Perceived_Life_Stress, ordered = TRUE),
    Perceived_Work_Stress = factor(Perceived_Work_Stress, ordered = TRUE),
    #Perceived_Health = factor(Perceived_Health, ordered = TRUE),
    Self_Perceived_MH = factor(Self_Perceived_MH, ordered = TRUE),
    Perceived_Help_Family = factor(Perceived_Help_Family, ordered = TRUE)
  )

# Convert 0/1 variables to factors for consistency if they represent Yes/No
binary_vars_to_factor <- c(
  "Nurse_Video_Talk", "SocialWorker_Counsellor_12m", "Psychologist_Phone",
  "Psychologist_Video", "Psychologist_Text", "Nurse_InPerson", "Nurse_Phone",
  "Psychiatrist_Video", "Psychiatrist_Phone", "Psychiatrist_Text",
  "Psychiatrist_InPerson", "FamilyDoc_Text", "FamilyDoc_Video",
  "FamilyDoc_Phone", "FamilyDoc_InPerson", "SocialWorker_Text",
  "SocialWorker_Phone", "SocialWorker_Video", "SocialWorker_InPerson",
  "Consulted_Nurse_12m", "Consulted_FamilyDoc_GP_12m", "Consulted_Other_12m",
  "Consulted_Psychiatrist_12m", "Consulted_Psychologist_12m",
  "Professional_Services_Used_12m_Flag", "Internet_Discuss_People",
  "Internet_Other", "Consulted_CoWorker_Boss_12m", "Internet_Learn_Symptoms",
  "SelfHelp_Group_12m", "Telephone_Helpline_12m", "Family_Video",
  "Internet_Info_Help_Support_12m", "Friend_Text", "Internet_Online_Therapy",
  "OtherPeople_Video", "Family_Phone", "OtherPeople_Phone",
  "Consulted_Family_12m", "Consulted_Friend_12m", "OtherPeople_InPerson",
  "CoWorker_Supervisor_Phone", "OtherPeople_Text", "CoWorker_Supervisor_Video",
  "CoWorker_Supervisor_Text", "Family_InPerson", "Internet_Find_Help",
  "SelfHelp_Alcohol_Drug", "SelfHelp_Other", "Friend_InPerson",
  "CoWorker_Supervisor_InPerson", "Friend_Phone", "Friend_Video",
  "NonProfessional_Services_Used_12m_Flag",
  
  # Barriers (assuming these are binary: reported/not reported)
  "SW_Not_Helping", "Psychologist_Embarrassed", "SW_Embarrassed",
  "Psychiatrist_Other_Reason", "Dr_Problem_Better_NoHelp",
  "Psychologist_Transport_Childcare", "Psychiatrist_Not_Helping",
  "Psychologist_Discrimination", "SW_Service_Not_Available",
  "Dr_Not_Comfortable_Approach", "Dr_Discrimination",
  "Psychologist_Solve_Problem_NoHelp", "SW_Felt_Better",
  "Psychologist_Felt_Better", "Psychiatrist_Service_Not_Available",
  "Nurse_Other_Reason", "SW_Transport_Childcare", "Dr_Other_Reason",
  "Dr_Transport_Childcare", "SW_Not_Comfortable_Approach",
  "SW_Problem_Better_NoHelp", "Nurse_Transport_Childcare",
  "Dr_Service_Not_Available", "SW_Solve_Problem_NoHelp",
  "Nurse_Solve_Problem_NoHelp", "Nurse_Completed_Treatment",
  "Psychiatrist_Cant_Afford", "Psychiatrist_Transport_Childcare",
  "SW_Discrimination", "Stopped_Psychologist", "Psychologist_Cant_Afford",
  "Nurse_Not_Comfortable_Approach", "Psychologist_Not_Comfortable_Approach",
  "Psychiatrist_Completed_Treatment", "Dr_Cant_Afford", "Dr_Embarrassed",
  "Stopped_Psychiatrist", "Psychologist_Service_Not_Available",
  "Nurse_Problem_Better_NoHelp", "Psychiatrist_Felt_Better",
  "Nurse_Not_Helping", "Dr_Completed_Treatment", "Dr_Solve_Problem_NoHelp",
  "Psychiatrist_Solve_Problem_NoHelp", "SW_Other_Reason",
  "Psychiatrist_Not_Comfortable_Approach", "Nurse_Embarrassed",
  "Stopped_Nurse", "Psychiatrist_Problem_Better_NoHelp",
  "Nurse_Cant_Afford", "Nurse_Felt_Better", "Psychologist_Problem_Better",
  "Stopped_FamilyDoc", "SW_Cant_Afford", "Psychologist_Other_Reason",
  "SW_Completed_Treatment", "Dr_Felt_Better", "Nurse_Service_Not_Available",
  "Psychologist_Completed_Treatment", "Psychologist_Not_Helping",
  "Nurse_Discrimination", "Dr_Not_Helping",
  
  # Suicidal Ideation / Depression
  "Suicide_Thought_12m_Flag", "Depression_Hospitalized_Life",
  "Depression_Consulted_Prof_Life", "Suicide_Ideation_12m",
  "Suicide_Ideation_WorstEpisode", "Suicide_Ideation_Life"
)

data_processed <- data_processed %>%
  mutate(across(all_of(binary_vars_to_factor), ~ factor(., levels = c(0, 1), labels = c("No", "Yes"))))

# Invert 'No_Ment_Hlth_Consult_12m' to represent 'Consulted_Ment_Hlth_12m'
data_processed <- data_processed %>%
  mutate(Consulted_Ment_Hlth_12m = factor(case_when(
    No_Ment_Hlth_Consult_12m == "Yes" ~ "No",
    No_Ment_Hlth_Consult_12m == "No" ~ "Yes",
    TRUE ~ NA_character_
  ), levels = c("No", "Yes"))) %>%
  select(-No_Ment_Hlth_Consult_12m) # Remove the original variable


# --- 4. Missing Data Imputation (Example using mice) ---
# Identify variables for imputation
# Exclude variables that are purely identifiers or those not needed for analysis
# For a full model, you might include all relevant variables.
# This is a simplified example; a real imputation model should be carefully considered.
vars_for_imputation <- names(data_processed)
# Exclude the original raw variables if still present, or identifiers.
# For simplicity, we'll impute everything in data_processed.
# For large datasets, consider a subset or method='cart' or 'rf' for complex relationships.

# Ensure all variables are in appropriate types for mice (numeric for continuous, factor for categorical)
# For ordered factors, mice sometimes works better if treated as numeric then converted back, or use specific methods.

# If you have too many columns or complex types, you might need a custom `meth` argument for `mice`.
# imp <- mice(data_processed, m = 5, maxit = 50, seed = 123)
# data_imputed <- complete(imp, 1) # Get the first imputed dataset for demonstration
# For actual analysis, you would run analyses across all imputed datasets using `with()` and `pool()`.
# For this script, we'll proceed assuming complete data or a single imputation for clarity.
# In a real scenario, consider multiple imputation and pooling results.

# For now, let's just remove NA for demonstration if imputation is not fully implemented
data_clean <- na.omit(data_processed)
cat("\nDimensions after removing NAs (for demo):", dim(data_clean), "\n")


# --- 5. Chapter Two Analysis ---
# 5.1. Research Question 1: Mental Health Service Utilization Patterns
# Objective 1: Understand and compare professional/formal and informal mental health service utilization patterns

# Derive combined utilization variables
data_clean <- data_clean %>%
  mutate(
    Professional_Service_Any = factor(
      pmax(as.numeric(Professional_Services_Used_12m_Flag == "Yes"),
           as.numeric(Nurse_Video_Talk == "Yes"),
           as.numeric(SocialWorker_Counsellor_12m == "Yes"),
           as.numeric(Psychologist_Phone == "Yes"),
           as.numeric(Psychiatrist_Video == "Yes"),
           as.numeric(Consulted_Nurse_12m == "Yes"),
           as.numeric(Consulted_FamilyDoc_GP_12m == "Yes"),
           as.numeric(Consulted_Other_12m == "Yes"),
           as.numeric(Consulted_Psychiatrist_12m == "Yes"),
           as.numeric(Consulted_Psychologist_12m == "Yes"),
           na.rm = TRUE),
      levels = c(0, 1), labels = c("No", "Yes")),
    
    Informal_Support_Any = factor(
      pmax(as.numeric(NonProfessional_Services_Used_12m_Flag == "Yes"),
           as.numeric(Internet_Discuss_People == "Yes"),
           as.numeric(Consulted_CoWorker_Boss_12m == "Yes"),
           as.numeric(SelfHelp_Group_12m == "Yes"),
           as.numeric(Telephone_Helpline_12m == "Yes"),
           as.numeric(Family_Video == "Yes"),
           as.numeric(Internet_Info_Help_Support_12m == "Yes"),
           as.numeric(Friend_Text == "Yes"),
           as.numeric(Internet_Online_Therapy == "Yes"),
           as.numeric(Consulted_Family_12m == "Yes"),
           as.numeric(Consulted_Friend_12m == "Yes"),
           as.numeric(CoWorker_Supervisor_Phone == "Yes"),
           na.rm = TRUE),
      levels = c(0, 1), labels = c("No", "Yes"))
  )

# Descriptive Statistics
cat("\n--- Chapter Two: Mental Health Service Utilization Patterns ---\n")
cat("\nPrevalence of Professional Mental Health Service Use (Past 12 Months):\n")
print(prop.table(table(data_clean$Professional_Service_Any)))

cat("\nPrevalence of Informal Mental Health Support Use (Past 12 Months):\n")
print(prop.table(table(data_clean$Informal_Support_Any)))

cat("\nDetailed breakdown of Professional Service Types:\n")
prof_vars <- c("Nurse_Video_Talk", "SocialWorker_Counsellor_12m", "Psychologist_Phone",
               "Psychiatrist_Video", "Consulted_Nurse_12m", "Consulted_FamilyDoc_GP_12m",
               "Consulted_Psychiatrist_12m", "Consulted_Psychologist_12m")
for (var in prof_vars) {
  cat(paste0("\n", var, ":\n"))
  print(prop.table(table(data_clean[[var]])))
}

cat("\nDetailed breakdown of Informal Support Types:\n")
informal_vars <- c("Internet_Discuss_People", "Consulted_CoWorker_Boss_12m",
                   "SelfHelp_Group_12m", "Telephone_Helpline_12m", "Family_Video",
                   "Consulted_Family_12m", "Consulted_Friend_12m")
for (var in informal_vars) {
  cat(paste0("\n", var, ":\n"))
  print(prop.table(table(data_clean[[var]])))
}

# Sociodemographic characteristics of the sample
cat("\nSociodemographic Characteristics:\n")
print(CreateTableOne(vars = c("Pop_Centre_Rural", "Marital_Status", "Gender",
                              "Age_Group", "Years_in_Canada", "Education",
                              "Perceived_MH_Compared_Prev_Year"), data = data_clean))


# Bivariate Analysis (Chi-square for categorical variables)
cat("\n--- Bivariate Analysis for Service Utilization ---\n")
socio_vars <- c("Pop_Centre_Rural", "Marital_Status", "Gender",
                "Age_Group", "Years_in_Canada", "Education")

for (s_var in socio_vars) {
  cat(paste0("\nProfessional Service Use by ", s_var, ":\n"))
  print(chisq.test(table(data_clean$Professional_Service_Any, data_clean[[s_var]])))
  
  cat(paste0("\nInformal Support Use by ", s_var, ":\n"))
  print(chisq.test(table(data_clean$Informal_Support_Any, data_clean[[s_var]])))
}

# Multivariable Analysis (Logistic Regression)
cat("\n--- Multivariable Analysis for Service Utilization (Logistic Regression) ---\n")

# Model 1: Professional Service Utilization
model_prof_util <- glm(Professional_Service_Any ~ Gender + Age_Group + Years_in_Canada +
                         Education + Pop_Centre_Rural + Marital_Status + Perceived_MH_Compared_Prev_Year,
                       data = data_clean, family = "binomial")
cat("\nLogistic Regression for Professional Service Utilization:\n")
print(summary(model_prof_util))
print(exp(coef(model_prof_util))) # Odds Ratios

# Model 2: Informal Support Utilization
model_informal_util <- glm(Informal_Support_Any ~ Gender + Age_Group + Years_in_Canada +
                             Education + Pop_Centre_Rural + Marital_Status + Perceived_MH_Compared_Prev_Year,
                           data = data_clean, family = "binomial")
cat("\nLogistic Regression for Informal Support Utilization:\n")
print(summary(model_informal_util))
print(exp(coef(model_informal_util))) # Odds Ratios


# 5.2. Research Question 2: Barriers to Formal Mental Health Services
# Objective 2: Examine reported barriers/reasons to accessing formal mental health services

cat("\n--- Chapter Two: Barriers to Formal Mental Health Services ---\n")

# Aggregate common barrier categories (example, you'll need to confirm your grouping logic)
data_clean <- data_clean %>%
  mutate(
    Barrier_Stigma = factor(pmax(as.numeric(Psychologist_Embarrassed == "Yes"),
                                 as.numeric(SW_Embarrassed == "Yes"),
                                 as.numeric(Dr_Embarrassed == "Yes"),
                                 as.numeric(Nurse_Embarrassed == "Yes"),
                                 na.rm = TRUE), levels = c(0,1), labels = c("No", "Yes")),
    Barrier_Affordability = factor(pmax(as.numeric(Psychiatrist_Cant_Afford == "Yes"),
                                        as.numeric(Psychologist_Cant_Afford == "Yes"),
                                        as.numeric(Dr_Cant_Afford == "Yes"),
                                        as.numeric(Nurse_Cant_Afford == "Yes"),
                                        as.numeric(SW_Cant_Afford == "Yes"),
                                        na.rm = TRUE), levels = c(0,1), labels = c("No", "Yes")),
    Barrier_Access_Logistics = factor(pmax(as.numeric(Psychologist_Transport_Childcare == "Yes"),
                                           as.numeric(SW_Transport_Childcare == "Yes"),
                                           as.numeric(Dr_Transport_Childcare == "Yes"),
                                           as.numeric(Nurse_Transport_Childcare == "Yes"),
                                           as.numeric(Psychiatrist_Transport_Childcare == "Yes"),
                                           na.rm = TRUE), levels = c(0,1), labels = c("No", "Yes")),
    Barrier_Not_Helping_Problem_Resolved = factor(pmax(as.numeric(SW_Not_Helping == "Yes"),
                                                       as.numeric(Psychiatrist_Not_Helping == "Yes"),
                                                       as.numeric(Nurse_Not_Helping == "Yes"),
                                                       as.numeric(Dr_Not_Helping == "Yes"),
                                                       as.numeric(Psychologist_Not_Helping == "Yes"),
                                                       as.numeric(SW_Felt_Better == "Yes"),
                                                       as.numeric(Psychologist_Felt_Better == "Yes"),
                                                       as.numeric(Psychiatrist_Felt_Better == "Yes"),
                                                       as.numeric(Nurse_Felt_Better == "Yes"),
                                                       as.numeric(Dr_Felt_Better == "Yes"),
                                                       as.numeric(SW_Solve_Problem_NoHelp == "Yes"),
                                                       as.numeric(Nurse_Solve_Problem_NoHelp == "Yes"),
                                                       as.numeric(Dr_Solve_Problem_NoHelp == "Yes"),
                                                       as.numeric(Psychologist_Solve_Problem_NoHelp == "Yes"),
                                                       as.numeric(Psychiatrist_Solve_Problem_NoHelp == "Yes"),
                                                       na.rm = TRUE), levels = c(0,1), labels = c("No", "Yes"))
  )


# Descriptive Statistics for Barriers
cat("\nMost Commonly Reported Barriers:\n")
barrier_vars <- c("Barrier_Stigma", "Barrier_Affordability", "Barrier_Access_Logistics",
                  "Barrier_Not_Helping_Problem_Resolved")
for (var in barrier_vars) {
  cat(paste0("\n", var, ":\n"))
  print(prop.table(table(data_clean[[var]])))
}

# Bivariate Analysis for Barriers by Sociodemographics
cat("\n--- Bivariate Analysis for Barriers ---\n")
for (b_var in barrier_vars) {
  for (s_var in c("Gender", "Age_Group", "Years_in_Canada")) {
    cat(paste0("\n", b_var, " by ", s_var, ":\n"))
    print(chisq.test(table(data_clean[[b_var]], data_clean[[s_var]])))
  }
}

# Multivariable Analysis (Logistic Regression for each barrier)
cat("\n--- Multivariable Analysis for Barriers (Logistic Regression) ---\n")

# Example for Stigma barrier
model_stigma_barrier <- glm(Barrier_Stigma ~ Gender + Age_Group + Years_in_Canada +
                              Pop_Centre_Rural + Marital_Status,
                            data = data_clean, family = "binomial")
cat("\nLogistic Regression for Stigma Barrier:\n")
print(summary(model_stigma_barrier))
print(exp(coef(model_stigma_barrier))) # Odds Ratios

# Repeat for other barriers (Affordability, Access_Logistics, Not_Helping_Problem_Resolved)
# ... (code for other barrier models would go here, similar to above)


# --- 6. Chapter Three Analysis ---
# 6.1. Research Question 1: Prevalence of Suicidal Ideations
# Objective 1: Assess the prevalence of suicidal ideations

cat("\n--- Chapter Three: Suicidal Ideations ---\n")

# Descriptive Statistics
cat("\nPrevalence of Lifetime Suicidal Ideation:\n")
print(prop.table(table(data_clean$Suicide_Ideation_Life)))

cat("\nPrevalence of 12-Month Suicidal Ideation:\n")
print(prop.table(table(data_clean$Suicide_Ideation_12m)))

# Stratified Prevalence
cat("\nLifetime Suicidal Ideation by Gender:\n")
print(prop.table(table(data_clean$Suicide_Ideation_Life, data_clean$Gender), margin = 2))

cat("\n12-Month Suicidal Ideation by Age Group:\n")
print(prop.table(table(data_clean$Suicide_Ideation_12m, data_clean$Age_Group), margin = 2))


# Bivariate Analysis
cat("\n--- Bivariate Analysis for Suicidal Ideations ---\n")
health_vars <- c("Perceived_Life_Stress", "Perceived_Work_Stress", "Perceived_Health",
                 "Self_Perceived_MH", "Depression_Interference_12m",
                 "Depression_Hospitalized_Life", "Depression_Consulted_Prof_Life")

for (h_var in c(socio_vars, health_vars)) {
  cat(paste0("\nLifetime Suicidal Ideation by ", h_var, ":\n"))
  print(chisq.test(table(data_clean$Suicide_Ideation_Life, data_clean[[h_var]])))
  
  cat(paste0("\n12-Month Suicidal Ideation by ", h_var, ":\n"))
  print(chisq.test(table(data_clean$Suicide_Ideation_12m, data_clean[[h_var]])))
}

# Multivariable Analysis (Logistic Regression)
cat("\n--- Multivariable Analysis for Suicidal Ideations (Logistic Regression) ---\n")

# Model 1: Lifetime Suicidal Ideation
model_sui_life <- glm(Suicide_Ideation_Life ~ Gender + Age_Group + Years_in_Canada + Education +
                        Perceived_Life_Stress + Self_Perceived_MH + Depression_Interference_12m +
                        Depression_Hospitalized_Life + Depression_Consulted_Prof_Life,
                      data = data_clean, family = "binomial")
cat("\nLogistic Regression for Lifetime Suicidal Ideation:\n")
print(summary(model_sui_life))
print(exp(coef(model_sui_life))) # Odds Ratios

# Model 2: 12-Month Suicidal Ideation
model_sui_12m <- glm(Suicide_Ideation_12m ~ Gender + Age_Group + Years_in_Canada + Education +
                       Perceived_Life_Stress + Self_Perceived_MH + Depression_Interference_12m +
                       Depression_Hospitalized_Life + Depression_Consulted_Prof_Life,
                     data = data_clean, family = "binomial")
cat("\nLogistic Regression for 12-Month Suicidal Ideation:\n")
print(summary(model_sui_12m))
print(exp(coef(model_sui_12m))) # Odds Ratios


# 6.2. Research Question 2: Impact of Family Support on Suicidal Ideations
# Objective 2: Evaluate the impact of family support in managing suicidal ideations

cat("\n--- Chapter Three: Impact of Family Support on Suicidal Ideations ---\n")

# Derive Family Support Composite (example: binary if they consulted family for MH concerns)
data_clean <- data_clean %>%
  mutate(
    Consulted_Family_For_MH_Any = factor(
      pmax(as.numeric(Consulted_Family_12m == "Yes"),
           as.numeric(Family_InPerson == "Yes"),
           as.numeric(Family_Phone == "Yes"),
           as.numeric(Family_Video == "Yes"),
           na.rm = TRUE),
      levels = c(0, 1), labels = c("No", "Yes"))
  )

# You might also consider a continuous score from `Perceived_Help_Family` if it makes sense.
# For simplicity, we'll use the binary consultation variable.

# Descriptive Statistics
cat("\nPrevalence of Consulting Family for Mental Health Concerns:\n")
print(prop.table(table(data_clean$Consulted_Family_For_MH_Any)))

cat("\n12-Month Suicidal Ideation by Family Consultation Status:\n")
print(prop.table(table(data_clean$Suicide_Ideation_12m, data_clean$Consulted_Family_For_MH_Any), margin = 2))

# Bivariate Analysis
cat("\n--- Bivariate Analysis for Family Support Impact ---\n")
print(chisq.test(table(data_clean$Suicide_Ideation_12m, data_clean$Consulted_Family_For_MH_Any)))
print(chisq.test(table(data_clean$Suicide_Ideation_Life, data_clean$Consulted_Family_For_MH_Any)))

# Multivariable Analysis (Logistic Regression)
cat("\n--- Multivariable Analysis for Family Support Impact (Logistic Regression) ---\n")

# Model 1: 12-Month Suicidal Ideation with Family Support
model_sui_12m_family <- glm(Suicide_Ideation_12m ~ Consulted_Family_For_MH_Any + Gender + Age_Group +
                              Years_in_Canada + Self_Perceived_MH + Depression_Interference_12m +
                              Depression_Hospitalized_Life + Depression_Consulted_Prof_Life,
                            data = data_clean, family = "binomial")
cat("\nLogistic Regression for 12-Month Suicidal Ideation (with Family Support):\n")
print(summary(model_sui_12m_family))
print(exp(coef(model_sui_12m_family))) # Odds Ratios

# Model 2: Lifetime Suicidal Ideation with Family Support
model_sui_life_family <- glm(Suicide_Ideation_Life ~ Consulted_Family_For_MH_Any + Gender + Age_Group +
                               Years_in_Canada + Self_Perceived_MH + Depression_Interference_12m +
                               Depression_Hospitalized_Life + Depression_Consulted_Prof_Life,
                             data = data_clean, family = "binomial")
cat("\nLogistic Regression for Lifetime Suicidal Ideation (with Family Support):\n")
print(summary(model_sui_life_family))
print(exp(coef(model_sui_life_family))) # Odds Ratios

# --- 7. Visualization Examples (Optional but highly recommended) ---
# Example: Bar plot for Professional Service Utilization by Gender
ggplot(data_clean, aes(x = Gender, fill = Professional_Service_Any)) +
  geom_bar(position = "fill") +
  labs(title = "Professional Mental Health Service Utilization by Gender",
       y = "Proportion", fill = "Used Services") +
  theme_minimal()

# Example: Bar plot for Suicidal Ideation by Years in Canada
ggplot(data_clean, aes(x = Years_in_Canada, fill = Suicide_Ideation_12m)) +
  geom_bar(position = "fill") +
  labs(title = "12-Month Suicidal Ideation by Years in Canada",
       y = "Proportion", fill = "Had Ideation") +
  theme_minimal()

# Example: Bar plot for Top Barriers
barrier_summary <- data_clean %>%
  select(starts_with("Barrier_")) %>%
  pivot_longer(cols = everything(), names_to = "Barrier_Type", values_to = "Reported") %>%
  filter(Reported == "Yes") %>%
  group_by(Barrier_Type) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / n_immigrants) %>% # Use total immigrants for overall proportion
  arrange(desc(Proportion))

ggplot(barrier_summary, aes(x = reorder(Barrier_Type, Proportion), y = Proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # For horizontal bars, often better for long labels
  labs(title = "Prevalence of Reported Barriers to Formal MH Services",
       x = "Barrier Type", y = "Proportion of Immigrants Reporting Barrier") +
  theme_minimal()

# --- End of Script ---