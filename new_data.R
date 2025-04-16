library(dplyr)
library(readr)

# STEP 1: Load the original raw data
data <- read_csv("C:/Users/shoha/Downloads/SubjectComb_Final_RANDOMISED.csv") %>%
  filter(!is.na(Grade_1), Grade_1 != "z") %>%
  mutate(
    PPE_TakingSubj2 = suppressWarnings(as.numeric(PPE_TakingSubj2)),
    PPE_NOTTakingSubj2 = suppressWarnings(as.numeric(PPE_NOTTakingSubj2))
  )

# STEP 2: Rename the grade column for consistency
data <- data %>% rename(grade = Grade_1)

# STEP 3: Group and calculate aggregates
precomputed_summary <- data %>%
  group_by(subject_1, subject_2) %>%
  summarise(
    number_students = sum(number_students, na.rm = TRUE),
    PPE_TakingSubj2 = mean(PPE_TakingSubj2, na.rm = TRUE),
    PPE_NOTTakingSubj2 = mean(PPE_NOTTakingSubj2, na.rm = TRUE),
    Performance_Diff = round(PPE_TakingSubj2 - PPE_NOTTakingSubj2, 2),
    .groups = "drop"
  )

# STEP 4: Write to a new CSV
write_csv(precomputed_summary, "subject_combination_precomputed.csv")

cat("Precomputed file saved to: subject_combination_precomputed.csv\n")
