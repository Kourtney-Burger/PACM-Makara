library(tidyverse)

#read in data----
old_df <- read_csv('Makara/Sheets/analyses_old.csv')
old_detections <- read_csv('Makara/Sheets/detections_old.csv')
deploy_df <- read_csv('Makara/MakaraSubmission10012025/deployments.csv')

#Analyses----
# Extract proj_code from BOTH dataframes
old_df <- old_df %>%
  mutate(proj_code = str_extract(deployment_code, "[A-Z]+_\\d{3}$"))

deploy_df <- deploy_df %>%
  mutate(proj_code = str_extract(deployment_code, "[A-Z]+_\\d{3}$"))

# Join using proj_code
analyses <- old_df %>%
  left_join(
    deploy_df %>% select(proj_code, new_deployment_code = deployment_code),
    by = "proj_code"
  ) %>%
  mutate(
    deployment_code = coalesce(new_deployment_code, deployment_code)
  ) %>%
  select(-proj_code, -new_deployment_code)

# Update other colunms
analyses <- analyses %>%
  mutate(
    analysis_release_data = TRUE,
    analysis_release_pacm = TRUE
  )

analyses <- analyses %>%
  mutate(
    recording_codes = case_when(
      recording_codes == "SM3M_RECORDING" ~ "SM3M_Recordings",
      recording_codes == "SOUNDTRAP_RECORDING" ~ "SoundTrap_Recordings",
      recording_codes == "SM2BAT_RECORDING" ~ "SM2BAT_Recordings"
    )
  )

# analyses <- analyses %>%
#   mutate(analysis_quality_code

# Save updated analysis sheet
write_csv(analyses, 'Makara/MakaraSubmission10012025/analyses.csv')

#Detections----
# Extract proj_code from BOTH dataframes
old_df <- old_detections %>%
  mutate(proj_code = str_extract(deployment_code, "[A-Z]+_\\d{3}$"))

deploy_df <- deploy_df %>%
  mutate(proj_code = str_extract(deployment_code, "[A-Z]+_\\d{3}$"))

# Join using proj_code
detections <- old_df %>%
  left_join(
    deploy_df %>% select(proj_code, new_deployment_code = deployment_code),
    by = "proj_code"
  ) %>%
  mutate(
    deployment_code = coalesce(new_deployment_code, deployment_code)
  ) %>%
  select(-proj_code, -new_deployment_code)

# Save updated detections sheet
write_csv(detections, 'Makara/MakaraSubmission10012025/detections.csv')
