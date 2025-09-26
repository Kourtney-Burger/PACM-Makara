# Create MAKARA Sheets from Deployment Details
#Load packages----
library(openxlsx)
library(googlesheets4)
library(dplyr)
library(here)
library(tidyverse)
library(purrr)
library(readr)
library(jsonlite)
library(tibble)

# Taiki's helper function for dates
# Helper function to create all the "YYYY-MM-DDThh:mm:ssZ" formatted date strings that JSON will want. *Note that this requires
# that you convert all your dates in the deploy details data that you read in earlier (or anywhere else you might read dates from) to POSIXct format.*
posixToText <- function(x) {
  format(x, '%Y-%m-%dT%H:%M:%S')
}

#Read in Data----
# Deployment Details
deployDetails <- read_sheet(
  "https://docs.google.com/spreadsheets/d/10bxlwfVOe1LFfj69B_YddxcA0V14m7codYwgD2YncFk/edit?gid=42687545#gid=42687545",
  sheet = 'deployDetails'
)

#ADRIFT-----
# select deployments of interest
deployDetails_ADRIFT <- deployDetails %>%
  filter(Project == 'ADRIFT', Status == 'Complete')

# initial data cleaning
## GPS Devices
deployDetails_ADRIFT <- deployDetails_ADRIFT %>%
  mutate(GPS_ID = str_to_upper(GPS_ID))

gps_devices <- deployDetails_ADRIFT$GPS_ID %>%
  str_split("/") %>%
  map(~ paste0("satellite-tracker_", str_trim(.x))) %>%
  map_chr(~ paste(.x, collapse = ","))

##Deployments----
# Create sheet
deployments <- deployDetails_ADRIFT %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    project_code = 'SWFSC_ADRIFT_2021-2023',
    site_code = Site,
    deployment_device_codes = paste0(
      "SoundTrap_",
      Instrument_ID,
      ",",
      "HTI_",
      SensorNumber_1,
      ",",
      "HTI_",
      SensorNumber_2,
      ",",
      gps_devices,
      ",",
      map_chr(
        str_split(Depth_Sensor, "/"),
        ~ paste0("depth-sensor_", str_trim(.x)) %>% paste(collapse = ",")
      )
    ),
    deployment_platform_type_code = 'drifting_buoy',
    deployment_water_depth_m = '',
    deployment_datetime = paste0(
      posixToText(as_datetime(as.numeric(Deployment_Date))),
      "Z"
    ),
    deployment_latitude = unlist(Deployment_Latitude),
    deployment_longitude = unlist(Deployment_Longitude),
    deployment_vessel = Deploy_Vessel,
    deployment_cruise = Cruise,
    deployment_alias = '',
    deployment_json = '',
    deployment_comments = '',
    recovery_datetime = paste0(
      posixToText(as_datetime(as.numeric(Recovery_Date))),
      "Z"
    ),
    recovery_latitude = unlist(Recovery_Latitude),
    recovery_longitude = unlist(Recovery_Longitude),
    recovery_vessel = Retrieve_Vessel,
    recovery_cruise = Cruise,
    recovery_surface_datetime = '',
    recovery_burn_datetime = '',
    recovery_json = '',
    recovery_comments = '',
    parent_deployment_code = ''
  )

##Recordings----
# Create sheet
recordings <- deployDetails_ADRIFT %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    recording_code = "SoundTrap_Recordings",
    recording_device_codes = paste0(
      "SoundTrap_",
      Instrument_ID,
      ",",
      "HTI_",
      SensorNumber_1,
      ",",
      "HTI_",
      SensorNumber_2
    ),
    recording_start_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_Start))),
      "Z"
    ),
    recording_end_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_End))),
      "Z"
    ),
    recording_duration_secs = if_else(
      str_to_lower(RecordingDuration_m) == "continuous" &
        (is.na(RecordingInterval_m) |
          str_to_lower(RecordingInterval_m) == "na"),
      1350,
      as.numeric(RecordingDuration_m) * 60
    ),
    recording_interval_secs = if_else(
      str_to_lower(RecordingDuration_m) == "continuous" &
        (is.na(RecordingInterval_m) |
          str_to_lower(RecordingInterval_m) == "na"),
      1350,
      as.numeric(RecordingInterval_m) * 60
    ),
    recording_sample_rate_khz = SampleRate_kHz,
    recording_bit_depth = 16,
    recording_channel = 1,
    recording_n_channels = 2,
    recording_filetypes = "WAV",
    recording_timezone = "UTC",
    recording_usable_start_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_Start))),
      "Z"
    ),
    recording_usable_end_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_End))),
      "Z"
    ),
    recording_usable_min_frequency_khz = unlist(Quality_LowFreq) / 1000,
    recording_usable_max_frequency_khz = unlist(Quality_HighFreq) / 1000,
    recording_quality_code = toupper(Quality_Category),
    recording_device_lost = '',
    recording_device_depth_m = Deployment_Depth_m,
    recording_redacted = '',
    recording_json = '',
    recording_uri = paste0(
      "gs:/swfsc-1/2021-23_ADRIFT_Rankin/drifting_recorder/audio_wav/",
      Data_ID
    ),
    recording_comments = ''
  )

##Tracks----
# Create sheet
tracks <- deployDetails_ADRIFT %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    track_code = "drifting-buoy_track",
    track_uri = paste0(
      "gs:/swfsc-1-working/2021-23_ADRIFT_Rankin/drifting_recorder/",
      Data_ID,
      "/metadata/gps/",
      Data_ID,
      "_GPS.csv"
    ),
    track_json = '',
    track_comments = ''
  )

##Tracks Positions----
# Create sheet
#Load and clean GPS data
gps_files <- list.files(
  path = here('R/GPS/'),
  pattern = "ADRIFT_.*\\.csv$",
  full.names = TRUE
)

allGPS <- map_dfr(
  gps_files,
  ~ read_csv(.x, show_col_types = FALSE) %>%
    mutate(DeviceId = as.character(DeviceId))
)

deployment_lookup <- deployDetails_ADRIFT %>%
  transmute(
    DriftName = as.character(Data_ID),
    deployment_code = paste0(
      "SWFSC",
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    )
  )


# Join GPS data with deployment codes from tracks based on DriftName and Data_ID
track_positions <- allGPS %>%
  mutate(DriftName = as.character(DriftName)) %>%
  left_join(deployment_lookup, by = "DriftName") %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = deployment_code,
    track_code = "drifting-buoy_track",
    track_position_datetime = paste0(
      format(as_datetime(UTC), "%Y-%m-%dT%H:%M:%S"),
      "Z"
    ),
    track_position_latitude = Latitude,
    track_position_longitude = Longitude,
    track_position_speed_knots = knots,
    track_position_depth_m = 100,
    track_position_json = '',
    track_position_comments = ''
  )

##Save Sheets----
write_csv(deployments, 'Makara/Sheets/ADRIFT/deployments.csv')
write_csv(recordings, 'Makara/Sheets/ADRIFT/recordings.csv')
write_csv(tracks, 'Makara/Sheets/ADRIFT/tracks.csv')
write_csv(track_positions, 'Makara/Sheets/ADRIFT/track_positions.csv')

##FOR TESTING
# Create a new workbook
wb <- createWorkbook()

# Add worksheets with each data frame
addWorksheet(wb, "deployments")
writeData(wb, "deployments", deployments)

addWorksheet(wb, "recordings")
writeData(wb, "recordings", recordings)

addWorksheet(wb, "tracks")
writeData(wb, "tracks", tracks)

addWorksheet(wb, "track_positions")
writeData(wb, "track_positions", track_positions)


# Save the workbook
saveWorkbook(
  wb,
  file = "Makara/Sheets/ADRIFT/ADRIFT_MAKARA_Sheets.xlsx",
  overwrite = TRUE
)

#PASCAL-----
# select deployments of interest
deployDetails_PASCAL <- deployDetails %>%
  filter(Project == 'PASCAL')

# initial data cleaning
## GPS Devices
deployDetails_PASCAL <- deployDetails_PASCAL %>%
  mutate(GPS_ID = str_to_upper(GPS_ID))

gps_devices <- deployDetails_PASCAL$GPS_ID %>%
  str_split("/") %>%
  map(~ paste0("satellite-tracker_", str_trim(.x))) %>%
  map_chr(~ paste(.x, collapse = ","))

##Deployments----
# Create sheet
deployments <- deployDetails_PASCAL %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    project_code = 'SWFSC_PASCAL_2016',
    site_code = Site,
    deployment_device_codes = paste0(
      "SoundTrap_",
      Instrument_ID,
      ",",
      "HTI_",
      SensorNumber_1,
      ",",
      "HTI_",
      SensorNumber_2,
      ",",
      gps_devices,
      ",",
      map_chr(
        str_split(Depth_Sensor, "/"),
        ~ paste0("depth-sensor_", str_trim(.x)) %>% paste(collapse = ",")
      )
    ),
    deployment_platform_type_code = 'drifting_buoy',
    deployment_water_depth_m = '',
    deployment_datetime = paste0(
      posixToText(as_datetime(as.numeric(Deployment_Date))),
      "Z"
    ),
    deployment_latitude = unlist(Deployment_Latitude),
    deployment_longitude = unlist(Deployment_Longitude),
    deployment_vessel = Deploy_Vessel,
    deployment_cruise = Cruise,
    deployment_alias = '',
    deployment_json = '',
    deployment_comments = '',
    recovery_datetime = paste0(
      posixToText(as_datetime(as.numeric(Recovery_Date))),
      "Z"
    ),
    recovery_latitude = unlist(Recovery_Latitude),
    recovery_longitude = unlist(Recovery_Longitude),
    recovery_vessel = Retrieve_Vessel,
    recovery_cruise = Cruise,
    recovery_surface_datetime = '',
    recovery_burn_datetime = '',
    recovery_json = '',
    recovery_comments = '',
    parent_deployment_code = ''
  )

##Recordings----
# Create sheet
recordings <- deployDetails_PASCAL %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    recording_code = if_else(
      str_starts(Type, "ST"),
      "SoundTrap_Recordings",
      paste0(Type, "_Recordings")
    ),
    recording_device_codes = paste0(
      "SoundTrap_",
      Instrument_ID,
      ",",
      "HTI_",
      SensorNumber_1,
      ",",
      "HTI_",
      SensorNumber_2
    ),
    recording_start_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_Start))),
      "Z"
    ),
    recording_end_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_End))),
      "Z"
    ),
    recording_duration_secs = if_else(
      str_to_lower(RecordingDuration_m) == "continuous" &
        (is.na(RecordingInterval_m) |
          str_to_lower(RecordingInterval_m) == "na"),
      114,
      as.numeric(RecordingDuration_m) * 60
    ),
    recording_interval_secs = if_else(
      str_to_lower(RecordingDuration_m) == "continuous" &
        (is.na(RecordingInterval_m) |
          str_to_lower(RecordingInterval_m) == "na"),
      114,
      as.numeric(RecordingInterval_m) * 60
    ),
    recording_sample_rate_khz = SampleRate_kHz,
    recording_bit_depth = 16,
    recording_channel = 1,
    recording_n_channels = 2,
    recording_filetypes = "WAV",
    recording_timezone = "UTC",
    recording_usable_start_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_Start))),
      "Z"
    ),
    recording_usable_end_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_End))),
      "Z"
    ),
    recording_usable_min_frequency_khz = as.numeric(Quality_LowFreq) / 1000,
    recording_usable_max_frequency_khz = as.numeric(Quality_HighFreq) / 1000,
    recording_quality_code = toupper(Quality_Category),
    recording_device_lost = '',
    recording_device_depth_m = Deployment_Depth_m,
    recording_redacted = '',
    recording_json = '',
    recording_uri = paste0(
      "gs:/swfsc-1/2016_PASCAL/drifting_recorder/audio_wav/",
      Data_ID
    ),
    recording_comments = ''
  )

##Tracks----
# Create sheet
tracks <- deployDetails_PASCAL %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    track_code = "drifting-buoy_track",
    track_uri = paste0(
      "gs:/swfsc-1-working/2016_PASCAL/drifting_recorder/",
      Data_ID,
      "/metadata/gps/",
      Data_ID,
      "_GPS.csv"
    ),
    track_json = '',
    track_comments = ''
  )

##Tracks Positions----
# Create sheet
#Load and clean GPS data
gps_files <- list.files(
  path = here('R/GPS/'),
  pattern = "PASCAL_.*\\.csv$",
  full.names = TRUE
)

allGPS <- map_dfr(
  gps_files,
  ~ read_csv(.x, show_col_types = FALSE) %>%
    mutate(DeviceId = as.character(DeviceId))
)

deployment_lookup <- deployDetails_PASCAL %>%
  transmute(
    DriftName = as.character(Data_ID),
    deployment_code = paste0(
      "SWFSC",
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    )
  )


# Join GPS data with deployment codes from tracks based on DriftName and Data_ID
track_positions <- allGPS %>%
  mutate(DriftName = as.character(DriftName)) %>%
  left_join(deployment_lookup, by = "DriftName") %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = deployment_code,
    track_code = "drifting-buoy_track",
    track_position_datetime = paste0(
      format(as_datetime(UTC), "%Y-%m-%dT%H:%M:%S"),
      "Z"
    ),
    track_position_latitude = Latitude,
    track_position_longitude = Longitude,
    track_position_speed_knots = knots,
    track_position_depth_m = 100,
    track_position_json = '',
    track_position_comments = ''
  )

##Save Sheets----
write_csv(deployments, 'Makara/Sheets/PASCAL/deployments.csv')
write_csv(recordings, 'Makara/Sheets/PASCAL/recordings.csv')
write_csv(tracks, 'Makara/Sheets/PASCAL/tracks.csv')
write_csv(track_positions, 'Makara/Sheets/PASCAL/track_positions.csv')

##FOR TESTING
# Create a new workbook
wb <- createWorkbook()

# Add worksheets with each data frame
addWorksheet(wb, "deployments")
writeData(wb, "deployments", deployments)

addWorksheet(wb, "recordings")
writeData(wb, "recordings", recordings)

addWorksheet(wb, "tracks")
writeData(wb, "tracks", tracks)

addWorksheet(wb, "track_positions")
writeData(wb, "track_positions", track_positions)

# Save the workbook
saveWorkbook(
  wb,
  file = "Makara/Sheets/PASCAL/PASCAL_MAKARA_Sheets.xlsx",
  overwrite = TRUE
)

#CCES-----
# select deployments of interest
deployDetails_CCES <- deployDetails %>%
  filter(Project == 'CCES', Status == 'Complete')

# initial data cleaning
## GPS Devices
deployDetails_CCES <- deployDetails_CCES %>%
  mutate(GPS_ID = str_to_upper(GPS_ID))

gps_devices <- deployDetails_CCES$GPS_ID %>%
  str_split("/") %>%
  map(~ paste0("satellite-tracker_", str_trim(.x))) %>%
  map_chr(~ paste(.x, collapse = ","))

##Deployments----
# Create sheet
deployments <- deployDetails_CCES %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    project_code = 'SWFSC_CCES_2018',
    site_code = Site,
    deployment_device_codes = paste0(
      "SoundTrap_",
      Instrument_ID,
      ",",
      "HTI_",
      SensorNumber_1,
      ",",
      "HTI_",
      SensorNumber_2,
      ",",
      gps_devices,
      ",",
      map_chr(
        str_split(Depth_Sensor, "/"),
        ~ paste0("depth-sensor_", str_trim(.x)) %>% paste(collapse = ",")
      )
    ),
    deployment_platform_type_code = 'drifting_buoy',
    deployment_water_depth_m = '',
    deployment_datetime = paste0(
      posixToText(as_datetime(as.numeric(Deployment_Date))),
      "Z"
    ),
    deployment_latitude = unlist(Deployment_Latitude),
    deployment_longitude = unlist(Deployment_Longitude),
    deployment_vessel = Deploy_Vessel,
    deployment_cruise = Cruise,
    deployment_alias = '',
    deployment_json = '',
    deployment_comments = '',
    recovery_datetime = paste0(
      posixToText(as_datetime(as.numeric(Recovery_Date))),
      "Z"
    ),
    recovery_latitude = unlist(Recovery_Latitude),
    recovery_longitude = unlist(Recovery_Longitude),
    recovery_vessel = Retrieve_Vessel,
    recovery_cruise = Cruise,
    recovery_surface_datetime = '',
    recovery_burn_datetime = '',
    recovery_json = '',
    recovery_comments = '',
    parent_deployment_code = ''
  )

##Recordings----
# Create sheet
recordings <- deployDetails_CCES %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    recording_code = if_else(
      str_starts(Type, "ST"),
      "SoundTrap_Recordings",
      paste0(Type, "_Recordings")
    ),
    recording_device_codes = paste0(
      "SoundTrap_",
      Instrument_ID,
      ",",
      "HTI_",
      SensorNumber_1,
      ",",
      "HTI_",
      SensorNumber_2
    ),
    recording_start_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_Start))),
      "Z"
    ),
    recording_end_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_End))),
      "Z"
    ),
    recording_duration_secs = if_else(
      str_to_lower(RecordingDuration_m) == "continuous" &
        (is.na(RecordingInterval_m) |
          str_to_lower(RecordingInterval_m) == "na"),
      1350,
      as.numeric(RecordingDuration_m) * 60
    ),
    recording_interval_secs = if_else(
      str_to_lower(RecordingDuration_m) == "continuous" &
        (is.na(RecordingInterval_m) |
          str_to_lower(RecordingInterval_m) == "na"),
      1350,
      as.numeric(RecordingInterval_m) * 60
    ),
    recording_sample_rate_khz = SampleRate_kHz,
    recording_bit_depth = 16,
    recording_channel = 1,
    recording_n_channels = 2,
    recording_filetypes = "WAV",
    recording_timezone = "UTC",
    recording_usable_start_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_Start))),
      "Z"
    ),
    recording_usable_end_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_End))),
      "Z"
    ),
    recording_usable_min_frequency_khz = unlist(Quality_LowFreq) / 1000,
    recording_usable_max_frequency_khz = unlist(Quality_HighFreq) / 1000,
    recording_quality_code = toupper(Quality_Category),
    recording_device_lost = '',
    recording_device_depth_m = Deployment_Depth_m,
    recording_redacted = '',
    recording_json = '',
    recording_uri = paste0(
      "gs:/swfsc-1/2018_CCES/drifting_recorder/audio_wav/",
      Data_ID
    ),
    recording_comments = ''
  )

##Tracks----
# Create sheet
tracks <- deployDetails_CCES %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    track_code = "drifting-buoy_track",
    track_uri = paste0(
      "gs:/swfsc-1-working/2018_CCES/drifting_recorder/",
      Data_ID,
      "/metadata/gps/",
      Data_ID,
      "_GPS.csv"
    ),
    track_json = '',
    track_comments = ''
  )

##Tracks Positions----
# Create sheet
#Load and clean GPS data
gps_files <- list.files(
  path = here('R/GPS/'),
  pattern = "CCES_.*\\.csv$",
  full.names = TRUE
)

allGPS <- map_dfr(
  gps_files,
  ~ read_csv(.x, show_col_types = FALSE) %>%
    mutate(DeviceId = as.character(DeviceId))
)

deployment_lookup <- deployDetails_CCES %>%
  transmute(
    DriftName = as.character(Data_ID),
    deployment_code = paste0(
      "SWFSC",
      "_",
      Site,
      "_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    )
  )


# Join GPS data with deployment codes from tracks based on DriftName and Data_ID
track_positions <- allGPS %>%
  mutate(DriftName = as.character(DriftName)) %>%
  left_join(deployment_lookup, by = "DriftName") %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = deployment_code,
    track_code = "drifting-buoy_track",
    track_position_datetime = paste0(
      format(as_datetime(UTC), "%Y-%m-%dT%H:%M:%S"),
      "Z"
    ),
    track_position_latitude = Latitude,
    track_position_longitude = Longitude,
    track_position_speed_knots = knots,
    track_position_depth_m = 100,
    track_position_json = '',
    track_position_comments = ''
  )

##Save Sheets----
write_csv(deployments, 'Makara/Sheets/CCES/deployments.csv')
write_csv(recordings, 'Makara/Sheets/CCES/recordings.csv')
write_csv(tracks, 'Makara/Sheets/CCES/tracks.csv')
write_csv(track_positions, 'Makara/Sheets/CCES/track_positions.csv')

##FOR TESTING
# Create a new workbook
wb <- createWorkbook()

# Add worksheets with each data frame
addWorksheet(wb, "deployments")
writeData(wb, "deployments", deployments)

addWorksheet(wb, "recordings")
writeData(wb, "recordings", recordings)

addWorksheet(wb, "tracks")
writeData(wb, "tracks", tracks)

addWorksheet(wb, "track_positions")
writeData(wb, "track_positions", track_positions)

# Save the workbook
saveWorkbook(
  wb,
  file = "Makara/Sheets/CCES/CCES_MAKARA_Sheets.xlsx",
  overwrite = TRUE
)

#CalCurCEAS-----
# select deployments of interest
deployDetails_CalCurCEAS <- deployDetails %>%
  filter(Project == 'CalCurCEAS', Status == 'Complete')

# initial data cleaning
## GPS Devices
deployDetails_CalCurCEAS <- deployDetails_CalCurCEAS %>%
  mutate(GPS_ID = str_to_upper(GPS_ID))

gps_devices <- deployDetails_CalCurCEAS$GPS_ID %>%
  str_split("/") %>%
  map(~ paste0("satellite-tracker_", str_trim(.x))) %>%
  map_chr(~ paste(.x, collapse = ","))

##Deployments----
# Create sheet
deployments <- deployDetails_CalCurCEAS %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_NEPac_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    project_code = 'SWFSC_CalCurCEAS_2024',
    site_code = 'NEPac',
    deployment_device_codes = paste0(
      "SoundTrap_",
      Instrument_ID,
      ",",
      "HTI_",
      SensorNumber_1,
      ",",
      "HTI_",
      SensorNumber_2,
      ",",
      gps_devices,
      ",",
      map_chr(
        str_split(Depth_Sensor, "/"),
        ~ paste0("depth-sensor_", str_trim(.x)) %>% paste(collapse = ",")
      )
    ),
    deployment_platform_type_code = 'drifting_buoy',
    deployment_water_depth_m = '',
    deployment_datetime = paste0(
      posixToText(as_datetime(as.numeric(Deployment_Date))),
      "Z"
    ),
    deployment_latitude = unlist(Deployment_Latitude),
    deployment_longitude = unlist(Deployment_Longitude),
    deployment_vessel = 'R/V Bold Horizon',
    deployment_cruise = 'CalCurCEAS_2024',
    deployment_alias = '',
    deployment_json = '',
    deployment_comments = '',
    recovery_datetime = paste0(
      posixToText(as_datetime(as.numeric(Recovery_Date))),
      "Z"
    ),
    recovery_latitude = unlist(Recovery_Latitude),
    recovery_longitude = unlist(Recovery_Longitude),
    recovery_vessel = 'R/V Bold Horizon',
    recovery_cruise = 'CalCurCEAS_2024',
    recovery_surface_datetime = '',
    recovery_burn_datetime = '',
    recovery_json = '',
    recovery_comments = '',
    parent_deployment_code = ''
  )

##Recordings----
# Create sheet
recordings <- deployDetails_CalCurCEAS %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_NEPac_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    recording_code = "SoundTrap_Recordings",
    recording_device_codes = paste0(
      "SoundTrap_",
      Instrument_ID,
      ",",
      "HTI_",
      SensorNumber_1,
      ",",
      "HTI_",
      SensorNumber_2
    ),
    recording_start_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_Start))),
      "Z"
    ),
    recording_end_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_End))),
      "Z"
    ),
    recording_duration_secs = if_else(
      str_to_lower(RecordingDuration_m) == "continuous" &
        (is.na(RecordingInterval_m) |
          str_to_lower(RecordingInterval_m) == "na"),
      1350,
      as.numeric(RecordingDuration_m) * 60
    ),
    recording_interval_secs = if_else(
      str_to_lower(RecordingDuration_m) == "continuous" &
        (is.na(RecordingInterval_m) |
          str_to_lower(RecordingInterval_m) == "na"),
      1350,
      as.numeric(RecordingInterval_m) * 60
    ),
    recording_sample_rate_khz = SampleRate_kHz,
    recording_bit_depth = 16,
    recording_channel = 1,
    recording_n_channels = 2,
    recording_filetypes = "WAV",
    recording_timezone = "UTC",
    recording_usable_start_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_Start))),
      "Z"
    ),
    recording_usable_end_datetime = paste0(
      posixToText(as_datetime(as.numeric(Data_End))),
      "Z"
    ),
    recording_usable_min_frequency_khz = unlist(Quality_LowFreq) / 1000,
    recording_usable_max_frequency_khz = unlist(Quality_HighFreq) / 1000,
    recording_quality_code = toupper(Quality_Category),
    recording_device_lost = '',
    recording_device_depth_m = Deployment_Depth_m,
    recording_redacted = '',
    recording_json = '',
    recording_uri = paste0(
      "gs:/swfsc-1/2024_CalCurCEAS/drifting_recorder/audio_wav/",
      Data_ID
    ),
    recording_comments = ''
  )

##Tracks----
# Create sheet
tracks <- deployDetails_CalCurCEAS %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = paste0(
      organization_code,
      "_NEPac_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    ),
    track_code = "drifting-buoy_track",
    track_uri = paste0(
      "gs:/swfsc-1-working/2024_CalCurCEAS/drifting_recorder/",
      Data_ID,
      "/metadata/gps/",
      Data_ID,
      "_GPS.csv"
    ),
    track_json = '',
    track_comments = ''
  )

##Tracks Positions----
# Create sheet
#Load and clean GPS data
gps_files <- list.files(
  path = here('R/GPS/'),
  pattern = "CalCurCEAS_.*\\.csv$",
  full.names = TRUE
)

allGPS <- map_dfr(
  gps_files,
  ~ read_csv(.x, show_col_types = FALSE) %>%
    mutate(DeviceId = as.character(DeviceId))
)

deployment_lookup <- deployDetails_CalCurCEAS %>%
  mutate(Data_ID = as.character(Data_ID)) %>%
  group_by(Data_ID) %>%
  slice_min(Deployment_Date, with_ties = FALSE) %>% # Or slice_max() depending on logic
  ungroup() %>%
  transmute(
    DriftName = Data_ID,
    deployment_code = paste0(
      "SWFSC",
      "_NEPac_",
      format(as_datetime(as.numeric(Deployment_Date)), "%Y%m%d"),
      "_",
      Data_ID
    )
  )

# Join GPS data with deployment codes from tracks based on DriftName and Data_ID
track_positions <- allGPS %>%
  mutate(DriftName = as.character(DriftName)) %>%
  left_join(deployment_lookup, by = "DriftName") %>%
  transmute(
    organization_code = 'SWFSC',
    deployment_code = deployment_code,
    track_code = "drifting-buoy_track",
    track_position_datetime = paste0(
      format(as_datetime(UTC), "%Y-%m-%dT%H:%M:%S"),
      "Z"
    ),
    track_position_latitude = Latitude,
    track_position_longitude = Longitude,
    track_position_speed_knots = knots,
    track_position_depth_m = 100,
    track_position_json = '',
    track_position_comments = ''
  )

##Save Sheets----
write_csv(deployments, 'Makara/Sheets/CalCurCEAS/deployments.csv')
write_csv(recordings, 'Makara/Sheets/CalCurCEAS/recordings.csv')
write_csv(tracks, 'Makara/Sheets/CalCurCEAS/tracks.csv')
write_csv(track_positions, 'Makara/Sheets/CalCurCEAS/track_positions.csv')

##FOR TESTING
# Create a new workbook
wb <- createWorkbook()

# Add worksheets with each data frame
addWorksheet(wb, "deployments")
writeData(wb, "deployments", deployments)

addWorksheet(wb, "recordings")
writeData(wb, "recordings", recordings)

addWorksheet(wb, "tracks")
writeData(wb, "tracks", tracks)

addWorksheet(wb, "track_positions")
writeData(wb, "track_positions", track_positions)

# Save the workbook
saveWorkbook(
  wb,
  file = "Makara/Sheets/CalCurCEAS/CalCurCEAS_MAKARA_Sheets.xlsx",
  overwrite = TRUE
)


#Optional: Metadata Tables----
##Devices Table----
###read in data----
# read in all devices from inventory spreadsheets
recorders <- read_sheet(
  "https://docs.google.com/spreadsheets/d/12u6cwwbuM7BfPt2GKjMXEOwr9yeK0_ztD04aNVYoFIk/edit?gid=395300949#gid=395300949",
  sheet = 'RECORDERS'
)

hydrophones <- read_sheet(
  "https://docs.google.com/spreadsheets/d/12u6cwwbuM7BfPt2GKjMXEOwr9yeK0_ztD04aNVYoFIk/edit?gid=395300949#gid=395300949",
  sheet = 'Hydrophones'
)

gps <- read_sheet(
  "https://docs.google.com/spreadsheets/d/12u6cwwbuM7BfPt2GKjMXEOwr9yeK0_ztD04aNVYoFIk/edit?gid=395300949#gid=395300949",
  sheet = 'GPS'
)

depth <- read_sheet(
  "https://docs.google.com/spreadsheets/d/12u6cwwbuM7BfPt2GKjMXEOwr9yeK0_ztD04aNVYoFIk/edit?gid=395300949#gid=395300949",
  sheet = 'depthSensors'
)

###Recorders----
recording_devices <- recorders %>%
  transmute(
    organization_code = 'SWFSC',
    device_code = paste0(
      "SoundTrap_",
      as.character(`Instrument_ID (serial number)`)
    ),
    device_type_code = 'recording_device',
    device_manufacturer = 'Ocean Instruments',
    device_model_number = gsub('^ST', 'SoundTrap ', Type),
    device_model_number = gsub('HF', ' High Frequency', device_model_number),
    device_model_number = gsub('STD', '', device_model_number),
    device_serial_number = as.character(`Instrument_ID (serial number)`),
  )

# device_json
recording_devices$device_json <- ifelse(
  is.na(recorders$`Calibration dB re. 1 μPa (gain for 1 channel)`),
  recorders$`Sensitivity dB re. 1 μPa/V`,
  recorders$`Calibration dB re. 1 μPa (gain for 1 channel)`
)

# Function to transform device_json values
transform_device_json <- function(x) {
  if (is.na(x) || x == "") {
    return(NA)
  }

  # Check if it contains both High and Low
  if (str_detect(x, "High:") & str_detect(x, "Low:")) {
    high_val <- as.numeric(str_extract(x, "(?<=High: )[-+]?[0-9]*\\.?[0-9]+"))
    low_val <- as.numeric(str_extract(x, "(?<=Low: )[-+]?[0-9]*\\.?[0-9]+"))
    return(toJSON(
      list(GAIN = list(HIGH = high_val, LOW = low_val)),
      auto_unbox = TRUE
    ))
  }

  # If it's just a single value (like -4 or -1.8)
  sensitivity_val <- as.numeric(x)
  return(toJSON(list(SENSITIVITY = sensitivity_val), auto_unbox = TRUE))
}

# Apply transformation
recording_devices <- recording_devices %>%
  mutate(device_json = sapply(device_json, transform_device_json))

###Hydrophones----
hydrophone_devices <- hydrophones %>%
  mutate(
    organization_code = 'SWFSC',
    device_code = paste0("HTI_", as.character(`Serial Number`)),
    device_type_code = 'hydrophone-individual',
    device_manufacturer = 'High Tech, Inc.',
    device_model_number = Model,
    device_serial_number = as.character(`Serial Number`),
    device_json = sapply(`Hydrophone Sensitivity dB re: 1V/uPa`, function(x) {
      toJSON(list(SENSITIVITY = x), auto_unbox = TRUE)
    })
  ) %>%
  select(
    organization_code,
    device_code,
    device_type_code,
    device_manufacturer,
    device_model_number,
    device_serial_number,
    device_json
  )

###GPS Sensors----
gps_devices <- gps %>%
  transmute(
    organization_code = 'SWFSC',
    device_code = paste0('satellite-tracker_', as.character(`GPS Name`)),
    device_type_code = 'satellite-tracker',
    device_manufacturer = case_when(
      str_detect(`GPS Name`, regex("SO", ignore_case = TRUE)) ~ "Globalstar",
      TRUE ~ "SPOT LLC"
    ),
    device_model_number = case_when(
      str_detect(`GPS Name`, regex("SO", ignore_case = TRUE)) ~
        "SmartOne Solar",
      TRUE ~ "SPOT"
    ),
    device_serial_number = as.character(`GPS Name`),
    device_json = ""
  )

###Depth Sensors----
depth_devices <- depth %>%
  transmute(
    organization_code = 'SWFSC',
    device_code = paste0(
      'depth-sensor_',
      str_remove(as.character(`Depth Sensor (serial number)`), "^U-")
    ),
    device_manufacturer = 'ReefNet Inc.',
    device_type_code = 'depth-sensor',
    device_model_number = Type,
    device_serial_number = str_remove(
      as.character(`Depth Sensor (serial number)`),
      "^U-"
    ),
    device_json = ""
  )

###Combine sheets----
devices <- bind_rows(
  recording_devices,
  hydrophone_devices,
  gps_devices,
  depth_devices
)

# add and reorder empty columns
devices <- devices %>%
  mutate(
    device_description = '',
    device_start_date = '',
    device_end_date = '',
    device_alias = '',
    device_comments = '',
    child_device_codes = '',
    device_model_name = ''
  )

devices <- devices %>%
  select(
    organization_code,
    device_code,
    device_type_code,
    device_manufacturer,
    device_model_name,
    device_model_number,
    device_serial_number,
    device_description,
    device_start_date,
    device_end_date,
    device_alias,
    device_json,
    device_comments,
    child_device_codes
  )

##Projects Table----
# created from scratch, add onto columns for new project
projects <- tibble(
  organization_code = "SWFSC",
  project_code = c(
    "SWFSC_PASCAL_2016",
    "SWFSC_CCES_2018",
    "SWFSC_ADRIFT_2021-2023",
    "SWFSC_CalCurCEAS_2024"
  ),
  project_name = c(
    "PASCAL_2016",
    "CCES_2018",
    "ADRIFT_2021-2023",
    "CalCurCEAS_2024"
  ),
  project_description = c(
    "The Passive Acoustics Survey of Cetacean Abundance Levels (PASCAL 2016), was a large scale passive acoustic survey to obtain improved distribution and population size data for deep-diving species, namely beaked whales (Ziphiidae), sperm whales (Physeter macrocephalus), and dwarf and pygmy sperm whales (Kogia sp.).",
    "The 2018 California Current Ecosystem Survey (CCES) was a multidisciplinary survey of the marine ecosystem from southern British Columbia, Canada to northern Baja California, Mexico. This survey was a collaboration between the Southwest Fisheries Science Center’s (SWFSC) Fishery Resource Division (FRD) and Marine Mammal and Turtle Division (MMTD). The survey included oceanographic measurements, use of multi-frequency echosounders, surface trawls, vertically and obliquely integrating net tows, continuous underway fish egg sampling, visual line-transect surveys for marine mammals, photographic capture-recapture studies of marine mammals, strip transect surveys for seabirds, and passive acoustic surveys of marine mammals using Drifting Acoustic Spar Buoy Recorders (DASBRs). The recordings collected in this project have been analyzed to detect echolocation signals from beaked whales, sperm whales, and dwarf and pygmy sperm whales.",
    "ADRIFT in the California Current uses passive acoustic drifting buoys to study ocean sound in the California Current Ecosystem. The relatively low-cost buoys can be deployed and recovered from most vessels, including: research, fishing, and tourist boats. They drift autonomously and can be monitored shoreside via a satellite messenger. Data collected through the ADRIFT project will be used to assess noise levels and seasonal marine mammal acoustic presence in the California Current.",
    "The California Current Cetacean and Ecosystem Assessment Survey 2024 (CalCurCEAS 2024) was a 4 month shipboard line-transect survey off the contiguous U.S. West Coast. Data collected included marine mammal visual observations, seabird sightings, cetacean biopsies, eDNA water samples, passive acoustic recordings, and uncrewed ariel systems (UAS) photogrammetry. These data will contribute to estimations of population abundance and distribution of cetaceans in the California Current."
  ),
  project_alias = '',
  project_contacts = 'Shannon Rankin <shannon.rankin@noaa.gov>',
  project_funding = '',
  project_collaborator_organization_codes = '',
  project_comments = ''
)


##SitesTable----
siteList <- read_sheet(
  "https://docs.google.com/spreadsheets/d/10bxlwfVOe1LFfj69B_YddxcA0V14m7codYwgD2YncFk/edit?gid=1245292419#gid=1245292419",
  sheet = 'Site List'
)

sites <- siteList %>%
  transmute(
    organization_code = 'SWFSC',
    site_code = `Region Code`,
    site_name = '',
    site_description = Description,
    site_latitude = '',
    site_longitude = '',
    site_alias = '',
    site_comments = ''
  )

sites <- na.omit(sites)

##Save Sheets----
write_csv(devices, 'Makara/Sheets/devices.csv')
write_csv(projects, 'Makara/Sheets/projects.csv')
write_csv(sites, 'Makara/Sheets/sites.csv')

#Optional: Combine Sheets----
# Get a list of all deployments CSV files from the project subdirectories
deployments_files <- list.files(
  path = here("Makara", "Sheets"),
  pattern = "deployments\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Get a list of all recordings CSV files
recordings_files <- list.files(
  path = here("Makara", "Sheets"),
  pattern = "recordings\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Get a list of all tracks CSV files
tracks_files <- list.files(
  path = here("Makara", "Sheets"),
  pattern = "tracks\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Get a list of all track_positions CSV files
track_positions_files <- list.files(
  path = here("Makara", "Sheets"),
  pattern = "track_positions\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Combine all deployments sheets into a single data frame
combined_deployments <- map_dfr(
  deployments_files,
  read_csv,
  show_col_types = FALSE
)

# Combine all recordings sheets
combined_recordings <- map_dfr(
  recordings_files,
  read_csv,
  show_col_types = FALSE
)

# Combine all tracks sheets
combined_tracks <- map_dfr(
  tracks_files,
  read_csv,
  show_col_types = FALSE
)

# Combine all track_positions sheets
combined_track_positions <- map_dfr(
  track_positions_files,
  read_csv,
  show_col_types = FALSE
)

# save sheets
write_csv(
  combined_deployments,
  here("Makara", "Sheets", "deployments.csv")
)

write_csv(
  combined_recordings,
  here("Makara", "Sheets", "recordings.csv")
)

write_csv(combined_tracks, here("Makara", "Sheets", "tracks.csv"))

write_csv(
  combined_track_positions,
  here("Makara", "Sheets", "track_positions.csv")
)

# For Testing: Create a new workbook
wb <- createWorkbook()

addWorksheet(wb, "deployments")
writeData(wb, "deployments", combined_deployments)

addWorksheet(wb, "recordings")
writeData(wb, "recordings", combined_recordings)

addWorksheet(wb, "tracks")
writeData(wb, "tracks", combined_tracks)

addWorksheet(wb, "track_positions")
writeData(wb, "track_positions", combined_track_positions)

# add the other optional metadata tables if you want to
devices <- read_csv(here("Makara", "Sheets", "devices.csv"))
projects <- read_csv(here("Makara", "Sheets", "projects.csv"))
sites <- read_csv(here("Makara", "Sheets", "sites.csv"))
addWorksheet(wb, "devices")
writeData(wb, "devices", devices)
addWorksheet(wb, "projects")
writeData(wb, "projects", projects)
addWorksheet(wb, "sites")
writeData(wb, "sites", sites)

# Save the complete workbook
openxlsx::saveWorkbook(
  wb,
  file = here("Makara", "Sheets", "Combined_MAKARA_Sheets.xlsx"),
  overwrite = TRUE
)
