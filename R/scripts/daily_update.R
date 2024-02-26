# daily update script
#### using tidyedgar
setwd("~/Documents/GitHub/stockscreener/R/scripts/")
library(tidyedgar)
library(dplyr)

df <- yearly_data(years = 2019:2023)

top20 <- df |>
  arrange(year) |>
  group_by(data.entityName) |>
  slice_tail(n=1) |>
  ungroup() |>
  filter(net_income > 1e6,
         change_R > 0.6,
         year == 2023) |>
  arrange(desc(revenue)) |>
  slice_head(n = 20) |>
  pull(data.entityName)

dfwei <- df |>
  filter(data.entityName %in%  top20) |>
  group_by(data.entityName) |>
  summarise(weights = list(revenue))

### generating a dataframe with average growth rates
dflatest8 <- df |>
  arrange(year) |>
  group_by(data.entityName) |>
  slice_tail(n=3) |>
  summarise(av_rev = mean(change_R, na.rm=T),
            av_ni = mean(change_NI, na.rm=T),
            av_oi = mean(change_OI, na.rm=T),
            n = n())

dflatest <- df |>
  arrange(year) |>
  group_by(data.entityName) |>
  slice_tail(n=1) |>
  left_join(dfwei, by = "data.entityName") |>
  dplyr::left_join(dflatest8 |> select(-n), by = c("data.entityName")) |>
  dplyr::select(data.entityName, data.cik, year, data.end,
                revenue, change_R, av_rev,OperatingIncomeLoss,change_OI,av_oi,
                net_income, change_NI, av_ni, uom, weights) |>
  ungroup() |>
  group_by(data.cik) |>
  slice_max(year) |>
  ungroup()

# saving today's file
saveRDS(dflatest, paste0("../../summary_latest_", format(Sys.time(), tz = "UTC", "%Y-%m-%d"),".rds"))


# Today's file name
file_name_today <- paste0("../../", "summary_latest_", format(Sys.time(), tz = "UTC", "%Y-%m-%d"), ".rds")

# List all files in the directory that match your pattern
existing_files <- list.files(path = "../..", pattern = paste0("^", file_prefix, ".*\\.rds$"), full.names = TRUE)

# Ensure there are more than 2 files (this will run right after assessing and before saving)
if (length(existing_files) > 1) {
  # Sort the files so the older file is first
  existing_files <- sort(existing_files)
  
  # Remove the first file (oldest) in the list
  file_to_remove <- existing_files[1]
  unlink(file_to_remove)
}
setwd("~/Documents/GitHub/stockscreener/")

### Logic for dailly updates
max_attempts <- 3  # Maximum number of attempts
attempt <- 0       # Current attempt counter
success <- FALSE   # Flag to track success
wait_time <- 300   # Time to wait before retrying, in seconds (5 minutes)

while (!success & attempt < max_attempts) {
  attempt <- attempt + 1  # Increment attempt counter
  
  # Attempt to deploy the app
  result <- tryCatch({
    rsconnect::deployApp()
    TRUE         # If deployApp succeeds, set success indicator to TRUE
  }, error = function(e) {
    # Check if the error message is the one we want to retry on
    if (grepl("HTTP status 409", e$message)) {
      message(paste("Attempt", attempt, "failed. Retrying in 5 mins..."))
      FALSE  # Return FALSE to indicate failure & need to retry
    } else {
      stop(e)  # If a different error occurs, stop and print the error message
    }
  })
  
  # Check result to determine if another attempt should be made
  if (!result) {
    Sys.sleep(wait_time)  # Wait for 5 minutes before retrying
  } else {
    success <- TRUE  # Set success to true to exit the loop
  }
}

if (!success) {
  stop("Deployment failed after 5 attempts.")
} else {
  message("Deployment succeeded.")
}

