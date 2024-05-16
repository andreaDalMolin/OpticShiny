library(cronR)

# Define the command to run your R script
cmd <- cron_rscript("/home/shiny-app/surge_periods_updater.R")

# Add a new cron job to run the script every 5 minutes, suppressing the prompt
cron_add(command = cmd, frequency = '*/5 * * * *', id = "load_and_calculate_statistics", ask = FALSE)
