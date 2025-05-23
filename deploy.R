# deploy.R - Script to update date information and deploy the Shiny app

# Update the current date and save it
currentDate <- format(Sys.time(), '%a, %b %d, %Y at %I:%M %p %Z')
saveRDS(currentDate, file = 'dat/currentDate.rds')

# Alternatively, use git commit date if available
# Uncomment the following lines to use git commit date instead
# tryCatch({
#   git_info <- system("git log -1 --format='%cd' --date=iso", intern = TRUE)
#   currentDate <- format(as.POSIXct(git_info), "%a, %b %d, %Y at %I:%M %p %Z")
#   saveRDS(currentDate, file = 'dat/currentDate.rds')
# }, error = function(e) {
#   message("Could not retrieve git information. Using system time instead.")
# })

# Print confirmation of the saved date
cat("Updated timestamp to:", currentDate, "\n")

# Deploy the app to RStudio Connect
cat("Deploying application to RStudio Connect...\n")
rsconnect::deployApp(
  account = 'kdph',
  appName = 'pht-dashboard',
  appVisibility = 'public',
  logLevel = 'verbose'
)

cat("Deployment completed.\n")