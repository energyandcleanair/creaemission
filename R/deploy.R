#' Deploy CREA Emission Portal to Shinyapps.io
#'
#' @param app_name The name for your app on Shinyapps.io
#' @param app_title The title to display for your app
#' @param account Your Shinyapps.io account name
#' @param force_update Whether to force update if app already exists
#' @param ... Additional arguments passed to \code{rsconnect::deployApp}
#'
#' @return Invisible NULL, called for side effects
#' @export
#'
#' @examples
#' \dontrun{
#' deploy_shinyapp("crea-emissions", "CREA Emission Portal")
#' }
deploy_shinyapp <- function(app_name = "crea-emissions",
                           account = NULL,
                           force_update = FALSE,
                           ...) {

  if(!require(rsconnect)) install.packages('rsconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  remotes::install_github("energyandcleanair/creahelpers", upgrade = F)
  remotes::install_github("energyandcleanair/rcrea", upgrade = F)

  try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))


  # We need to install the package from github so that
  # ShinyApps can reproduce the environment
  remotes::install_github("energyandcleanair/creaemission@feat/edgar", upgrade = T)
  remotes::install_version('curl', version = '6.2.3')

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))

  # Create symlink to data folder
  create_data_symlink()

  # Deploy production - now using the inst/ folder structure
  rsconnect::deployApp("inst",
                       appFiles = c(
                         file.path(gsub("inst/", "", fs::dir_ls("inst", recurse = TRUE))),
                         file.path(fs::dir_ls("data", recurse = TRUE))
                       ),
                       appName=app_name,
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = T)
}


create_data_symlink <- function() {
  # Create a symlink inst/data to data
  data_path <- file.path(getwd(), "data")
  symlink_path <- file.path(getwd(), "inst", "data")

  # Remove any existing symlink or folder with the same name
  if (file.exists(symlink_path) || dir.exists(symlink_path)) {
    unlink(symlink_path, recursive = TRUE)
  }

  # Create the symlink (macOS / Linux)
  system2("ln", c("-s", shQuote(data_path), shQuote(symlink_path)))
}


#' Deploy CREA Emission Portal to Google Cloud Run
#'
#' @param project_id Google Cloud project ID (default: "crea-aq-data")
#' @param region Google Cloud region (default: "europe-west1")
#' @param service_name Name for the Cloud Run service (default: "creaemission")
#' @param memory Memory allocation (default: "2Gi")
#' @param cpu CPU allocation (default: 2)
#' @param max_instances Maximum number of instances (default: 10)
#' @param timeout Request timeout in seconds (default: 3600)
#' @param allow_unauthenticated Whether to allow unauthenticated access (default: TRUE)
#' @param dry_run If TRUE, only show the command without executing (default: FALSE)
#'
#' @return Invisible NULL, called for side effects
#' @export
#'
#' @examples
#' \dontrun{
#' # Deploy with defaults
#' deploy_cloudrun()
#'
#' # Deploy with custom settings
#' deploy_cloudrun(
#'   project_id = "my-project",
#'   region = "us-central1",
#'   memory = "4Gi",
#'   cpu = 4
#' )
#'
#' # Dry run to see command
#' deploy_cloudrun(dry_run = TRUE)
#' }
deploy_cloudrun <- function(project_id = "crea-aq-data",
                           region = "europe-west1",
                           service_name = "emissiondashboard",
                           memory = "2Gi",
                           cpu = 2,
                           max_instances = 10,
                           timeout = 3600,
                           allow_unauthenticated = TRUE,
                           dry_run = FALSE) {

  # Check if we're in the right directory
  if (!file.exists("creaemission.Rproj")) {
    stop("This function must be run from the package root directory (where creaemission.Rproj is located)")
  }

  # Check if gcloud is available
  # Add path (Not ideal as quite machine specific)
  Sys.setenv(PATH = paste("~/google-cloud-sdk/bin", Sys.getenv("PATH"), sep = ":"))


  # Build the deployment command
  cmd <- sprintf(
    "gcloud run deploy %s --source . --platform managed --region %s --project %s --port 8080 --memory %s --cpu %d --timeout %d --min-instances 0 --max-instances %d",
    service_name, region, project_id, memory, cpu, timeout, max_instances
  )

  if (allow_unauthenticated) {
    cmd <- paste(cmd, "--allow-unauthenticated")
  }

  # Show the command
  message("🚀 Deploying CREA Emission Portal to Google Cloud Run")
  message("Project: ", project_id)
  message("Region: ", region)
  message("Service: ", service_name)
  message("Memory: ", memory)
  message("CPU: ", cpu)
  message("Max instances: ", max_instances)
  message("Timeout: ", timeout, "s")
  message("Public access: ", ifelse(allow_unauthenticated, "Yes", "No"))
  message()

  if (dry_run) {
    message("🔍 DRY RUN - Command that would be executed:")
    message(cmd)
    return(invisible(NULL))
  }

  # Check if user wants to proceed
  message("This will deploy your app to Google Cloud Run.")
  message("Make sure you have:")
  message("  - gcloud CLI installed and configured")
  message("  - Docker running")
  message("  - Proper permissions for project: ", project_id)
  message()

  proceed <- readline("Do you want to proceed? (y/N): ")
  if (!grepl("^[Yy]", proceed)) {
    message("Deployment cancelled.")
    return(invisible(NULL))
  }

  # Execute the deployment
  message("Starting deployment...")
  message("This may take several minutes...")

  tryCatch({
    result <- system(cmd, intern = TRUE)

    if (attr(result, "status") == 0) {
      message("✅ Deployment completed successfully!")

      # Get the service URL
      url_cmd <- sprintf(
        "gcloud run services describe %s --region=%s --project=%s --format='value(status.url)'",
        service_name, region, project_id
      )

      service_url <- system(url_cmd, intern = TRUE)
      if (length(service_url) > 0 && nzchar(service_url[1])) {
        message("🌐 Your app is available at: ", service_url[1])
      }

      message()
      message("Useful commands:")
      message("  - View logs: gcloud run services logs tail ", service_name, " --region=", region)
      message("  - Update service: gcloud run services update ", service_name, " --region=", region)
      message("  - Delete service: gcloud run services delete ", service_name, " --region=", region)

    } else {
      message("❌ Deployment failed. Check the output above for errors.")
    }

  }, error = function(e) {
    message("❌ Deployment failed: ", e$message)
    message("Please check your gcloud configuration and try again.")
  })

  invisible(NULL)
}

#' Get deployment status for Cloud Run service
#'
#' @param service_name Name of the Cloud Run service (default: "creaemission")
#' @param project_id Google Cloud project ID (default: "crea-aq-data")
#' @param region Google Cloud region (default: "europe-west1")
#'
#' @return A list with service information
#' @export
#'
#' @examples
#' \dontrun{
#' status <- get_cloudrun_status()
#' print(status)
#' }
get_cloudrun_status <- function(service_name = "creaemission",
                               project_id = "crea-aq-data",
                               region = "europe-west1") {

  if (!nzchar(Sys.which("gcloud"))) {
    stop("gcloud CLI is not available. Please install Google Cloud SDK first.")
  }

  cmd <- sprintf(
    "gcloud run services describe %s --region=%s --project=%s --format=json",
    service_name, region, project_id
  )

  tryCatch({
    result <- system(cmd, intern = TRUE)

    if (attr(result, "status") == 0) {
      jsonlite::fromJSON(paste(result, collapse = "\n"))
    } else {
      stop("Failed to get service status")
    }

  }, error = function(e) {
    stop("Error getting service status: ", e$message)
  })
}

#' View Cloud Run service logs
#'
#' @param service_name Name of the Cloud Run service (default: "creaemission")
#' @param project_id Google Cloud project ID (default: "crea-aq-data")
#' @param region Google Cloud region (default: "europe-west1")
#' @param tail Whether to follow logs in real-time (default: TRUE)
#' @param limit Number of log lines to show (default: 50)
#'
#' @return Invisible NULL, called for side effects
#' @export
#'
#' @examples
#' \dontrun{
#' # View recent logs
#' view_cloudrun_logs(tail = FALSE, limit = 100)
#'
#' # Follow logs in real-time
#' view_cloudrun_logs(tail = TRUE)
#' }
view_cloudrun_logs <- function(service_name = "creaemission",
                               project_id = "crea-aq-data",
                               region = "europe-west1",
                               tail = TRUE,
                               limit = 50) {

  if (!nzchar(Sys.which("gcloud"))) {
    stop("gcloud CLI is not available. Please install Google Cloud SDK first.")
  }

  if (tail) {
    cmd <- sprintf(
      "gcloud run services logs tail %s --region=%s --project=%s",
      service_name, region, project_id
    )
    message("Following logs in real-time (press Ctrl+C to stop)...")
  } else {
    cmd <- sprintf(
      "gcloud run services logs read %s --region=%s --project=%s --limit=%d",
      service_name, region, project_id, limit
    )
    message("Showing last ", limit, " log entries...")
  }

  system(cmd)

  invisible(NULL)
}

#' Delete Cloud Run service
#'
#' @param service_name Name of the Cloud Run service (default: "creaemission")
#' @param project_id Google Cloud project ID (default: "crea-aq-data")
#' @param region Google Cloud region (default: "europe-west1")
#' @param confirm Whether to ask for confirmation (default: TRUE)
#'
#' @return Invisible NULL, called for side effects
#' @export
#'
#' @examples
#' \dontrun{
#' delete_cloudrun_service()
#' }
delete_cloudrun_service <- function(service_name = "creaemission",
                                   project_id = "crea-aq-data",
                                   region = "europe-west1",
                                   confirm = TRUE) {

  if (!nzchar(Sys.which("gcloud"))) {
    stop("gcloud CLI is not available. Please install Google Cloud SDK first.")
  }

  if (confirm) {
    message("⚠️  WARNING: This will permanently delete the Cloud Run service!")
    message("Service: ", service_name)
    message("Region: ", region)
    message("Project: ", project_id)
    message()

    proceed <- readline("Are you sure you want to delete this service? (y/N): ")
    if (!grepl("^[Yy]", proceed)) {
      message("Deletion cancelled.")
      return(invisible(NULL))
    }
  }

  cmd <- sprintf(
    "gcloud run services delete %s --region=%s --project=%s --quiet",
    service_name, region, project_id
  )

  message("Deleting service...")
  result <- system(cmd, intern = TRUE)

  if (attr(result, "status") == 0) {
    message("✅ Service deleted successfully!")
  } else {
    message("❌ Failed to delete service. Check the output above for errors.")
  }

  invisible(NULL)
}
