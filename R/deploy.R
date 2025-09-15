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
                           # Readiness + startup options
                           use_startup_probe = TRUE,
                           startup_path = "/ready",
                           startup_period_seconds = 5,
                           startup_timeout_seconds = 2,
                           startup_failure_threshold = 120,
                           cpu_boost = TRUE,
                           dry_run = FALSE) {

  # Check if we're in the right directory
  if (!file.exists("creaemission.Rproj")) {
    stop("This function must be run from the package root directory (where creaemission.Rproj is located)")
  }

  # Check if gcloud is available
  # Add path (Not ideal as quite machine specific)
  Sys.setenv(PATH = paste("~/google-cloud-sdk/bin", Sys.getenv("PATH"), sep = ":"))


  # Build the deployment command (initial deploy to create/update the service)
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
  message("Starting deployment (source build + deploy)...")
  message("This may take several minutes...")

  res <- system(cmd)
  if (res != 0) {
    stop("Initial gcloud run deploy failed (source build). See output above.")
  }

  # Optionally enable CPU boost and apply startup probe via YAML replace
  # We fetch the resolved image and generate a minimal Service spec to set startupProbe
  if (use_startup_probe || cpu_boost) {
    message("Configuring readiness (startup probe) and CPU boost...")

    img_cmd <- sprintf(
      "gcloud run services describe %s --region=%s --project=%s --format=\"value(spec.template.spec.containers[0].image)\"",
      service_name, region, project_id
    )
    image <- tryCatch(trimws(system(img_cmd, intern = TRUE)[1]), error = function(e) "")
    if (!nzchar(image)) {
      warning("Could not resolve deployed image; skipping startupProbe apply.")
    } else {
      yaml_path <- tempfile(fileext = ".yaml")
      # Construct Service YAML
      yaml <- sprintf("apiVersion: serving.knative.dev/v1\nkind: Service\nmetadata:\n  name: %s\nspec:\n  template:\n    metadata:\n      annotations:\n        %s\n    spec:\n      timeoutSeconds: %d\n      containers:\n      - image: \"%s\"\n        ports:\n        - containerPort: 8080\n        resources:\n          limits:\n            memory: '%s'\n            cpu: '%s'\n%s",
        service_name,
        if (isTRUE(cpu_boost)) "run.googleapis.com/cpu-boost: \"true\"" else "",
        as.integer(timeout),
        image,
        memory,
        as.character(cpu),
        if (isTRUE(use_startup_probe)) sprintf("        startupProbe:\n          httpGet:\n            path: %s\n            port: 8080\n          periodSeconds: %d\n          failureThreshold: %d\n          timeoutSeconds: %d\n",
            startup_path, as.integer(startup_period_seconds), as.integer(startup_failure_threshold), as.integer(startup_timeout_seconds)) else ""
      )
      writeLines(yaml, yaml_path)

      # Apply YAML via replace and optionally enable cpu-boost (also as direct flag)
      if (isTRUE(cpu_boost)) {
        system(sprintf("gcloud run services update %s --region %s --cpu-boost", service_name, region))
      }
      repl_cmd <- sprintf("gcloud run services replace %s --region %s --project %s", shQuote(yaml_path), region, project_id)
      rc <- system(repl_cmd)
      if (rc != 0) {
        warning("gcloud run services replace failed; startupProbe may not be applied. Check logs.")
      }
    }
  }

  # Emit service URL and helpful commands
  url_cmd <- sprintf(
    "gcloud run services describe %s --region=%s --project=%s --format='value(status.url)'",
    service_name, region, project_id
  )
  service_url <- tryCatch(system(url_cmd, intern = TRUE)[1], error = function(e) "")
  if (nzchar(service_url)) message("🌐 Your app is available at: ", service_url)
  message()
  message("Useful commands:")
  message("  - Tail logs: gcloud beta run services logs read ", service_name, " --region=", region)
  message("  - Update service: gcloud run services update ", service_name, " --region=", region)
  message("  - Replace from YAML: gcloud run services replace <file.yaml> --region=", region)

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
