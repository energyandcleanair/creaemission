# CREA Emission Portal - Deployment Examples

This directory contains examples of how to deploy the CREA Emission Portal using the built-in R functions.

## Quick Start

```r
# Load the package
library(creaemission)

# Deploy to Google Cloud Run with defaults
deploy_cloudrun()

# Deploy to Shinyapps.io
deploy_shinyapp("crea-emissions", account = "your-account")
```

## Available Functions

### Deployment Functions

- **`deploy_cloudrun()`** - Deploy to Google Cloud Run (recommended)
- **`deploy_shinyapp()`** - Deploy to Shinyapps.io

### Management Functions

- **`get_cloudrun_status()`** - Check service status
- **`view_cloudrun_logs()`** - View service logs
- **`check_deployment_health()`** - Health check
- **`delete_cloudrun_service()`** - Delete service

## Examples

See `deployment_examples.R` for comprehensive examples including:

- Basic deployment
- Custom resource allocation
- Environment-specific deployments
- Health monitoring
- Cleanup procedures

## Prerequisites

### For Google Cloud Run
- Google Cloud SDK (`gcloud` CLI)
- Docker running
- Proper permissions for project `crea-aq-data`

### For Shinyapps.io
- `rsconnect` package
- Shinyapps.io account configured

## Configuration

The functions use these defaults:
- **Project**: `crea-aq-data`
- **Region**: `europe-west1`
- **Service**: `creaemission`
- **Memory**: `2Gi`
- **CPU**: `2`

You can override any of these parameters when calling the functions.
