# Deploying CREA Emission Portal to Google Cloud Run

This guide walks you through deploying the CREA Emission Portal Shiny app to Google Cloud Run using the Google Cloud CLI.

## Environment Variables

Set these environment variables for your deployment:

```bash
export PROJECT_ID="crea-aq-data"
export REGION="europe-west1"
export SERVICE_NAME="emissiondashboard"
```

## Prerequisites

1. **Google Cloud Account**: You need a Google Cloud account with billing enabled
2. **Google Cloud CLI**: Install and configure the `gcloud` CLI tool
3. **Docker**: Install Docker on your local machine
4. **R Environment**: Ensure you have R installed locally for testing

## Run Locally

Before deploying to the cloud, you can test your app locally using Docker:

```bash
# Build the image
docker build --platform linux/amd64 -t creaemission .

# Run the container
docker run -d -p 8080:8080 --name creaemission creaemission

# Check logs
docker logs creaemission

# Stop and cleanup
docker stop creaemission
docker rm creaemission
```

## Setup Steps

### 1. Install and Configure Google Cloud CLI

```bash
# Install gcloud CLI (macOS)
curl https://sdk.cloud.google.com | bash
exec -l $SHELL

# Or install via Homebrew
brew install google-cloud-sdk

# Initialize and authenticate
gcloud init
gcloud auth login
gcloud config set project $PROJECT_ID
```

### 2. Enable Required APIs

```bash
# Enable Cloud Run API
gcloud services enable run.googleapis.com

# Enable Container Registry API (if using Artifact Registry)
gcloud services enable artifactregistry.googleapis.com

# Enable Cloud Build API (if building in the cloud)
gcloud services enable cloudbuild.googleapis.com
```

### 3. Configure Docker for Google Cloud

```bash
# Configure Docker to use gcloud as a credential helper
gcloud auth configure-docker
```

## Building and Deploying

### Cloud Build (Recommended)

```bash
# Deploy using Cloud Build (automatically builds and deploys)
gcloud run deploy $SERVICE_NAME \
  --source . \
  --platform managed \
  --region $REGION \
  --project $PROJECT_ID \
  --allow-unauthenticated \
  --port 8080 \
  --memory 2Gi \
  --cpu 2 \
  --timeout 3600 \
  --min-instances 0 \
  --max-instances 2
```
