# Deploying CREA Emission Portal to Google Cloud Run

This guide walks you through deploying the CREA Emission Portal (Shiny app + TiTiler server) to Google Cloud Run as a single service using the Google Cloud CLI.

## Architecture Overview

This project uses a **hybrid deployment strategy** with separate Dockerfiles for different environments:

### Local Development (docker-compose + Dockerfile.shiny)
- **Multi-container setup** using `docker-compose.yml`
- **TiTiler**: Separate container (`ghcr.io/developmentseed/titiler:latest`) on port 8000
- **Shiny App**: `Dockerfile.shiny` (Shiny-only) with nginx reverse proxy
- **nginx**: Proxies TiTiler requests to external service at `http://titiler:8000`
- **Benefits**: Clean separation, no conflicts, easy debugging

### Production (Cloud Run + Dockerfile)
- **Single-container deployment** using main `Dockerfile`
- **All services** (nginx + Shiny + TiTiler) managed by supervisord
- **Benefits**: Cost-effective, simpler scaling, Cloud Run compatible
- **TiTiler**: Runs locally within container at `localhost:8000`

### Why Two Dockerfiles?

| Aspect | Local (Dockerfile.shiny) | Production (Dockerfile) |
|--------|-------------------------|-------------------------|
| **TiTiler Source** | External (docker-compose) | Internal (supervisord) |
| **Complexity** | Low (Shiny + nginx only) | Medium (3 services) |
| **Conflicts** | ❌ None | ❌ Port conflicts avoided |
| **Resource Usage** | Lower (no duplicate TiTiler) | Higher (includes TiTiler) |
| **Deployment** | Docker Compose only | Cloud Run + Docker Compose |

**Key Insight**: Separate Dockerfiles prevent conflicts while maintaining flexibility - use docker-compose locally for clean development, deploy combined container to Cloud Run for production efficiency.

## Environment Variables

Set these environment variables for your deployment:

```bash
export PROJECT_ID="crea-aq-data"
export REGION="europe-west1"
export SERVICE_NAME="emissiondashboard"
export SERVICE_URL="emission.energyandcleanair.org"
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

## Run Locally (Single Service - Production Simulation)

For testing the production setup locally with all services in one container:

```bash
# Build the combined image (includes TiTiler + Shiny + nginx)
docker build -t creaemission .  # Uses main Dockerfile

# Run the container
docker run -d -p 8080:8080 --name creaemission creaemission

# View logs
docker logs -f creaemission

# Stop and cleanup
docker stop creaemission
docker rm creaemission
```

This simulates production deployment with:
- **Combined Service**: http://localhost:8080
- **Shiny App**: http://localhost:8080/
- **TiTiler API**: http://localhost:8080/titiler/

**Note**: For regular development, use `docker-compose up` instead, which uses the cleaner `Dockerfile.shiny` without conflicts.

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
  --memory 4Gi \
  --cpu 2 \
  --timeout 3600 \
  --min-instances 0 \
  --max-instances 2
```

### Single Service Deployment

For deploying both services in a single Cloud Run service:

```bash
# Deploy the combined service
gcloud run deploy $SERVICE_NAME \
  --source . \
  --platform managed \
  --region $REGION \
  --project $PROJECT_ID \
  --allow-unauthenticated \
  --port 8080 \
  --memory 4Gi \
  --cpu 2 \
  --timeout 3600 \
  --min-instances 0 \
  --max-instances 2 \
  --set-env-vars="R_CONFIG_ACTIVE=production,TITILER_PORT=8000"
```

This single service will provide:
- **Shiny App**: `https://[SERVICE_URL]/`
- **TiTiler API**: `https://[SERVICE_URL]/titiler/`

## Custom Domain Setup

To use a custom subdomain like `emission.energyandcleanair.org`:

### 1. Map Custom Domain

```bash
# Map the custom domain to your Cloud Run service
gcloud beta run domain-mappings create \
  --service $SERVICE_NAME \
  --region $REGION \
  --domain $SERVICE_URL \
  --project $PROJECT_ID
```

### 2. DNS Configuration

You'll need to add a CNAME record in Cloudflare:

```
Type: CNAME
Name: emission
Value: ghs.googlehosted.com.
TTL: 3600 (or default)
```

### 3. SSL Certificate

Google Cloud automatically provisions SSL certificates for custom domains. The process may take 24-48 hours.
