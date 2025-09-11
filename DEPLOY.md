# Deploying CREA Emission Portal to Google Cloud Run

This guide walks you through deploying the CREA Emission Portal (Shiny app + TiTiler server) to Google Cloud Run as a single service using the Google Cloud CLI.


## Architecture Overview

This project uses a **hybrid deployment strategy** with separate Dockerfiles for different environments:

### Local Development (docker-compose + Dockerfile.shiny)
- **Multi-container setup** using `docker-compose.yml`
- **TiTiler**: Separate container (`ghcr.io/developmentseed/titiler:latest`) on port 8000
- **Shiny App**: `Dockerfile.shiny` (Shiny-only) with nginx reverse proxy
- **nginx**: Proxies TiTiler requests to `localhost:8000` (docker network)
- **R Code**: Always uses `http://localhost:8000`
- **Benefits**: Clean separation, no conflicts, easy debugging

### Production (Cloud Run + Dockerfile)
- **Single-container deployment** using main `Dockerfile`
- **All services** (nginx + Shiny + TiTiler) managed by supervisord
- **Port Configuration**: nginx (8080) → Shiny (3838), TiTiler (8000)


## Deployment

Set these environment variables for staging deployment:

```bash
export PROJECT_ID="crea-aq-data"
export REGION="europe-west1"
export SERVICE_NAME="emissiondashboard"

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


## Run Locally (single container)

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

## Run Locally (composer)
For regular development, use `docker-compose up` instead.
