# Deploying CREA Emission Portal to Google Cloud Run

This guide walks you through deploying the CREA Emission Portal (Shiny app + TiTiler server) to Google Cloud Run as a single service using the Google Cloud CLI.


## Architecture Overview

This project uses a **hybrid deployment strategy** with separate Dockerfiles for different environments:

### Local Development (docker-compose + Dockerfile.shiny)
- **Multi-container setup** using `docker-compose.yml`
- **TiTiler**: Separate container (`ghcr.io/developmentseed/titiler:latest`) on port 8000
- **Shiny App**: `Dockerfile.shiny` (Shiny-only) with nginx reverse proxy
 - **nginx**: Proxies TiTiler requests to TiTiler service; app uses same-origin `/cog/` path
 - **R Code**: Uses relative base (e.g., `/cog/...`) so it works in all envs
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
export SERVICE_NAME="emissiondashboard-staging"

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
  --cpu-boost \
  --timeout 3600 \
  --startup-probe=httpGet.path=/ready,httpGet.port=8080,periodSeconds=2,timeoutSeconds=2,failureThreshold=120 \
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
- **TiTiler API**: http://localhost:8080/cog/

## Run Locally (composer)
For regular development, use `docker-compose up` instead.

## Use TiTiler with native Shiny (no Docker for Shiny)
When running Shiny directly (e.g., from RStudio), start TiTiler separately and expose your local `data/` so tiles can be served fast:

```bash
# 1) Start TiTiler (Docker)
#    Note: maps host port 80 -> container 8000
docker run -d --name titiler-local \
  -p 80:8000 \
  -e TITILER_LOCALFILE_ENABLED=true \
  -e TITILER_TILESIZE=1024 \
  -v "$PWD/data":/data:ro \
  -v "$PWD/cache":/cache:ro \
  ghcr.io/developmentseed/titiler:latest

# 2) Serve static files so TiTiler can fetch COGs over HTTP at http://127.0.0.1:8080/data/
#    Run from the project root so /data is accessible at /data/...
python3 -m http.server 8080 --bind 127.0.0.1

# 3) In your R session (before launching the app), point the app to TiTiler
export TITILER_URL=http://localhost
# TiTiler container must fetch COGs from your host; use host.docker.internal on macOS
export TITILER_DATA_BASE=http://host.docker.internal:8080
# or inside R:
# Sys.setenv(TITILER_URL = "http://localhost",
#            TITILER_DATA_BASE = "http://host.docker.internal:8080")
```

Then run the Shiny app normally (e.g., runApp("inst")) and choose TiTiler rendering in the UI. Health check: `http://localhost/cog/tileMatrixSets` should return JSON when TiTiler is up.
