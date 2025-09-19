# Fast debug Dockerfile - skips R package installations for quicker testing
FROM --platform=linux/amd64 rocker/shiny-verse:4

# Set environment variables
ENV PORT=8080
ENV R_CONFIG_ACTIVE=production
ENV TITILER_PORT=8001

# Install minimal additional packages for running multiple services
RUN apt-get update && apt-get install -y \
    supervisor \
    nginx \
    curl \
    # Python for TiTiler
    python3 python3-pip python3-dev python3-venv \
    # System libs required by R packages (units, magick, geospatial)
    libudunits2-dev \ 
    libmagick++-dev \
    libgdal-dev gdal-bin \
    libgeos-dev libproj-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy the whole project (including data for testing)
COPY . /app/

# Clean up any dangling symlinks in inst/ that can break package install
RUN find /app/inst -xtype l -exec rm -f {} + || true

# Install pak (faster dependency resolution/installs) and install local package
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')"
RUN R -e "remotes::install_local('/app', dependencies = TRUE, upgrade = 'never')"

# Prebuild lightweight available_data caches (optional)
RUN  R -f /app/inst/scripts/prebuild_available_data.R

# Install TiTiler in an isolated virtual environment (avoid PEP 668 issues)
RUN python3 -m venv /opt/titiler-venv \
 && /opt/titiler-venv/bin/pip install --upgrade pip setuptools wheel \
 && /opt/titiler-venv/bin/pip install "uvicorn[standard]" titiler

# Ensure venv binaries are on PATH at runtime
ENV PATH="/opt/titiler-venv/bin:${PATH}"

# Configure supervisor to run multiple services (main config)
COPY supervisord.conf /etc/supervisor/supervisord.conf

# Configure nginx
COPY nginx.conf /etc/nginx/nginx.conf

# Make scripts executable
RUN chmod +x /app/start_titiler_service.sh /app/start_shiny_app.sh

EXPOSE 8080 8001

# Start supervisor to manage both services
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord.conf", "-n"]
