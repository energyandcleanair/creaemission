# Use the rocker/tidyverse image which includes devtools
FROM --platform=linux/amd64 rocker/shiny-verse:4.3

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV SHINY_PORT=8080
ENV SHINY_HOST=0.0.0.0
ENV PORT=8080

# Install system dependencies and Shiny Server
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libgsl-dev \
    libhdf5-dev \
    libnetcdf-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff-dev \
    libcairo2-dev \
    libxt-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    xdg-utils \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install pak for better package management
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')"

# Install Shiny server
RUN R -e "pak::pkg_install('shiny', ask = FALSE)"

# Set working directory
WORKDIR /app

# Copy the R package files
COPY . /app/


# Install the package with explicit version handling
RUN R -e "pak::local_install_deps('.', ask = FALSE); pak::local_install('.', ask = FALSE)"


# Create a simple startup script
RUN echo '#!/bin/bash\n\
R -e "shiny::runApp(\"/app/inst\", port = 8080, host = \"0.0.0.0\")"' > /app/start.sh && \
    chmod +x /app/start.sh

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:8080/ || exit 1

# Start the app
CMD ["/app/start.sh"]
