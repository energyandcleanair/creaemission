# Use the rocker/tidyverse image which includes devtools
FROM --platform=linux/amd64 rocker/shiny-verse:4

# Set environment variables
ENV PORT=8080
ENV R_CONFIG_ACTIVE=production
ENV TITILER_PORT=8000

# Install pak for better package management
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')"

# Install additional packages for running multiple services
RUN apt-get update && apt-get install -y \
    supervisor \
    nginx \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy the whole project
COPY . /app/
RUN R -e "pak::local_install_deps('.', ask = FALSE)"
RUN R -e "pak::local_install('.', ask = FALSE)"

# Configure nginx for reverse proxy
COPY nginx.conf /etc/nginx/nginx.conf

# Configure supervisor to run multiple services
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# Make scripts executable
RUN chmod +x /app/start_services.sh

# Expose both ports
EXPOSE 8080 8000

# Start supervisor to manage both services
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord.conf"]
