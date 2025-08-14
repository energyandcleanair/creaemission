# Use the rocker/tidyverse image which includes devtools
FROM --platform=linux/amd64 rocker/shiny-verse:4

# # Set environment variables
ENV PORT=8080

# Install pak for better package management
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')"


# Set working directory
WORKDIR /app

# THIS DOES NOT WORK FOR SOME REASON
# Copy the R package files to avoid invalidating cache unnecessarily
# COPY ./DESCRIPTION /app/DESCRIPTION
# RUN R -e "pak::local_install_deps('.', ask = FALSE)"
# Install the package with explicit version handling

# SO WE COPY THE WHOLE THING
COPY . /app/
RUN R -e "pak::local_install_deps('.', ask = FALSE)"
RUN R -e "pak::local_install('.', ask = FALSE)"

CMD ["R", "-e", "shiny::runApp('/app/inst', port = 8080, host = '0.0.0.0')"]
