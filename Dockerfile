# Use the Rocker project’s Shiny base image
FROM rocker/shiny:4.5.1

# Install system dependencies
RUN apt-get update && apt-get install -y libcurl4-openssl-dev curl libssl-dev \
libxml2-dev libicu-dev libudunits2-dev zlib1g-dev libwebp-dev \
libpoppler-cpp-dev pkg-config libcairo2-dev libxt-dev tcl8.6 tk8.6 libtcl8.6 \
libtk8.6 tcl-dev tk-dev texlive-latex-base texlive-latex-recommended \
texlive-fonts-recommended texlive-latex-extra libharfbuzz-dev libfribidi-dev \
g++

RUN rm -rf /var/lib/apt/lists/*

# Copy your app to the container
COPY ./ /srv/shiny-server/

# Move custom config into place
RUN mv /srv/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Install R package dependencies
RUN R -e "install.packages(c('shiny', 'shinyBS', 'shinyjs', 'tidyr', \
'PBSmapping', 'formattable', 'ggplot2', 'ggrepel', 'readxl', 'WriteXLS', 'DT', \
'pracma', 'plyr', 'svglite', 'Cairo', 'tikzDevice', 'shinybusy'))"

# Expose the Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]
