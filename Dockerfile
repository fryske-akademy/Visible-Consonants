# Use the Rocker project’s Shiny base image
FROM rocker/shiny:4.5.1

# Install system dependencies
RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y libxml2-dev
RUN apt-get install -y libicu-dev
RUN apt-get install -y libudunits2-dev
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y libwebp-dev
RUN apt-get install -y libpoppler-cpp-dev
RUN apt-get install -y pkg-config
RUN apt-get install -y libcairo2-dev
RUN apt-get install -y libxt-dev
RUN apt-get install -y tcl8.6
RUN apt-get install -y tk8.6
RUN apt-get install -y libtcl8.6 
RUN apt-get install -y libtk8.6 
RUN apt-get install -y tcl-dev 
RUN apt-get install -y tk-dev
RUN apt-get install -y texlive-latex-base 
RUN apt-get install -y texlive-latex-recommended
RUN apt-get install -y texlive-fonts-recommended 
RUN apt-get install -y texlive-latex-extra
RUN apt-get install -y libharfbuzz-dev
RUN apt-get install -y libfribidi-dev
RUN apt-get install -y g++
RUN rm -rf /var/lib/apt/lists/*

# Copy your app to the container
COPY ./ /srv/shiny-server/

# Move custom config into place
RUN mv /srv/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Install R package dependencies
RUN R -e "install.packages('shiny',       dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyBS',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyjs',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tidyr',       dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('PBSmapping',  dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('formattable', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggplot2',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggrepel',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('readxl',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('WriteXLS',    dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('DT',          dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('pracma',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('plyr',        dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('svglite',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('Cairo',       dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tikzDevice',  dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinybusy',   dependencies=TRUE, repos='https://cloud.r-project.org/')"

# Expose the Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]
