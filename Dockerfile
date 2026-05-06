FROM rocker/shiny:4.4.2

RUN R -e "install.packages(c('leaflet','dplyr','readr','htmltools'), repos='https://cran.rstudio.com', dependencies=TRUE)"

COPY app.R /srv/shiny-server/app.R
COPY App2_impl.R /srv/shiny-server/
COPY App2.r /srv/shiny-server/
COPY ito_sites_master.csv /srv/shiny-server/

EXPOSE 7860

RUN printf 'run_as shiny;\nserver {\n  listen 7860;\n  location / {\n    site_dir /srv/shiny-server;\n    log_dir /var/log/shiny-server;\n    directory_index on;\n  }\n}\n' > /etc/shiny-server/shiny-server.conf

CMD ["/usr/bin/shiny-server"]
