FROM rocker/shiny:4.4.2

RUN R -e "install.packages(c('leaflet','dplyr','readr','htmltools'), repos='https://cran.rstudio.com', dependencies=TRUE)"

COPY app.R /srv/shiny-server/app.R
COPY App2_impl.R /srv/shiny-server/
COPY App2.r /srv/shiny-server/
COPY ito_sites_master.csv /srv/shiny-server/

EXPOSE 7860

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=7860)"]
