FROM rocker/shiny:4.4.2

# cache bust: 2026-05-06
RUN R -e "install.packages('leaflet', repos='https://cran.rstudio.com', dependencies=TRUE)"
RUN R -e "install.packages('dplyr', repos='https://cran.rstudio.com', dependencies=TRUE)"
RUN R -e "install.packages('readr', repos='https://cran.rstudio.com', dependencies=TRUE)"
RUN R -e "install.packages('htmltools', repos='https://cran.rstudio.com', dependencies=TRUE)"

COPY app.R /srv/shiny-server/app.R
COPY App2_impl.R /srv/shiny-server/
COPY App2.r /srv/shiny-server/
COPY ito_sites_master.csv /srv/shiny-server/

EXPOSE 7860

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=7860)"]
