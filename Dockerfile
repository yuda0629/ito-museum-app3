FROM rocker/shiny:4.4.2

# 必要なパッケージをインストール
RUN R -e "install.packages(c('leaflet','dplyr','readr','htmltools'), repos='https://cran.rstudio.com')"

# アプリファイルをコピー
COPY app.R /srv/shiny-server/
COPY App2_impl.R /srv/shiny-server/
COPY App2.r /srv/shiny-server/
COPY ito_sites_master.csv /srv/shiny-server/

# Hugging Face Spacesは7860番ポートを使用
EXPOSE 7860

# shiny-serverの設定でポートを7860に変更
RUN echo 'run_as shiny;\n\
server {\n\
  listen 7860;\n\
  location / {\n\
    site_dir /srv/shiny-server;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}' > /etc/shiny-server/shiny-server.conf

CMD ["/usr/bin/shiny-server"]
