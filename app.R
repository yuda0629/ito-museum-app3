# shinyapps.io / Posit Connect 用の既定エントリ（app.R）
# LC_CTYPE=C だと日本語 UI が <U+XXXX> になるため、parse 前に UTF-8 ロケールを試す。
for (.loc in c("C.UTF-8", "en_US.UTF-8", "ja_JP.UTF-8")) {
  if (!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", .loc)))) {
    break
  }
}

library(shiny)
library(leaflet)
library(dplyr)
library(readr)

.app_cmd <- commandArgs(trailingOnly = FALSE)
.app_file <- sub("^--file=", "", .app_cmd[startsWith(.app_cmd, "--file=")])
.app_dir <- if (length(.app_file) && nzchar(.app_file[[1]])) {
  dirname(normalizePath(.app_file[[1]], winslash = "/", mustWork = TRUE))
} else {
  getwd()
}
.app_impl <- file.path(.app_dir, "App2_impl.R")
if (!file.exists(.app_impl)) {
  stop(
    "App2_impl.R が見つかりません: ", .app_impl,
    "\nターミナルではプロジェクト直下に cd してから実行するか、",
    "Run App 対象をこのフォルダの app.R / App2.r にしてください。",
    call. = FALSE
  )
}

setwd(.app_dir)

app_env <- new.env(parent = globalenv())
eval(parse(.app_impl, encoding = "UTF-8"), envir = app_env)
shiny::shinyApp(ui = app_env$ui, server = app_env$server)
