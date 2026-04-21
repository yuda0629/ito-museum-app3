# Entry: UTF-8 LC_CTYPE を設定してから App2_impl.R を parse（LC_CTYPE=C 時の <U+XXXX> 対策）
for (.loc in c("C.UTF-8", "en_US.UTF-8", "ja_JP.UTF-8")) {
  if (!is.na(suppressWarnings(Sys.setlocale("LC_CTYPE", .loc)))) {
    break
  }
}
# App2_impl.R はエントリと同じフォルダを優先（別ディレクトリから Rscript したときの対策）
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
    "ファイルにフルパスを指定してください。",
    call. = FALSE
  )
}
# データ CSV 等は相対パスのため、実体と同じフォルダを作業ディレクトリにする
setwd(.app_dir)
eval(parse(.app_impl, encoding = "UTF-8"))
shinyApp(ui, server)
