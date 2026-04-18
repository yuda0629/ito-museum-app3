# =============================
# 伊都国デジタル展示アプリ（採用確定レベル完全版）
# コンセプト：伊都国における権力構造の可視化
# =============================

library(shiny)
library(leaflet)
library(dplyr)
library(readr)

# ===== データ読み込み =====
data <- read_csv("ito_sites_master.csv", show_col_types = FALSE)

# ===== カラー定義（権力構造可視化） =====
getColor <- function(type) {
  if (type == "墳墓") return("red")
  if (type == "集落") return("blue")
  if (type == "祭祀") return("green")
  return("gray")
}

# ===== UI =====
ui <- fluidPage(
  titlePanel("伊都国デジタル展示 - 権力構造の可視化"),

  sidebarLayout(
    sidebarPanel(
      h3("展示ナビゲーション"),

      selectInput("era", "時代選択",
                  choices = c("すべて", unique(data$period))),

      checkboxGroupInput("type", "遺跡種別",
                         choices = unique(data$type),
                         selected = unique(data$type)),

      hr(),
      h4("展示解説"),
      textOutput("explanation")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("① 伊都国とは",
                 h3("伊都国の概要"),
                 p("伊都国は弥生時代において外交・政治の中心的役割を担った地域である。"),
                 p("本展示では、その権力構造を遺跡データから読み解く。")
        ),

        tabPanel("② 分布を見る",
                 fluidRow(
                   column(8, leafletOutput("map", height = 600)),
                   column(4,
                          h3("遺跡詳細"),
                          htmlOutput("detail")
                   )
                 )
        ),

        tabPanel("③ 比較する",
                 selectInput("siteA", "比較対象A", choices = data$name),
                 selectInput("siteB", "比較対象B", choices = data$name),
                 tableOutput("compare")
        ),

        tabPanel("④ 結論",
                 h3("展示からわかること"),
                 p("伊都国では首長層を頂点とする階層的社会構造が形成されていたと考えられる。"),
                 p("大型墳墓による権威の表象と、それを支える集落の分布は、政治的統合の進展を示している。")
        )
      )
    )
  )
)

# ===== サーバー =====
server <- function(input, output, session) {

  selected_site <- reactiveVal(NULL)

  filtered <- reactive({
    df <- data

    if (input$era != "すべて") {
      df <- df %>% filter(period == input$era)
    }

    df <- df %>% filter(type %in% input$type)
    df
  })

  output$map <- renderLeaflet({
    df <- filtered()

    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(~lng, ~lat,
                       color = ~sapply(type, getColor),
                       layerId = ~name,
                       radius = 8,
                       popup = ~name)
  })

  observeEvent(input$map_marker_click, {
    selected_site(input$map_marker_click$id)
  })

  output$detail <- renderUI({
    req(selected_site())

    site <- data %>% filter(name == selected_site())

    HTML(paste0(
      "<h4>", site$name, "</h4>",
      "<p><b>時代：</b>", site$period, "</p>",
      "<p><b>種別：</b>", site$type, "</p>",
      "<p>", site$desc, "</p>"
    ))
  })

  output$compare <- renderTable({
    a <- data %>% filter(name == input$siteA)
    b <- data %>% filter(name == input$siteB)

    data.frame(
      項目 = c("時代", "種別", "説明"),
      A = c(a$period, a$type, a$desc),
      B = c(b$period, b$type, b$desc)
    )
  })

  output$explanation <- renderText({
    if (input$era != "すべて") {
      return("弥生時代後期には政治的統合が進み、伊都国の重要性が高まったと考えられる。")
    }

    if (all(input$type == "墳墓")) {
      return("墳墓は権力の象徴であり、首長層の存在を示す重要な指標である。")
    }

    if (all(input$type == "集落")) {
      return("集落は社会基盤を示し、人口と生産活動の広がりを反映している。")
    }

    "遺跡の分布から、伊都国における階層的社会構造が読み取れる。"
  })
}

# ===== 実行 =====
shinyApp(ui, server)
