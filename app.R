library(shiny)
library(leaflet)
library(dplyr)
library(readr)

# ===== データ読み込み =====
data <- read_csv("ito_sites_master.csv", show_col_types = FALSE)
colnames(data) <- tolower(colnames(data))

# 必須列チェック
required_cols <- c("name", "lat", "lng", "type", "period", "desc")
missing <- setdiff(required_cols, colnames(data))
if(length(missing) > 0){
  stop(paste("不足列:", paste(missing, collapse=", ")))
}

# NA除去
data <- data %>% filter(!is.na(lat), !is.na(lng))

# ===== カラーパレット（時代別）=====
period_levels <- sort(unique(as.character(data$period)))
n_period <- length(period_levels)
base_pal <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
pal <- colorFactor(
  palette = grDevices::colorRampPalette(base_pal)(max(n_period, 1L)),
  domain = period_levels
)

type_levels_ui <- sort(unique(as.character(data$type)))

data <- data %>%
  mutate(
    marker_id = paste0("m", row_number()),
    popup_html = paste0(
      "<b>", htmltools::htmlEscape(as.character(name)), "</b><br>",
      "時代：", htmltools::htmlEscape(as.character(period)), "<br>",
      "種別：", htmltools::htmlEscape(as.character(type))
    )
  )

# ===== UI =====
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background:#f5f5f5; }
      .card {
        border-radius:12px;
        padding:15px;
        background:white;
        box-shadow:0 2px 6px rgba(0,0,0,0.1);
        margin-top:10px;
      }
      .topbox {
        background:white;
        padding:10px;
        margin-bottom:10px;
        border-radius:10px;
      }
    "))
  ),
  
  titlePanel("伊都国遺跡デジタルアーカイブ"),
  
  div(class="topbox",
      h4("本アプリについて"),
      p("伊都国の遺跡を地図上に可視化し、時代ごとの分布や文化の変遷を直感的に理解できるよう設計した。")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("フィルタ"),
      
      selectInput("period","時代",
                  choices = c("すべて", period_levels),
                  selected = "すべて"),
      
      selectInput("type","種別",
                  choices = c("すべて", type_levels_ui),
                  selected = "すべて")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px"),
      uiOutput("detail")
    )
  )
)

# ===== サーバー =====
server <- function(input, output, session){
  
  # フィルタ
  filtered <- reactive({
    df <- data
    
    if(input$period != "すべて"){
      df <- df %>% filter(period == input$period)
    }
    if(input$type != "すべて"){
      df <- df %>% filter(type == input$type)
    }
    
    df
  })
  
  # ===== 地図 =====
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat,
        color = ~pal(period),
        radius = 7,
        fillOpacity = 0.9,
        layerId = ~marker_id,
        
        popup = ~popup_html
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = period_levels,
                title = "時代")
  })
  
  # 更新
  observe({
    leafletProxy("map", data = filtered()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        ~lng, ~lat,
        color = ~pal(period),
        radius = 7,
        fillOpacity = 0.9,
        layerId = ~marker_id,
        popup = ~popup_html
      )
  })
  
  # ===== クリック =====
  selected <- reactiveVal(NULL)
  
  observeEvent(input$map_marker_click, {
    selected(input$map_marker_click$id)
  })
  
  # ===== 展示カード =====
  output$detail <- renderUI({
    
    req(selected())
    
    item <- data %>% filter(marker_id == selected()) %>% slice(1)
    if(nrow(item) == 0) return(NULL)
    
    div(class="card",
        h2(item$name),
        tags$hr(),
        p(strong("時代："), item$period),
        p(strong("種別："), item$type),
        br(),
        p(item$desc)
    )
  })
}

# ===== 実行 =====
shinyApp(ui, server)