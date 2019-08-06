library(shiny)
library(shinymaterial)
library(DT)
library(plotly)
library(DBI)
library(dplyr)
library(glue)

# canal

conn <- dbConnect(RSQLite::SQLite(), dbname = "dados.sqlite")

if(!dbExistsTable(conn, "cenarios")){
  
  dbWriteTable(conn, "cenarios", value = tibble(cenario = character(), 
                                                desc = character(),
                                                custos = numeric())) 
  
}


ui <- material_page(
  
  material_side_nav(
    
    tags$h4("Painel de controle"),
    material_text_box(input_id = "name", label = "Nome do cenário"),
    material_number_box(input_id = "fator",
                        label = "Fator retorno veículo (1 ou 2)",
                        min_value = 1, 
                        max_value = 2, 
                        initial_value = 1),
    material_number_box(input_id = "dura",
                        label = "Duração (meses)",
                        min_value = 1,
                        max_value = Inf,
                        initial_value = 1),
    material_number_box(input_id = "exte",
                        label = "Extensão (km)",
                        min_value = 1,
                        max_value = Inf,
                        initial_value = 1),
    material_dropdown(input_id = "porte",
                      label = "Porte",
                      choices = c("Pequeno", "Médio", "Grande")),
    material_dropdown(input_id = "relevo",
                      label = "Relevo",
                      choices = c("Plano", "Montanhoso", "Ondulado")),
    material_dropdown(input_id = "classe",
                      label = "Classe",
                      choices = c("0", "I-A", "I-B", "II", "III")),
    material_dropdown(input_id = "canteiro",
                      label = "Padrão do canteiro",
                      choices = c("Permanente", "Provisório")),
    material_slider(input_id = "pavi_a",
                    label = "Pavimento tipo A (%)",
                    min_value = 0,
                    max_value = 100,
                    initial_value = 0,
                    step_size = 1),
    material_slider(input_id = "pavi_b",
                    label = "Pavimento tipo B (%)",
                    min_value = 0,
                    max_value = 100,
                    initial_value = 0,
                    step_size = 1),
    material_slider(input_id = "pavi_c",
                    label = "Pavimento tipo C (%)",
                    min_value = 0,
                    max_value = 100,
                    initial_value = 0,
                    step_size = 1),
    material_slider(input_id = "pavi_d",
                    label = "Pavimento tipo D (%)",
                    min_value = 0,
                    max_value = 100,
                    initial_value = 0,
                    step_size = 1),
    material_slider(input_id = "pavi_e",
                    label = "Pavimento tipo E (%)",
                    min_value = 0,
                    max_value = 100,
                    initial_value = 0,
                    step_size = 1),
    material_slider(input_id = "pavi_f",
                    label = "Pavimento tipo F (%)",
                    min_value = 0,
                    max_value = 100,
                    initial_value = 0,
                    step_size = 1),
    material_slider(input_id = "pavi_g",
                    label = "Pavimento tipo G (%)",
                    min_value = 0,
                    max_value = 100,
                    initial_value = 0,
                    step_size = 1)
  ),
  
  title = "Custo Médio Gerencial",
  nav_bar_color = "indigo",
  material_tabs(
    tabs = c("Cenário" = "resumo",
             "Comparação de cenários" = "cenario")
  ),
  
  material_tab_content(
    
    tab_id = "resumo",
    dataTableOutput("resultado"),
    tags$br(),
    plotlyOutput("pieplot"),
    material_button(input_id = "enviar", label = "Enviar")
    
  ),
  
  material_tab_content(
    
    tab_id = "cenario",
    dataTableOutput("cenarios"),
    plotlyOutput("plotcenarios"),
    material_button(input_id = "limpar", label = "Limpar")
    
  )
  
)

server <- function(input, output, session) {
  
  source("cmg.R")
  
  tabela <- reactive({
    
    custos <- 
      cmg(porte = input$porte, 
          relevo = input$relevo, 
          classe = input$classe,
          padrao = input$canteiro,
          k = input$fator,
          d = input$dura,
          et = input$exte,
          peca = input$pavi_a/100,
          pecb = input$pavi_b/100,
          pecc = input$pavi_c/100,
          pecd = input$pavi_d/100,
          pece = input$pavi_e/100,
          pecf = input$pavi_f/100,
          pecg = input$pavi_g/100)
    
    tabela <- 
      tibble(cenario = input$name, 
             desc = c("Mobilização e desmobilização (CM1)", 
                      "Administração local (CM2)", 
                      "Canteiro de obras (CM3)", 
                      "Terraplenagem, drenagem e OAC, obras complementares, sinalização e proteção ambiental (CM4)",
                      "Pavimentação, aquisição e transporte de material betuminoso  (CM5)",
                      "Custo médio gerencial  (CMG)"), 
             custos = c(custos$Parcial$CM1, 
                        custos$Parcial$CM2, 
                        custos$Parcial$CM3, 
                        custos$Parcial$CM4,
                        custos$Parcial$CM5,
                        custos$CMG))
    
  })
  
  output$resultado <- renderDataTable({
    
    tabela() %>% 
      datatable(rownames = FALSE, 
                colnames = c("Cenário", "Custos", "Valores"),
                options = list(dom = "t", columnDefs = list(list(className = 'dt-center', targets = 2)))) %>% 
      formatCurrency(columns = "custos", digits = 2, mark = ".", dec.mark = ",", currency = "R$")
    
  })
  
  output$pieplot <- renderPlotly({
    
    tabela() %>% 
      slice(-6) %>% 
      plot_ly(labels = ~desc,
              values = ~custos,
              hoverinfo = "percent+label",
              type = "pie") %>% 
      layout(showlegend = FALSE)
    
  })
  
  observeEvent(input$enviar, {
    dbWriteTable(conn, "cenarios", value = tabela(), append = TRUE)
    showNotification("Enviado",
                     action = a(href = "javascript:location.reload();", "Atualizar tabela")
    )
  }, ignoreInit = TRUE)
  
  output$cenarios <- renderDataTable({
    datatable(tbl(conn, "cenarios") %>% as_tibble(), 
              options = list(dom = "tip", pageLength = 6, columnDefs = list(list(className = 'dt-center', targets = 2))),
              colnames = c("Cenário", "Custos", "Valores (R$)"),
              rownames = FALSE) %>% 
      formatCurrency(columns = "custos", digits = 2, mark = ".", dec.mark = ",", currency = "R$")
  })
  
  output$plotcenarios <- renderPlotly({
    
    tbl(conn, "cenarios") %>% 
      filter(desc != "Custo médio gerencial  (CMG)") %>% 
      as_tibble() %>% 
      mutate(desc = factor(desc, levels = c("Mobilização e desmobilização (CM1)", 
                                            "Administração local (CM2)",
                                            "Canteiro de obras (CM3)",
                                            "Terraplenagem, drenagem e OAC, obras complementares, sinalização e proteção ambiental (CM4)",
                                            "Pavimentação, aquisição e transporte de material betuminoso  (CM5)"))) %>% 
      ggplot(aes(cenario, custos/1000000, fill = desc, text = glue("{desc} <br> Cenário: {cenario} <br> R${format(custos, big.mark = '.', decimal.mark = ',')}"))) + 
      geom_col() +
      labs(x = "Cenários", y = "Custos em milhões (R$)", fill = "") +
      guides(fill = FALSE) +
      theme_minimal()
    
    ggplotly(tooltip = "text") %>% 
      layout(showlegend = FALSE)
    
  })
  
  observeEvent(input$limpar, {
    dbWriteTable(conn, "cenarios", value = tibble(cenario = character(), 
                                                  desc = character(),
                                                  custos = numeric()), 
                 overwrite = TRUE) 
    showNotification("Enviado",
                     action = a(href = "javascript:location.reload();", "Atualizar tabela")
    )
  }, ignoreInit = TRUE)
  
}

shinyApp(ui = ui, server = server)