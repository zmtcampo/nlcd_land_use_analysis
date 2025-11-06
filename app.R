library(shiny)
library(DT)
library(dplyr)
library(readr)
library(htmltools)
library(tidyr)
library(bslib)

# Load transition data
transition_data <- read_csv("transition_matrices_percent.csv")[-1]
transition_data <- as.data.frame(transition_data)
transition_data$county <- as.factor(transition_data$county)

# Prepare Developed matrix for Page 4
developed_matrix <- transition_data %>%
  dplyr::select(county, class2014, Developed) %>%
  tidyr::pivot_wider(names_from = county, values_from = Developed) %>%
  column_to_rownames(var = "class2014")

# Sort by the first county column descending by default
first_county <- names(developed_matrix)[1]
developed_matrix <- developed_matrix[order(-developed_matrix[[first_county]]), ]

# ----------------------------------------------------------
# UI
# ----------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "slate"),
  
  titlePanel("County Land Cover Analysis"),
  
  tabsetPanel(
    id = "mainTabs",
    
    # Page 1: Introduction
    tabPanel(
      "Introduction",
      mainPanel(
        br(),
        tags$h3("Land Use Analysis"),
        tags$ul(
          tags$li("Understanding current patterns of land use"),
          tags$li("Analyzing urban growth"),
          tags$li("Transition matrices (tracking class changes, such as forest → developed)")
        ),
        br(),
        tags$h3("Key Questions"),
        tags$ul(
          tags$li("How much land has transitioned to development?"),
          tags$li("Which counties experienced the fastest growth in developed land?")
        ),
        br(),
        tags$hr(),
        tags$p(
          "This dashboard provides insights into land cover dynamics across counties, 
           showing how landscapes have changed from 2014 to 2023. Use the tabs above to 
           explore maps, transition matrices, and development trends."
        )
      )
    ),
    
    # Page 2: Land Use Maps
    tabPanel(
      "Land Use Maps",
      sidebarLayout(
        sidebarPanel(
          selectInput("county", "Select County:",
                      choices = c("Petersburg", "Colonial_H", "Hopewell", 
                                  "Chesterfield", "Dinwiddie", "Prince_G"),
                      selected = "Petersburg")
        ),
        mainPanel(
          imageOutput("gifDisplay")
        )
      )
    ),
    
    # Page 3: Land Use Transition Matrix
    tabPanel(
      "Land Use Transition Matrix",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selected_county",
            label = "Select County:",
            choices = unique(transition_data$county),
            selected = unique(transition_data$county)[1]
          )
        ),
        mainPanel(
          DTOutput("transition_table"),
          br(),
          tags$hr(),
          tags$h4("Question: How much land has transitioned to development?"),
          tags$p("Use the table above to identify the land cover classes that contributed most to new development.")
        )
      )
    ),
    
    # Page 4: Developed Land Growth
    tabPanel(
      "Developed Land Growth",
      mainPanel(
        DTOutput("developed_table"),
        br(),
        tags$hr(),
        tags$h4("Question: Which counties experienced the fastest growth in developed land?"),
        tags$p("Compare the developed land percentages across counties to determine which have experienced the most growth since 2014.")
      )
    )
  )
)

# ----------------------------------------------------------
# SERVER
# ----------------------------------------------------------
server <- function(input, output, session) {
  
  # Page 2: GIF Display
  output$gifDisplay <- renderImage({
    list(src = paste0(input$county, "_LandCover_2014_2023.gif"),
         contentType = "image/gif",
         width = "100%",
         alt = paste("Land cover animation for", input$county))
  }, deleteFile = FALSE)
  
  # Page 3: Transition Matrix Table
  output$transition_table <- renderDT({
    df <- transition_data %>%
      dplyr::filter(county == input$selected_county)
    
    rownames(df) <- df$class2014
    
    df_display <- df[, !(names(df) %in% c("county", "class2014"))]
    df_display <- df_display[, colSums(df_display != 0) > 0, drop = FALSE]
    
    if ("Developed" %in% names(df_display)) {
      df_display <- df_display[order(-df_display$Developed), ]
    }
    
    dt <- datatable(
      df_display,
      options = list(
        pageLength = nrow(df_display),
        dom = 't',                 
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      caption = tags$caption(
        style = 'caption-side: top; text-align: center; font-weight: bold;',
        paste("Land Cover Transition Matrix for", input$selected_county)
      )
    ) %>%
      formatRound(columns = names(df_display), digits = 2)
    
    if ("Developed" %in% names(df_display)) {
      top4_rows <- order(-df_display$Developed)[1:min(4, nrow(df_display))]
      dt <- dt %>%
        formatStyle(
          "Developed",
          color = styleRow(
            seq_len(nrow(df_display)),
            ifelse(seq_len(nrow(df_display)) %in% top4_rows, "red", "white")
          )
        )
    }
    
    dt
  })
  
  # Page 4: Developed Land Growth Table
  output$developed_table <- renderDT({
    
    dt <- datatable(
      developed_matrix,
      options = list(
        pageLength = nrow(developed_matrix),
        dom = 't',
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      caption = tags$caption(
        style = 'caption-side: top; text-align: center; font-weight: bold;',
        "Developed Land Growth by County (Sorted Descending)"
      )
    ) %>%
      formatRound(columns = names(developed_matrix), digits = 2)
    
    # Apply gradient coloring for each county column
    for(col in names(developed_matrix)) {
      dt <- dt %>%
        formatStyle(
          columns = col,
          background = styleColorBar(range(developed_matrix[[col]], na.rm = TRUE), 'lightgreen'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    }
    
    # Highlight top 3 land classes overall in bold red font
    overall_top <- rowSums(developed_matrix, na.rm = TRUE)
    top3_rows <- order(overall_top, decreasing = TRUE)[1:min(3, length(overall_top))]
    
    dt <- dt %>%
      formatStyle(
        columns = names(developed_matrix),
        target = 'row',
        fontWeight = styleEqual(top3_rows, rep('bold', length(top3_rows))),
        color = styleEqual(top3_rows, rep('red', length(top3_rows)))
      )
    
    dt
  })
}

# ----------------------------------------------------------
# Run App
# ----------------------------------------------------------
shinyApp(ui = ui, server = server)
