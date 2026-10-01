# ============================================================
# NETFLIX CONTENT ANALYSIS DASHBOARD
# ============================================================

# 1. Install packages if needed
packages <- c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "ggplot2",
  "plotly",
  "DT",
  "readr"
)

missing <- packages[!(packages %in% installed.packages()[, "Package"])]

if(length(missing) > 0) {
  install.packages(missing)
}

# 2. Load packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)

# ============================================================
# 3. LOAD DATA
# ============================================================

if(file.exists("netflix_titles.csv")) {
  
  netflix <- read_csv(
    "netflix_titles.csv",
    show_col_types = FALSE
  )
  
} else {
  
  # Demo data if CSV is not available
  set.seed(123)
  
  netflix <- data.frame(
    show_id = 1:1000,
    
    type = sample(
      c("Movie", "TV Show"),
      1000,
      replace = TRUE,
      prob = c(0.7, 0.3)
    ),
    
    title = paste("Netflix Title", 1:1000),
    
    country = sample(
      c(
        "United States",
        "India",
        "United Kingdom",
        "Japan",
        "South Korea",
        "Canada",
        "France",
        "Spain",
        "Germany",
        "Australia"
      ),
      1000,
      replace = TRUE
    ),
    
    release_year = sample(
      2000:2021,
      1000,
      replace = TRUE
    ),
    
    rating = sample(
      c(
        "TV-MA",
        "TV-14",
        "TV-PG",
        "R",
        "PG-13",
        "PG"
      ),
      1000,
      replace = TRUE
    )
  )
}

# ============================================================
# 4. CLEAN DATA
# ============================================================

names(netflix) <- tolower(names(netflix))

netflix$type[
  is.na(netflix$type) |
    netflix$type == ""
] <- "Unknown"

netflix$country[
  is.na(netflix$country) |
    netflix$country == ""
] <- "Unknown"

netflix$rating[
  is.na(netflix$rating) |
    netflix$rating == ""
] <- "Unknown"

# ============================================================
# 5. USER INTERFACE
# ============================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "NETFLIX ANALYTICS"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Content Analysis",
        tabName = "content",
        icon = icon("film")
      ),
      
      menuItem(
        "Country Analysis",
        tabName = "country",
        icon = icon("globe")
      ),
      
      menuItem(
        "Netflix Data",
        tabName = "data",
        icon = icon("table")
      )
      
    ),
    
    hr(),
    
    h4(
      "FILTERS",
      style = "color:white; padding-left:15px;"
    ),
    
    selectInput(
      "type",
      "Content Type",
      choices = c(
        "All",
        sort(unique(netflix$type))
      )
    ),
    
    sliderInput(
      "year",
      "Release Year",
      
      min = min(
        netflix$release_year,
        na.rm = TRUE
      ),
      
      max = max(
        netflix$release_year,
        na.rm = TRUE
      ),
      
      value = c(
        min(
          netflix$release_year,
          na.rm = TRUE
        ),
        
        max(
          netflix$release_year,
          na.rm = TRUE
        )
      ),
      
      step = 1,
      sep = ""
    ),
    
    selectInput(
      "rating",
      "Rating",
      
      choices = c(
        "All",
        sort(unique(netflix$rating))
      )
    )
    
  ),
  
  dashboardBody(
    
    # ========================================================
    # CSS
    # ========================================================
    
    tags$head(
      
      tags$style(HTML("

        .content-wrapper {
          background-color: #f4f4f4;
        }

        .skin-blue .main-header .logo {
          background-color: #b20710;
          font-weight: bold;
        }

        .skin-blue .main-header .navbar {
          background-color: #e50914;
        }

        .skin-blue .main-sidebar {
          background-color: #141414;
        }

        .skin-blue .sidebar-menu > li.active > a {
          background-color: #e50914;
        }

        .box {
          border-radius: 12px;
          box-shadow: 0 3px 10px rgba(0,0,0,0.15);
        }

        .small-box {
          border-radius: 12px;
          box-shadow: 0 3px 10px rgba(0,0,0,0.15);
        }

        .dashboard-title {
          font-size: 30px;
          font-weight: bold;
          margin-bottom: 5px;
        }

        .dashboard-subtitle {
          color: #777;
          font-size: 16px;
          margin-bottom: 20px;
        }

      "))
      
    ),
    
    # ========================================================
    # TABS
    # ========================================================
    
    tabItems(
      
      # ======================================================
      # DASHBOARD
      # ======================================================
      
      tabItem(
        
        tabName = "dashboard",
        
        div(
          class = "dashboard-title",
          "Netflix Content Analysis"
        ),
        
        div(
          class = "dashboard-subtitle",
          "Interactive analysis of Netflix movies and TV shows"
        ),
        
        # KPI CARDS
        
        fluidRow(
          
          valueBoxOutput(
            "total",
            width = 3
          ),
          
          valueBoxOutput(
            "movie",
            width = 3
          ),
          
          valueBoxOutput(
            "tv",
            width = 3
          ),
          
          valueBoxOutput(
            "countries",
            width = 3
          )
          
        ),
        
        # CHART 1 + CHART 2
        
        fluidRow(
          
          box(
            title = "Movies vs TV Shows",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "type_chart",
              height = "350px"
            )
          ),
          
          box(
            title = "Release Year Trend",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "year_chart",
              height = "350px"
            )
          )
          
        ),
        
        # CHART 3 + CHART 4
        
        fluidRow(
          
          box(
            title = "Top Countries",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "country_chart",
              height = "400px"
            )
          ),
          
          box(
            title = "Content Ratings",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "rating_chart",
              height = "400px"
            )
          )
          
        )
        
      ),
      
      # ======================================================
      # CONTENT TAB
      # ======================================================
      
      tabItem(
        
        tabName = "content",
        
        h2("Content Analysis"),
        
        fluidRow(
          
          box(
            title = "Content Distribution",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "distribution",
              height = "450px"
            )
          ),
          
          box(
            title = "Content Trend",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "trend",
              height = "450px"
            )
          )
          
        )
        
      ),
      
      # ======================================================
      # COUNTRY TAB
      # ======================================================
      
      tabItem(
        
        tabName = "country",
        
        h2("Country Analysis"),
        
        box(
          title = "Top 15 Countries",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          
          plotlyOutput(
            "country_analysis",
            height = "550px"
          )
        )
        
      ),
      
      # ======================================================
      # DATA TAB
      # ======================================================
      
      tabItem(
        
        tabName = "data",
        
        h2("Netflix Dataset"),
        
        box(
          title = "Netflix Content",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          
          DTOutput("table")
          
        )
        
      )
      
    )
    
  )
  
)

# ============================================================
# 6. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # FILTERED DATA
  # ----------------------------------------------------------
  
  filtered <- reactive({
    
    data <- netflix
    
    if(input$type != "All") {
      
      data <- data %>%
        filter(type == input$type)
      
    }
    
    data <- data %>%
      filter(
        release_year >= input$year[1],
        release_year <= input$year[2]
      )
    
    if(input$rating != "All") {
      
      data <- data %>%
        filter(rating == input$rating)
      
    }
    
    data
    
  })
  
  # ----------------------------------------------------------
  # KPI - TOTAL
  # ----------------------------------------------------------
  
  output$total <- renderValueBox({
    
    valueBox(
      value = nrow(filtered()),
      subtitle = "Total Titles",
      icon = icon("film"),
      color = "red"
    )
    
  })
  
  # ----------------------------------------------------------
  # KPI - MOVIES
  # ----------------------------------------------------------
  
  output$movie <- renderValueBox({
    
    valueBox(
      value = sum(
        filtered()$type == "Movie",
        na.rm = TRUE
      ),
      subtitle = "Movies",
      icon = icon("video"),
      color = "black"
    )
    
  })
  
  # ----------------------------------------------------------
  # KPI - TV SHOWS
  # ----------------------------------------------------------
  
  output$tv <- renderValueBox({
    
    valueBox(
      value = sum(
        filtered()$type == "TV Show",
        na.rm = TRUE
      ),
      subtitle = "TV Shows",
      icon = icon("tv"),
      color = "red"
    )
    
  })
  
  # ----------------------------------------------------------
  # KPI - COUNTRIES
  # ----------------------------------------------------------
  
  output$countries <- renderValueBox({
    
    valueBox(
      value = length(
        unique(
          filtered()$country
        )
      ),
      subtitle = "Countries",
      icon = icon("globe"),
      color = "black"
    )
    
  })
  
  # ==========================================================
  # MOVIE VS TV SHOW
  # ==========================================================
  
  output$type_chart <- renderPlotly({
    
    data <- filtered() %>%
      count(type)
    
    p <- ggplot(
      data,
      aes(
        x = "",
        y = n,
        fill = type,
        text = paste(
          "Type:", type,
          "<br>Count:", n
        )
      )
    ) +
      
      geom_col(width = 1) +
      
      coord_polar(theta = "y") +
      
      theme_void()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # YEAR TREND
  # ==========================================================
  
  output$year_chart <- renderPlotly({
    
    data <- filtered() %>%
      count(release_year)
    
    p <- ggplot(
      data,
      aes(
        x = release_year,
        y = n,
        text = paste(
          "Year:", release_year,
          "<br>Titles:", n
        )
      )
    ) +
      
      geom_line(
        linewidth = 1
      ) +
      
      geom_point(
        size = 2
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Release Year",
        y = "Titles"
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # COUNTRY CHART
  # ==========================================================
  
  output$country_chart <- renderPlotly({
    
    data <- filtered() %>%
      count(
        country,
        sort = TRUE
      ) %>%
      slice_head(n = 10) %>%
      arrange(n)
    
    p <- ggplot(
      data,
      aes(
        x = n,
        y = reorder(country, n),
        text = paste(
          "Country:", country,
          "<br>Titles:", n
        )
      )
    ) +
      
      geom_col() +
      
      theme_minimal() +
      
      labs(
        x = "Titles",
        y = "Country"
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # RATING CHART
  # ==========================================================
  
  output$rating_chart <- renderPlotly({
    
    data <- filtered() %>%
      count(
        rating,
        sort = TRUE
      )
    
    p <- ggplot(
      data,
      aes(
        x = reorder(rating, n),
        y = n,
        text = paste(
          "Rating:", rating,
          "<br>Titles:", n
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      theme_minimal() +
      
      labs(
        x = "Rating",
        y = "Titles"
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # CONTENT DISTRIBUTION
  # ==========================================================
  
  output$distribution <- renderPlotly({
    
    data <- filtered() %>%
      count(type)
    
    p <- ggplot(
      data,
      aes(
        x = type,
        y = n,
        text = paste(
          "Type:", type,
          "<br>Titles:", n
        )
      )
    ) +
      
      geom_col() +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # CONTENT TREND
  # ==========================================================
  
  output$trend <- renderPlotly({
    
    data <- filtered() %>%
      count(
        release_year,
        type
      )
    
    p <- ggplot(
      data,
      aes(
        x = release_year,
        y = n,
        color = type,
        group = type,
        text = paste(
          "Year:", release_year,
          "<br>Type:", type,
          "<br>Titles:", n
        )
      )
    ) +
      
      geom_line(
        linewidth = 1
      ) +
      
      geom_point() +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # COUNTRY ANALYSIS
  # ==========================================================
  
  output$country_analysis <- renderPlotly({
    
    data <- filtered() %>%
      count(
        country,
        sort = TRUE
      ) %>%
      slice_head(n = 15) %>%
      arrange(n)
    
    p <- ggplot(
      data,
      aes(
        x = n,
        y = reorder(country, n),
        text = paste(
          "Country:", country,
          "<br>Titles:", n
        )
      )
    ) +
      
      geom_col() +
      
      theme_minimal() +
      
      labs(
        x = "Number of Titles",
        y = "Country"
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # DATA TABLE
  # ==========================================================
  
  output$table <- renderDT({
    
    datatable(
      filtered(),
      filter = "top",
      rownames = FALSE,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
      
    )
    
  })
  
}

# ============================================================
# 7. START APPLICATION
# ============================================================

shiny::shinyApp(
  ui = ui,
  server = server
)

