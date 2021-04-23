# Packages
library('shiny')
library('tidyverse')
library('ggiraph')
library(lubridate)
# Read data
    # Stream time series data (created using create_timeSeries_for_viewTimeSeries.R)
    ts <- read_csv('Data/streamData_for_viewTimeSeriesApp.csv', col_types = cols(event_start = col_character(),
                                                                                 rain_start = col_character(),
                                                                                 falling_inf_pt = col_character(),
                                                                                 event_end = col_character(),
                                                                                 event_start_7dB4 = col_character(),
                                                                                 event_end_7dAF = col_character(),
                                                                                 eventID = col_character())) %>% 
        mutate(timestamp = ymd_hms(timestamp, tz = "Etc/GMT+4")) %>% 
        filter(!is.na(eventID))

    # Clustered events with linear regression data (created using Results_yields_summary.Rmd)
    lr <- read_csv('../Data/results_clusters_withLinearRegressions.csv') %>% 
        # Add an event ID
        mutate(siteabbrev = ifelse(site == "Hungerford", "HB", "WB"),
             eventID = paste(siteabbrev, "-", as.character(event_start), sep = "")) %>% 
        select(-siteabbrev)    

# Tab 2 plots
    # Plotting specifics
    theme1 <- theme_classic() +
                theme(axis.text = element_text(size = 11),
                      axis.title = element_text(size = 12),
                      axis.title.x = element_text(margin=margin(5,0,0,0)),
                      axis.title.y = element_text(margin=margin(0,5,0,0)),
                      legend.title = element_text(size = 9),
                      legend.text = element_text(size = 9),
                      strip.text = element_text(size = 12))

    
# User Interface
in1 <- selectInput(
  inputId = 'selected_event',
  label = 'Select an event to view',
  choices = unique(ts[['eventID']])
)

# Tab 1 outputs
    # This will output the event ID
    out1 <- textOutput('eventID')
    # This will output the plot below the event ID
    out2 <- plotOutput('ts_plot')

# This is the first tab/page of the app (may only have 1 tab)
tab1 <- tabPanel(title = 'View event TS',
  in1, out1, out2)

# This is the second tab
# tab2 <- tabPanel(title = 'Linear regs. vs. TS',
#   fluidRow(selectInput(
#       inputId = 'selected_site',
#       label = 'Select a site',
#       choices = unique(lr[['site']]))
#   ),
#   fluidRow(
#       column(width = 7, ggiraph::ggiraphOutput('int_reg_plot')),
#       column(width = 5, ggiraph::ggiraphOutput('ts_plot_2'))
#   )
# )

tab2 <- tabPanel(title = 'Linear regs. vs. TS',
  fluidRow(selectInput(
      inputId = 'selected_site',
      label = 'Select a site',
      choices = unique(lr[['site']]))
  ),
  fluidRow(ggiraph::ggiraphOutput('int_reg_plot')),
  fluidRow(plotOutput('ts_plot_2'))
)

ui <- navbarPage(title = 'Event Time Series Explorer',
  tab1, 
  tab2)

# Server
server <- function(input, output) {

  # Tab 1: event ID text output above static time series plot      
  output[['eventID']] <- renderText({
    input[['selected_event']]
  })
  
  # Tab 1: static time series plot
  output[['ts_plot']] <- renderPlot({
    df <- ts %>% 
        dplyr::filter(eventID == input[['selected_event']]) %>% 
        tidyr::pivot_longer(cols = c(NO3, SRP, turb, q_cms), names_to = "var", values_to = "value") %>% 
        dplyr::mutate(var = factor(var, levels = c("q_cms", "NO3", "SRP", "turb"), labels = c("Q (cms)", "NO3 (mg N/L)", "SRP (mg P/L)", "Turbidity (NTU)")))
    ggplot(df, aes(x = timestamp, y = value)) +
        facet_wrap(~var, scales = "free_y", ncol = 1) +
        geom_vline(aes(xintercept = ymd_hms(event_start, tz = "Etc/GMT+4")), linetype = "dashed") +
        geom_vline(aes(xintercept = ymd_hms(event_end, tz = "Etc/GMT+4")), linetype = "dashed") +
        # geom_point(size = 2, shape = 1) +
        geom_path() +
        scale_x_datetime(date_breaks = "4 days") +
        ylab("Value") +
        theme_classic() +
        theme(strip.background = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text.y = element_text(size = 12),
              strip.text = element_text(size = 14))
  })
  
  # Tab 2: establish the selected data point and eventID on interactive linear regression plot as input
  selected_eventID <- reactive({
    input$plot_selected
  })
  
  # Tab 2: interactive linear regression plot
  output$int_reg_plot <- renderGirafe({
    df <- lr %>% 
        dplyr::filter(site == input[['selected_site']]) %>% 
        dplyr::mutate(season = factor(season, levels = c("spring", "summer", "fall"), labels = c("Spring", "Summer", "Fall"))) %>% 
        dplyr::mutate(var = factor(var, levels = c("NO3_kg_km2", "SRP_kg_km2", "turb_kg_km2"), labels = c(expression(NO[3]^-{}~(kg~N~km^{-2})) , expression(SRP~(kg~P~km^{-2})), expression(Turb.~(Sigma~NTU~km^{-2})))))
    gg_linReg <-
        ggplot(df, aes(x = q_mm, y = yield)) +
          facet_grid(var~season, scales = "free", labeller = label_parsed) +
          geom_smooth(method=lm, se=FALSE, color = "black") +
          geom_point_interactive(aes(data_id = eventID,
                                     tooltip = eventID,
                                     color = factor(clust_5cl),
                                     shape = factor(year(event_start))),
                                 size = 3, stroke = 0.75, alpha = 0.8) +
          scale_shape_manual(name = "Year",
                             values = c(19, 17, 15)) +
          scale_color_manual(name = "Cluster",
                            values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
          ylab(expression(Event~NO[3]^-{}~or~SRP~or~turb.~yield)) +
          xlab("Event water yield (mm)") +
          theme1 +
          theme(strip.background = element_blank(),
                axis.title.x = element_blank(),
                legend.position = "none") +
          ggtitle("Hungerford")
    x <- girafe(code = print(gg_linReg),
                width_svg = 8, 
                height_svg = 6,
                options = list(
                  opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;", reactive = TRUE),
                  opts_selection(
                    type = "multiple", css = "fill:#FF3333;stroke:black;")
                ))
    x
  })
  
  
  # Tab 2: static time series plot
  output$ts_plot_2 <- renderPlot({
    df <- ts %>% 
      dplyr::filter(eventID %in% input$int_reg_plot_selected) %>%
      tidyr::pivot_longer(cols = c(NO3, SRP, turb, q_cms), names_to = "var", values_to = "value") %>% 
      dplyr::mutate(var = factor(var, levels = c("q_cms", "NO3", "SRP", "turb"), labels = c("Q (cms)", "NO3 (mg N/L)", "SRP (mg P/L)", "Turbidity (NTU)")))
    ggplot(df, aes(x = timestamp, y = value)) +
        facet_wrap(~var, scales = "free_y", ncol = 1) +
        geom_vline(aes(xintercept = ymd_hms(event_start, tz = "Etc/GMT+4")), linetype = "dashed") +
        geom_vline(aes(xintercept = ymd_hms(event_end, tz = "Etc/GMT+4")), linetype = "dashed") +
        # geom_point(size = 2, shape = 1) +
        geom_path() +
        scale_x_datetime(date_breaks = "4 days") +
        ylab("Value") +
        theme_classic() +
        theme(strip.background = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text.y = element_text(size = 12),
              strip.text = element_text(size = 14))
  })   
}

# Create the Shiny App
shinyApp(ui = ui, server = server)