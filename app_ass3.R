source("building tables.R")
# Earthy tones for Aboriginal children
aboriginal_colors <- c(
  "NSW" = "#8B4513",  # SaddleBrown
  "VIC" = "#A0522D",  # Sienna
  "QLD" = "#CD853F",  # Peru
  "WA"  = "#556B2F",  # DarkOliveGreen
  "SA"  = "#2F4F4F",  # DarkSlateGray
  "TAS" = "#D2691E",  # Chocolate
  "NT"  = "#5C4033",  # Earth brown
  "ACT" = "#6B4226"   # Dusty ochre
)

# Muted cool tones for Disadvantaged children
disadvantaged_colors <- c(
  "NSW" = "#708090",  # SlateGray
  "VIC" = "#778899",  # LightSlateGray
  "QLD" = "#4682B4",  # SteelBlue
  "WA"  = "#6A5ACD",  # SlateBlue
  "SA"  = "#5F9EA0",  # CadetBlue
  "TAS" = "#B0C4DE",  # LightSteelBlue
  "NT"  = "#6495ED",  # CornflowerBlue
  "ACT" = "#A9A9A9"   # DarkGray
)
spectral_colors <- c(
  "NSW" = "#9E0142",  # Dark Pink / Burgundy
  "VIC" = "#D53E4F",  # Deep Red
  "QLD" = "#F46D43",  # Orange
  "WA"  = "#FDAE61",  # Light Orange
  "SA"  = "#E6F598",  # Pale Yellow-Green
  "TAS" = "#ABDDA4",  # Light Green
  "NT"  = "#66C2A5",  # Aqua
  "ACT" = "#3288BD"   # Blue
)

# UI
ui <- fluidPage(
  tags$h2("Preschool Reform Agreement - Performance Indication Tool", align = "center"),
  HTML("<p style='text-align: center; max-width: 800px; margin: auto;'>
         The Preschool Reform Agreement (PRA) dictates five performance indicators (PI) which determine the allocation of funding for preschool education from the commonwealth. In the yearly operationalisation of each PI states negotiate an acceptable value for the percent of the population which meets the PI (X%), and an acceptable value for the percent of 600 hours in attendance (Y%). This tool is intended to assist exploration of potential X% and Y% values.<br><br>
         <strong>Please note:</strong> The child-specific year before full time schooling (YBFS) population noted in the PRA is not publicly available. See citations for substituted populations.
       </p>"),
  
  checkboxGroupInput("states", "Select States:", 
                     choices = unique(PI1_table$state), 
                     selected = unique(PI1_table$state), 
                     inline = TRUE),
  selectInput("x_label", "Select Population X%:", 
              choices = c(60, 70, 80, 90, 100), selected = 100),
  
  hr(),
  
  # Centered Graph PI1
  fluidRow(
    column(12,
           tags$h4("PI 1 – Percent of Children Enrolled That Were Attending", align = "center"),

           selectInput("y_label", "Select Attendance Hours Y%:", 
                       choices = unique(PI1_table$y_label)),
           plotOutput("plot1", height = "300px"),
           tags$p(HTML("<em>Source (count):</em> Preschool Attendance, 2024, Table 1<br>
                        <em>Source (population):</em> Preschool Education, 2024, Table 30"))
    )
  ),
  
  hr(),
  
  # Enrollment indigenous and disadvantaged Graphs Side by Side (PI2.1 and PI3.1)
  fluidRow(
    column(6,
           tags$h4("PI 2.1 - Percent of Aboriginal and Torres Strait Islander Children Enrolled"),
           # selectInput("x_label2.1", "Select Population X%:", 
           #             choices = c(60, 70, 80, 90, 100), selected = 100),
           plotOutput("plot2.1", height = "300px"),
           tags$p(HTML( "<em>Source (count):</em> Preschool Education, 2024, Table 30<br>
                        <em>Source (population):</em> Preschool Education, 2024, A4 State-specific YBFS age cohorts based on months and years of birth."))
    ),
    column(6,
           tags$h4("PI 3.1 - Percent of Disadvantaged Children Enrolled"),
           # selectInput("x_label", "Select Population X%:", 
           #             choices = c(60, 70, 80, 90, 100), selected = 100),
           plotOutput("plot3.1", height = "300px"),
           tags$p(HTML( "<em>Source (count):</em> Preschool Education, 2024, Table 16<br>
                        <em>Source (population):</em> Census of Population and Housing, 2021 [TableBuilder]."))
 
    )
  ),
  
  hr(),
  
  # Attendance indigenous and disadvantaged Graphs Side by Side (PI2.2 and PI3.2)
  fluidRow(
    column(6,
           tags$h4("PI 2.2 Percent of Aboriginal and Torres Strait Islander Children Attending"),
           # selectInput("x_label2.2", "Select Population X%:", 
           #             choices = c(60, 70, 80, 90, 100), selected = 100),
           selectInput("y_label2.2", "Select Attendance Hours Y%", 
                       choices = unique(PI2.2_table$y_label)),
           plotOutput("plot2.2", height = "300px"),
           tags$p(HTML( "<em>Source (count):</em> Preschool Education, 2024, Table 17<br>
                        <em>Source (population):</em> Preschool Education, 2024, A4 State-specific YBFS age cohorts based on months and years of birth."
           ))
    ),
    column(6,
           tags$h4("PI 3.2 - Percent of Disadvantaged Children Attending"),
           # selectInput("x_label3.2", "Select Population X%:", 
           #             choices = c(60, 70, 80, 90, 100), selected = 100),
           selectInput("y_label3.2", "Select Attendance Hours Y%:", 
                       choices = unique(PI3.2_table$y_label)),
           plotOutput("plot3.2", height = "300px"),
           tags$p(HTML( "<em>Source (count):</em> Preschool Education, 2024, Table 17<br>
                        <em>Source (population):</em> Census of Population and Housing, 2021 [TableBuilder]."
           ))
    )
    ),
      hr(),
      tags$h3("References"),
      tags$ul(
        tags$li("Australian Bureau of Statistics (2021). State_seifa_ybfs [Census of Population and Housing, 2021, TableBuilder] accessed 13 June 2025."),
        tags$li(HTML("Australian Bureau of Statistics. (2024). <em>Preschool Attendance</em>. ABS. <a href='https://www.abs.gov.au/statistics/people/education/preschool-attendance/latest-release' target='_blank'>https://www.abs.gov.au/statistics/people/education/preschool-attendance/latest-release</a>")),
        tags$li(HTML("Australian Bureau of Statistics. (2024). <em>Preschool Education methodology</em>. ABS. <a href='https://www.abs.gov.au/methodologies/preschool-education-methodology/2024' target='_blank'>https://www.abs.gov.au/methodologies/preschool-education-methodology/2024</a>")),
        tags$li(HTML("Australian Bureau of Statistics. (2024). <em>Preschool Education</em>. ABS.<a href='https://www.abs.gov.au/statistics/people/education/preschool-education/latest-release' target='_blank'>https://www.abs.gov.au/statistics/people/education/preschool-education/latest-release</a>")),
        
  )
)

# Server
server <- function(input, output, session) {
  
  make_attendance_plot <- function(df, x_thresh, y_var, pop_label, colour_scheme = NULL) {
    df <- df %>%
      filter(state %in% input$states, y_label == y_var) %>%
      mutate(y_value = (cummulative_n / (erp * (as.numeric(x_thresh)/100))*100),
             bar_label = paste0(round(y_value, 0),"%"))
      
    ggplot(df, aes(x = state, y = y_value, fill = state)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 100, linetype = "dotted", color = "black", linewidth = 0.8) +
      geom_text(aes(label = bar_label), y = 10, vjust = 1.3, size = 5)+
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = paste(x_thresh,"% of", unique(df$population),"Children Attended Preschool Programs\n For At Least",
                         unique(df$at_least_hours),ifelse(unique(df$at_least_hours)!= 1, "Hours", "Hour")),
        y = pop_label,
        x = "State") +
      theme_minimal() +
      theme(legend.position = "none")+
      scale_fill_manual(values = colour_scheme)
  }
  make_enrolment_plot <- function(df, x_thresh, pop_label, colour_scheme = NULL) {
    df <- df %>%
      filter(state %in% input$states) %>%
      mutate(y_value = (enrolled_n / (erp * (as.numeric(x_thresh)/100))*100),
             bar_label = paste0(round(y_value, 0),"%"))
    
    ggplot(df, aes(x = state, y = y_value, fill = state)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 100, linetype = "dotted", color = "black", linewidth = 0.8) +
      geom_text(aes(label = bar_label), y = 10, vjust = -0.2, size = 5)+
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = paste(x_thresh,"% of", unique(df$population),"Children Were Enrolled in \nPreschool Programs For At Least 600 Hours"),
           y = pop_label, 
           x = "State") +
      #ylim(0, 120)+
      theme_minimal() +
      theme(legend.position = "none")+
      scale_fill_manual(values = colour_scheme)
      
  }
  
  output$plot1 <- renderPlot({
    make_attendance_plot(PI1_table, input$x_label, input$y_label, "Child Specific YBFS Population", spectral_colors)
  })
  
  output$plot2.1 <- renderPlot({
    make_enrolment_plot(PI2.1_table, input$x_label, "SSYBFS Population", aboriginal_colors)
  })
  
  output$plot3.1 <- renderPlot({
    make_enrolment_plot(PI3.1_table, input$x_label, "4-5 yr old population", disadvantaged_colors) 
  })
  
  output$plot2.2 <- renderPlot({
    make_attendance_plot(PI2.2_table, input$x_label, input$y_label2.2, "SSYBFS Population", aboriginal_colors)
  })
  
  output$plot3.2 <- renderPlot({
    make_attendance_plot(PI3.2_table, input$x_label, input$y_label3.2, "4-5 yr old population", disadvantaged_colors)
  })
}

shinyApp(ui = ui, server = server)

