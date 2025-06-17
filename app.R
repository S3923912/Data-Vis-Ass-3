# data to build the tables
library(shiny)
library(tidyverse)
library(rsconnect)
#NPAC Table 1 - attendance total ybfs (PI 1)
attend_total_raw <- readxl::read_xlsx("data/Preschool Attendance, 2024 Tables 1-3.xlsx", sheet = 2,  range = "A5:H50")
attend_total_formatted <- attend_total_raw%>%
  rename(service_type = `Service type`)%>%
  mutate(
    state = ifelse(grepl("[a-zA-Z]", `Attending 1-359 hours`), `Attending 1-359 hours`, NA),
    state = recode(state,"New South Wales(f)" = "NSW",
                   "Victoria(h)" = "VIC",
                   "Queensland"="QLD",
                   "South Australia" = "SA",
                   "Western Australia" = "WA", 
                   "Tasmania(i)" = "TAS",
                   "Northern Territory" = "NT",
                   "Australian Capital Territory" = "ACT"))%>%
  fill(state, .direction = "down")%>%
  filter(!is.na(service_type))%>%
  mutate(across(contains("Attending"), as.numeric))%>%
  pivot_longer(cols = contains("Attending"), names_to = "attending_hours", values_to = "attend_n")
#cummulative hours
attend_total_cumulative <- attend_total_formatted%>%
  mutate(attending_hours = trimws(gsub("(Attending|hours)", "", attending_hours)),
         y_label = recode(attending_hours, 
                          "600+" = "100%",
                          "540-599" = "90%",
                          "480-539" = "80%",
                          "420-479" = "70%", 
                          "360-419" = "60%",
                          "1-359" = "1%"), 
         at_least_hours = str_extract(attending_hours, "^[0-9]+"), 
         at_least_hours = ifelse(is.na(at_least_hours), "600", at_least_hours),
         at_least_hours = as.numeric(at_least_hours))%>%
  filter(!str_detect(attending_hours, "total"),
         service_type == "Total children in a preschool program", 
         state != "Australia")%>%
  arrange(desc(at_least_hours))%>%
  group_by(service_type, state)%>%
  mutate(cummulative_n = cumsum(attend_n),
         population = "Enrolled")%>%
  ungroup()


#NPAC Table 2 - attendance inidg ybfs (PI 2.2)
attend_indig_raw <- readxl::read_xlsx("data/Preschool Attendance, 2024 Tables 1-3.xlsx", sheet = 3, range = "A5:F50")
attend_indig_formatted <- attend_indig_raw%>%
  rename(service_type = `Service type`)%>%
  mutate(
    state = ifelse(grepl("[a-zA-Z]", `Attending 1-359 hours`), `Attending 1-359 hours`, NA),
    state = recode(state,"New South Wales(f)" = "NSW",
                   "Victoria(h)" = "VIC",
                   "Queensland"="QLD",
                   "South Australia" = "SA",
                   "Western Australia" = "WA", 
                   "Tasmania(i)" = "TAS",
                   "Northern Territory" = "NT",
                   "Australian Capital Territory" = "ACT"),
    `Attending 1-359 hours` = ifelse(grepl("[0-9]+$", `Attending 1-359 hours`), `Attending 1-359 hours`, NA))%>%
  fill(state, .direction = "down")%>%
  filter(!is.na(service_type))%>%
  mutate(across(contains("Attending"), as.numeric))%>%
  pivot_longer(cols = contains("Attending"), names_to = "attending_hours", values_to = "attend_n")
atten_indig_cumulative <- attend_indig_formatted %>%
  mutate(attending_hours = trimws(gsub("(Attending|hours)", "", attending_hours)),
         y_label = recode(attending_hours, 
                          "600+" = "100%",
                          "480-599" = "80%",
                          "360-479" = "60%", 
                          "1-359" = "1%"),
         at_least_hours = str_extract(attending_hours, "^[0-9]+"), 
         at_least_hours = ifelse(is.na(at_least_hours), "600", at_least_hours),
         at_least_hours = as.numeric(at_least_hours))%>%
  filter(!str_detect(attending_hours, "total")&
           service_type == "Total children in a preschool program" & 
           state != "Australia")%>%
  group_by(service_type, state)%>%
  arrange(desc(at_least_hours))%>%
  mutate(cummulative_n = cumsum(attend_n),
         population = "Aboriginal and Torres Strait Islanders")%>%
  ungroup()

#NECCEC Table 17 - attendance disad 4-5 (PI 3.2)
attend_disad_raw <- readxl::read_xlsx("data/Preschool Education, 2024 (Tables 1 to 32).xlsx", sheet = "Table_17",  range = "A5:I32")
attend_disad_formatted <- attend_disad_raw%>%
  rename(col_1 = `Index of Relative Socio-economic Disadvantage(d)(e)`)%>%
  mutate(attending_hours  = ifelse(grepl("Attending", col_1), col_1, NA))%>%
  fill(attending_hours, .direction = "down")%>%
  filter(col_1 == "Quintile 1")%>%
  pivot_longer(cols = -c(col_1, attending_hours), names_to = "state", values_to = "attend_n")%>%
  mutate(state = str_to_upper(gsub("\\([^\\)]*\\)", "", state)))
attend_disad_cummulative <- attend_disad_formatted%>%
  #the neccec tables have hours per week this converts to per 600 hours to match npac attending
  mutate(attending_hours = recode(attending_hours, 
                                  "Attending less than 10 hours" = "<400", 
                                  "Attending 10-14 hours" = "400-560", 
                                  "Attending 15 hours or more" = "600+"),
         y_label = recode(attending_hours, 
                          "600+" = "100%",
                          "400-560" = "66%",
                          "<400" = "1%"),
         at_least_hours = str_extract(attending_hours, "^[0-9]+"), 
         at_least_hours = ifelse(is.na(at_least_hours), "1", at_least_hours),
         at_least_hours = as.numeric(at_least_hours))%>%
  group_by(state)%>%
  arrange(desc(at_least_hours))%>%
  mutate(cummulative_n = cumsum(attend_n),
         population = "Disadvantaged")

#NECCEC Table 30 - enrollment total & indig (PI 1 & PI 2.1)
enrol_indig_total_raw <- readxl::read_xlsx("data/Preschool Education, 2024 (Tables 1 to 32).xlsx", sheet = "Table_30",  range = "A5:Q78")
enrol_indig_total <- enrol_indig_total_raw%>%
  rename(col_1 = names(enrol_indig_total_raw[1]), indig_enrolled = ...3, indig_attend = ...5, total_enrolled = ...16, total_attend = ...17)%>%
  select(col_1, indig_enrolled, indig_attend, total_attend, total_enrolled)%>%
  filter(col_1 %in% c("Total",
                      "New South Wales(i)",
                      "Victoria(j)", 
                      "Queensland(k)", 
                      "South Australia(l)", 
                      "Western Australia(m)", 
                      "Tasmania(n)", 
                      "Australian Capital Territory(h)(p)", 
                      "Northern Territory(o)"))%>%
  mutate(state = ifelse(col_1 != "Total", col_1, NA),
         state = gsub("\\([^\\)]*\\)", "", state),
         state = recode(state, "New South Wales" = "NSW",
                        "Victoria" = "VIC",
                        "Queensland"="QLD",
                        "South Australia" = "SA",
                        "Western Australia" = "WA", 
                        "Tasmania" = "TAS",
                        "Northern Territory" = "NT",
                        "Australian Capital Territory" = "ACT"))%>%
  fill(state, .direction = "down")%>%
  filter(!is.na(indig_enrolled))

enrol_total <- enrol_indig_total%>%
  select(state, total_enrolled)

enrol_indig <- enrol_indig_total%>%
  select(state, indig_enrolled)

#NECCEC Table 16 - enrollment disadvantage (PI 3.1)
enrol_disad_raw <- readxl::read_xlsx("data/Preschool Education, 2024 (Tables 1 to 32).xlsx", sheet = "Table_16",  range = "A5:I41")
enrol_disad <- enrol_disad_raw %>%
  rename(col_1 = `Index of Relative Socio-economic Disadvantage(c)(d)`)%>%
  mutate(enrolled_hours  = ifelse(is.na(NSW), col_1, NA))%>%
  fill(enrolled_hours, .direction = "down")%>%
  filter(enrolled_hours == "Total"& col_1 == "Quintile 1")%>%
  pivot_longer(cols = -c(col_1, enrolled_hours), names_to = "state", values_to = "enrolled_n")%>%
  mutate(state = str_to_upper(gsub("\\([^\\)]*\\)", "", state)))

#SSYBFS (PI 2.1 & PI 2.2)
ssybfs_raw<- read_csv("data/A5 Population estimates in state-specific YBFS cohorts.csv")
colnames(ssybfs_raw) <- ssybfs_raw[1,]
colnames(ssybfs_raw)[1] <- "population"
ssybfs_pop <- ssybfs_raw[2:4,]%>%
  filter(population %in% c("Aboriginal and Torres Strait Islanders","Total(b)"))%>%
  pivot_longer(cols = -population, names_to = "state", values_to = "ssybfs_erp")%>%
  mutate(state = str_to_upper(state), 
         state = gsub("\\.", "", state),
         ssybfs_erp = gsub(",", "", ssybfs_erp),
         ssybfs_erp = as.numeric(ssybfs_erp), 
         population = gsub("\\([^\\)]*\\)", "", population))

ssybfs_total <- ssybfs_pop%>%
  filter(population == "Total")

ssybfs_indig <- ssybfs_pop%>%
  filter(population == "Aboriginal and Torres Strait Islanders")

#SEIFA (PI 3.1 & 3.2)
seifa_raw<- read_csv("data/state_seifa_ybfs.csv")
colnames(seifa_raw) <- c("state", "seifa_decile", "age", "seifa_erp")
seifa_pop <- seifa_raw%>%
  slice(11:90)%>%
  fill(state, seifa_decile, .direction = "down")%>%
  pivot_wider(names_from = seifa_decile, values_from = seifa_erp)%>%
  mutate(Quintile_1 = `Decile 1` + `Decile 2`)%>%
  filter (age %in% c("4", "5") & !(state %in% c("Total", "Other Territories")))%>%
  group_by(state)%>%
  summarise(disad_erp = sum(Quintile_1))%>%
  mutate(state = recode(state,"New South Wales" = "NSW",
                        "Victoria" = "VIC",
                        "Queensland"="QLD",
                        "South Australia" = "SA",
                        "Western Australia" = "WA", 
                        "Tasmania" = "TAS",
                        "Northern Territory" = "NT",
                        "Australian Capital Territory" = "ACT"))

#PI 1 - attend_total_cumulative/enrol_total

PI1_table <- attend_total_cumulative%>%
  left_join(enrol_total, by = "state")%>%
  mutate(erp = as.numeric(total_enrolled))%>%
  select(state, population,  cummulative_n, erp, at_least_hours, y_label)

enrol_total_pop <- enrol_total%>%
  left_join(ssybfs_total, by = "state")%>%
  rename(enrolled_n = total_enrolled, 
         erp = ssybfs_erp)%>%
  select(state, population, enrolled_n, erp)

attend_total_pop <- attend_total_cumulative%>%
  left_join(ssybfs_total, by = c("state", "population"))%>%
  rename(erp = ssybfs_erp)%>%
  select(state, population, cummulative_n, erp, at_least_hours, y_label)

#PI 2.1 - enrol_indig/ssybfs_pop
PI2.1_table <- enrol_indig%>%
  left_join(ssybfs_indig, by = "state")%>%
  mutate(enrolled_n = as.numeric(indig_enrolled), 
         erp = ssybfs_erp)%>%
  select(state, population, enrolled_n, erp)

#PI 2.2 - atten_indig_cumulative/ssybfs_pop
PI2.2_table <- atten_indig_cumulative%>%
  left_join(ssybfs_indig, by = c("state", "population")) %>%
  rename(erp = ssybfs_erp)%>%
  select(state, population, cummulative_n, erp, at_least_hours, y_label)

#P1 3.1 - enrol_disad/seifa_pop
PI3.1_table <- enrol_disad%>%
  left_join(seifa_pop, by = "state")%>%
  mutate(population = "Disadvantaged")%>%
  rename(erp = disad_erp)%>%
  select(state, population, enrolled_n, erp)

#P1 3.2 - attend_disad_cummulative/seifa_pop
PI3.2_table <- attend_disad_cummulative%>%
  left_join(seifa_pop, by = "state")%>%
  rename(erp = disad_erp)%>%
  select(state, population, cummulative_n, erp, at_least_hours, y_label)


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

rsconnect::writeManifest(
  appDir = getwd(),
  appFiles = c("app.R", "building tables.R"),
  appPrimaryDoc = "app.R"
)
