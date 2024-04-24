library(shiny)
library(plotly)
library(dplyr)
library(maps)
library(shinydashboard)
teen_birth_rates<-read.csv("CDC Health Data - Teen Birth Rate 2023-March 2024 copy.csv", header=TRUE)
general_requirements<-read.csv("General Requirements for Sex Ed copy.csv", header=TRUE)
general_requirements<-general_requirements%>%
  mutate(sex_ed = ifelse(grepl("X", sex_ed), "required", "not required"))%>%
  mutate(hiv_ed = ifelse(grepl("X", hiv_ed), "required", "not required"))%>%
  mutate(medically_accurate = ifelse(medically_accurate == "" | is.na(medically_accurate), "not required", ifelse(medically_accurate == "X", "required", medically_accurate)))%>%
  mutate(opt_out_allowed = ifelse(opt_out_allowed == "" | is.na(opt_out_allowed), "not required", ifelse(opt_out_allowed == "X", "required", opt_out_allowed)))
categorical_columns<-c("sex_ed", "hiv_ed", "medically_accurate", "opt_out_allowed")
general_requirements<-general_requirements %>%
  mutate_at(vars(categorical_columns), as.factor)
teen_birth_rates$percentage_births_in_US <- substr(teen_birth_rates$percentage_births_in_US, 1, nchar(teen_birth_rates$percentage_births_in_US) - 1)
teen_birth_rates$percentage_births_in_US <- as.numeric(teen_birth_rates$percentage_births_in_US)
pregnancies_vs_ed_reqs<-left_join(teen_birth_rates, general_requirements, by="state")
usa_map<-map_data("state")
test_data<-pregnancies_vs_ed_reqs
test_data$births<-as.numeric(test_data$births)
test_data$state<-tolower(test_data$state)
data_map<-usa_map %>%
  left_join(test_data, by=c("region" = "state"))
data<-pregnancies_vs_ed_reqs

ui<-dashboardPage(
  skin="black",
  header<-dashboardHeader(title="Adolescent Pregnancies and Sex-Ed Requirements in the United States",
                          titleWidth = 800
                          ),
  dashboardSidebar(
    radioButtons("map_type", "Select Map", 
                 choices = c("Adolescent Pregnancies", "Sex-Ed Requirements"),
                 selected = "Adolescent Pregnancies"),
    tags$p("This visualization tool uses adolescent pregnancy data from the CDC 
           from January 2023 - March 2024, along with the current sex education 
           requirement statuses for each state. The table corresponding to the 
           Adolescent Pregnancies map includes the top five states with the highest 
           overall count of adolescent pregnancies, and the table corresponding 
           to the Sex-Ed Requirements map includes more detailed state counts 
           for each requirement status.", style = "margin-left: 20px; margin-right: 20px;")
  ),
  dashboardBody(
    fluidRow(
      box(width=NULL, solidHeader=TRUE,
          plotlyOutput("map", height=400)
      ),
      box(width = NULL,
          uiOutput("stateCount")
      )
    )
  )
)

server <- function(input, output) {
  output$map <- renderPlotly({
    if (input$map_type == "Adolescent Pregnancies") {
      layout<-list(
        geo=list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = TRUE,
          lakecolor = toRGB('white')
        )
      )
      color_scale <- list(
        c(0, 1),
        c("thistle", "#801818") 
      )
      plotly_version <- plot_geo(data, locationmode = 'USA-states') %>%
        add_trace(
          z = ~births, 
          text = ~state, 
          locations = ~abbrev.x,
          color = ~births, 
          colorscale = color_scale,
          colorbar = list(title = "Births")
        ) %>%
        layout(
          title='Adolescent Pregnancies in the United States',
          geo = layout$geo
        )
      plotly_version
    } else {
      data$numeric_sex_ed<-as.numeric(data$sex_ed)
      layout<-list(
        geo=list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = TRUE,
          lakecolor = toRGB('white')
        )
      )
      colorScale <- data.frame(z=c(0, 0.5, 0.5, 1),col=c("#801818", "#801818","thistle","thistle"))
      colorScale$col <- as.character(colorScale$col)
      second_map_version <- plot_geo(data, locationmode = 'USA-states') %>%
        add_trace(
          z = ~numeric_sex_ed, 
          text = ~paste(state, '<br>', "Sex Education: ", sex_ed, '<br>',
                        "Medically Accurate: ", medically_accurate, '<br>',
                        "Opt-out Option :", opt_out_allowed), 
          hoverinfo='text',
          locations = ~abbrev.x,
          color = ~numeric_sex_ed,
          colorscale = colorScale,
          colorbar = list(title = "Sex Education",
                          tickvals=c(1.25, 1.75), 
                          ticktext=c("not required","required"))
        ) %>%
        layout(
          title='Sex Education Requirement Status by State',
          geo = layout$geo
        )
      second_map_version
    }
  })

  output$stateCount<- renderUI({
    if (input$map_type == "Sex-Ed Requirements") {
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Requirement Type"),
                 tags$th("Not Required"),
                 tags$th("Required"),
                 tags$th("HIV Education Only")
               )),
               tags$tbody(
                 tags$tr(
                   tags$td("Sex Education"),
                   tags$td(nrow(pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$sex_ed == "required",])),
                   tags$td(nrow(pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$sex_ed == "not required",]))
                 ),
                 tags$tr(
                   tags$td("Education Must be Medically Accurate"),
                   tags$td(nrow(pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$medically_accurate == "required",])),
                   tags$td(nrow(pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$medically_accurate == "not required",])),
                   tags$td(nrow(pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$medically_accurate == "HIV",]))
                 ),
                 tags$tr(
                   tags$td("Must Include an Opt-out Option"),
                   tags$td(nrow(pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$opt_out_allowed == "required",])),
                   tags$td(nrow(pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$opt_out_allowed == "not required",])),
                   tags$td(nrow(pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$opt_out_allowed == "HIV",]))
                 )

               )
    )
    } else {
      texas_births<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "Texas", ]$births
      california_births<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "California", ]$births
      florida_births<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "Florida", ]$births
      georgia_births<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "Georgia", ]$births
      ohio_births<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "Ohio", ]$births
     
      texas_percent<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "Texas", ]$percentage_births_in_US
      california_percent<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "California", ]$percentage_births_in_US
      florida_percent<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "Florida", ]$percentage_births_in_US
      georgia_percent<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "Georgia", ]$percentage_births_in_US
      ohio_percent<-pregnancies_vs_ed_reqs[pregnancies_vs_ed_reqs$state == "Ohio", ]$percentage_births_in_US
     
       tags$table(class = "table",
                 tags$thead(tags$tr(
                   tags$th("State"),
                   tags$th("Pregnancies"),
                   tags$th("Percentage of Total Nationwide Pregnancies Accounted for")
                 )),
                 tags$tbody(
                   tags$tr(
                     tags$td("Texas"),
                     tags$td(texas_births),
                     tags$td(texas_percent)
                   ),
                   tags$tr(
                     tags$td("California"),
                     tags$td(california_births),
                     tags$td(california_percent)
                   ),
                   tags$tr(
                     tags$td("Florida"),
                     tags$td(florida_births),
                     tags$td(florida_percent)
                   ),
                   tags$tr(
                     tags$td("Georgia"),
                     tags$td(georgia_births),
                     tags$td(georgia_percent)
                   ),
                   tags$tr(
                     tags$td("Ohio"),
                     tags$td(ohio_births),
                     tags$td(ohio_percent)
                 )
        )
       )
    }
  })
}

shinyApp(ui = ui, server = server)
