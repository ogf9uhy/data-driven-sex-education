library(shiny)
library(plotly)
library(dplyr)
library(shinydashboard)
library(ggplot2)
content_requirements<-read.csv("Content Requirements for Sex Ed - Content Requirements for Sex Ed copy.csv", header=TRUE)
content_requirements<-content_requirements%>%
  mutate(contraception = ifelse(grepl("X", contraception), "required", "not required"))%>%
  mutate(abstinence = case_when(
    abstinence == "Stress" ~ "stressed",
    abstinence == "Cover" ~ "covered",
    is.na(abstinence) | abstinence == "" ~ "not covered",
    TRUE ~ abstinence
  ))%>%
  mutate(sowm = ifelse(grepl("X", sowm), "required", "not required"))%>%
  mutate(sexual_orientation = case_when(
    grepl("Inclusive", sexual_orientation) ~ "inclusive",
    grepl("Negative", sexual_orientation) ~ "exclusive",
    grepl("ᶲ", sexual_orientation) ~ "exclusive",
    grepl("Prohibited", sexual_orientation) ~ "prohibited",
    is.na(sexual_orientation) | sexual_orientation == "" ~ "unknown",
    TRUE ~ sexual_orientation
  ))%>%
  mutate(hiv_condoms = ifelse(grepl("X", hiv_condoms), "required", "not required"))%>%
  mutate(hiv_abstinence = case_when(
    hiv_abstinence == "Stress" ~ "stressed",
    hiv_abstinence == "Cover" ~ "covered",
    is.na(hiv_abstinence) | hiv_abstinence == "" ~ "not covered",
    TRUE ~ hiv_abstinence
  ))
categorical_columns1<-c("contraception", "abstinence", "sowm", "sexual_orientation", "hiv_condoms", "hiv_abstinence")
content_requirements<-content_requirements %>%
  mutate_at(vars(categorical_columns1), as.factor)

ui<-dashboardPage(
  skin="black",
  header<-dashboardHeader(titleWidth = 800,
                          title = div(style="text-align: right; padding-right: 50px;", "Sex Education Curriculum Requirements in the United States")
                          
  ),
  dashboardSidebar(
    checkboxInput("flip_checkbox","Horizontal Bars",value = FALSE),
    tags$p("This visualization tool uses sex education curriculum requirement 
           statuses as of 2024 for each state in the US. The different content 
           requirements are each represented by a bar plot, which includes 
           the number of states that currently cover, do not cover, 
           or stress/emphasize that content in sex education programs.", style = "margin-left: 20px; margin-right: 20px;")
  ),
  dashboardBody(
    fluidRow(
      box(width=NULL, 
        radioButtons("variable_choice", "Content Specification", 
                   choices = c("Contraceptive Education" = "contraception", "Abstinence" = "abstinence", "Sex Only When Married" = "sowm")
        )
      ),
      box(width=NULL, solidHeader=TRUE,
          plotOutput("bar", height=500)
      )
  ),
  tags$head(tags$style(HTML("
    .skin-black .main-sidebar {
        background-color:  grey;
        .radio-inline label {
      padding-left: 100px;
                            }")))
  )
)

server <- function(input, output) {
    output$bar <- renderPlot({
      barplot<-ggplot(content_requirements, aes_string(x=input$variable_choice, fill=input$variable_choice)) +
        geom_bar(stat="count") +
        theme(legend.position="none", axis.text=element_text(size=12), axis.title=element_text(size=13))
      if (input$variable_choice == "abstinence") {
        barplot<-barplot + scale_fill_manual(values=c("#801818", "thistle", "brown")) +
          labs(x="Abstinence Only", y="Count of States")
      }
      else if (input$variable_choice == "contraception") {
        barplot<-barplot + scale_fill_manual(values=c("#801818", "thistle")) +
          labs(x="Contraceptive Education", y="Count of States")
      }
      else {
        barplot<-barplot + scale_fill_manual(values=c("#801818", "thistle")) +
          labs(x="Sex Only When Married", y="Count of States")
      }
      if (input$flip_checkbox == TRUE){
        barplot <- barplot + coord_flip()
      }
      barplot
    })
}

shinyApp(ui = ui, server = server)
