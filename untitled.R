library(shiny)
library(dplyr)
library(tableone)

# Load data
ob_data_tbl1_shny <- readRDS("All Factored Complete Ready OB Dataset for Analytics.RDS") %>% 
  dplyr::select(-c(age, grav, para, weight, apg1, apg5, diff_grav_para, gest_age_days, adm_to_del_tm, gest_age, adm_date, delivery_date, event, time, baby_sq)) %>% 
  dplyr::mutate(cluster = as.factor(if_else(cluster == 2, 1, 0)))

ui <- fluidPage(
  selectInput("variable", "Select a Variable:", choices = names(ob_data_tbl1_shny)),
  actionButton("update", "Update Table"),
  tableOutput("table_one")
)

server <- function(input, output, session) {
  selected_var <- reactive({
    req(input$variable)
    input$variable
  })
  
  table_one_data <- eventReactive(input$update, {
    tableone::CreateTableOne(data = ob_data_tbl1_shny, vars = selected_var())
  })
  
  output$table_one <- renderTable({
    table_df <- as.data.frame(print(table_one_data(), printToggle = FALSE))
    table_df
  }, rownames = TRUE)
}

shinyApp(ui, server)


sessionInfo()
install.packages("shiny")
install.packages("tableone")
install.packages("dplyr")
