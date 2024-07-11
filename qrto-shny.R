library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(myRFunctions)  # Assuming this contains ggforest3
ob_data_compact_qrto <- readRDS("ob_data_compact.RDS")
ob_hghRsk_qrto <- readRDS("High Risk OB.RDS")
ob_data_hghRsk <- readRDS("High Risk OB.RDS")
dte_range_NMC_data <- range(ob_data_compact_qrto$adm_date)

# Define UI
ui <- fluidPage(
    titlePanel("Time to Event Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("predictor", "Choose Main Predictor:",
                        choices = c("age", "grav", "para", "gest_age_days", "weight", "diff_grav_para", "conditions_cnsldt")),
            selectInput("strataVar", "Choose Stratification Variable:", 
                        choices = c("conditions_cnsldt")),
            actionButton("updateModel", "Update Model")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Survival Plot", plotOutput("survPlot")),
                tabPanel("Cumulative Hazard Plot", plotOutput("cumHazPlot")),
                tabPanel("Kaplan-Meier Plot", plotOutput("kmPlot")),
                tabPanel("Hazard Ratios", plotOutput("hrPlot"))
            )
        )
    )
)

# Define Server
server <- function(input, output) {
    fit_shiny <- eventReactive(input$updateModel, {
        formula <- as.formula(paste("Surv(adm_to_del_tm, event) ~", input$predictor, "+ strata(", input$strataVar, ")"))
        model_shiny <- coxph(formula, data = ob_data_hghRsk)
        print(summary(model_shiny))  # Debugging model output
        return(model_shiny)
    })
    
    output$hrPlot <- renderPlot({
        fit_shiny <- fit_shiny()
        if (is.null(fit_shiny)) {
            print("Model fitting failed or incomplete for HR plot.")
            return()
        }
        ggforest3(fit_shiny, data = ob_data_hghRsk)  # Using your function directly
    })
    
    output$survPlot <- renderPlot({
        fit_shiny <- fit_shiny()
        surv_fit_shiny <- survfit(fit_shiny)
        plot(surv_fit_shiny, fun = "event", col = 1:3, main = "Survival Probability",
             xlab = "Time", ylab = "Delivery-as-Event Probability", mark.time = TRUE)
        legend("bottomright", legend = levels(ob_data_hghRsk[[input$strataVar]]), col = 1:3, lty = 1)
    })
    
    output$cumHazPlot <- renderPlot({
        fit_shiny <- fit_shiny()
        surv_fit_shiny <- survfit(fit_shiny)
        plot(surv_fit_shiny, fun = "cumhaz", col = 1:3, main = "Cumulative Hazard",
             xlab = "Time", ylab = "Cumulative Hazard of Delivery_as_Event", mark.time = TRUE)
        legend("topleft", legend = levels(ob_data_hghRsk[[input$strataVar]]), col = 1:3, lty = 1)
    })
    
    output$kmPlot <- renderPlot({
        fit_shiny <- fit_shiny()
        ggsurvplot(survfit(fit_shiny), data = ob_data_hghRsk, conf.int = FALSE, risk.table = TRUE, fun = "event")
    })
}

# Run the application
shinyApp(ui = ui, server = server)


library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(myRFunctions)  # Ensure your custom package is loaded
ob_data_compact_qrto <- readRDS("ob_data_compact.RDS")
ob_hghRsk_qrto <- readRDS("High Risk OB.RDS")
ob_data_hghRsk <- readRDS("High Risk OB.RDS")

# Define UI
ui <- fluidPage(
    titlePanel("Time to Event Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("predictor", "Choose Predictor:", choices = c("hghRsk", "weight", "gest_age_days")),
            selectInput("strataVar", "Choose Stratification Variable:", choices = c("hghRsk", "cluster", "weight")),
            actionButton("updateModel", "Update Model")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Survival Plot", plotOutput("survPlot")),
                tabPanel("Cumulative Hazard Plot", plotOutput("cumHazPlot")),
                tabPanel("Kaplan-Meier Plot", plotOutput("kmPlot")),
                tabPanel("Hazard Ratios", plotOutput("hrPlot"))
            )
        )
    )
)

# Define Server
server <- function(input, output) {
    fitModel <- eventReactive(input$updateModel, {
        formula <- as.formula(paste("Surv(adm_to_del_tm, event) ~", input$predictor))
        model <- coxph(formula, data = ob_data_hghRsk)
        print(summary(model))  # Debugging model output
        return(model)
    })
    
    output$survPlot <- renderPlot({
        fit <- fitModel()
        surv_fit <- survfit(fit)
        plot(surv_fit, fun = "event", col = 1:3, main = "Survival Probability",
             xlab = "Time", ylab = "Delivery-as-Event Probability", mark.time = TRUE)
        legend("bottomright", legend = levels(ob_data_hghRsk[[input$strataVar]]), col = 1:3, lty = 1)
    })
    
    output$cumHazPlot <- renderPlot({
        fit <- fitModel()
        surv_fit <- survfit(fit)
        plot(surv_fit, fun = "cumhaz", col = 1:3, main = "Cumulative Hazard",
             xlab = "Time", ylab = "Cumulative Hazard of Delivery_as_Event", mark.time = TRUE)
        legend("topleft", legend = levels(ob_data_hghRsk[[input$strataVar]]), col = 1:3, lty = 1)
    })
    
    output$kmPlot <- renderPlot({
        fit <- fitModel()
        ggsurvplot(survfit(fit), data = ob_data_hghRsk, conf.int = FALSE, risk.table = TRUE, fun = "event")
    })
    
    output$hrPlot <- renderPlot({
        fit <- fitModel()
        if (is.null(fit)) {
            print("Model fitting failed or incomplete for HR plot.")
            return()
        }
        myRFunctions::ggforest3(fit, data = ob_data_hghRsk)  # Assuming ggforest3() requires similar arguments to ggforest()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
