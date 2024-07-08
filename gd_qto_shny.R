library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(myRFunctions)  # Assuming this contains ggforest3

# Load data
ob_data_hghRsk <- readRDS("High Risk OB.RDS")
ob_data_hghRsk$hghRsk <- factor(ob_data_hghRsk$hghRsk)

# Define UI
ui <- fluidPage(
    titlePanel("Time to Event Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("predictor", "Choose Predictor:", choices = c("hghRsk", "weight", "gest_age_days")),
            selectInput("strataVar", "Choose Stratification Variable:", choices = c("None", "hghRsk", "cluster", "weight")),
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
        strata_part <- if(input$strataVar != "None") paste("+ strata(", input$strataVar, ")", sep="") else ""
        formula <- as.formula(paste("Surv(adm_to_del_tm, event) ~", input$predictor, strata_part))
        model <- coxph(formula, data = ob_data_hghRsk)
        print(summary(model))  # Debugging model output
        return(model)
    })
    
    output$survPlot <- renderPlot({
        fit <- fitModel()
        surv_fit <- survfit(fit)
        plot(surv_fit, fun = "event", col = 1:3, main = "Survival Probability",
             xlab = "Time", ylab = "Delivery-as-Event Probability", mark.time = TRUE)
        if (input$strataVar != "None") {
            legend("bottomright", legend = levels(ob_data_hghRsk[[input$strataVar]]), col = 1:3, lty = 1)
        }
    })
    
    output$cumHazPlot <- renderPlot({
        fit <- fitModel()
        surv_fit <- survfit(fit)
        plot(surv_fit, fun = "cumhaz", col = 1:3, main = "Cumulative Hazard",
             xlab = "Time", ylab = "Cumulative Hazard of Delivery_as_Event", mark.time = TRUE)
        if (input$strataVar != "None") {
            legend("topleft", legend = levels(ob_data_hghRsk[[input$strataVar]]), col = 1:3, lty = 1)
        }
    })
    
    output$kmPlot <- renderPlot({
        fit <- fitModel()
        ggsurvplot(survfit(fit), data = ob_data_hghRsk, conf.int = FALSE, risk.table = TRUE, fun = "event")
    })
    
    output$hrPlot <- renderPlot({
        fit <- fitModel()
        hr_plot <- ggforest3(fit, data = ob_data_hghRsk)
        
        # Assuming ggforest3 returns a ggplot object. Adjust 10 and 1000 to your actual needs
        hr_plot <- hr_plot + xlim(10, 1000)
        
        print(hr_plot)  # This will display the plot in the Shiny app
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
