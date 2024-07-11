library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(myRFunctions)  # Assuming this contains ggforest3
library(nnet)  # for multinom()

# Load data
ob_data_hghRsk <- readRDS("High Risk OB.RDS")
ob_data_hghRsk$hghRsk <- factor(ob_data_hghRsk$hghRsk)

# Define UI
ui <- fluidPage(
    titlePanel("Time to Event Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("predictor", "Choose Predictor:", choices = colnames(ob_data_hghRsk)),
            selectInput("strataVar", "Choose Stratification Variable:", choices = c("None", colnames(ob_data_hghRsk))),
            actionButton("updateModel", "Update Model")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Survival Plot", plotOutput("survPlot")),
                tabPanel("Cumulative Hazard Plot", plotOutput("cumHazPlot")),
                tabPanel("Kaplan-Meier Plot", plotOutput("kmPlot")),
                tabPanel("Hazard Ratios", plotOutput("hrPlot")),
                tabPanel("Multinomial Coefficients", verbatimTextOutput("multiCoeffs"))  # New tab for multinomial coeffs
            )
        )
    )
)

# # Define Server
# server <- function(input, output) {
#     fitModel <- eventReactive(input$updateModel, {
#         strata_part <- if(input$strataVar != "None") paste("+ strata(", input$strataVar, ")", sep="") else ""
#         formula <- as.formula(paste("Surv(adm_to_del_tm, event) ~", input$predictor, strata_part))
#         model <- coxph(formula, data = ob_data_hghRsk)
#         print(summary(model))  # Debugging model output
#         return(model)
#     })
#     
#     fitMultinom <- eventReactive(input$updateModel, {
#         # Assuming 'conditions_cnsldt' is your outcome variable and using 'hghRsk' and 'weight' as predictors
#         multinom_model <- multinom(conditions_cnsldt ~ hghRsk + weight, data = ob_data_hghRsk)
#         print(summary(multinom_model))  # Debugging model output
#         return(multinom_model)
#     })
#     
#     output$survPlot <- renderPlot({
#         fit <- fitModel()
#         surv_fit <- survfit(fit)
#         plot(surv_fit, fun = "event", col = 1:3, main = "Survival Probability",
#              xlab = "Time", ylab = "Delivery-as-Event Probability", mark.time = TRUE)
#         if (input$strataVar != "None") {
#             legend("bottomright", legend = levels(ob_data_hghRsk[[input$strataVar]]), col = 1:3, lty = 1)
#         }
#     })
#     
#     output$cumHazPlot <- renderPlot({
#         fit <- fitModel()
#         surv_fit <- survfit(fit)
#         plot(surv_fit, fun = "cumhaz", col = 1:3, main = "Cumulative Hazard",
#              xlab = "Time", ylab = "Cumulative Hazard of Delivery_as_Event", mark.time = TRUE)
#         if (input$strataVar != "None") {
#             legend("topleft", legend = levels(ob_data_hghRsk[[input$strataVar]]), col = 1:3, lty = 1)
#         }
#     })
#     
#     output$kmPlot <- renderPlot({
#         fit <- fitModel()
#         ggsurvplot(survfit(fit), data = ob_data_hghRsk, conf.int = FALSE, risk.table = TRUE, fun = "event")
#     })
#     
#     output$hrPlot <- renderPlot({
#         fit <- fitModel()
#         ggforest3(fit, data = ob_data_hghRsk)
#     })
#     
#     output$multiCoeffs <- renderPrint({
#         fit <- fitMultinom()
#         if (is.null(fit)) {
#             print("Model fitting failed.")  # Debugging statement
#             return()
#         }
#         summary(fit)  # Display the summary of the multinomial model
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)


library(ggplot2)

# # Define Server
# server <- function(input, output) {
#     # Existing model fitting code...
#     
#     fitMultinom <- eventReactive(input$updateModel, {
#         multinom_model <- multinom(conditions_cnsldt ~ hghRsk + weight, data = ob_data_hghRsk)
#         print(summary(multinom_model))  # Debugging model output
#         return(multinom_model)
#     })
#     
#     # Render plots for multinomial coefficients
#     output$multiCoeffs <- renderPlot({
#         fit <- fitMultinom()
#         if (is.null(fit)) {
#             print("Model fitting failed.")  # Debugging statement
#             return()
#         }
#         coef_df <- tidyMultinom(fit)
#         ggplot(coef_df, aes(x = Predictor, y = Coefficient, fill = Category)) +
#             geom_bar(stat = "identity", position = position_dodge()) +
#             theme_minimal() +
#             labs(title = "Multinomial Logistic Regression Coefficients",
#                  y = "Coefficient Value", x = "") +
#             theme(axis.text.x = element_text(angle = 45, hjust = 1))
#     })
# }
# 
# 
# # Run the application
# shinyApp(ui = ui, server = server)

library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(myRFunctions)  # Assuming this contains ggforest3
library(nnet)  # for multinom()

# Load data
ob_data_hghRsk <- readRDS("High Risk OB.RDS")
ob_data_hghRsk$hghRsk <- factor(ob_data_hghRsk$hghRsk)

# Helper function to convert multinomial coefficients to a tidy data frame
tidyMultinom <- function(model) {
    coefs <- coef(summary(model))
    data.frame(
        Predictor = rep(row.names(coefs), times = ncol(coefs)),
        Coefficient = as.vector(coefs),
        Category = rep(colnames(coefs), each = nrow(coefs))
    )
}

# Define UI
ui <- fluidPage(
    titlePanel("Time to Event Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("predictor", "Choose Predictor:", choices = colnames(ob_data_hghRsk)),
            selectInput("strataVar", "Choose Stratification Variable:", choices = c("None", colnames(ob_data_hghRsk))),
            actionButton("updateModel", "Update Model")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Survival Plot", plotOutput("survPlot")),
                tabPanel("Cumulative Hazard Plot", plotOutput("cumHazPlot")),
                tabPanel("Kaplan-Meier Plot", plotOutput("kmPlot")),
                tabPanel("Hazard Ratios", plotOutput("hrPlot")),
                tabPanel("Multinomial Coefficients", plotOutput("multiCoeffs"))  # Updated to plotOutput for the coefficients
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
    
    fitMultinom <- eventReactive(input$updateModel, {
        multinom_model <- multinom(conditions_cnsldt ~ hghRsk + weight, data = ob_data_hghRsk)
        print(summary(multinom_model))  # Debugging model output
        return(multinom_model)
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
        ggforest3(fit, data = ob_data_hghRsk)
    })
    
    output$multiCoeffs <- renderPlot({
        fit <- fitMultinom()
        if (is.null(fit)) {
            print("Model fitting failed.")  # Debugging statement
            return()
        }
        coef_df <- tidyMultinom(fit)
        ggplot(coef_df, aes(x = Predictor, y = Coefficient, fill = Category)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            theme_minimal() +
            labs(title = "Multinomial Logistic Regression Coefficients",
                 y = "Coefficient Value", x = "") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
