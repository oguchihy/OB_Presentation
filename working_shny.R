library(shiny)
library(survival)
library(survminer)
library(ggplot2)
library(myRFunctions)  # Assuming this contains ggforest3
library(nnet)  # for multinom()
library(dplyr)
library(randomForest)

# Load data
ob_data <- readRDS("Factorized OB DATA with Selected Variables.RDS")

# Convert adm_to_del_tm_cat to a factor
ob_data <- ob_data %>%
    mutate(adm_to_del_tm_cat = factor(case_when(
        adm_to_del_tm < 12 ~ "Under 12 hours",
        adm_to_del_tm >= 12 & adm_to_del_tm < 24 ~ "13-24 hours",
        adm_to_del_tm >= 24 & adm_to_del_tm < 48 ~ "25-48 hours",
        adm_to_del_tm >= 48 ~ "Over 48 hours"
    )))

# Define UI
ui <- fluidPage(
    titlePanel("Time to Event Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("predictor", "Choose Predictor for Cox Model:", choices = colnames(ob_data)),
            selectInput("strataVar", "Choose Stratification Variable:", choices = c("None", colnames(ob_data))),
            selectInput("multinomOutcome", "Choose Outcome for Multinomial Logistic Regression:", 
                        choices = c("adm_to_del_tm_cat", "intrapartal_events", "conditions_cnsldt")),
            selectInput("multinomPredictor", "Choose Predictor for Multinomial Logistic Regression:", 
                        choices = colnames(ob_data)),
            selectInput("orOutcome", "Choose Outcome for Odds Ratio Analysis:", 
                        choices = c("adm_to_del_tm_cat", "intrapartal_events", "conditions_cnsldt")),
            selectInput("orPredictor", "Choose Predictor for Odds Ratio Analysis:", 
                        choices = colnames(ob_data)),
            selectInput("varImpOutcome", "Choose Outcome for Variable Importance:", 
                        choices = c("adm_to_del_tm_cat", "conditions_cnsldt", "cluster", "delivery_cnsldt")),
            actionButton("updateModel", "Update Model")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Survival Plot", plotOutput("survPlot")),
                tabPanel("Cumulative Hazard Plot", plotOutput("cumHazPlot")),
                tabPanel("Kaplan-Meier Plot", plotOutput("kmPlot")),
                tabPanel("Hazard Ratios", plotOutput("hrPlot")),
                tabPanel("Multinomial Coefficients", plotOutput("multiCoeffs")),
                tabPanel("Odds Ratios", plotOutput("orPlot")),  # New tab for Odds Ratios
                tabPanel("Variable Importance", plotOutput("varImportancePlot"))  # New tab for variable importance
            )
        )
    )
)

# Define Server
server <- function(input, output, session) {
    set.seed(123)  # For reproducibility
    
    fitModel <- eventReactive(input$updateModel, {
        strata_part <- if(input$strataVar != "None") paste("+ strata(", input$strataVar, ")", sep="") else ""
        formula <- as.formula(paste("Surv(adm_to_del_tm, event) ~", input$predictor, strata_part))
        model <- coxph(formula, data = ob_data)
        print(summary(model))  # Debugging model output
        return(model)
    })
    
    fitMultinom <- eventReactive(input$updateModel, {
        # Use a smaller subset for multinomial logistic regression to simplify computation
        ob_data_subset <- ob_data %>% sample_n(500)  # Adjust the number as needed
        
        formula <- as.formula(paste(input$multinomOutcome, "~", input$multinomPredictor))
        multinom_model <- multinom(formula, data = ob_data_subset, maxit = 100)
        print(summary(multinom_model))  # Debugging model output
        return(multinom_model)
    })
    
    fitOR <- eventReactive(input$updateModel, {
        formula <- as.formula(paste(input$orOutcome, "~", input$orPredictor))
        glm_model <- glm(formula, data = ob_data, family = binomial)
        summary(glm_model)  # Debugging model output
        return(glm_model)
    })
    
    output$survPlot <- renderPlot({
        fit <- fitModel()
        surv_fit <- survfit(fit)
        plot(surv_fit, col = 1:3, main = paste("Survival Probability by", input$predictor),
             xlab = "Time", ylab = "Delivery-as-Event Probability", mark.time = TRUE)
        if (input$strataVar != "None") {
            legend("bottomright", legend = levels(ob_data[[input$strataVar]]), col = 1:3, lty = 1)
        }
    })
    
    output$cumHazPlot <- renderPlot({
        fit <- fitModel()
        surv_fit <- survfit(fit)
        plot(surv_fit, fun = "cumhaz", col = 1:3, main = paste("Cumulative Hazard by", input$predictor),
             xlab = "Time", ylab = "Cumulative Hazard of Delivery_as_Event", mark.time = TRUE)
        if (input$strataVar != "None") {
            legend("topleft", legend = levels(ob_data[[input$strataVar]]), col = 1:3, lty = 1)
        }
    })
    
    output$kmPlot <- renderPlot({
        fit <- fitModel()
        ggsurvplot(survfit(fit), data = ob_data, conf.int = FALSE, risk.table = TRUE, fun = "event",
                   title = paste("Kaplan-Meier Plot by", input$predictor))
    })
    
    output$hrPlot <- renderPlot({
        fit <- fitModel()
        ggforest3(fit, data = ob_data) + ggtitle(paste("Hazard Ratios by", input$predictor))
    })
    
    output$multiCoeffs <- renderPlot({
        fit <- fitMultinom()
        if (is.null(fit)) {
            print("Model fitting failed.")  # Debugging statement
            return()
        }
        coef_df <- data.frame(
            Predictor = rownames(coef(fit)),
            Coefficient = coef(fit)[, 1],
            StdError = summary(fit)$standard.errors[, 1]
        )
        ggplot(coef_df, aes(x = Predictor, y = Coefficient, fill = Coefficient)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(title = paste("Multinomial Logistic Regression Coefficients by", input$multinomPredictor),
                 y = "Coefficient Value", x = "") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$orPlot <- renderPlot({
        fit <- fitOR()
        or_df <- data.frame(
            Predictor = rownames(summary(fit)$coefficients),
            OR = exp(summary(fit)$coefficients[, 1]),
            Lower = exp(confint(fit)[, 1]),
            Upper = exp(confint(fit)[, 2])
        )
        ggplot(or_df, aes(x = Predictor, y = OR, ymin = Lower, ymax = Upper, color = OR)) +
            geom_pointrange() +
            theme_minimal() +
            labs(title = paste("Odds Ratios by", input$orPredictor),
                 y = "Odds Ratio", x = "Predictor") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_color_gradient(low = "blue", high = "red")
    })
    
    output$varImportancePlot <- renderPlot({
        rf_model <- randomForest(as.formula(paste(input$varImpOutcome, "~ .")), data = ob_data)
        importance <- importance(rf_model)
        importance_df <- data.frame(
            Predictor = rownames(importance),
            Importance = importance[, 1]
        )
        ggplot(importance_df, aes(x = reorder(Predictor, Importance), y = Importance, fill = Importance)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(title = paste("Variable Importance for", input$varImpOutcome),
                 x = "Predictors", y = "Importance") +
            theme_minimal() +
            theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
            scale_fill_gradient(low = "blue", high = "red")
    })
}

shinyApp(ui = ui, server = server)
