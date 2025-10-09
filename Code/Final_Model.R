

fit_cox_model <- function(dependent, predictor, data) {
  # Formula propuesta 
  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_nac) + vulnerability"))
  
  # Modelo Ajustado
  model_fit <- coxph(formula, data = data)
}