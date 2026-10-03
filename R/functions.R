#' Sample Size Calculation for Cross-Sectional Analytical Studies
#' 
#' Plans an expected event-count target for cross-sectional studies with a
#' binary outcome using an events-per-predictor-parameter heuristic. Modified
#' Poisson regression with robust variance may estimate prevalence ratios; the
#' calculator does not fit that model or establish a sufficient EPV for it.
#'
#' @param k Number of predictor coefficients, excluding the intercept
#' @param prevalence Expected prevalence of the outcome (proportion between 0 and 1)
#' @param EPV Chosen events-per-predictor-parameter target. Default is 20,
#'   an operational planning criterion, not a universal threshold
#' @param scenarios Logical. If TRUE, calculates sample sizes for multiple 
#'   EPV values (10, 20, 30, 40, 50)
#' @param language Language for messages: 'en' (English) or 'es' (Spanish). Default is 'en'
#' @return List or data.frame with results. For single calculations,
#'   n_parameters is k; n_variables is retained as a compatibility alias
#' @details
#' The conventional abbreviation EPV is retained, but its denominator is the
#' number of predictor coefficients, excluding the intercept. A categorical
#' predictor with c levels contributes c-1 coefficients when indicator coded.
#' A single linear or prespecified transformed continuous term contributes one;
#' polynomial, spline basis and interaction terms contribute their respective
#' coefficients. Users specify k from the planned model matrix; the calculator
#' does not construct the matrix. Use the same counting convention in VerifyEPV().
#'
#' EPV=20 is the default planning choice, not a universal adequacy threshold.
#' The output satisfies an expected event-count criterion; no model is fitted
#' and stability, precision, confidence-interval coverage and model assumptions
#' are not assessed. The formulas do not adjust for overdispersion, clustering,
#' collinearity or uncertainty in outcome frequency.
#'
#' The output denotes the required analytical sample. If complete-case analysis
#' is planned, an anticipated exclusion proportion m may be allowed for using
#' ceiling(n/(1-m)), assuming the event proportion among analyzable participants
#' matches the planning input. Account jointly for missingness and other losses
#' without double counting. This inflation does not correct selection bias or
#' quantify the information retained by multiple imputation. VerifyEPV() uses
#' events in the analytical sample and its predictor-parameter count.
#'
#' Specify probabilities on a 0-1 scale, for example 0.75 for 75 percent.
#' Use comparable populations and outcome definitions; cumulative incidence
#' and observed survival event proportions must match the observation horizon.
#' Without reliable estimates, document plausible values and repeat calls across
#' that range. scenarios=TRUE varies EPV, not the outcome frequency.
#'
#' Model-specific assumptions still require separate assessment, including
#' proportional hazards and censoring assumptions for Cox analyses. Recruitment
#' feasibility should be evaluated separately from statistical adequacy.
#' @references
#' van Smeden M, et al. (2016). No rationale for 1 variable per 10 events
#' criterion for binary logistic regression analysis. BMC Medical Research
#' Methodology 16:163. doi:10.1186/s12874-016-0267-3.
#' @export
#' @examples
#' # Cross-sectional study of factors associated with diabetes (8% prevalence)
#' SampleCrossSection(k = 12, prevalence = 0.08, EPV = 20)
#' 
#' # Spanish version
#' SampleCrossSection(k = 12, prevalence = 0.08, EPV = 20, language = 'es')
#' 
#' # View multiple scenarios
#' SampleCrossSection(k = 12, prevalence = 0.08, scenarios = TRUE)
#'
#' # Age, binary sex and four-level education require five coefficients.
#' SampleCrossSection(k = 5, prevalence = 0.25, EPV = 20)
SampleCrossSection <- function(k, prevalence, EPV = 20, scenarios = FALSE, language = 'en') {
  
  # Validate language
  if (!language %in% c('en', 'es')) {
    stop('language must be "en" or "es"')
  }
  
  # Error messages
  if (missing(k)) {
    stop(ifelse(language == 'es', 
                'Debe especificar k (numero de parametros predictores sin intercepto)',
                'You must specify k (number of predictor parameters excluding the intercept)'))
  }
  if (missing(prevalence)) {
    stop(ifelse(language == 'es',
                'Debe especificar prevalencia esperada del outcome',
                'You must specify expected outcome prevalence'))
  }
  if (k <= 0 || k != round(k)) {
    stop(ifelse(language == 'es',
                'k debe ser entero positivo',
                'k must be a positive integer'))
  }
  if (prevalence <= 0 || prevalence >= 1) {
    stop(ifelse(language == 'es',
                'Prevalencia debe estar entre 0 y 1',
                'Prevalence must be between 0 and 1'))
  }
  if (EPV <= 0) {
    stop(ifelse(language == 'es',
                'EPV debe ser positivo',
                'EPV must be positive'))
  }
  
  if (scenarios) {
    valores_EPV <- c(10, 20, 30, 40, 50)
    res <- data.frame(
      EPV = valores_EPV,
      eventos_necesarios = valores_EPV * k,
      n_total = ceiling((valores_EPV * k) / prevalence)
    )
    
    if (language == 'es') {
      names(res) <- c('EPV', 'eventos_necesarios', 'n_total')
      cat('\nESCENARIOS DE TAMA\u00d1O MUESTRAL - TRANSVERSAL ANALITICO\n')
      cat('Parametros predictores (k, sin intercepto):', k, '\n')
      cat('Prevalencia del outcome:', prevalence*100, '%\n\n')
      print(res, row.names = FALSE)
      cat('\nCriterio predeterminado: EPV = 20; no garantiza estabilidad\n')
      cat('Nota: EPV se calcula con personas que tienen el outcome\n\n')
    } else {
      names(res) <- c('EPV', 'events_needed', 'n_total')
      cat('\nSAMPLE SIZE SCENARIOS - CROSS-SECTIONAL ANALYTICAL\n')
      cat('Predictor parameters (k, excluding intercept):', k, '\n')
      cat('Outcome prevalence:', prevalence*100, '%\n\n')
      print(res, row.names = FALSE)
      cat('\nDefault planning criterion: EPV = 20; no guarantee of stability\n')
      cat('Note: EPV is calculated with people who have the outcome\n\n')
    }
    return(invisible(res))
  }
  
  if (EPV < 10) {
    warning(ifelse(language == 'es',
                   'EPV elegido < 10; evaluar precision y supuestos por separado',
                   'Chosen EPV < 10; assess precision and assumptions separately'))
  }
  
  # Calculations
  eventos_necesarios <- k * EPV
  n_total <- ceiling(eventos_necesarios / prevalence)
  n_con_outcome <- eventos_necesarios
  n_sin_outcome <- n_total - n_con_outcome
  
  resultados <- list(
    design = ifelse(language == 'es', 'Transversal analitico', 'Cross-sectional analytical'),
    model = ifelse(language == 'es', 
                   'Regresion de Poisson con varianza robusta',
                   'Poisson regression with robust variance'),
    association_measure = ifelse(language == 'es',
                                 'Razon de Prevalencia (PR)',
                                 'Prevalence Ratio (PR)'),
    n_variables = k,
    target_EPV = EPV,
    expected_prevalence = prevalence,
    events_needed = eventos_necesarios,
    n_with_outcome = n_con_outcome,
    n_without_outcome = n_sin_outcome,
    n_total = n_total,
    language = language,
    n_parameters = k
  )
  class(resultados) <- c('SampleCrossSection', 'list')
  
  # Print results
  if (language == 'es') {
    cat('\n=== TAMA\u00d1O MUESTRAL - TRANSVERSAL ANALITICO ===\n')
    cat('Modelo: Regresion de Poisson con varianza robusta (PR)\n')
    cat('Parametros predictores (k, sin intercepto):', k, '\n')
    cat('EPV:', EPV, '\n')
    cat('Prevalencia del outcome:', prevalence*100, '%\n')
    cat('\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n')
    cat('Eventos necesarios (con outcome):', eventos_necesarios, '\n')
    cat('Personas sin outcome esperadas:', n_sin_outcome, '\n')
    cat('\n>>> TAMA\u00d1O TOTAL:', n_total, '<<<\n\n')
    
    cat('EPV es un criterio de conteo de eventos; no garantiza precision ni estabilidad del modelo.\n\n')
    
    if (prevalence < 0.05) {
      cat('\u2139\ufe0f  Nota: Prevalencia baja (<5%). Para mayor eficiencia\n')
      cat('   considere un dise\u00f1o caso-control\n\n')
    }
  } else {
    cat('\n=== SAMPLE SIZE - CROSS-SECTIONAL ANALYTICAL ===\n')
    cat('Model: Poisson regression with robust variance (PR)\n')
    cat('Predictor parameters (k, excluding intercept):', k, '\n')
    cat('EPV:', EPV, '\n')
    cat('Outcome prevalence:', prevalence*100, '%\n')
    cat('\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n')
    cat('Events needed (with outcome):', eventos_necesarios, '\n')
    cat('People without outcome expected:', n_sin_outcome, '\n')
    cat('\n>>> TOTAL SIZE:', n_total, '<<<\n\n')
    
    cat('EPV is an event-count planning criterion; it does not guarantee precision or model stability.\n\n')
    
    if (prevalence < 0.05) {
      cat('\u2139\ufe0f  Note: Low prevalence (<5%). For greater efficiency\n')
      cat('   consider a case-control design\n\n')
    }
  }
  
  return(invisible(resultados))
}

#' Logistical Planning for Cross-Sectional Study
#' 
#' @param n_final Total sample required
#' @param rejection_rate Proportion of participants who will reject
#' @param eligibility_rate Proportion of eligible people
#' @param participants_per_day Participants that can be recruited per day
#' @param working_days_month Working days per month
#' @param language Language for messages: 'en' (English) or 'es' (Spanish). Default is 'en'
#' @return List with logistics
#' @details
#' Enter proportions on a 0-1 scale: 75 percent eligibility is 0.75 and
#' 12 percent refusal or loss is 0.12. Required final counts denote the
#' analytical target. Account jointly for anticipated incomplete-case
#' exclusions and other losses without double counting. These logistical
#' calculations assess resource requirements, not model adequacy.
#' @export
StudyLogistics <- function(n_final,
                          rejection_rate,
                          eligibility_rate,
                          participants_per_day,
                          working_days_month = 22,
                          language = 'en') {
  
  if (!language %in% c('en', 'es')) {
    stop('language must be "en" or "es"')
  }
  
  if (missing(n_final)) {
    stop(ifelse(language == 'es', 
                'Especifique n_final',
                'Specify n_final'))
  }
  if (missing(rejection_rate)) {
    stop(ifelse(language == 'es',
                'Especifique rejection_rate',
                'Specify rejection_rate'))
  }
  if (missing(eligibility_rate)) {
    stop(ifelse(language == 'es',
                'Especifique eligibility_rate',
                'Specify eligibility_rate'))
  }
  if (missing(participants_per_day)) {
    stop(ifelse(language == 'es',
                'Especifique participants_per_day',
                'Specify participants_per_day'))
  }
  
  # Calculations
  participants_to_evaluate <- n_final / (1 - rejection_rate)
  participants_to_invite <- participants_to_evaluate / eligibility_rate
  
  days_needed <- ceiling(participants_to_invite / participants_per_day)
  months_needed <- ceiling(days_needed / working_days_month)
  
  resultados <- list(
    n_final = n_final,
    participants_to_evaluate = ceiling(participants_to_evaluate),
    participants_to_invite = ceiling(participants_to_invite),
    days_needed = days_needed,
    months_needed = months_needed,
    language = language
  )
  class(resultados) <- c('StudyLogistics', 'list')
  
  if (language == 'es') {
    cat('\n=== LOGISTICA - TRANSVERSAL ANALITICO ===\n')
    cat('Muestra final requerida:', n_final, '\n')
    cat('A evaluar:', ceiling(participants_to_evaluate), '(', rejection_rate*100, '% rechazo)\n')
    cat('A invitar:', ceiling(participants_to_invite), '(', eligibility_rate*100, '% elegibles)\n')
    cat('Capacidad:', participants_per_day, 'participantes/dia\n')
    cat('\n>>> DURACION: ', days_needed, 'dias (', months_needed, 'meses) <<<\n\n')
    
    if (months_needed > 12) cat('\u26a0\ufe0f  ADVERTENCIA: Reclutamiento > 1 a\u00f1o\n\n')
  } else {
    cat('\n=== LOGISTICS - CROSS-SECTIONAL ANALYTICAL ===\n')
    cat('Final sample required:', n_final, '\n')
    cat('To evaluate:', ceiling(participants_to_evaluate), '(', rejection_rate*100, '% rejection)\n')
    cat('To invite:', ceiling(participants_to_invite), '(', eligibility_rate*100, '% eligible)\n')
    cat('Capacity:', participants_per_day, 'participants/day\n')
    cat('\n>>> DURATION: ', days_needed, 'days (', months_needed, 'months) <<<\n\n')
    
    if (months_needed > 12) cat('\u26a0\ufe0f  WARNING: Recruitment > 1 year\n\n')
  }
  
  return(invisible(resultados))
}

#' Post-Study EPV Verification for Cross-Sectional Studies
#' 
#' @param n_final Final sample obtained
#' @param n_with_outcome Participants with the observed outcome
#' @param k Number of predictor coefficients, excluding the intercept
#' @param language Language for messages: 'en' (English) or 'es' (Spanish). Default is 'en'
#' @return List with observed EPV
#' @details
#' k is the number of predictor coefficients excluding the intercept, using the
#' same convention as the initial sample-size calculation. Count c-1 indicator
#' coefficients for a categorical predictor with c levels and count all
#' polynomial, spline and interaction coefficients included in the model.
#' Use events from the analytical sample after the planned missing-data handling.
#' This function reports the observed event-per-parameter ratio. It does not fit
#' a regression model or assess stability, precision or model assumptions, and
#' it does not certify adequacy from a fixed EPV threshold.
#' @export
VerifyEPV <- function(n_final, n_with_outcome, k, language = 'en') {
  
  if (!language %in% c('en', 'es')) {
    stop('language must be "en" or "es"')
  }
  
  if (missing(n_final)) {
    stop(ifelse(language == 'es',
                'Especifique n_final',
                'Specify n_final'))
  }
  if (missing(n_with_outcome)) {
    stop(ifelse(language == 'es',
                'Especifique n_with_outcome',
                'Specify n_with_outcome'))
  }
  if (missing(k)) {
    stop(ifelse(language == 'es',
                'Especifique k',
                'Specify k'))
  }
  
  if (k <= 0 || k != round(k)) {
    stop(ifelse(language == 'es',
                'k debe ser entero positivo (parametros predictores sin intercepto)',
                'k must be a positive integer (predictor parameters excluding intercept)'))
  }

  EPV_observed <- n_with_outcome / k
  prevalence_observed <- n_with_outcome / n_final
  
  if (language == 'es') {
    cat('\n=== VERIFICACI\u00d3N EPV POST-ESTUDIO ===\n')
    cat('Muestra final:', n_final, '\n')
    cat('Participantes con outcome:', n_with_outcome, '\n')
    cat('Parametros predictores en modelo (sin intercepto):', k, '\n')
    cat('Prevalencia observada:', round(prevalence_observed*100, 2), '%\n')
    cat('\n>>> EPV OBSERVADO:', round(EPV_observed, 2), '<<<\n\n')
    
    cat('El EPV observado resume eventos por parametro; no evalua estabilidad, precision ni adecuacion del modelo.\n\n')
  } else {
    cat('\n=== POST-STUDY EPV VERIFICATION ===\n')
    cat('Final sample:', n_final, '\n')
    cat('Participants with outcome:', n_with_outcome, '\n')
    cat('Predictor parameters in model (excluding intercept):', k, '\n')
    cat('Observed prevalence:', round(prevalence_observed*100, 2), '%\n')
    cat('\n>>> OBSERVED EPV:', round(EPV_observed, 2), '<<<\n\n')
    
    cat('Observed EPV summarizes events per parameter; it does not assess stability, precision or model adequacy.\n\n')
  }
  
  return(invisible(list(
    n_final = n_final,
    with_outcome = n_with_outcome,
    k = k,
    EPV = EPV_observed,
    prevalence = prevalence_observed,
    language = language
  )))
}

#' @export
print.SampleCrossSection <- function(x, ...) {
  if (x$language == 'es') {
    cat('\nTAMA\u00d1O MUESTRAL TOTAL:', x$n_total, '\n')
    cat('  Con outcome:', x$n_with_outcome, '\n')
    cat('  Sin outcome:', x$n_without_outcome, '\n')
  } else {
    cat('\nTOTAL SAMPLE SIZE:', x$n_total, '\n')
    cat('  With outcome:', x$n_with_outcome, '\n')
    cat('  Without outcome:', x$n_without_outcome, '\n')
  }
  invisible(x)
}

#' @export
print.StudyLogistics <- function(x, ...) {
  if (x$language == 'es') {
    cat('\nMuestra:', x$n_final, '| Duracion:', x$months_needed, 'meses\n')
  } else {
    cat('\nSample:', x$n_final, '| Duration:', x$months_needed, 'months\n')
  }
  invisible(x)
}

