#' Sample Size Calculation for Cross-Sectional Analytical Studies
#' 
#' Calculates the required sample size for multivariable analysis of associated
#' factors in cross-sectional studies. Uses the events per variable (EPV) method
#' appropriate for Poisson regression with robust variance estimating prevalence
#' ratios (PR).
#' 
#' @param k Number of independent variables to include in the multivariable model
#' @param prevalence Expected prevalence of the outcome (proportion between 0 and 1)
#' @param EPV Events per variable (recommended 20-50). Events are people WITH the outcome
#' @param scenarios Logical. If TRUE, calculates sample sizes for multiple 
#'   EPV values (10, 20, 30, 40, 50)
#' @param language Language for messages: 'en' (English) or 'es' (Spanish). Default is 'en'
#' @return List or data.frame with results
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
SampleCrossSection <- function(k, prevalence, EPV = 20, scenarios = FALSE, language = 'en') {
  
  # Validate language
  if (!language %in% c('en', 'es')) {
    stop('language must be "en" or "es"')
  }
  
  # Error messages
  if (missing(k)) {
    stop(ifelse(language == 'es', 
                'Debe especificar k (numero de variables)',
                'You must specify k (number of variables)'))
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
      cat('\nESCENARIOS DE TAMAÑO MUESTRAL - TRANSVERSAL ANALITICO\n')
      cat('Variables (k):', k, '\n')
      cat('Prevalencia del outcome:', prevalence*100, '%\n\n')
      print(res, row.names = FALSE)
      cat('\nRecomendacion: EPV >= 20\n')
      cat('Nota: EPV se calcula con personas que tienen el outcome\n\n')
    } else {
      names(res) <- c('EPV', 'events_needed', 'n_total')
      cat('\nSAMPLE SIZE SCENARIOS - CROSS-SECTIONAL ANALYTICAL\n')
      cat('Variables (k):', k, '\n')
      cat('Outcome prevalence:', prevalence*100, '%\n\n')
      print(res, row.names = FALSE)
      cat('\nRecommendation: EPV >= 20\n')
      cat('Note: EPV is calculated with people who have the outcome\n\n')
    }
    return(invisible(res))
  }
  
  if (EPV < 10) {
    warning(ifelse(language == 'es',
                   'EPV < 10 muy bajo. Se recomienda EPV >= 20',
                   'EPV < 10 very low. EPV >= 20 is recommended'))
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
    language = language
  )
  class(resultados) <- c('SampleCrossSection', 'list')
  
  # Print results
  if (language == 'es') {
    cat('\n=== TAMAÑO MUESTRAL - TRANSVERSAL ANALITICO ===\n')
    cat('Modelo: Regresion de Poisson con varianza robusta (PR)\n')
    cat('Variables (k):', k, '\n')
    cat('EPV:', EPV, '\n')
    cat('Prevalencia del outcome:', prevalence*100, '%\n')
    cat('───────────────────────────────────────\n')
    cat('Eventos necesarios (con outcome):', eventos_necesarios, '\n')
    cat('Personas sin outcome esperadas:', n_sin_outcome, '\n')
    cat('\n>>> TAMAÑO TOTAL:', n_total, '<<<\n\n')
    
    if (EPV < 20) {
      cat('⚠️  ADVERTENCIA: EPV < 20 puede comprometer validez\n\n')
    } else if (EPV >= 50) {
      cat('✓ Excelente: EPV >= 50 proporciona estimaciones muy robustas\n\n')
    }
    
    if (prevalence < 0.05) {
      cat('ℹ️  Nota: Prevalencia baja (<5%). Para mayor eficiencia\n')
      cat('   considere un diseño caso-control\n\n')
    }
  } else {
    cat('\n=== SAMPLE SIZE - CROSS-SECTIONAL ANALYTICAL ===\n')
    cat('Model: Poisson regression with robust variance (PR)\n')
    cat('Variables (k):', k, '\n')
    cat('EPV:', EPV, '\n')
    cat('Outcome prevalence:', prevalence*100, '%\n')
    cat('───────────────────────────────────────\n')
    cat('Events needed (with outcome):', eventos_necesarios, '\n')
    cat('People without outcome expected:', n_sin_outcome, '\n')
    cat('\n>>> TOTAL SIZE:', n_total, '<<<\n\n')
    
    if (EPV < 20) {
      cat('⚠️  WARNING: EPV < 20 may compromise validity\n\n')
    } else if (EPV >= 50) {
      cat('✓ Excellent: EPV >= 50 provides very robust estimates\n\n')
    }
    
    if (prevalence < 0.05) {
      cat('ℹ️  Note: Low prevalence (<5%). For greater efficiency\n')
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
    
    if (months_needed > 12) cat('⚠️  ADVERTENCIA: Reclutamiento > 1 año\n\n')
  } else {
    cat('\n=== LOGISTICS - CROSS-SECTIONAL ANALYTICAL ===\n')
    cat('Final sample required:', n_final, '\n')
    cat('To evaluate:', ceiling(participants_to_evaluate), '(', rejection_rate*100, '% rejection)\n')
    cat('To invite:', ceiling(participants_to_invite), '(', eligibility_rate*100, '% eligible)\n')
    cat('Capacity:', participants_per_day, 'participants/day\n')
    cat('\n>>> DURATION: ', days_needed, 'days (', months_needed, 'months) <<<\n\n')
    
    if (months_needed > 12) cat('⚠️  WARNING: Recruitment > 1 year\n\n')
  }
  
  return(invisible(resultados))
}

#' Post-Study EPV Verification for Cross-Sectional Studies
#' 
#' @param n_final Final sample obtained
#' @param n_with_outcome Participants with the observed outcome
#' @param k Number of variables in the model
#' @param language Language for messages: 'en' (English) or 'es' (Spanish). Default is 'en'
#' @return List with observed EPV
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
  
  EPV_observed <- n_with_outcome / k
  prevalence_observed <- n_with_outcome / n_final
  
  if (language == 'es') {
    cat('\n=== VERIFICACIÓN EPV POST-ESTUDIO ===\n')
    cat('Muestra final:', n_final, '\n')
    cat('Participantes con outcome:', n_with_outcome, '\n')
    cat('Variables en modelo:', k, '\n')
    cat('Prevalencia observada:', round(prevalence_observed*100, 2), '%\n')
    cat('\n>>> EPV OBSERVADO:', round(EPV_observed, 2), '<<<\n\n')
    
    if (EPV_observed < 10) {
      cat('❌ CRÍTICO: EPV < 10. Resultados muy inestables\n')
      cat('   Recomendacion: Reducir variables o reportar limitacion\n\n')
    } else if (EPV_observed < 20) {
      cat('⚠️  ADVERTENCIA: EPV < 20. Interpretacion cautelosa\n')
      cat('   Recomendacion: Analisis de sensibilidad\n\n')
    } else if (EPV_observed >= 20 && EPV_observed < 30) {
      cat('✓ Aceptable: EPV >= 20\n\n')
    } else {
      cat('✓✓ Excelente: EPV >= 30\n\n')
    }
  } else {
    cat('\n=== POST-STUDY EPV VERIFICATION ===\n')
    cat('Final sample:', n_final, '\n')
    cat('Participants with outcome:', n_with_outcome, '\n')
    cat('Variables in model:', k, '\n')
    cat('Observed prevalence:', round(prevalence_observed*100, 2), '%\n')
    cat('\n>>> OBSERVED EPV:', round(EPV_observed, 2), '<<<\n\n')
    
    if (EPV_observed < 10) {
      cat('❌ CRITICAL: EPV < 10. Very unstable results\n')
      cat('   Recommendation: Reduce variables or report limitation\n\n')
    } else if (EPV_observed < 20) {
      cat('⚠️  WARNING: EPV < 20. Cautious interpretation\n')
      cat('   Recommendation: Sensitivity analysis\n\n')
    } else if (EPV_observed >= 20 && EPV_observed < 30) {
      cat('✓ Acceptable: EPV >= 20\n\n')
    } else {
      cat('✓✓ Excellent: EPV >= 30\n\n')
    }
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
    cat('\nTAMAÑO MUESTRAL TOTAL:', x$n_total, '\n')
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

