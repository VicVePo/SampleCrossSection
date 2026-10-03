# SampleCrossSection

Version 0.1.2 provides an events-per-predictor-parameter planning heuristic, logistical calculations and an observed EPP summary for the design covered by this package.

## Installation

```r
devtools::install_github("VicVePo/SampleCrossSection@v0.1.2")
```

English is the default output language. Set `language="es"` for Spanish. EPP=20 is the default planning choice and is not a universal sufficiency threshold.

## Count coefficients rather than clinical characteristics

`k` counts predictor coefficients, excluding the intercept. With reference indicator coding, a categorical predictor with c levels contributes c-1 coefficients. A single linear or prespecified transformed continuous term contributes one; polynomial, spline basis and interaction terms contribute the coefficients needed by that specification. The user specifies k; the calculator does not build the model matrix. Use the same convention in the initial calculation and `VerifyEPP()`.

Age, binary sex and four-level education require five coefficients. A reproducible coding example is included in `inst/examples/revision_examples.R`:

```r
k <- 5
planned <- SampleCrossSection::SampleCrossSection(k=k, EPP=20, prevalence=0.25)
SampleCrossSection::VerifyEPP(n_final=planned$n_total,
                      n_with_outcome=planned$events_needed, k=k)
```

For individual calculations, `n_parameters` reports k. The existing `n_variables` field is retained as an alias for compatibility. Existing positional arguments, default language, formulas and rounding conventions are preserved. `EPP` is the preferred target argument and `VerifyEPP()` is the preferred verification function. `EPV` and `VerifyEPV()` remain supported for compatibility; specifying both target arguments with different values is an error. `target_EPP` and observed `EPP` are added without removing `target_EPV` or observed `EPV`; scenario tables also retain the `EPV` column. Printed messages and scenario headers use EPP.

## Analytical sample and input assumptions

The sample-size output denotes the required analytical sample. The calculator does not process participant-level missing data. If complete-case exclusions are anticipated, an exclusion proportion m can be allowed for with `ceiling(n/(1-m))`, assuming the outcome frequency in analyzable participants matches the planning input. Account jointly for missingness and other losses without double counting. Inflation does not correct selection bias or quantify information from multiple imputation. Use analytical events and the model's predictor-parameter count in `VerifyEPP()`.

Enter probabilities as proportions, for example 0.75 for 75 percent eligibility and 0.12 for 12 percent anticipated loss. Cumulative incidence and observed survival event proportions must match the planned follow-up period. Use comparable populations and outcome definitions; when previous estimates are unreliable, document plausible values and repeat calls over that range. `scenarios=TRUE` varies EPP only.

The functions do not fit a regression model, evaluate coefficient stability or precision, or adjust for overdispersion, clustering, collinearity or input uncertainty. Model-specific assumptions need separate assessment, including proportional hazards and censoring assumptions for Cox regression. When logistical requirements exceed resources, reconsider recruitment capacity, retention and prespecified model complexity; a feasible EPP target alone does not establish adequacy.

## Uso e interpretación en español

`k` representa el número de coeficientes de los predictores, sin incluir el intercepto. Una variable categórica con c niveles aporta c-1 coeficientes si se utiliza codificación habitual por indicadores. Una transformación continua con un solo término conserva un coeficiente; los términos cuadráticos, las bases de splines y las interacciones pueden aumentar el conteo. Edad, sexo binario y educación con cuatro niveles requieren k=5. La definición debe ser la misma al planificar y al ejecutar `VerifyEPP()`. `EPV` y `VerifyEPV()` siguen funcionando como nombres equivalentes por compatibilidad.

EPP=20 es el criterio predeterminado de planificación. Los mensajes describen el conteo, sin certificar estabilidad ni precisión. El tamaño calculado es el necesario para el análisis. Las exclusiones anticipadas por información incompleta deben considerarse junto con las otras pérdidas, sin duplicarlas; inflar la muestra no corrige el sesgo de selección. Las probabilidades se ingresan en escala 0–1 y la incidencia o proporción de eventos debe corresponder al horizonte de seguimiento. La herramienta no ajusta modelos ni evalúa sobredispersión o sus supuestos.

## Methodological reference

van Smeden M, et al. No rationale for 1 variable per 10 events criterion for binary logistic regression analysis. BMC Medical Research Methodology. 2016;16:163. https://doi.org/10.1186/s12874-016-0267-3

The reference documents limitations of fixed EPV rules for binary logistic regression; it is not presented as validation of a universal threshold for Cox or modified Poisson regression.
