# Version 0.1.2

- Use EPP (events per predictor parameter) in messages, help and examples.
- Add the preferred EPP argument while preserving existing positional arguments and EPV calls.
- Export VerifyEPP and retain VerifyEPV as an equivalent compatibility alias.
- Add target_EPP and observed EPP result fields and an EPP scenario column without removing legacy fields.
- Reject conflicting EPP and EPV inputs. Preserve all sample-size and logistical calculations.

# Version 0.1.1

- Define k as predictor coefficients excluding the intercept, consistently in planning and VerifyEPV.
- Document indicator-coded categorical predictors, transformations, nonlinear terms and interactions.
- Replace adequacy and stability claims with descriptive EPV messages in English and Spanish.
- Explain analytical sample size, anticipated incomplete-case exclusions, input uncertainty, model assumptions and feasibility.
- Clarify proportion inputs, observation horizons, default English output and survival logistics duration.
- Add n_parameters while retaining the existing n_variables field and public function signatures.
- Add a linked survival example and focused regression checks. Sample-size and logistical formulas are unchanged.
- Normalize Unicode string encoding and MIT license metadata for package checks; retain the original license text and attribution in LICENSE.md.
- Remove unused dependency and empty-data declarations.
