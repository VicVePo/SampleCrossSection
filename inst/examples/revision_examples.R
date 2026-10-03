# Version 0.1.2: predictor-parameter counting example.
# This grid defines coding; it is not a patient dataset.
design <- expand.grid(age=c(30,40,50,60,70),
                      sex=factor(c("F","M")),
                      education=factor(c("A","B","C","D")))
mm <- stats::model.matrix(~ age + sex + education, data=design)
k <- sum(colnames(mm) != "(Intercept)")
stopifnot(k == 5L, qr(mm)$rank == ncol(mm))
planned <- SampleCrossSection::SampleCrossSection(k=k, EPP=20, prevalence=0.25)
SampleCrossSection::VerifyEPP(n_final=planned$n_total,
                       n_with_outcome=planned$events_needed, k=k)

# A log transformation uses one coefficient; nonlinear terms can use more.
forms <- list(linear=~age+sex+education,
              logarithmic=~log(age)+sex+education,
              quadratic=~age+I(age^2)+sex+education,
              spline=~splines::ns(age,df=3)+sex+education,
              interaction=~age*sex+education)
parameter_counts <- vapply(forms, function(f) {
  m <- stats::model.matrix(f, data=design)
  sum(colnames(m) != "(Intercept)")
}, integer(1))
stopifnot(identical(unname(parameter_counts),c(5L,5L,6L,7L,6L)))
print(parameter_counts)
