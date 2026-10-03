# Focused checks for the reviewer-related 0.1.1 update; base R only.
quiet <- function(fun, ...) {
  capture.output(value <- suppressWarnings(fun(...)))
  value
}
k <- 5
plan <- quiet(SampleCrossSection::SampleCrossSection, k=k, EPV=20, prevalence=0.25)
stopifnot(plan$n_total == 400, plan$events_needed == 100,
          plan$n_parameters == 5, plan$n_variables == plan$n_parameters)
verification <- quiet(SampleCrossSection::VerifyEPV, n_final=plan$n_total,
                      n_with_outcome=plan$events_needed, k=k)
stopifnot(verification$EPV==20, verification$k==5)
stopifnot(inherits(try(quiet(SampleCrossSection::VerifyEPV,n_final=300,
                           n_with_outcome=100,k=0),silent=TRUE),"try-error"))
stopifnot(inherits(try(quiet(SampleCrossSection::VerifyEPV,n_final=300,
                           n_with_outcome=100,k=2.5),silent=TRUE),"try-error"))
for (lang in c("en","es")) {
  for (epv in c(5,10,20,30,50)) {
    output <- capture.output(ans <- SampleCrossSection::VerifyEPV(
      n_final=500,n_with_outcome=epv*k,k=k,language=lang))
    stopifnot(ans$EPV==epv,
      !any(grepl("Excellent|Excelente|Acceptable|Aceptable|very unstable|muy inestables|has robust|tiene estimaciones robustas",output)))
    stopifnot(any(grepl(if(lang=="en") "excluding intercept" else "sin intercepto", output)),
              any(grepl(if(lang=="en") "does not assess" else "no evalua", output)))
  }
}
example_file <- system.file("examples","revision_examples.R",package="SampleCrossSection")
stopifnot(nzchar(example_file))
invisible(capture.output(source(example_file,local=new.env())))
cat("Reviewer update checks passed for SampleCrossSection 0.1.1\n")
