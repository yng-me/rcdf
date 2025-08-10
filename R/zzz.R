utils::globalVariables(
  c(
    ".",
    ":="
  )
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rcbms <- list(
    rcbms.options = list(
      verbose = TRUE,
      survey_round = "2024",
      records = list(
        "2024" = c(
          "cbms_interview_record",
          "cbms_person_record",
          "cbms_person_record_tvet",
          "cbms_household_record",
          "cbms_household_record_child_mortality",
          "cbms_barangay_record",
          "cbms_barangay_record_list"
        )
      )
    )
  )

  to_set <- !(names(op.rcbms) %in% names(op))
  if (any(to_set)) options(op.rcbms[to_set])

  invisible()
}
