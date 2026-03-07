sample_dcf <- dplyr::tibble(
  variable_name = c("name", "sex", "gender", "height", "mass", "hair_color", "skin_color", "eye_color", "homeworld"),
  label = c("Character name", "Sex", "Gender", "Height", "Mass", "Hair color", "Skin color", "Eye color", "Homeworld"),
  type = c("c", "c", "c", "d", "d", "c", "c", "c", "c"),
  valueset = c(
    list(NULL),
    list(
      dplyr::tibble(
        value = c("male", "female", "none", "hermaphroditic"),
        label = c("Male", "Female", "None", "Hermaphroditic")
      )
    ),
    list(NULL),
    list(NULL),
    list(NULL),
    list(NULL),
    list(NULL),
    list(NULL),
    list(NULL)
  )
)



test_that("add_metadata works correctly", {

  starwars <- dplyr::select(
    dplyr::starwars,
    name,
    height,
    mass,
    dplyr::ends_with('color'),
    sex,
    species,
    gender
  )

  starwars_with_meta <- add_metadata(starwars, metadata = sample_dcf)

  expect_true(attributes(starwars_with_meta$name)$label == "Character name")
  expect_true(is.null(attributes(starwars_with_meta$species)$label))
  expect_true(!is.null(attributes(starwars_with_meta$sex)$labels))
  expect_identical(
    as.character(attributes(starwars_with_meta$sex)$labels),
    c("male", "female", "none", "hermaphroditic")
  )
  expect_identical(
    names(attributes(starwars_with_meta$sex)$labels),
    c("Male", "Female", "None", "Hermaphroditic")
  )

})
