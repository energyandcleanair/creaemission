test_that("Tests use the same cache folder as main code", {
  # Get cache folder from the function
  cache_folder <- get_cache_folder()

  # Verify it's a valid path
  expect_true(dir.exists(cache_folder))

  # Verify it's within the project root
  project_root <- get_project_root()
  expect_true(grepl(project_root, cache_folder, fixed = TRUE))

  # Verify it ends with "cache"
  expect_true(endsWith(cache_folder, "cache") ||
              endsWith(cache_folder, file.path("cache", "")))

  # Test subdirectory functionality
  ceds_cache <- get_cache_folder("ceds")
  expect_true(dir.exists(ceds_cache))
  expect_true(endsWith(ceds_cache, file.path("cache", "ceds")))

  edgar_cache <- get_cache_folder("edgar")
  expect_true(dir.exists(edgar_cache))
  expect_true(endsWith(edgar_cache, file.path("cache", "edgar")))

  # Test nested subdirectories
  nested_cache <- get_cache_folder(c("edgar", "gridded"))
  expect_true(dir.exists(nested_cache))
  expect_true(endsWith(nested_cache, file.path("cache", "edgar", "gridded")))
})

test_that("get_project_root works correctly from test context", {
  # This test verifies that get_project_root can find the project root
  # even when run from the testthat context
  project_root <- get_project_root()

  # Should exist and contain key directories
  expect_true(dir.exists(project_root))
  expect_true(dir.exists(file.path(project_root, "R")))
  expect_true(dir.exists(file.path(project_root, "data")))
  expect_true(dir.exists(file.path(project_root, "cache")))

  # Should not be the testthat directory
  expect_false(basename(project_root) == "testthat")
  expect_false(basename(project_root) == "tests")
})

test_that("Cache paths are consistent between main code and tests", {
  # This test ensures that the cache paths used in the main code
  # are the same as those used in tests

  # Get cache paths using our functions
  main_cache <- get_cache_folder()
  ceds_cache <- get_cache_folder("ceds")
  edgar_cache <- get_cache_folder("edgar")

  # These should all be within the same project root
  project_root <- get_project_root()

  expect_equal(dirname(main_cache), file.path(project_root))
  expect_equal(dirname(ceds_cache), file.path(project_root, "cache"))
  expect_equal(dirname(edgar_cache), file.path(project_root, "cache"))

  # Verify the paths are absolute and normalized
  expect_true(grepl("^/", main_cache) || grepl("^[A-Z]:", main_cache))
  expect_true(grepl("^/", ceds_cache) || grepl("^[A-Z]:", ceds_cache))
  expect_true(grepl("^/", edgar_cache) || grepl("^[A-Z]:", edgar_cache))
})
