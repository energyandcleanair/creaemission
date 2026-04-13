test_that("test cache seeding reuses repo cache artifacts when available", {
  repo_cache_root <- file.path(get_project_root(), "cache")
  cache_candidates <- list.files(repo_cache_root, recursive = TRUE, full.names = TRUE)
  cache_candidates <- cache_candidates[file.exists(cache_candidates) & !dir.exists(cache_candidates)]

  if (length(cache_candidates) == 0) {
    skip("Repo cache is empty; nothing to seed into test cache")
  }

  candidate_sizes <- file.info(cache_candidates)$size
  source_file <- cache_candidates[order(candidate_sizes)][1]
  relative_path <- substring(source_file, nchar(repo_cache_root) + 2)
  destination_path <- file.path(get_project_root(), "tests", "testthat", "cache", relative_path)

  if (file.exists(destination_path)) {
    unlink(destination_path)
  }

  seeded_path <- seed_test_cache_path(relative_path)

  expect_equal(normalizePath(seeded_path), normalizePath(destination_path))
  expect_true(file.exists(destination_path))
  expect_equal(file.info(destination_path)$size, file.info(source_file)$size)
})
