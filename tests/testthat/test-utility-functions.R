test_that("clean_sector_name function works correctly", {
  # Test basic cleaning
  expect_equal(clean_sector_name("Industrial_Sector"), "Sector [Industrial]")
  expect_equal(clean_sector_name("Energy_Sector"), "Sector [Energy]")
  expect_equal(clean_sector_name("Transportation_Sector"), "Sector [Transportation]")
  
  # Test with sector IDs
  expect_equal(clean_sector_name("[1A4b] Residential"), "Residential [1A4b]")
  expect_equal(clean_sector_name("1A4b_Residential"), "Residential [1A4b]")
  
  # Test edge cases
  expect_equal(clean_sector_name(""), "")
  expect_equal(clean_sector_name("Sector"), "Sector")
})

test_that("clean_fuel_name function works correctly", {
  expect_equal(clean_fuel_name("natural_gas"), "Natural gas")
  expect_equal(clean_fuel_name("coal"), "Coal")
  expect_equal(clean_fuel_name("oil"), "Oil")
  expect_equal(clean_fuel_name("diesel_fuel"), "Diesel fuel")
})

test_that("clean_country_name function works correctly", {
  expect_equal(clean_country_name("Indonesia"), "Indonesia")
  expect_equal(clean_country_name(NA), "International")
  expect_equal(clean_country_name(c("Indonesia", NA, "China")), 
               c("Indonesia", "International", "China"))
})

test_that("iso3_to_country function works correctly", {
  expect_equal(iso3_to_country("idn"), "Indonesia")
  expect_equal(iso3_to_country("chn"), "China")
  expect_equal(iso3_to_country("usa"), "United States")
  
  # Test special cases
  expect_equal(iso3_to_country("GLOBAL"), "Global")
  expect_equal(iso3_to_country("WORLD"), "World")
  
  # Test invalid codes
  expect_equal(iso3_to_country("invalid"), "invalid")
}) 