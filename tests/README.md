# Testing Framework

This directory contains the test suite for the CEDS Dashboard package using the `testthat` framework.

## Test Structure

- `testthat.R` - Main test runner
- `setup.R` - Test setup and environment configuration
- `test-edgar-download.R` - Tests for EDGAR data download functionality
- `test-edgar-validation.R` - Tests for EDGAR provincial vs national validation
- `test-ceds-validation.R` - Tests for CEDS provincial vs national validation
- `test-utility-functions.R` - Tests for utility functions

## Running Tests

### Run all tests
```r
devtools::test()
```

### Run specific test file
```r
testthat::test_file("tests/testthat/test-edgar-download.R")
```

### Run tests interactively
```r
devtools::test_active_file()
```

## Test Categories

### Unit Tests
- Utility functions (`clean_sector_name`, `clean_fuel_name`, etc.)
- Data validation functions
- Helper functions

### Integration Tests
- EDGAR data download and processing
- CEDS data download and processing
- Provincial vs national validation

### Validation Tests
- Compare provincial sums with known national totals
- Verify data structure and format
- Check for data consistency

## Test Data

Test data is stored in `tests/testthat/test_data/` and is automatically cleaned up after tests.