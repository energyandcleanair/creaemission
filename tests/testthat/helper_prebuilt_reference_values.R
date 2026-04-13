# Values used for sanity checking of built dataset.
# Values were collected from raw data at country level (with some sector grouping)
# They will be used to validate national/provincial/raster values against.

PREBUILT_REFERENCE_VALUES <- list(
  ceds = list(
    provincial = list(
      list(
        iso3 = "chn",
        year = 2022,
        sector_group = "Industry",
        reference_sum_kt = 16251.5749,
        reference_origin = "CEDS China NMVOC Industry, 2022",
        tolerance = 0.01
      ),
      list(
        iso3 = "idn",
        year = 2022,
        sector_group = "Industry",
        reference_sum_kt = 523.5622,
        reference_origin = "CEDS Indonesia NMVOC Industry, 2022",
        tolerance = 0.02
      ),
      list(
        iso3 = "idn",
        year = 2022,
        sector_group = "Transport",
        reference_sum_kt = 1327.0559,
        reference_origin = "CEDS Indonesia NMVOC Transport, 2022",
        tolerance = 0.01
      )
    ),
    raster = list(
      list(
        iso3 = "chn",
        year = 2023,
        sector_group = "Industry",
        reference_sum_kt = 16405.7453,
        reference_origin = "CEDS China NMVOC Industry, 2023",
        tolerance = 0.02
      ),
      list(
        iso3 = "wld",
        year = 2023,
        sector_group = "Industry",
        reference_sum_kt = 41801.8866,
        reference_origin = "CEDS World NMVOC Industry, 2023",
        tolerance = 0.01
      ),
      list(
        iso3 = "wld",
        year = 2023,
        sector_group = "Buildings",
        reference_sum_kt = 20627.6429,
        reference_origin = "CEDS World NMVOC Buildings, 2023",
        tolerance = 0.07
      ),
      list(
        iso3 = "idn",
        year = 2023,
        sector_group = "Industry",
        reference_sum_kt = 526.5067,
        reference_origin = "CEDS Indonesia NMVOC Industry, 2023",
        tolerance = 0.12
      ),
      list(
        iso3 = "idn",
        year = 2023,
        sector_group = "Buildings",
        reference_sum_kt = 465.3265,
        reference_origin = "CEDS Indonesia NMVOC Buildings, 2023",
        tolerance = 0.02
      )
    )
  ),
  edgar = list(
    provincial = list(
      list(
        iso3 = "idn",
        year = 2022,
        sector_group = "Buildings",
        reference_sum_kt = 384.2984,
        reference_origin = "EDGAR Indonesia NMVOC Buildings, 2022",
        tolerance = 0.01
      ),
      list(
        iso3 = "chn",
        year = 2022,
        sector_group = "Industry",
        reference_sum_kt = 13522.6560,
        reference_origin = "EDGAR China NMVOC Industry, 2022",
        tolerance = 0.03
      ),
      list(
        iso3 = "ind",
        year = 2022,
        sector_group = "Industry",
        reference_sum_kt = 5572.1586,
        reference_origin = "EDGAR India NMVOC Industry, 2022",
        tolerance = 0.02
      )
    ),
    raster = list(
      list(
        iso3 = "wld",
        year = 2022,
        sector_group = "Buildings",
        map_sector = "Residential & Commercial",
        reference_sum_kt = 22085.13,
        reference_origin = "EDGAR World NMVOC Buildings, 2022",
        tolerance = 0.02
      ),
      list(
        iso3 = "idn",
        year = 2022,
        sector_group = "Buildings",
        map_sector = "Residential & Commercial",
        reference_sum_kt = 384.2984,
        reference_origin = "EDGAR Indonesia NMVOC Buildings, 2022",
        tolerance = 0.06
      ),
      list(
        iso3 = "ind",
        year = 2022,
        sector_group = "Buildings",
        map_sector = "Residential & Commercial",
        reference_sum_kt = 3123.0430,
        reference_origin = "EDGAR India NMVOC Buildings, 2022",
        tolerance = 0.01
      ),
      list(
        iso3 = "chn",
        year = 2022,
        sector_group = "Buildings",
        map_sector = "Residential & Commercial",
        reference_sum_kt = 2427.3340,
        reference_origin = "EDGAR China NMVOC Buildings, 2022",
        tolerance = 0.10
      )
    )
  )
)
