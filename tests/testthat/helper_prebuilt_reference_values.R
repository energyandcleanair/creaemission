# Manually curated reference sums for prebuilt sanity tests.
# These values are collected from the national cached/prebuilt baseline for the
# corresponding source, pollutant, year, geography, and sector group.

PREBUILT_REFERENCE_VALUES <- list(
  ceds = list(
    provincial = list(
      list(
        iso3 = "idn",
        year = 2022,
        sector_group = "Buildings",
        reference_sum_kt = 466.6810,
        reference_origin = "CEDS national cache baseline: Indonesia NMVOC Buildings, 2022",
        reference_tolerance = 0.10,
        comparison_tolerance = 0.10
      ),
      list(
        iso3 = "ind",
        year = 2022,
        sector_group = "Buildings",
        reference_sum_kt = 3972.0660,
        reference_origin = "CEDS national cache baseline: India NMVOC Buildings, 2022",
        reference_tolerance = 0.10,
        comparison_tolerance = 0.10
      ),
      list(
        iso3 = "chn",
        year = 2022,
        sector_group = "Buildings",
        reference_sum_kt = 2143.1580,
        reference_origin = "CEDS national cache baseline: China NMVOC Buildings, 2022",
        reference_tolerance = 0.25,
        comparison_tolerance = 0.25
      )
    ),
    raster = list(
      list(
        iso3 = "wld",
        year = 2023,
        sector_group = "Buildings",
        map_sector = "Residential, Commercial, Other",
        reference_sum_kt = 20627.64,
        reference_origin = "CEDS national cache baseline: World NMVOC Buildings, 2023",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.10
      ),
      list(
        iso3 = "idn",
        year = 2023,
        sector_group = "Buildings",
        map_sector = "Residential, Commercial, Other",
        reference_sum_kt = 465.3265,
        reference_origin = "CEDS national cache baseline: Indonesia NMVOC Buildings, 2023",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.10
      ),
      list(
        iso3 = "ind",
        year = 2023,
        sector_group = "Buildings",
        map_sector = "Residential, Commercial, Other",
        reference_sum_kt = 3961.3090,
        reference_origin = "CEDS national cache baseline: India NMVOC Buildings, 2023",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.10
      ),
      list(
        iso3 = "chn",
        year = 2023,
        sector_group = "Buildings",
        map_sector = "Residential, Commercial, Other",
        reference_sum_kt = 2101.0040,
        reference_origin = "CEDS national cache baseline: China NMVOC Buildings, 2023",
        reference_tolerance = 0.25,
        comparison_tolerance = 0.25
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
        reference_origin = "EDGAR national cache baseline: Indonesia NMVOC Buildings, 2022",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.15
      ),
      list(
        iso3 = "chn",
        year = 2022,
        sector_group = "Industry",
        reference_sum_kt = 13522.6560,
        reference_origin = "EDGAR national cache baseline: China NMVOC Industry, 2022",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.15
      ),
      list(
        iso3 = "ind",
        year = 2022,
        sector_group = "Industry",
        reference_sum_kt = 5572.1586,
        reference_origin = "EDGAR national cache baseline: India NMVOC Industry, 2022",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.15
      )
    ),
    raster = list(
      list(
        iso3 = "wld",
        year = 2022,
        sector_group = "Buildings",
        map_sector = "Residential & Commercial",
        reference_sum_kt = 22085.13,
        reference_origin = "EDGAR national cache baseline: World NMVOC Buildings, 2022",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.10
      ),
      list(
        iso3 = "idn",
        year = 2022,
        sector_group = "Buildings",
        map_sector = "Residential & Commercial",
        reference_sum_kt = 384.2984,
        reference_origin = "EDGAR national cache baseline: Indonesia NMVOC Buildings, 2022",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.10
      ),
      list(
        iso3 = "ind",
        year = 2022,
        sector_group = "Buildings",
        map_sector = "Residential & Commercial",
        reference_sum_kt = 3123.0430,
        reference_origin = "EDGAR national cache baseline: India NMVOC Buildings, 2022",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.10
      ),
      list(
        iso3 = "chn",
        year = 2022,
        sector_group = "Buildings",
        map_sector = "Residential & Commercial",
        reference_sum_kt = 2427.3340,
        reference_origin = "EDGAR national cache baseline: China NMVOC Buildings, 2022",
        reference_tolerance = 0.15,
        comparison_tolerance = 0.15
      )
    )
  )
)
