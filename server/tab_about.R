output$about <- renderUI({
  HTML("
  <div class='about-content'>
   The data presented in this dashboard is based on the Community Emissions Data System (CEDS) dataset.

    <h3>About CEDS</h3>
    <p>The Community Emissions Data System (CEDS) produces consistent estimates of global air emissions species over the industrial era (1750-present). CEDS provides a comprehensive, consistent dataset of anthropogenic emissions of greenhouse gases, reactive gases, aerosols, and aerosol precursors from 1750 to present.</p>

      <p>Key features of CEDS:</p>
      <ul>
        <li>Annual estimates from 1750 to present</li>
        <li>Consistent methodology across species, countries, and time</li>
        <li>Monthly seasonality profiles</li>
        <li>Detailed sector and fuel divisions</li>
        <li>Both country-level and gridded (0.1° x 0.1°) data products</li>
      </ul>

      <h3>Data Sources</h3>
      <p>This dashboard uses the following CEDS data sources:</p>
      <ul>
        <li>Country-level emissions data: <a href='https://zenodo.org/records/10904361' target='_blank'>CEDS v2024.04.01 (Zenodo)</a></li>
        <li>Gridded emissions data: <a href='https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/' target='_blank'>CEDS Gridded Data</a></li>
      </ul>


      <h3>Methodology</h3>
      <p>The emissions data in this dashboard is processed and analyzed using the following methodology:</p>

      <h4>Country-Level Data</h4>
      <ul>
        <li>Data is downloaded from the CEDS detailed country-level emissions dataset</li>
        <li>Emissions are provided for multiple species (CO2, CH4, SO2, NOx, etc.)</li>
        <li>Data is organized by country, sector, and fuel type</li>
        <li>All emissions are converted to kilotonnes (kt) for consistency</li>
      </ul>

      <h4>Provincial Data</h4>
      <ul>
        <li>Monthly spatial emissions data is extracted at 0.1° resolution</li>
        <li>Provincial boundaries are defined using GADM4 (Global Administrative Areas) data</li>
        <li>A 20km buffer is applied to coastal regions to account for offshore emissions</li>
        <li>Emissions are aggregated to provincial level and converted to annual values</li>
        <li>Results are validated against national totals (typically within 2% difference)</li>
      </ul>

      <p>For detailed CEDS documentation and methodology, visit: <a href='https://github.com/JGCRI/CEDS/' target='_blank'>CEDS Documentation</a></p>


    <h3>About CREA</h3>
    <p>The Centre for Research on Energy and Clean Air (CREA) is an independent research organisation focused on revealing the trends,
    causes, and health impacts, as well as the solutions to air pollution. CREA uses scientific data, research, and evidence to support
    the efforts of governments, companies, and campaigning organisations worldwide in their efforts to move towards clean energy and
    clean air, believing that effective research and communication are the key to successful policies, investment decisions,
    and advocacy efforts. CREA was founded in December 2019 in Helsinki and has staff in several Asian and European countries.</p>


  </div>
  ")
})
