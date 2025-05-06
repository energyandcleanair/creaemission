output$about <- renderUI({
  HTML("
  <div class='about-content'>
   The data presented in this dashboard is based on multiple emission inventory datasets.
   
   <h2>Data Sources</h2>
   
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
   
   <h3>About EDGAR</h3>
   <p>The Emissions Database for Global Atmospheric Research (EDGAR) is a global emission inventory covering greenhouse gases and air pollutants from 1970 to present. EDGAR is developed by the European Commission's Joint Research Centre.</p>
   
   <p>Key features of EDGAR:</p>
   <ul>
     <li>Annual estimates from 1970 to present</li>
     <li>Global coverage with country-level resolution</li>
     <li>Multiple sectors and activities</li>
     <li>Gridded data at 0.1° x 0.1° resolution</li>
     <li>Consistent methodology for all countries</li>
   </ul>
   
   <h3>Data Sources</h3>
   <p>This dashboard uses the following data sources:</p>
   <ul>
     <li>CEDS country-level emissions data: <a href='https://zenodo.org/records/10904361' target='_blank'>CEDS v2024.04.01 (Zenodo)</a></li>
     <li>CEDS gridded emissions data: <a href='https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/' target='_blank'>CEDS Gridded Data</a></li>
     <li>EDGAR emissions data: <a href='https://edgar.jrc.ec.europa.eu/dataset_ghg70' target='_blank'>EDGAR v7.0 Greenhouse Gas Emissions</a></li>
   </ul>
   
   <h3>Methodology</h3>
   <p>The emissions data in this dashboard is processed and analyzed using the following methodology:</p>
   
   <h4>Country-Level Data</h4>
   <ul>
     <li>Data is downloaded from the respective emission inventory datasets</li>
     <li>Emissions are provided for multiple species (CO2, CH4, SO2, NOx, etc.)</li>
     <li>Data is organized by country, sector, and fuel type</li>
     <li>All emissions are converted to kilotonnes (kt) for consistency</li>
   </ul>
   
   <h4>Provincial Data</h4>
   <ul>
     <li>Gridded emission data is aggregated to provincial boundaries</li>
     <li>Administrative boundaries are based on GADM database</li>
     <li>Provincial data is available for selected countries</li>
   </ul>
  </div>
  ")
})
