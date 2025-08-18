output$about <- renderUI({
  HTML("
  <div class='about-content'>
   <h2>Contact</h2>
   <p>For questions about this data portal, please contact: <a href='mailto:data-team@energyandcleanair.org'>data-team@energyandcleanair.org</a></p>
   
   <h2>About CREA</h2>
   <p>The Centre for Research on Energy and Clean Air (CREA) is an independent research organisation focused on revealing the trends, causes, and health impacts, as well as the solutions to air pollution. We use scientific data, research and evidence to support the efforts of governments, companies and campaigning organizations worldwide in their efforts to move towards clean energy and clean air.</p>
   
   <p>For more information, visit <a href='https://energyandcleanair.org/' target='_blank'>https://energyandcleanair.org/</a></p>
   
   <h2>Methodology</h2>
   <p>The emissions data in this portal is processed and analyzed using the following methodology:</p>
   
   <h3>Country-Level Data</h3>
   <ul>
     <li>Data is downloaded from the respective emission inventory datasets</li>
     <li>Emissions are provided for multiple species (CO2, CH4, SO2, NOx, etc.)</li>
     <li>Data is organized by country, sector, and fuel type</li>
     <li>All emissions are converted to kilotonnes (kt) for consistency</li>
   </ul>
   
   <h3>Provincial Data</h3>
   <ul>
     <li>Gridded emission data is aggregated to provincial boundaries</li>
     <li>Administrative boundaries are based on GADM database</li>
     <li>GADM polygons are buffered 10 km into the sea to avoid missing coastal installations</li>
     <li>Provincial data is available for selected countries</li>
   </ul>
   
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
   
   <p>Learn more about CEDS: <a href='https://github.com/JGCRI/CEDS/' target='_blank'>CEDS GitHub Repository</a></p>
   
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
   
   <p>Learn more about EDGAR: <a href='https://edgar.jrc.ec.europa.eu/dataset_ghg70' target='_blank'>EDGAR v7.0 Greenhouse Gas Emissions</a></p>
   
   <h3>Data Sources</h3>
   <p>This portal uses the following data sources:</p>
   <ul>
     <li>CEDS country-level emissions data: <a href='https://zenodo.org/records/10904361' target='_blank'>CEDS v2024.04.01 (Zenodo)</a></li>
     <li>CEDS gridded emissions data: <a href='https://rcdtn1.pnl.gov/data/CEDS/CEDS_release-v_2024_07_08_metadata_fix/gridded_emissions/bulk_emissions/fine_grids/' target='_blank'>CEDS Gridded Data</a></li>
     <li>EDGAR emissions data: <a href='https://edgar.jrc.ec.europa.eu/dataset_ghg70' target='_blank'>EDGAR v7.0 Greenhouse Gas Emissions</a></li>
   </ul>
  </div>
  ")
})
