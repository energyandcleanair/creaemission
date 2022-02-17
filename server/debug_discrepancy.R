


d <- list(
  "USA"=list(
    "pm25"=tibble(poll=8, pop=320.7e6),
    "no2"=tibble(poll=0, pop=320.7e6)
  ),
  "CAN"=list(
    "pm25"=tibble(poll=7, pop=38e6),
    "no2"=tibble(poll=0, pop=38e6)
  ),
  "CHN"=list(
    "pm25"=tibble(poll=57.5, pop=1371e6),
    "no2"=tibble(poll=0, pop=1371e6)
  ),
  "IND"=list(
    "pm25"=tibble(poll=74, pop=1310e6),
    "no2"=tibble(poll=0, pop=1310e6)
  )
)


hia_regions <- hia.compute(distribs=d)


regions <- list(
  "Canada,USA"=c("USA","CAN"),
  "China"="CHN",
  "India"="IND"
)


region_deaths <- lapply(names(regions), function(r){
  print(regions[r])
  hia_regions %>% filter(region_id %in% regions[[r]]) %>%
    group_by(cause, unit, pollutant) %>%
    summarise(central=sum(central)) %>%
    filter(cause=="NCD.LRI",
           unit=="Deaths") %>%
    mutate(region=r)
}) %>% bind_rows(.)


region_deaths_article <- list(
  "Canada,USA"=213e3,
  "China"=2470e3,
  "India"=2219e3
) %>%
  enframe("region","central") %>%
  unnest(central) %>%
  mutate(source="Burnett")

bind_rows(
  region_deaths %>% mutate(source="CREA"),
  region_deaths_article
) %>%
  ggplot() + geom_bar(stat="identity", aes(source, central, fill=source), position="dodge") +
  facet_wrap(~region) +
  labs(title="PM2.5 NCD.LRI deaths",
       y="Deaths",
       x=NULL)




# Compare w. Lauri --------------------------------------------------------
hia_chn <- hia.compute(distribs=distribs["CHN"])




