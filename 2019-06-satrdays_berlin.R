# Init --------------------------------------------------------------------

library(eurostat)      # eurostat data
library(rnaturalearth) # worldwide map data
library(tidyverse)     # tidy data transformation
library(lubridate)     # date and time support
library(sf)            # simple features GIS
library(tricolore)     # ternary color coding
library(ggtern)        # ternary ggplots

# download geospatial data for European, Asian and African countries,
# project to crs 3035 and crop to Europe
eura_sf <-
  # download geospatial data for European, Asian and African countries
  ne_countries(continent = c('europe', 'asia', 'africa'),
               returnclass = 'sf', scale = 10) %>%
  # project to crs 3035
  st_transform(crs = 3035) %>%
  # merge into single polygon
  st_buffer(dist = 10) %>% # avoids self-intersection errors
  st_union(by_feature = FALSE) %>%
  st_crop(xmin = 25e5, xmax = 75e5, ymin = 13.5e5, ymax = 54.5e5)

save(eura_sf, file = 'data/eura_sf.Rdata')

# download geospatial data for NUTS-2 regions,
# project to crs 3035 and crop to Europe
euro_nuts2_sf <-
  get_eurostat_geospatial(
    output_class = 'sf',
    year = 2016,
    resolution = '01',
    nuts_level = 2
  ) %>%
  st_transform(
    crs = 3035
  ) %>%
  st_buffer(dist = 0) %>% # avoids self-intersection errors
  st_crop(
    xmin = 25e5, xmax = 75e5,
    ymin = 13.5e5, ymax = 54.5e5
  )

save(euro_nuts2_sf, file = 'data/euro_nuts2_sf.Rdata')

# download geospatial data for NUTS-3 regions,
# project to crs 3035 and crop to Europe
euro_nuts3_sf <-
  get_eurostat_geospatial(
    output_class = 'sf',
    year = 2013,
    resolution = '01',
    nuts_level = 3
  ) %>%
  st_transform(
    crs = 3035
  ) %>%
  st_buffer(dist = 0) %>% # avoids self-intersection errors
  st_crop(
    xmin = 25e5, xmax = 75e5,
    ymin = 13.5e5, ymax = 54.5e5
  )

save(euro_nuts3_sf, file = 'data/euro_nuts3_sf.Rdata')

# Median age --------------------------------------------------------------

# download the data on pop median age at NUTS-3 level
euro_median_age <-
  get_eurostat(
    'demo_r_pjanind3',
    stringsAsFactors = FALSE
  ) %>%
  # filter NUTS-3, 2015, total population
  filter(
    indic_de == 'MEDAGEPOP',
    str_length(geo) == 5,
    year(time) == 2015
  ) %>%
  # cleaning
  select(
    geo, values
  ) %>%
  drop_na()

save(euro_median_age, file = 'data/euro_median_age.Rdata')

# merge geodata and regional median age
euro_median_age_sf <-
  euro_nuts3_sf %>%
  left_join(
    euro_median_age,
    by = c('id' = 'geo')
  ) %>%
  rename(medage = values)

plot_median_age <-
  ggplot(euro_median_age_sf) +
  geom_sf(
    aes(fill = medage),
    color = NA,
    show.legend = TRUE
  ) +
  geom_sf(
    data = eura_sf,
    color = 'black',
    fill = NA,
    size = 0.2
  ) +
  scale_fill_gradient(
    low = 'black',
    high = 'grey95',
    na.value = 'white'
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.key.size = unit(0.5, 'in')
  ) +
  labs(fill = '')

ggsave(
  filename = 'plot_median_age.png',
  plot = plot_median_age,
  path = './fig/raw_svg/',
  width = 27*0.5,
  height = 23*0.5,
  units = 'in',
  dpi = 300
)

ggsave(
  filename = 'plot_median_age_no_lgnd.png',
  plot = plot_median_age + guides(fill = FALSE),
  path = './fig/raw_svg/',
  width = 27*0.5,
  height = 23*0.5,
  units = 'in',
  dpi = 300
)

# Deviation from median age -----------------------------------------------

# merge geodata and regional age structures and population counts
euro_median_age_sf <-
  euro_median_age_sf %>%
  mutate(
    diff_medage = medage - 42.4 # EU-28 median age 2015
  )

plot_diff_median_age <-
  ggplot(euro_median_age_sf) +
  geom_sf(
    aes(fill = diff_medage),
    color = NA,
    show.legend = TRUE
  ) +
  geom_sf(
    data = eura_sf,
    color = 'black',
    fill = NA,
    size = 0.2
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    low = '#053061',
    high = '#67001f',
    mid = '#f7f7f7',
    limits = c(-15, 15),
    oob = scales::squish
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.key.size = unit(0.5, 'in')
  ) +
  labs(fill = '')

ggsave(
  filename = 'plot_diff_median_age.png',
  plot = plot_diff_median_age,
  path = './fig/raw_svg/',
  width = 27*0.5,
  height = 23*0.5,
  units = 'in',
  dpi = 300
)

ggsave(
  filename = 'plot_diff_median_age_no_lgnd.png',
  plot = plot_diff_median_age + guides(fill = FALSE),
  path = './fig/raw_svg/',
  width = 27*0.5,
  height = 23*0.5,
  units = 'in',
  dpi = 300
)

# Regional age structures -------------------------------------------------

# download the data on pop counts by age at NUTS-3 level
euro_age <-
  get_eurostat(
    'demo_r_pjanaggr3',
    stringsAsFactors = FALSE
  ) %>%
  # filter NUTS-3, 2015, total population
  filter(
    sex == 'T',
    str_length(geo) == 5,
    year(time) == 2015,
    age %in% c('Y_LT15', 'Y15-64', 'Y_GE65', 'TOTAL')
  ) %>%
  # cleaning
  select(
    age, geo, values
  ) %>%
  spread(
    age, values
  ) %>%
  rename(
    total = TOTAL,
    age65plus = Y_GE65,
    age0to15 = Y_LT15,
    age15to65 = `Y15-64`
  ) %>%
  drop_na()

save(euro_age, file = 'data/euro_age.Rdata')

# average European age structure in 2015
euro_age_center <-
  with(
    euro_age,
    c(p_age0to15 = sum(age0to15)/sum(total),
      p_age15to65 = sum(age15to65)/sum(total),
      p_age65plus = sum(age65plus)/sum(total))
  )

# merge geodata and regional age structures and
# add population shares by age and
# differences from European average
euro_age_sf <-
  euro_nuts3_sf %>%
  left_join(
    euro_age,
    by = c('id' = 'geo')
  ) %>%
  mutate(
    p_age0to15 = age0to15/total,
    p_age15to65 = age15to65/total,
    p_age65plus = age65plus/total
  )

# generate centered ternary colors
tric_age <-
  Tricolore(
    euro_age_sf,
    p1 = 'p_age65plus', p2 = 'p_age15to65', p3 = 'p_age0to15',
    label_as = 'pct_diff', crop = TRUE,
    center = rev(euro_age_center), spread = 2.9, breaks = Inf,
    contrast = .5, lightness = 1, chroma = 1, hue = 2/12
  )

key_age <-
  tric_age$key +
  labs(
    caption = '',
    x = '65+', y = '15-65', z = '0-15'
  ) +
  theme(
    plot.background = element_rect(color = 'grey60')
  )
euro_age_sf$col <- tric_age$rgb

plot_age <-
  ggplot(euro_age_sf) +
  geom_sf(
    aes(fill = col),
    color = NA
  ) +
  geom_sf(
    data = eura_sf,
    color = 'black',
    fill = NA,
    size = 0.2
  ) +
  scale_fill_identity() +
  # annotation_custom(
  #   ggplotGrob(key_centered),
  #   xmin = 55e5, xmax = 75e5, ymin = 37e5, ymax = 54e5
  # ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void()

ggsave(
  filename = 'plot_age_no_lgnd.png',
  plot = plot_age,
  path = './fig/raw_svg/',
  width = 27*0.5,
  height = 23*0.5,
  units = 'in',
  dpi = 300
)

# Labor force composition -------------------------------------------------

euro_lf <-
  # download data on labor-force composition by NUTS-2 level for Europe
  get_eurostat("lfst_r_lfe2en2") %>%
  # recode time as year
  mutate(year = as.integer(lubridate::year(time))) %>%
  # subset to total age, year 2015 and NUTS-2 regions
  filter(
    age == 'Y_GE15',
    str_length(geo) == 4,
    year == 2015
  ) %>%
  # recode into three sectors
  mutate(
    sector = recode(as.character(nace_r2),
                    `A` = 'primary',
                    `B-E` = 'secondary',
                    `F` = 'secondary'),
    sector = ifelse(!sector %in% c('primary', 'secondary', 'TOTAL'),
                    'tertiary',
                    sector)
  ) %>%
  group_by(year, geo, sector) %>%
  summarise(N = sum(values, na.rm = TRUE)) %>%
  ungroup() %>%
  # calculate shares on total
  spread(sector, N) %>%
  mutate(
    p_primary = primary/TOTAL,
    p_secondary = secondary/TOTAL,
    p_tertiary = tertiary/TOTAL
  )

save(euro_lf, file = 'data/euro_lf.Rdata')

# average European laborforce structure in 2017
euro_lf_center <-
  with(
    euro_lf,
    c(p_primary = sum(primary, na.rm = TRUE)/sum(TOTAL),
      p_secondary = sum(secondary, na.rm = TRUE)/sum(TOTAL),
      p_tertiary = sum(tertiary, na.rm = TRUE)/sum(TOTAL))
  ) %>% prop.table()

# generate colors based on compositions in `euro_sectors`, default options
tric_lf <-
  Tricolore(
    euro_lf,
    'primary', 'secondary', 'tertiary',
    center = euro_lf_center, show_center = TRUE,
    hue = 0.35, crop = TRUE
  )

# add vector of colors with with map data
euro_lf$col <- tric_lf$rgb

# merge geodata and regional labor force structures and
euro_lf_sf <-
  euro_nuts2_sf %>%
  left_join(
    euro_lf,
    by = c('id' = 'geo')
  )

plot_lf <-
  ggplot(euro_lf_sf) +
  geom_sf(
    aes(fill = col),
    color = NA
  ) +
  geom_sf(
    data = eura_sf,
    color = 'black',
    fill = NA,
    size = 0.2
  ) +
  scale_fill_identity() +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void()

ggsave(
  filename = 'plot_lf_no_lgnd.png',
  plot = plot_lf,
  path = './fig/raw',
  width = 27*0.5,
  height = 23*0.5,
  units = 'in',
  dpi = 300
)

ggsave(
  filename = 'key_lf.svg',
  plot = tric_lf$key,
  path = './fig/raw',
  width = 3,
  height = 3,
  units = 'in'
)

# Education ---------------------------------------------------------------

euro_educ <-
  get_eurostat('edat_lfse_04') %>%
  mutate(year = lubridate::year(time)) %>%
  filter(year == 2015,
         str_length(geo) == 4,
         isced11 %in% c('ED0-2', 'ED3_4', 'ED5-8'),
         sex == 'T') %>%
  spread(isced11, values) %>%
  select(geo, ed0_2 = `ED0-2`, ed3_4 = `ED3_4`, ed5_8 = `ED5-8`)

save(euro_educ, file = 'data/euro_educ.Rdata')

tric_educ <-
  Tricolore(
    euro_educ,
    p1 = 'ed0_2', p2 = 'ed3_4', p3 = 'ed5_8',
    breaks = 4,
    h = 0.2, lightness = 0.9, chroma = 1,
    contrast = 0.7, show_center = FALSE
  )
euro_educ$col <- tric_educ$rgb

# merge geodata and regional educational attainment
euro_educ_sf <-
  euro_nuts2_sf %>%
  left_join(
    euro_educ,
    by = c('id' = 'geo')
  )

plot_educ <-
  ggplot(euro_educ_sf) +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = col),
    color = NA
  ) +
  scale_fill_identity() +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void()

ggsave(
  filename = 'plot_educ.svg',
  plot = plot_educ,
  path = './fig',
  width = 8.5,
  height = 7,
  units = 'in'
)

ggsave(
  filename = 'key_educ.pdf',
  plot = tric_educ$key,
  path = './fig',
  width = 8.5,
  height = 7,
  units = 'in'
)
