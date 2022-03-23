library(tidyverse)
library(patchwork)
library(cityforwardcollective)
library(wisconsink12)
library(sf)
library(glue)

geo_schools <- read_csv("../strategic_regional_analysis/data/geocoded_mke_schools.csv") %>%
  select(-student_count) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

sbds <- st_read("../Shapefiles/Milwaukee/MPSSchoolBoardDistricts/MPS_School_Board_Districts.shp") %>%
  st_transform(., crs = st_crs(4326))

sbd_schools <- st_intersection(geo_schools, sbds)

d <- sbd_schools %>%
  as_tibble() %>%
  select(school_year,
         sbd = SCHOOL,
         dpi_true_id) %>%
  select(-school_year)

dd <- make_mke_rc() %>%
  left_join(., d) %>%
  group_by(school_year,
           sbd) %>%
  summarise(black = weighted.mean(per_b_aa, school_enrollment, na.rm = TRUE),
            lat = weighted.mean(per_hisp_lat, school_enrollment, na.rm = TRUE),
            white = weighted.mean(per_white, school_enrollment, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = c("black", "lat", "white"),
               names_to = "group", values_to = "perc") %>%
 mutate(perc = replace_na(perc, 0))

layout <- c(
  area(1,1),
  area(2,1), area(2,2),
  area(3,2), area(3,3),
  area(4,1), area(4,2),
  area(5,2)
)

plots <- map(1:8, function(x) {
  dd %>%
    filter(sbd == x) %>%
    ggplot(aes(school_year, perc, fill = group)) +
    geom_col() +
    geom_text(x = 3.5, y = 1.05, label = glue("SBD {x}")) +
    scale_y_continuous(limits = c(0, 1.1)) +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "grey80", color = NA),
          plot.title = element_text(hjust = .5),
          plot.margin = margin(rep(.5, 4))) 
})




wrap_plots(
  plots[[1]],
  plots[[2]], plots[[3]],
  plots[[4]], plots[[5]],
  plots[[7]], plots[[6]],
  plots[[8]],
  design = layout
  ) &
  plot_annotation(title = "Milwaukee School Board Districts",
                  theme = theme(
                    panel.background = element_rect(fill = "white", color =  NA)
                  ))

ggsave("temp.png", width = 6, h = 9)
