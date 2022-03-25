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

focus <- c("per_b_aa",
           "per_hisp_lat",
           "per_white")

dd <- make_mke_rc() %>%
  select(school_year,
         dpi_true_id,
         school_enrollment,
         starts_with("per_")) %>%
  pivot_longer(cols = -c(1:3), names_to = "group", values_to = "perc") %>%
  filter(!group %in% glue("per_{c('swd', 'ed', 'lep', 'open', 'choice')}")) %>%
  mutate(group = ifelse(group %in% focus, focus, "per_other")) %>%
  left_join(., d) %>%
  filter(!is.na(sbd)) %>%
  mutate(est = school_enrollment * perc) %>%
  group_by(school_year,
           sbd,
           group) %>%
  summarise(total = sum(est, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(school_year,
           sbd) %>%
  mutate(perc = total / sum(total)) %>%
  mutate(school_year = factor(school_year, levels = c("2015-16",
                                                     "2016-17",
                                                     "2017-18",
                                                     "2018-19",
                                                     "2019-20",
                                                     "2020-21"), ordered = TRUE)) %>%
  ungroup()

citywide <- make_mke_rc() %>%
  select(school_year,
         dpi_true_id,
         school_enrollment,
         starts_with("per_")) %>%
  pivot_longer(cols = -c(1:3), names_to = "group", values_to = "perc") %>%
  filter(!group %in% glue("per_{c('swd', 'ed', 'lep', 'open', 'choice')}")) %>%
  mutate(group = ifelse(group %in% focus, focus, "per_other")) %>%
  left_join(., d) %>%
  filter(!is.na(sbd)) %>%
  mutate(est = school_enrollment * perc) %>%
  group_by(school_year,
           group) %>%
  summarise(total = sum(est, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(school_year) %>%
  mutate(perc = total / sum(total)) %>%
  mutate(school_year = factor(school_year, levels = c("2015-16",
                                                      "2016-17",
                                                      "2017-18",
                                                      "2018-19",
                                                      "2019-20",
                                                      "2020-21"), ordered = TRUE)) %>%
  ungroup()

layout <- c(
  area(3,1,4,2),
  area(1,4),
  area(2,4), area(2,5),
  area(3,5), area(3,6),
  area(4,4), area(4,5),
  area(5,5)
)

bg <- "white"

plots <- map(1:8, function(x) {
  dd %>%
    filter(sbd == x & !is.na(perc)) %>%
    mutate(school_year = as.numeric(school_year)) %>%
    ggplot(aes(as.numeric(school_year), perc, fill = group)) +
    geom_area() +
    geom_text(x = 1.1, y = .97, label = glue("SBD {x}"), color = "white",
              hjust = 0, vjust = 1) +
    geom_rect(xmin = 4.75, xmax = 5.25,
              ymin = 0, ymax = 1, fill = "grey90") +
    geom_hline(yintercept = .5, linetype = 2, color = "white") +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(fill = bg, color = NA),
          panel.background = element_rect(fill = bg, color = NA),
          plot.title = element_text(hjust = .5),
          plot.margin = margin(1,1,1,1)) 
})

labs <- tibble(
  x = c(rep(6.1, 4),
        .9, .9),
  y = c(.85, .55, .4, .15,
        0, 1),
  l = c("Test",
        "Test",
        "Test",
        "Test",
        "0%",
        "100%"),
  h = c(rep(0, 4),
        1, 1),
  v = c(rep(.5, 4),
        0, 1)
)

city_plot <- citywide %>%
  filter(!is.na(perc)) %>%
  mutate(school_year = as.numeric(school_year)) %>%
  ggplot(aes(as.numeric(school_year), perc, fill = group)) +
  geom_area() +
  geom_text(x = 1.1, y = .97, label = "Citywide", color = "white",
            hjust = 0, vjust = 1) +
  geom_rect(xmin = 4.75, xmax = 5.25,
            ymin = 0, ymax = 1, fill = "grey90") +
  geom_hline(yintercept = .5, linetype = 2, color = "white") +
  geom_text(x = 5, y = .5, label = "NO DATA FOR THE 2019-20 SCHOOL YEAR",
            angle = 90, size = 2) +
  geom_text(data = labs, aes(x = x, y = y, label = l,
                             hjust = h, vjust = v),
            inherit.aes = FALSE, size = 3) +
  scale_y_continuous(limits = c(0, 1.25), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = bg, color = NA),
        panel.background = element_rect(fill = bg, color = NA),
        plot.title = element_text(hjust = .5),
        plot.margin = margin(1,1,1,20)) 

wrap_plots(
  city_plot,
  plots[[1]],
  plots[[2]], plots[[3]],
  plots[[4]], plots[[5]],
  plots[[7]], plots[[6]],
  plots[[8]],
  design = layout
  ) &
  plot_annotation(theme = theme(
                    plot.background = element_rect(fill = bg, color =  NA)
                  ))

ggsave("temp.png", width = 10, h = 7, bg = bg)
