auswertung.Rmd
================
2023-12-28

# Libraries

``` r
# Für Korrelationsmatrizen
library(Hmisc)

# Für Übersichts-Plots
#library(GGally)

# Für schönere Latex-Tabellen
library(stargazer)
library(kableExtra)
library(vtable)

# Für heteroscedasticity-consistent tests
#library(sandwich)
#library(lmtest)

# Für schönere Plots
library(ggpubr)
library(ggrepel)
library(gridExtra)

# Für schnelleres Einlesen von Daten
library(vroom)

# Für allgemeine Datenmanipulation und Plotten
library(tidyverse)

# Für tidy Regressionsergebnisse
#library(broom)

# Fürs Arbeiten mit Geodaten
library(sf)
```

# Importieren der Daten

Die Daten wurden im Dokument ‘cleaning.Rmd’ aufbereitet und
abgespeichert.

``` r
data_rent <- vroom("./daten/data_rent.csv")
data_social <- vroom("./daten/data_social.csv")

inspire_grid_berlin <- st_read("./daten/inspire_grid_berlin.gpkg", quiet = TRUE)
bezirksgrenzen_berlin <- st_read("./daten/bezirksgrenzen_berlin/bezirksgrenzen.shp", quiet = TRUE) %>%
  st_transform("EPSG:3035")
mauer_berlin <- st_read("./daten/Berliner_Mauer.geojson", quiet = TRUE)

data_social_sf <- data_social %>%
  left_join(inspire_grid_berlin %>% select(r1_id, geom), by = "r1_id") %>%
  st_as_sf()

baugenehmigungen <- read_csv2("./daten/genehmigte-wohnungen-monat.csv") %>%
  janitor::clean_names() %>%
  mutate(datum = ym(monat), .keep = "unused") %>%
  rename(wert = originalwerte,
         wert_trend = 2)
```

# Summary Statistics

Überblick über die beiden Datensätze mit Anzahl an Beobachtungen,
Mittelwert, Median, Quartilen und mehr.

``` r
# Data Social
data_social %>% select(-jahr, -r1_id) %>%
  mutate(kaufkraft_pro_haushalt_1000 = kaufkraft_pro_haushalt / 1000,
         anzahl_haushalte_1000 = anzahl_haushalte / 1000) %>%
  select(-kaufkraft_pro_haushalt, -anzahl_haushalte) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  drop_na() %>%
  summarise(n = n(), 
            mean = mean(value),
            Std.Dev. = sd(value),
            min = min(value),
            Pctl.25 = quantile(value, probs = 0.25),
            Pctl.50 = quantile(value, probs = 0.5),
            Pctl.75 = quantile(value, probs = 0.75),
            max = max(value)) %>%
  kbl(
    format = "latex",
    digits = 2,
    booktabs = T,
    toprule = "\\hline \\hline",
    midrule = "\\hline",
    bottomrule = "\\hline \\hline",
    linesep = c("", "", "", "", "\\addlinespace"),
  ) %>%
  cat()
```

    ## 
    ## \begin{tabular}[t]{lrrrrrrrr}
    ## \hline \hline
    ## name & n & mean & Std.Dev. & min & Pctl.25 & Pctl.50 & Pctl.75 & max\\
    ## \hline
    ## anteil\_60\_plus & 11205 & 28.00 & 6.81 & 7.77 & 23.66 & 28.54 & 32.75 & 100.00\\
    ## anteil\_auslaender & 11205 & 10.09 & 10.18 & 0.00 & 3.31 & 6.10 & 13.38 & 74.83\\
    ## anteil\_efh & 11205 & 36.82 & 31.47 & 0.00 & 6.23 & 30.81 & 63.16 & 100.00\\
    ## anteil\_mfh & 11205 & 26.81 & 17.94 & 0.00 & 12.24 & 25.27 & 39.00 & 100.00\\
    ## anteil\_oberklassewagen & 9319 & 5.25 & 2.49 & 0.00 & 3.56 & 4.80 & 6.38 & 26.26\\
    ## \addlinespace
    ## anteil\_wohnblock & 11205 & 32.67 & 30.83 & 0.00 & 2.98 & 24.91 & 57.93 & 100.00\\
    ## anzahl\_haushalte\_1000 & 10487 & 2.24 & 2.57 & 0.00 & 0.41 & 1.30 & 3.16 & 14.69\\
    ## arbeitslosenquote & 11205 & 8.54 & 5.14 & 0.00 & 4.49 & 8.15 & 11.95 & 26.58\\
    ## kaufkraft\_pro\_haushalt\_1000 & 10452 & 40.10 & 8.69 & 18.74 & 33.64 & 38.95 & 45.70 & 79.85\\
    ## kreditrisiko & 11205 & 0.60 & 0.20 & 0.11 & 0.43 & 0.59 & 0.78 & 1.00\\
    ## \hline \hline
    ## \end{tabular}

``` r
# Data Rent
# Allgemein
data_rent %>% select(-jahr, -r1_id) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  drop_na() %>%
  summarise(n = n(), 
            mean = mean(value),
            Std.Dev. = sd(value),
            min = min(value),
            Pctl.25 = quantile(value, probs = 0.25),
            Pctl.50 = quantile(value, probs = 0.5),
            Pctl.75 = quantile(value, probs = 0.75),
            max = max(value)) %>%
  kbl(
    format = "latex",
    digits = 2,
    booktabs = T,
    toprule = "\\hline \\hline",
    midrule = "\\hline",
    bottomrule = "\\hline \\hline",
    linesep = "",
  ) %>%
  cat()
```

    ## 
    ## \begin{tabular}[t]{lrrrrrrrr}
    ## \hline \hline
    ## name & n & mean & Std.Dev. & min & Pctl.25 & Pctl.50 & Pctl.75 & max\\
    ## \hline
    ## mietekalt & 1917043 & 627.08 & 516.80 & 0.01 & 346.00 & 467.86 & 706.54 & 15000\\
    ## mietekalt\_m2 & 1917043 & 8.50 & 115.04 & 0.00 & 5.59 & 7.00 & 9.45 & 58000\\
    ## wohnflaeche & 1917043 & 75.81 & 35.59 & 0.01 & 54.99 & 68.89 & 88.00 & 1000\\
    ## \hline \hline
    ## \end{tabular}

``` r
# Nach Jahren
data_rent %>% select(-r1_id) %>%
  group_by(jahr) %>%
  summarise(n = n(),
            mietekalt_mean = mean(mietekalt),
            mietekalt_median = median(mietekalt),
            wohnflaeche_mean = mean(wohnflaeche),
            wohnflaeche_median = median(wohnflaeche),
            mietekalt_m2_mean = mean(mietekalt_m2),
            mietekalt_m2_median = median(mietekalt_m2)) %>%
  kbl(
    format = "latex",
    digits = 2,
    booktabs = T,
    toprule = "\\hline \\hline",
    midrule = "\\hline",
    bottomrule = "\\hline \\hline",
    linesep = c("", "", "", "", "\\addlinespace"),
  ) %>%
  cat()
```

    ## 
    ## \begin{tabular}[t]{rrrrrrrr}
    ## \hline \hline
    ## jahr & n & mietekalt\_mean & mietekalt\_median & wohnflaeche\_mean & wohnflaeche\_median & mietekalt\_m2\_mean & mietekalt\_m2\_median\\
    ## \hline
    ## 2007 & 216882 & 439.53 & 362.00 & 74.61 & 69.19 & 6.81 & 5.49\\
    ## 2008 & 226760 & 452.74 & 368.56 & 73.89 & 68.67 & 6.22 & 5.60\\
    ## 2009 & 211381 & 471.83 & 382.00 & 74.54 & 69.00 & 6.62 & 5.80\\
    ## 2010 & 182552 & 520.49 & 401.38 & 76.37 & 69.30 & 6.77 & 6.11\\
    ## 2011 & 150706 & 562.42 & 432.73 & 76.56 & 69.00 & 7.86 & 6.58\\
    ## \addlinespace
    ## 2012 & 139518 & 624.59 & 475.00 & 77.71 & 70.00 & 8.02 & 7.30\\
    ## 2013 & 135140 & 700.08 & 525.00 & 79.64 & 70.50 & 8.56 & 8.00\\
    ## 2014 & 136700 & 736.57 & 564.00 & 79.32 & 70.51 & 9.58 & 8.50\\
    ## 2015 & 110705 & 758.32 & 595.00 & 79.07 & 70.00 & 9.52 & 8.91\\
    ## 2016 & 90353 & 769.64 & 599.00 & 77.03 & 69.00 & 9.83 & 9.10\\
    ## \addlinespace
    ## 2017 & 80657 & 847.20 & 662.00 & 76.34 & 67.88 & 10.88 & 10.18\\
    ## 2018 & 68290 & 874.30 & 700.08 & 73.55 & 66.20 & 11.93 & 11.00\\
    ## 2019 & 68548 & 918.83 & 729.23 & 71.75 & 65.10 & 13.42 & 11.64\\
    ## 2020 & 49753 & 975.44 & 780.00 & 70.88 & 64.03 & 14.47 & 12.66\\
    ## 2021 & 49098 & 1001.15 & 775.96 & 68.47 & 61.57 & 14.76 & 13.33\\
    ## \hline \hline
    ## \end{tabular}

Korrelation zwischen Variablen

``` r
mcor_social <- data_social %>%
  select_if(is.numeric) %>%
  select(-jahr) %>%
  as.matrix() %>%
  Hmisc::rcorr() %>%
  .$r
mcor_social[upper.tri(mcor_social)] <- NA

mcor_social %>%
  data.frame() %>%
  round(2) %>%
  kbl(
    format = "latex",
    digits = 2,
    booktabs = T,
    toprule = "\\hline \\hline",
    midrule = "\\hline",
    bottomrule = "\\hline \\hline",
    linesep = "",
  ) %>%
  cat()
```

    ## 
    ## \begin{tabular}[t]{lrrrrrrrrrr}
    ## \hline \hline
    ##   & anzahl\_haushalte & arbeitslosenquote & anteil\_oberklassewagen & kaufkraft\_pro\_haushalt & anteil\_efh & anteil\_mfh & anteil\_wohnblock & anteil\_auslaender & kreditrisiko & anteil\_60\_plus\\
    ## \hline
    ## anzahl\_haushalte & 1.00 & NA & NA & NA & NA & NA & NA & NA & NA & NA\\
    ## arbeitslosenquote & 0.40 & 1.00 & NA & NA & NA & NA & NA & NA & NA & NA\\
    ## anteil\_oberklassewagen & -0.02 & -0.16 & 1.00 & NA & NA & NA & NA & NA & NA & NA\\
    ## kaufkraft\_pro\_haushalt & -0.33 & -0.71 & 0.19 & 1.00 & NA & NA & NA & NA & NA & NA\\
    ## anteil\_efh & -0.63 & -0.50 & -0.01 & 0.42 & 1.00 & NA & NA & NA & NA & NA\\
    ## anteil\_mfh & -0.20 & -0.08 & 0.03 & 0.07 & -0.31 & 1.00 & NA & NA & NA & NA\\
    ## anteil\_wohnblock & 0.74 & 0.56 & -0.04 & -0.44 & -0.81 & -0.22 & 1.00 & NA & NA & NA\\
    ## anteil\_auslaender & 0.40 & 0.12 & 0.06 & -0.10 & -0.37 & -0.10 & 0.43 & 1.00 & NA & NA\\
    ## kreditrisiko & 0.56 & 0.59 & -0.15 & -0.52 & -0.68 & -0.04 & 0.70 & 0.45 & 1.00 & NA\\
    ## anteil\_60\_plus & -0.44 & -0.48 & 0.18 & 0.39 & 0.30 & 0.28 & -0.46 & -0.23 & -0.46 & 1\\
    ## \hline \hline
    ## \end{tabular}

Die größte Korrelation liegt zwischen dem Anteil an Wohnblocks und der
Anzahl der Haushalte vor. Das ist soweit nicht überraschend. Als
nächstes folgt mit einem Korrelationskoeffizienten von 0.7 der
Zusammenhang zwischen dem Kreditrisiko und dem Anteil an Wohnblocks.

Problem mit Ausländeranteil in Data Social

``` r
auslaender_berlin_destatis <- read_csv2("/Users/sebastiangeis/Downloads/12411-0014-DLAND_$F_flat.csv") %>%
  janitor::clean_names() %>%
  select(zeit, x2_auspraegung_label, x3_auspraegung_label, x4_auspraegung_label, bevstd_bevoelkerungsstand_anzahl) %>%
  filter(x2_auspraegung_label == "Insgesamt",
         x3_auspraegung_label %in% c("Ausl\xe4nder", "Insgesamt"),
         x4_auspraegung_label == "Insgesamt") %>%
  select(-x2_auspraegung_label, -x4_auspraegung_label) %>%
  mutate(datum = dmy(zeit),
         jahr = year(datum)+1) %>%
  select(-zeit, -datum) %>%
  filter(jahr %in% (data_social %>% distinct(jahr) %>% pull(jahr))) %>%
  mutate(x3_auspraegung_label = case_when(x3_auspraegung_label == "Ausl\xe4nder" ~ "Auslaender",
                                          TRUE ~ x3_auspraegung_label)) %>%
  pivot_wider(names_from = x3_auspraegung_label, values_from = bevstd_bevoelkerungsstand_anzahl) %>%
  mutate(anteil_auslaender_destatis = (Auslaender / Insgesamt) * 100) %>%
  select(jahr, anteil_auslaender_destatis)
```

    ## ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.

    ## Rows: 19044 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr (18): Statistik_Label, Zeit_Code, Zeit_Label, 1_Merkmal_Code, 1_Merkmal_...
    ## dbl  (3): Statistik_Code, 1_Auspraegung_Code, BEVSTD__Bevoelkerungsstand__An...
    ## num  (1): Zeit
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data_social %>%
  select(arbeitslosenquote, anteil_auslaender, anzahl_haushalte, jahr) %>%
  drop_na() %>%
  group_by(jahr) %>%
  summarise(mean_anteil_auslaender = mean(anteil_auslaender),
            mean_anteil_auslaender_weighted = weighted.mean(anteil_auslaender, anzahl_haushalte)) %>%
  left_join(auslaender_berlin_destatis, by = "jahr") %>%
  kbl(
    format = "latex",
    digits = 2,
    booktabs = T,
    toprule = "\\hline \\hline",
    midrule = "\\hline",
    bottomrule = "\\hline \\hline",
    linesep = c("", "", "", "\\addlinespace"),
  ) %>%
  cat()
```

    ## 
    ## \begin{tabular}[t]{rrrr}
    ## \hline \hline
    ## jahr & mean\_anteil\_auslaender & mean\_anteil\_auslaender\_weighted & anteil\_auslaender\_destatis\\
    ## \hline
    ## 2005 & 6.53 & 10.14 & 13.42\\
    ## 2009 & 6.51 & 10.09 & 14.00\\
    ## 2010 & 6.51 & 10.06 & 13.75\\
    ## 2011 & 6.50 & 10.04 & 13.65\\
    ## \addlinespace
    ## 2012 & 6.50 & 10.02 & 11.89\\
    ## 2013 & 6.45 & 9.96 & 12.63\\
    ## 2014 & 6.46 & 9.96 & 13.42\\
    ## 2015 & 6.45 & 9.95 & 14.31\\
    ## \addlinespace
    ## 2016 & 6.46 & 9.96 & 15.50\\
    ## 2017 & 20.91 & 28.79 & 16.74\\
    ## 2018 & 21.22 & 29.28 & 17.65\\
    ## 2019 & 22.93 & 31.87 & 18.53\\
    ## \hline \hline
    ## \end{tabular}

Der Ausländeranteil springt plötzlich in 2017. Betrachtet man jedoch die
räumliche Verteilung über Berlin, so bleibt diese ähnlich. Es scheint
sich nur das Niveau insgesamt zu ändern. Dies könnte auf eine veränderte
Methodik hindeuten.

# Plotting

## Baugenehmigungen

``` r
baugenehmigungen %>%
  ggplot(aes(x = datum))+
  geom_line(aes(y = wert), color = "darkgrey")+
  geom_line(aes(y = wert_trend), color = "black", linewidth = 1)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  theme_bw()+
  labs(x = "Jahr",
       y = "Anzahl in 1000")
```

![](auswertung_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#ggsave("./plots/plot_baugenehmigungen.png")
```

## Berlin Karte

Der INSPIRE Grid mit den Berliner Bezirken

``` r
set.seed(1)

ggplot()+
  geom_sf(data = inspire_grid_berlin)+
  geom_sf(data = bezirksgrenzen_berlin, fill = "white", alpha = .8, color = "black")+
  geom_sf(data = mauer_berlin, color = "red", alpha = .8)+
  ggrepel::geom_label_repel(data = bezirksgrenzen_berlin %>% mutate(Gemeinde_n = str_replace(Gemeinde_n, "-", "-\n")), 
                           aes(label = Gemeinde_n, geometry = geometry),
                           stat = "sf_coordinates",
                           min.segment.length = 0.2, 
                           force_pull = 50,
                           box.padding = 0.1,
                           label.padding = .1,
                           size = 2,
                           label.r = 0,
                           lineheight = 1,
                           fill = "white")+
  coord_sf(crs = 3035)+
  theme_bw()+
  labs(x = element_blank(),
       y = element_blank())
```

![](auswertung_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("./plots/plot_berlin_karte.png")
```

    ## Saving 5 x 3 in image

Der hier gezeigt Teil des Grids hat 1016 Quadrate, also eine Fläche von
1016km². Berlin hat genaugenommen nur eine Fläche von 891,8 km², wir
betrachten also auch teilweise Flächen, die eigentlich nicht mehr zu
Berlin gehören, da wir alle Quadrate einbeziehen, die (auch nur
teilweise) mit den Stadtgrenzen überlappen. Die andere Möglichkeit wäre
gewesen, die Fläche zu unterschätzen und nur Quadrate zu betrachten, die
komplett innerhalb der Stadtgrenzen liegen.

## Rent Daten

Wie hat sich die Verteilung der Kaltmieten über die Jahre geändert?

``` r
plot1 <- data_rent %>%
  filter(jahr %in% 2010:2020) %>%
  mutate(mietekalt_m2 = mietekalt / wohnflaeche) %>%
  filter(mietekalt_m2 > 0, mietekalt_m2 < 100) %>%
  ggplot(aes(x = jahr, y = mietekalt_m2, group = jahr))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0, 30))+
  scale_x_continuous(breaks = 2010:2020, labels = 10:20)+
  scale_y_continuous(breaks = seq(from = 0, to = 30, by = 5))+
  theme_bw()+
  labs(x = "Jahr", y = "Kaltmiete (€/m²)")

plot2 <- data_rent %>%
  filter(jahr == 2010) %>%
  filter(mietekalt > 50, mietekalt < 10000) %>%
  ggplot(aes(x = mietekalt, y = after_stat(density))) +
  geom_histogram(binwidth = 20) +
  geom_vline(aes(xintercept = mean(mietekalt), color = "Mean"), linewidth = .6, linetype = "dashed") +
  geom_vline(aes(xintercept = median(mietekalt), color = "Median"), linewidth = .6, linetype = "dashed") +
  scale_color_manual("Statistics", values = c("Mean" = "red", "Median" = "blue"))+
  coord_cartesian(xlim = c(0, 2000)) +
  scale_y_continuous(labels = scales::label_number(accuracy = .01, scale = 100))+
  theme_bw() +
  theme(legend.position = c(0.75, 0.75), legend.title = element_blank(), legend.background = element_blank())+
  labs(x = "Kaltmiete (€/m²) in 2010", y = "Dichte (%)")

plot3 <- data_rent %>%
  filter(jahr == 2020) %>%
  filter(mietekalt > 50, mietekalt < 10000) %>%
  ggplot(aes(x = mietekalt, y = after_stat(density))) +
  geom_histogram(binwidth = 20) +
  geom_vline(aes(xintercept = mean(mietekalt), color = "Mean"), linewidth = .6, linetype = "dashed") +
  geom_vline(aes(xintercept = median(mietekalt), color = "Median"), linewidth = .6, linetype = "dashed") +
  scale_color_manual("Statistics", values = c("Mean" = "red", "Median" = "blue"))+
  coord_cartesian(xlim = c(0, 2000)) +
  scale_y_continuous(labels = scales::label_number(accuracy = .01, scale = 100))+
  theme_bw() +
  theme(legend.position = "none", legend.title = element_blank(), legend.background = element_blank())+
  labs(x = "Kaltmiete (€/m²) in 2020", y = "Dichte (%)")

ggarrange(plot1,
          ggarrange(plot2, plot3, nrow = 2),
          nrow = 1)
```

![](auswertung_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#ggsave("./plots/plot_miete.png", width = 5, height = 3)
```

Mittlere Kaltmiete auf Karte

``` r
data_rent_sf <- data_rent %>%
  left_join(inspire_grid_berlin %>% select(r1_id, geom), by = "r1_id") %>%
  st_as_sf()

data_rent_sf_plot <- data_rent_sf %>%
  filter(jahr == 2015) %>%
  group_by(r1_id) %>%
  summarise(mietekalt = mean(mietekalt),
            wohnflaeche = mean(wohnflaeche),
            mietekalt_m2 = mean(mietekalt_m2))

plot1 <- data_rent_sf_plot %>%
  filter(mietekalt > quantile(mietekalt, probs = 0.01), mietekalt < quantile(mietekalt, probs = 0.99)) %>%
  ggplot()+
  geom_sf(aes(fill = mietekalt))+
  scale_fill_viridis_c(trans = "log10", breaks = c(400, 600, 900, 1300))+
  theme_bw()+
  theme(legend.position = "top",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.subtitle = element_text(hjust = .5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))+
  labs(fill = element_blank(),
       subtitle = "Kaltmiete (€)")

plot2 <- data_rent_sf_plot %>%
  filter(wohnflaeche > quantile(wohnflaeche, probs = 0.01), wohnflaeche < quantile(wohnflaeche, probs = 0.99)) %>%
  ggplot()+
  geom_sf(aes(fill = wohnflaeche))+
  scale_fill_viridis_c(trans = "log10", breaks = c(50, 70, 100, 130))+
  theme_bw()+
  theme(legend.position = "top",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.subtitle = element_text(hjust = .5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))+
  labs(fill = element_blank(),
       subtitle = "Wohnfläche (m²)")

plot3 <- data_rent_sf_plot %>%
  filter(mietekalt_m2 > quantile(mietekalt_m2, probs = 0.01), mietekalt_m2 < quantile(mietekalt_m2, probs = 0.99)) %>%
  ggplot()+
  geom_sf(aes(fill = mietekalt_m2))+
  scale_fill_viridis_c(trans = "log10", breaks = c(6, 8, 10, 12, 14))+
  theme_bw()+
  theme(legend.position = "top",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.subtitle = element_text(hjust = .5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))+
  labs(fill = element_blank(),
       subtitle = "Kaltmiete (€/m²)")

ggarrange(plot1, plot2, plot3, nrow = 1)
```

![](auswertung_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#rm(data_rent_sf_plot)

#ggsave("./plots/plot_miete_karte_2015.png", width = 7.5, height = 2.5)
```

## Social Daten

Wie sehen ausgewählte Social-Variablen auf einer Karte aus?

``` r
# arbeitslosenquote, kaufkraft_pro_haushalt, anteil_60_plus, anteil_auslaender
  
plot1 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = arbeitslosenquote, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Arbeitslosenquote",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

plot2 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = kaufkraft_pro_haushalt, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c(trans = "log10", breaks = c(30000, 50000, 70000))+
  labs(subtitle = "Kaufkraft",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

plot3 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  filter(anteil_60_plus < 80) %>%
  ggplot(aes(fill = anteil_60_plus, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Anteil 60+",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

plot4 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = anteil_auslaender, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Ausländeranteil",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

ggarrange(plot1, plot2, plot3, plot4, nrow = 1)
```

![](auswertung_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#ggsave("./plots/plot_data_social_2015.png")
```

Die restlichen Variablen auf einer Karte für den Anhang

``` r
# anzahl_haushalte, anteil_oberklassewagen, anteil_efh, anteil_mfh, anteil_wohnblock, kreditrisiko

plot1 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = anzahl_haushalte, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Haushalte",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

plot2 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = anteil_oberklassewagen, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Anteil Oberklassewagen",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

plot3 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  filter(anteil_60_plus < 80) %>%
  ggplot(aes(fill = anteil_efh, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Anteil EFH",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

plot4 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = anteil_mfh, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Anteil MFH",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

plot5 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = anteil_wohnblock, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Anteil Wohnblock",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

plot6 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = kreditrisiko, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Kreditrisiko",
       fill = element_blank())+
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))

arranged <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3, nrow = 2)
```

![](auswertung_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#ggsave("./plots/plot_data_social_2015_rest.png", plot = arranged)
```

# Clustering

k-means clustering mit den Variablen

- arbeitslosenquote
- kaufkraft_pro_haushalt
- anteil_auslaender
- anteil_efh
- anteil_60_plus

Schauen wieviele Cluster wir benutzen sollten.

``` r
data_clustering_2015 <- data_social %>%
  filter(jahr == 2015) %>%
  drop_na() %>%
  select(anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  as.matrix() %>%
  scale()


set.seed(1)
max_cluster <- 8
wss_sum <- numeric(max_cluster)

for(n_cluster in 1:max_cluster){
  kmeans_result_2015 <- kmeans(data_clustering_2015, centers = n_cluster)
  wss_sum[n_cluster] <- kmeans_result_2015$tot.withinss
}

tibble(n_cluster = 1:max_cluster,
       totss = wss_sum) %>%
  ggplot(aes(x = n_cluster, y = totss))+
  geom_line()+
  scale_x_continuous(breaks = 1:8)+
  theme_bw()+
  labs(x = "Anzahl Cluster", y = "Total WSS")
```

![](auswertung_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#ggsave("./plots/plot_cluster_elbow.png")
```

Es werden 4 Cluster verwendet. *Hier Begründung einfügen*

Zuerst clustering nur für ein Jahr, dann verwenden wir die Cluster
Center nochmal für alle Jahre. Somit sollten die Cluster über die Zeit
stabil bleiben.

``` r
set.seed(1)
kmeans_result_2015 <- kmeans(data_clustering_2015, centers = 4)
```

Und jetzt Clustern für alle Jahre mit dem Output von
`kmeans_result_2015`

``` r
# Split data by year
data_clustering_year <- data_social %>%
  select(jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  group_by(jahr) %>%
  filter(jahr %in% c(2005, 2010, 2015, 2019)) %>%
  group_split(.keep = FALSE)

kmeans_result_2005 <- data_clustering_year[[1]] %>% as.matrix() %>% scale() %>%
  kmeans(., centers = kmeans_result_2015$centers)
#kmeans_result_2005

kmeans_result_2010 <- data_clustering_year[[2]] %>% as.matrix() %>% scale() %>%
  kmeans(., centers = kmeans_result_2015$centers)
#kmeans_result_2010

kmeans_result_2015 <- data_clustering_year[[3]] %>% as.matrix() %>% scale() %>%
  kmeans(., centers = kmeans_result_2015$centers)
#kmeans_result_2015

kmeans_result_2019 <- data_clustering_year[[4]] %>% as.matrix() %>% scale() %>%
  kmeans(., centers = kmeans_result_2015$centers)
#kmeans_result_2019
```

Und jetzt plotten wirs mal für das Jahr 2015

``` r
data_social_sf %>%
  filter(jahr == 2015) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2015$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  geom_sf(data = bezirksgrenzen_berlin, fill = NA, linewidth = .2, color = "black")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()+
  labs(fill = "Cluster")
```

![](auswertung_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#ggsave("./plots/plot_cluster_2015.png")
```

Cluster sind sehr ähnlich zu den Wahlergebnissen der Bundestagswahl in
Berlin 2016 ![Wahlergebnissen Berlin
2016](https://img.welt.de/img/politik/deutschland/mobile158256707/6751622257-ci23x11-w2000/DWO-IP-WahlergebnisBerlin-js-eps.jpg)

Wer steckt hinter den Clustern? Um diese Frage zu beantworten können wir
die Mittelwerte der jeweiligen Cluster betrachten.

``` r
kmeans_result_2015$centers %>% t() %>% round(2)
```

    ##                            1     2     3     4
    ## anzahl_haushalte       -0.63  0.29 -0.53  2.12
    ## arbeitslosenquote      -0.06  0.80 -1.02  0.79
    ## kaufkraft_pro_haushalt -0.28 -0.62  1.02 -0.67
    ## anteil_auslaender      -0.52  0.13 -0.42  2.10
    ## anteil_efh              1.03 -0.83  0.52 -1.11
    ## anteil_60_plus         -0.12 -0.21  0.72 -1.43

``` r
kmeans_result_2015$centers %>% t() %>% round(2) %>%
  kbl(
    format = "latex",
    digits = 2,
    booktabs = T,
    toprule = "\\hline \\hline",
    midrule = "\\hline",
    bottomrule = "\\hline \\hline",
    linesep = "",
  ) %>% cat()
```

    ## 
    ## \begin{tabular}[t]{lrrrr}
    ## \hline \hline
    ##   & 1 & 2 & 3 & 4\\
    ## \hline
    ## anzahl\_haushalte & -0.63 & 0.29 & -0.53 & 2.12\\
    ## arbeitslosenquote & -0.06 & 0.80 & -1.02 & 0.79\\
    ## kaufkraft\_pro\_haushalt & -0.28 & -0.62 & 1.02 & -0.67\\
    ## anteil\_auslaender & -0.52 & 0.13 & -0.42 & 2.10\\
    ## anteil\_efh & 1.03 & -0.83 & 0.52 & -1.11\\
    ## anteil\_60\_plus & -0.12 & -0.21 & 0.72 & -1.43\\
    ## \hline \hline
    ## \end{tabular}

Die Werte können nicht direkt interpretiert werden, da sie vorher durch
`scale()` mittelwertbereinigt und varianzbereinigt wurden. Was aber
interpretiert werden kann ist der Unterschied zwischen den Werten.

Betrachten wir beispielsweise Cluster 3:

- Die Anzahl der Haushalte ist am geringsten, das heißt pro
  Quadratkilometer wohnen hier weniger Menschen als in den anderen
  Teilen Berlins.
- Die Arbeitslosenquote ist ebenfalls am kleinsten.
- Die Kaufkraft pro Haushalt ist dagegen im Vergleich am größten, es
  handelt sich also um eine wohlhabende Gegend.
- Der Anteil an Ausländern ist vergleichsweise klein, jedoch größer als
  in Cluster 1.
- Die Anzahl der Einfamilienhäuser ist im Vergleich groß, aber kleiner
  als in Cluster 1.
- Der Anteil der Bevölkerung über 60 Jahre ist ebenso mit Abstand am
  größten.

Anhand dieser Werte bzw. dem Vergleich zwischen den Werten haben wir
eine zusammenfassende Beschreibung für die 4 Gebiete erstellt:

- Cluster 1: Stabile Wohngebiete
- Cluster 2: Sozial herausgeforderte Viertel
- Cluster 3: Wohlhabende Seniorengemeinschaften
- Cluster 4: Multikulturelle Ballungsräume

``` r
plot1 <- data_social_sf %>%
  filter(jahr == 2005) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2005$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  scale_fill_brewer(palette = "Paired")+
  labs(subtitle = "Cluster in 2005")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = .5))

plot2 <- data_social_sf %>%
  filter(jahr == 2010) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2010$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  scale_fill_brewer(palette = "Paired")+
  labs(subtitle = "Cluster in 2010")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = .5))

plot3 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2015$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  scale_fill_brewer(palette = "Paired")+
  labs(subtitle = "Cluster in 2015")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = .5))

plot4 <- data_social_sf %>%
  filter(jahr == 2019) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2019$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  scale_fill_brewer(palette = "Paired")+
  labs(subtitle = "Cluster in 2019")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = .5))

plot_arranged <- grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)
```

![](auswertung_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggsave("./plots/plot_cluster_jahr_vergleich.png", plot = plot_arranged)
```

    ## Saving 5 x 4 in image

# Regressionen

## Miete über die Jahre

Wie hat sich die Miete im Laufe der Jahre geändert?

Um diese Frage zu beantworten schätzen wir das folgende
Regressionsmodell:
$\ln(\text{Kaltmiete pro Quadratmeter}_i) = \beta_0 + \beta_1 \cdot \text{jahr}_i + \epsilon_i$.
Wir benutzen den Logarithmus, um um näher an eine Normalverteilung der
Residuen (siehe unten) heranzukommen. Nur das erlaubt die Anwendung
eines t-Tests zur Bestimmung der statistischen Signifikanz.

``` r
# Regression Miete

lm_miete_jahr <- data_rent %>%
  mutate(mietekalt_m2 = mietekalt / wohnflaeche) %>%
  filter(!is.na(mietekalt_m2)) %>%
  filter(mietekalt_m2 < quantile(mietekalt_m2, 0.99), mietekalt_m2 > quantile(mietekalt_m2, 0.01)) %>%
  mutate(jahr = jahr - 2007) %>%
  lm(log(mietekalt_m2) ~ jahr, data = .)

summary(lm_miete_jahr)
```

    ## 
    ## Call:
    ## lm(formula = log(mietekalt_m2) ~ jahr, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.23238 -0.18946 -0.02226  0.17420  1.42667 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 1.701e+00  3.306e-04    5146   <2e-16 ***
    ## jahr        5.998e-02  5.206e-05    1152   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2785 on 1878607 degrees of freedom
    ## Multiple R-squared:  0.4141, Adjusted R-squared:  0.4141 
    ## F-statistic: 1.328e+06 on 1 and 1878607 DF,  p-value: < 2.2e-16

Das Ergebnis der Regression spiegelt das wieder, was bereits im Boxplot
oben gezeigt wurde: Die Miete steigt im Durchschnitt pro Jahr. Konkret
wissen wir nun, dass die Miete pro Jahr im Schnitt um
$\exp(0.06015)-1=6.1\%$ steigt. Der Wert des $\beta_0$ (Intercept) lässt
sich interpretieren, wenn er zurücktransformiert wird. Das Jahr Null
wurde auf 2007 gesetzt, so ergibt sich die Konstante, also die
Mietbelastung im Jahr 2007, zu $\exp(1.701) = 5.48$. Interessant ist für
uns noch der Wert der t-Statistik beziehungsweise der p-Wert. Dieser ist
nahe 0 und zeigt somit starke statistische Signifikanz. Dies impliziert,
dass die Wahrscheinlichkeit, dass der ermittelte Zusammenhang lediglich
auf Zufall beruht, äußerst gering ist. Das ist durch die schiere Größe
des Datensatzes (knapp 1.9 Mio. Beobachtungen) jedoch nicht weiter
überraschend. Umso wichtiger ist es hingegen, auch auf die ökonomische
Relevanz des Zusammenhangs zu achten. Eine Steigerung von knapp 6% pro
Jahr ist jedoch auch ökonomisch relevant, liegt diese doch weit über dem
zu dieser Zeit vorherrschenden allgemeinen Inflationsniveau von unter
2%.

``` r
# Regression Mietbelastung

lm_mietbelastung_jahr <- data_social %>%
  filter(jahr == 2015) %>%
  filter(!is.na(arbeitslosenquote), !is.na(kaufkraft_pro_haushalt)) %>%
  mutate(ist_brennpunkt = ifelse(arbeitslosenquote > 10 & kaufkraft_pro_haushalt < quantile(kaufkraft_pro_haushalt, .2), TRUE, FALSE)) %>%
  select(r1_id, ist_brennpunkt, kaufkraft_pro_haushalt) %>%
  right_join(data_rent, by = "r1_id") %>%
  filter(!is.na(ist_brennpunkt)) %>%
  mutate(mietbelastung = mietekalt*12 / kaufkraft_pro_haushalt) %>%
  filter(!is.na(mietekalt_m2)) %>%
  filter(mietekalt_m2 < quantile(mietekalt_m2, 0.99), mietekalt_m2 > quantile(mietekalt_m2, 0.01)) %>%
  mutate(jahr = jahr - 2007) %>%
  lm(log(mietbelastung) ~ jahr, data = .)

summary(lm_mietbelastung_jahr)
```

    ## 
    ## Call:
    ## lm(formula = log(mietbelastung) ~ jahr, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8733 -0.3402 -0.0720  0.2783  3.4974 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.0561198  0.0006052   -3397   <2e-16 ***
    ## jahr         0.0578439  0.0000953     607   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5097 on 1878428 degrees of freedom
    ## Multiple R-squared:  0.164,  Adjusted R-squared:  0.164 
    ## F-statistic: 3.684e+05 on 1 and 1878428 DF,  p-value: < 2.2e-16

NEU INTERPRETIEREN

``` r
# Stargazer Tabelle

stargazer(lm_miete_jahr, lm_mietbelastung_jahr, 
          dep.var.labels = c("log Kaltmiete (€/m\\textsuperscript{2})", "log Mietbelastung (\\%)"), 
          covariate.labels = c("Konstante", "Jahr"),
          omit.stat = c("ser", "f"), single.row = FALSE, 
          intercept.bottom = FALSE,
          no.space = TRUE,
          dep.var.caption = "\\textit{Abhängige Variable}",
          notes.label = "\\textit{Anmerkung:}")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Thu, Feb 22, 2024 - 14:49:12
    ## \begin{table}[!htbp] \centering 
    ##   \caption{} 
    ##   \label{} 
    ## \begin{tabular}{@{\extracolsep{5pt}}lcc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{2}{c}{\textit{Abhängige Variable}} \\ 
    ## \cline{2-3} 
    ## \\[-1.8ex] & log Kaltmiete (€/m\textsuperscript{2}) & log Mietbelastung (\%) \\ 
    ## \\[-1.8ex] & (1) & (2)\\ 
    ## \hline \\[-1.8ex] 
    ##  Konstante & 1.701$^{***}$ & $-$2.056$^{***}$ \\ 
    ##   & (0.0003) & (0.001) \\ 
    ##   Jahr & 0.060$^{***}$ & 0.058$^{***}$ \\ 
    ##   & (0.0001) & (0.0001) \\ 
    ##  \hline \\[-1.8ex] 
    ## Observations & 1,878,609 & 1,878,430 \\ 
    ## R$^{2}$ & 0.414 & 0.164 \\ 
    ## Adjusted R$^{2}$ & 0.414 & 0.164 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Anmerkung:} & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

Plot

``` r
coef <- exp(predict(lm_mietbelastung_jahr, data.frame(jahr = 0))) * 100 / 
  exp(predict(lm_miete_jahr, data.frame(jahr = 0)))

expand.grid(jahr = (2007:2020) - 2007) %>%
  mutate(mietekalt_m2 = exp(predict(lm_miete_jahr, .)),
         mietbelastung = exp(predict(lm_mietbelastung_jahr, .)) * 100) %>%
  mutate(jahr = jahr + 2007,
         jahr = lubridate::ymd(paste(jahr, 01, 01, sep = "-"))) %>%
  #pivot_longer(cols = c(mietekalt_m2, mietbelastung))
  ggplot(aes(x = jahr))+
  geom_line(aes(y = mietekalt_m2, color = "red"))+
  geom_line(aes(y = mietbelastung/coef, color = "blue"))+
  scale_y_continuous(
    # Features of the first axis
    name = "Miete (€/m²)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coef, name="Mietbelastung (%)")
  )+
  scale_color_manual(labels = c("Mietbelastung", "Miete"), values = c("blue", "red"))+
  labs(x = "Jahr",
       color = element_blank())+
  theme_bw()+
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_blank())
```

![](auswertung_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_jahr_modell.png", width = 5, height = 2)
```

Zunächst wollen wir noch die Robustheit der Regression überprüfen um
Sicher zu gehen, dass der t-Test überhaupt ein sinnvolles Ergebnis
liefert. Dafür erstellen wir für beide Regressionsmodelle jeweils einen
Plot, bei dem die Residuen den gefitteten, also durch das Modell
vorhergesagten Werten gegenübergestellt werden.

``` r
facet_names <- list(
  'mietekalt_m2'="Miete",
  'mietbelastung'="Mietbelastung"
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}

# ------

set.seed(1)

tibble(model = "mietekalt_m2",
  fitted = fitted(lm_miete_jahr),
       resids = resid(lm_miete_jahr)) %>%
  rbind(
    tibble(model = "mietbelastung",
      fitted = fitted(lm_mietbelastung_jahr),
       resids = resid(lm_mietbelastung_jahr))
  ) %>%
  mutate(model = factor(model, levels = c("mietekalt_m2", "mietbelastung"))) %>%
  slice_sample(n = 10000, by = model) %>%
  ggplot(aes(x = fitted, y = resids))+
  geom_point(alpha = .05, size = .2)+
  facet_wrap(~model, scales = "free", labeller = facet_labeller)+
  scale_y_continuous(limits = function(x) c(quantile(x, 0.05, na.rm = TRUE), quantile(x, 0.95, na.rm = TRUE))) +
  theme_bw()+
  labs(x = "Fitted", y = "Residuals")
```

![](auswertung_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_miete_mietbelastung_jahr.png", width = 5, height = 2)
```

Wie erwartet, liegt der Mittelwert der Residuen jeweils bei Null mit
einer gewissen Streuung um diesen herum. Diese Streuung ist auf beiden
Seiten (ins positive als auch ins negative) ungefähr gleich. Außerdem
bleibt die Streuung über die Jahre ziemlich stabil. All dies spricht für
ein Halten der Normalverteilungsannahme.

## Miete und Mietbelastung in “Brennpunkten”

Hat sich die Miete in Brennpunkten über die Jahre stärker erhöht?

Hierzu müssen wir neben `data_rent` auch `data_social` verwenden. Wir
definieren einen “Brennpunkt” als eine Gegend, in der die
Arbeitslosenquote 2015 über 10% liegt und wo die Kaufkraft im unteren
20% Quantil liegt.

``` r
data_social_sf %>%
  filter(jahr == 2015) %>%
  filter(!is.na(arbeitslosenquote), !is.na(kaufkraft_pro_haushalt)) %>%
  mutate(ist_brennpunkt = ifelse(arbeitslosenquote > 10 & kaufkraft_pro_haushalt < quantile(kaufkraft_pro_haushalt, .2), TRUE, FALSE)) %>%
  mutate(ist_brennpunkt = if_else(ist_brennpunkt, "Ja", "Nein")) %>%
  ggplot()+
  geom_sf(aes(fill = ist_brennpunkt))+
  scale_fill_manual(values = c("red", "lightgrey"))+
  theme_bw()+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(fill = "Brennpunkt?")
```

![](auswertung_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
#ggsave("./plots/plot_brennpunkte.png", width = 5, height = 2)
```

Als Regressionsmodell verwenden wir
$\log(\text{Kaltmiete}_i) = \beta_0 + \beta_1 \cdot \text{Jahr}_i + \beta_2 \cdot \text{Brennpunkt}_i + \beta_3 \cdot \text{Brennpunkt}_i\cdot \text{Jahr}_i + \epsilon_i$
mit einem Interaktionsterm zwischen dem Jahr und der binären
Ist-Brennpunkt Variable.

**TODO:** Erklären, wieso wir $jahr = jahr - 2007$ rechnen (um den
Effekt von ist_brennpunkt und dem Intercept interpretieren zu können…)

``` r
# Regression Miete

lm_miete_brennpunkt <- data_social %>%
  filter(jahr == 2015) %>%
  filter(!is.na(arbeitslosenquote), !is.na(kaufkraft_pro_haushalt)) %>%
  mutate(ist_brennpunkt = ifelse(arbeitslosenquote > 10 & kaufkraft_pro_haushalt < quantile(kaufkraft_pro_haushalt, .2), TRUE, FALSE)) %>%
  select(r1_id, ist_brennpunkt) %>%
  right_join(data_rent, by = "r1_id") %>%
  filter(!is.na(ist_brennpunkt)) %>%
  mutate(mietekalt_m2 = mietekalt / wohnflaeche) %>%
  filter(!is.na(mietekalt_m2)) %>%
  filter(mietekalt_m2 < quantile(mietekalt_m2, 0.99), mietekalt_m2 > quantile(mietekalt_m2, 0.01)) %>%
  mutate(jahr = jahr - 2007) %>%
  lm(log(mietekalt_m2) ~ jahr + ist_brennpunkt + ist_brennpunkt:jahr, data = .)

summary(lm_miete_brennpunkt)
```

    ## 
    ## Call:
    ## lm(formula = log(mietekalt_m2) ~ jahr + ist_brennpunkt + ist_brennpunkt:jahr, 
    ##     data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.23787 -0.18572 -0.01918  0.17350  1.39547 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              1.733e+00  3.946e-04 4391.23   <2e-16 ***
    ## jahr                     5.736e-02  6.147e-05  933.13   <2e-16 ***
    ## ist_brennpunktTRUE      -1.012e-01  7.126e-04 -142.03   <2e-16 ***
    ## jahr:ist_brennpunktTRUE  8.178e-03  1.140e-04   71.75   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2767 on 1878426 degrees of freedom
    ## Multiple R-squared:  0.4215, Adjusted R-squared:  0.4215 
    ## F-statistic: 4.562e+05 on 3 and 1878426 DF,  p-value: < 2.2e-16

Interpretation: Für Nicht-Brennpunktgebiete liegt log Kaltmiete pro m2
liegt in Jahr 2007 bei $\exp(1.733) = 5.66$ €/m2 und steigt pro Jahr um
$\exp(5.736\cdot 10^{2})-1 = 5.9\%$. Handelt es sich um ein
Brennpunktgebiet, so liegt die Miete im Jahr 2007 um
$\exp(1.012\cdot 10^{-1}) = 0.90$ €/m2 niedriger, also bei
$5.66 - 0.90 = 4.76$ €/m2. Von diesem niedrigeren Niveau steigt die
Miete dann um $\exp(8.178 \cdot 10^{-3}) = 0.8\%$ pro Jahr stärker. Alle
diese Effekte sind stark statistisch signifikant mit p-Werten nahe Null.

``` r
# Regression Mietbelastung

lm_mietbelastung_brennpunkt <- data_social %>%
  filter(jahr == 2015) %>%
  filter(!is.na(arbeitslosenquote), !is.na(kaufkraft_pro_haushalt)) %>%
  mutate(ist_brennpunkt = ifelse(arbeitslosenquote > 10 & kaufkraft_pro_haushalt < quantile(kaufkraft_pro_haushalt, .2), TRUE, FALSE)) %>%
  select(r1_id, ist_brennpunkt, kaufkraft_pro_haushalt) %>%
  right_join(data_rent, by = "r1_id") %>%
  filter(!is.na(ist_brennpunkt)) %>%
  mutate(mietbelastung = mietekalt*12 / kaufkraft_pro_haushalt) %>%
  filter(!is.na(mietekalt_m2)) %>%
  filter(mietekalt_m2 < quantile(mietekalt_m2, 0.99), mietekalt_m2 > quantile(mietekalt_m2, 0.01)) %>%
  mutate(jahr = jahr - 2007) %>%
  lm(log(mietbelastung) ~ jahr + ist_brennpunkt + ist_brennpunkt:jahr, data = .)

summary(lm_mietbelastung_brennpunkt)
```

    ## 
    ## Call:
    ## lm(formula = log(mietbelastung) ~ jahr + ist_brennpunkt + ist_brennpunkt:jahr, 
    ##     data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9006 -0.3410 -0.0718  0.2786  3.4898 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)             -2.0485209  0.0007265 -2819.56   <2e-16 ***
    ## jahr                     0.0556112  0.0001132   491.35   <2e-16 ***
    ## ist_brennpunktTRUE      -0.0253709  0.0013122   -19.34   <2e-16 ***
    ## jahr:ist_brennpunktTRUE  0.0078611  0.0002099    37.45   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5095 on 1878426 degrees of freedom
    ## Multiple R-squared:  0.1647, Adjusted R-squared:  0.1647 
    ## F-statistic: 1.235e+05 on 3 and 1878426 DF,  p-value: < 2.2e-16

Interpretation: Für Nicht-Brennpunktgebiete liegt die Mietbelastung im
Jahr 2007 bei $14.5\%$ und steigt von dort pro Jahr um $1.2\text{pp}$.
Handelt es sich um ein Brennpunktgebiet, so liegt die Mietbelastung 2007
um $1.9\%$ niedriger bei $14.5\% - 1.9\% = 12.6\%$. Von dort steigt
diese jedoch um $0.2\%$ stärker an, also um insgesamt um knapp
$1.5\text{pp}$. Wieder sind alle Effekte statistisch signifikant.

``` r
# Stargazer Tabelle

stargazer(lm_miete_brennpunkt, lm_mietbelastung_brennpunkt, 
          dep.var.labels = c("log Kaltmiete (€/m\\textsuperscript{2})", "log Mietbelastung (\\%)"), 
          covariate.labels = c("Konstante", "Jahr", "Brennpunkt", "Jahr*Brennpunkt"),
          omit.stat = c("ser", "f"), single.row = FALSE, 
          intercept.bottom = FALSE,
          no.space = TRUE,
          dep.var.caption = "\\textit{Abhängige Variable}",
          notes.label = "\\textit{Anmerkung:}")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Thu, Feb 22, 2024 - 14:49:17
    ## \begin{table}[!htbp] \centering 
    ##   \caption{} 
    ##   \label{} 
    ## \begin{tabular}{@{\extracolsep{5pt}}lcc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{2}{c}{\textit{Abhängige Variable}} \\ 
    ## \cline{2-3} 
    ## \\[-1.8ex] & log Kaltmiete (€/m\textsuperscript{2}) & log Mietbelastung (\%) \\ 
    ## \\[-1.8ex] & (1) & (2)\\ 
    ## \hline \\[-1.8ex] 
    ##  Konstante & 1.733$^{***}$ & $-$2.049$^{***}$ \\ 
    ##   & (0.0004) & (0.001) \\ 
    ##   Jahr & 0.057$^{***}$ & 0.056$^{***}$ \\ 
    ##   & (0.0001) & (0.0001) \\ 
    ##   Brennpunkt & $-$0.101$^{***}$ & $-$0.025$^{***}$ \\ 
    ##   & (0.001) & (0.001) \\ 
    ##   Jahr*Brennpunkt & 0.008$^{***}$ & 0.008$^{***}$ \\ 
    ##   & (0.0001) & (0.0002) \\ 
    ##  \hline \\[-1.8ex] 
    ## Observations & 1,878,430 & 1,878,430 \\ 
    ## R$^{2}$ & 0.421 & 0.165 \\ 
    ## Adjusted R$^{2}$ & 0.421 & 0.165 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Anmerkung:} & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

Plot des Regressionsmodells

``` r
facet_names <- list(
  'mietekalt_m2'="Miete (€/m²)",
  'mietbelastung'="Mietbelastung (%)"
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}

expand.grid(jahr = (2007:2020) - 2007,
            ist_brennpunkt = c(TRUE, FALSE)) %>%
  mutate(mietekalt_m2 = exp(predict(lm_miete_brennpunkt, .)),
         mietbelastung = exp(predict(lm_mietbelastung_brennpunkt, .)) * 100) %>%
  mutate(jahr = jahr + 2007,
         jahr = lubridate::ymd(paste(jahr, 01, 01, sep = "-"))) %>%
  pivot_longer(cols = c(mietekalt_m2, mietbelastung)) %>%
  mutate(name = factor(name, levels = c("mietekalt_m2", "mietbelastung"))) %>%
  ggplot(aes(x = jahr, y = value, color = ist_brennpunkt, group = ist_brennpunkt))+
  geom_line()+
  facet_wrap(~name, scales = "free", labeller = facet_labeller)+
  scale_x_date(date_breaks = "2 years", date_labels = "%y")+
  scale_color_manual(labels = c("nicht-Brennpunkt", "Brennpunkt"), values = c("blue", "red")) +
  theme_bw()+
  theme(legend.position = "top")+
  labs(x = "Jahr", y = element_blank(), color = element_blank())
```

![](auswertung_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_brennpunkte_modell.png", width = 5, height = 2.5)
```

Wir sollten noch die Güte der Regression überprüfen, indem wir die
Residuen betrachten.

``` r
facet_names <- list(
  'mietekalt_m2'="Miete",
  'mietbelastung'="Mietbelastung"
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}

# ------

set.seed(1)

tibble(model = "mietekalt_m2",
  fitted = fitted(lm_miete_brennpunkt),
       resids = resid(lm_miete_brennpunkt)) %>%
  rbind(
    tibble(model = "mietbelastung",
      fitted = fitted(lm_mietbelastung_brennpunkt),
       resids = resid(lm_mietbelastung_brennpunkt))
  ) %>%
  mutate(model = factor(model, levels = c("mietekalt_m2", "mietbelastung"))) %>%
  slice_sample(n = 10000, by = model) %>%
  ggplot(aes(x = fitted, y = resids))+
  geom_point(alpha = .05, size = .2)+
  facet_wrap(~model, scales = "free", labeller = facet_labeller)+
  scale_y_continuous(limits = function(x) c(quantile(x, 0.05, na.rm = TRUE), quantile(x, 0.95, na.rm = TRUE))) +
  theme_bw()+
  labs(x = "Fitted", y = "Residuals")
```

![](auswertung_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_brennpunkte.png", width = 5, height = 2)
```

## Miete in Clustern

Hat sich die Miete in in den verschiedenen Clustern gleichmäßig erhöht?

Jetzt Regression mit den verschiedenen Clustern

``` r
# Regression Miete

r1_id_cluster <- data_social %>%
  filter(jahr == 2015) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  select(r1_id) %>%
  cbind(cluster = kmeans_result_2015$cluster) %>%
  mutate(cluster = as_factor(cluster))

lm_miete_cluster <- data_social %>%
  left_join(r1_id_cluster, by = "r1_id") %>%
  filter(!is.na(cluster)) %>%
  right_join(data_rent, by = c("r1_id", "jahr")) %>%
  filter(mietekalt_m2 > quantile(mietekalt_m2, probs = .01), mietekalt_m2 < quantile(mietekalt_m2, probs = .99)) %>%
  mutate(jahr = jahr - 2007) %>%
  lm(log(mietekalt_m2) ~ jahr + cluster + cluster:jahr, data = .)

summary(lm_miete_cluster)
```

    ## 
    ## Call:
    ## lm(formula = log(mietekalt_m2) ~ jahr + cluster + cluster:jahr, 
    ##     data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.16214 -0.17849 -0.01785  0.15642  1.38710 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1.6375293  0.0029704  551.28   <2e-16 ***
    ## jahr           0.0565914  0.0004322  130.94   <2e-16 ***
    ## cluster2      -0.0316952  0.0030516  -10.39   <2e-16 ***
    ## cluster3       0.2151485  0.0035126   61.25   <2e-16 ***
    ## cluster4       0.0583646  0.0030776   18.96   <2e-16 ***
    ## jahr:cluster2  0.0054266  0.0004446   12.21   <2e-16 ***
    ## jahr:cluster3 -0.0105600  0.0005172  -20.42   <2e-16 ***
    ## jahr:cluster4  0.0206779  0.0004496   45.99   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2654 on 1357285 degrees of freedom
    ##   (521316 observations deleted due to missingness)
    ## Multiple R-squared:  0.4033, Adjusted R-squared:  0.4033 
    ## F-statistic: 1.311e+05 on 7 and 1357285 DF,  p-value: < 2.2e-16

Die Interpretation geschieht bei diesem Regressionsmodell ähnlich wie
bereits bei dem Modell mit den Brennpunktgebieten, nur dass es nun statt
einer binären Variable eine kategorische Variable Cluster mit vier
Leveln gibt. Betrachten wir den Regressions-Output, fällt auf, dass dort
Cluster1 nicht auftaucht. Das ist also die Baseline. Im Jahr 2007
startet Cluster1 also mit einer Kaltmiete von $\exp(1.63) = 5.14$ €/m
und steigt pro Jahr um $\exp(0.056)-1=5.82\%$. Cluster2 hingegen startet
mit einer Kaltmiete um $\exp(0.031) = 0.97$ €/m niedriger als Cluster1.
Die Entwicklung in Cluster2 ist hingegen um $\exp(0.0054266)-1 = 0.54\%$
pro Jahr höher.

``` r
# Regression Mietbelastung

lm_mietbelastung_cluster <- data_social %>%
  left_join(r1_id_cluster, by = "r1_id") %>%
  filter(!is.na(cluster)) %>%
  right_join(data_rent, by = c("r1_id", "jahr")) %>%
  filter(mietekalt_m2 > quantile(mietekalt_m2, probs = .01), mietekalt_m2 < quantile(mietekalt_m2, probs = .99)) %>%
  mutate(mietbelastung = mietekalt*12 / kaufkraft_pro_haushalt) %>%
  mutate(jahr = jahr - 2007) %>%
  lm(log(mietbelastung) ~ jahr + cluster + cluster:jahr, data = .)

summary(lm_mietbelastung_cluster)
```

    ## 
    ## Call:
    ## lm(formula = log(mietbelastung) ~ jahr + cluster + cluster:jahr, 
    ##     data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7834 -0.3392 -0.0676  0.2808  3.4416 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)   -1.7846038  0.0056655 -314.996  < 2e-16 ***
    ## jahr           0.0094997  0.0008243   11.524  < 2e-16 ***
    ## cluster2      -0.1213313  0.0058204  -20.846  < 2e-16 ***
    ## cluster3      -0.1813756  0.0066997  -27.072  < 2e-16 ***
    ## cluster4      -0.0248711  0.0058700   -4.237 2.27e-05 ***
    ## jahr:cluster2  0.0183340  0.0008480   21.621  < 2e-16 ***
    ## jahr:cluster3  0.0313419  0.0009865   31.770  < 2e-16 ***
    ## jahr:cluster4  0.0409786  0.0008576   47.784  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5063 on 1357285 degrees of freedom
    ##   (521316 observations deleted due to missingness)
    ## Multiple R-squared:  0.08901,    Adjusted R-squared:  0.08901 
    ## F-statistic: 1.895e+04 on 7 and 1357285 DF,  p-value: < 2.2e-16

``` r
# Stargazer Tabelle

stargazer(lm_miete_cluster, lm_mietbelastung_cluster,
          dep.var.labels = c("log Kaltmiete (€/m\\textsuperscript{2})", "log Mietbelastung (\\%)"), 
          covariate.labels = c("Konstante", "Jahr", "Cluster2", "Cluster3", "Cluster4", "Jahr*Cluster2", "Jahr*Cluster3", "Jahr*Cluster4"),
          omit.stat = c("ser", "f"), single.row = FALSE, 
          intercept.bottom = FALSE,
          no.space = TRUE,
          dep.var.caption = "\\textit{Abhängige Variable}",
          notes.label = "\\textit{Anmerkung:}")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Thu, Feb 22, 2024 - 14:49:22
    ## \begin{table}[!htbp] \centering 
    ##   \caption{} 
    ##   \label{} 
    ## \begin{tabular}{@{\extracolsep{5pt}}lcc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{2}{c}{\textit{Abhängige Variable}} \\ 
    ## \cline{2-3} 
    ## \\[-1.8ex] & log Kaltmiete (€/m\textsuperscript{2}) & log Mietbelastung (\%) \\ 
    ## \\[-1.8ex] & (1) & (2)\\ 
    ## \hline \\[-1.8ex] 
    ##  Konstante & 1.638$^{***}$ & $-$1.785$^{***}$ \\ 
    ##   & (0.003) & (0.006) \\ 
    ##   Jahr & 0.057$^{***}$ & 0.009$^{***}$ \\ 
    ##   & (0.0004) & (0.001) \\ 
    ##   Cluster2 & $-$0.032$^{***}$ & $-$0.121$^{***}$ \\ 
    ##   & (0.003) & (0.006) \\ 
    ##   Cluster3 & 0.215$^{***}$ & $-$0.181$^{***}$ \\ 
    ##   & (0.004) & (0.007) \\ 
    ##   Cluster4 & 0.058$^{***}$ & $-$0.025$^{***}$ \\ 
    ##   & (0.003) & (0.006) \\ 
    ##   Jahr*Cluster2 & 0.005$^{***}$ & 0.018$^{***}$ \\ 
    ##   & (0.0004) & (0.001) \\ 
    ##   Jahr*Cluster3 & $-$0.011$^{***}$ & 0.031$^{***}$ \\ 
    ##   & (0.001) & (0.001) \\ 
    ##   Jahr*Cluster4 & 0.021$^{***}$ & 0.041$^{***}$ \\ 
    ##   & (0.0004) & (0.001) \\ 
    ##  \hline \\[-1.8ex] 
    ## Observations & 1,357,293 & 1,357,293 \\ 
    ## R$^{2}$ & 0.403 & 0.089 \\ 
    ## Adjusted R$^{2}$ & 0.403 & 0.089 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Anmerkung:} & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

Plot des Regressionsmodells

``` r
facet_names <- list(
  'mietekalt_m2'="Miete (€/m²)",
  'mietbelastung'="Mietbelastung (%)"
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}


expand.grid(jahr = (2007:2020) - 2007,
            cluster = factor(1:4)) %>%
  mutate(mietekalt_m2 = exp(predict(lm_miete_cluster, .)),
         mietbelastung = exp(predict(lm_mietbelastung_cluster, .)) * 100) %>%
  pivot_longer(cols = c(mietekalt_m2, mietbelastung)) %>%
  mutate(name = factor(name, levels = c("mietekalt_m2", "mietbelastung")),
         jahr = jahr + 2007,
         jahr = lubridate::ymd(paste(jahr, 01, 01, sep = "-"))) %>%
  ggplot(aes(x = jahr, y = value, color = cluster))+
  geom_line()+
  facet_wrap(~name, scales = "free", labeller = facet_labeller)+
  scale_x_date(date_breaks = "2 years", date_labels = "%y")+
  #scale_color_brewer(palette = "Paired")+
  #scale_color_manual(values = c("blue", "red", "orange", "purple", "cyan", "magenta")) +
  theme_bw()+
  theme(legend.position = "top")+
  labs(x = "Jahr", y = element_blank(), color = "Cluster")
```

![](auswertung_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_cluster_modell.png", width = 5, height = 2.5)
```

**ACHTUNG:** Wir müssen je das oberste und unterste 1% der Beobachtungen
rauswerfen, da diese zu einer Verletzung der Normalverteilungsannahme
führen. Das führt zu leicht unterschiedlichen Werten für die Betas.

Insgesamt zeigt sich ein R^2 von 0.38. Das ist, gemessen an unserem
“kleinen” Modell, schon beachtlich.

Wir sollten noch die Güte der Regression überprüfen, indem wir die
Residuen betrachten.

``` r
facet_names <- list(
  'mietekalt_m2'="Miete",
  'mietbelastung'="Mietbelastung"
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}

# ------

set.seed(1)

tibble(model = "mietekalt_m2",
  fitted = fitted(lm_miete_cluster),
       resids = resid(lm_miete_cluster)) %>%
  rbind(
    tibble(model = "mietbelastung",
      fitted = fitted(lm_mietbelastung_cluster),
       resids = resid(lm_mietbelastung_cluster))
  ) %>%
  mutate(model = factor(model, levels = c("mietekalt_m2", "mietbelastung"))) %>%
  slice_sample(n = 10000, by = model) %>%
  ggplot(aes(x = fitted, y = resids))+
  geom_point(alpha = .05, size = .2)+
  facet_wrap(~model, scales = "free", labeller = facet_labeller)+
  scale_y_continuous(limits = function(x) c(quantile(x, 0.05, na.rm = TRUE), quantile(x, 0.95, na.rm = TRUE))) +
  theme_bw()+
  labs(x = "Fitted", y = "Residuals")
```

![](auswertung_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_cluster.png", width = 5, height = 2)
```

Jetzt (nach der Ausreißerbereinigung) ist die Normalverteilungsannahme
ziemlich gut erfüllt und wir können den Ergebnissen der Tests (besser)
vertrauen.

## LQ Quotienten

``` r
data_social_sf %>%
  select(r1_id, jahr, arbeitslosenquote, anteil_auslaender, anzahl_haushalte, kaufkraft_pro_haushalt) %>%
  group_by(jahr) %>%
  filter(!is.na(anzahl_haushalte)) %>%
  mutate(LQ_arbeitslose = arbeitslosenquote / weighted.mean(arbeitslosenquote, anzahl_haushalte, na.rm = TRUE),
         LQ_auslaender = anteil_auslaender  / weighted.mean(anteil_auslaender, anzahl_haushalte, na.rm = TRUE),
         .keep = c("unused")) %>%
  ungroup() %>%
  filter(jahr == 2015) %>%
  ggplot()+
  geom_sf(aes(fill = LQ_arbeitslose))
```

![](auswertung_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot()+
  geom_sf(aes(fill = arbeitslosenquote))
```

![](auswertung_files/figure-gfm/unnamed-chunk-34-2.png)<!-- -->

``` r
data_LQ <- data_social %>%
  select(r1_id, jahr, arbeitslosenquote, anteil_auslaender, anzahl_haushalte, kaufkraft_pro_haushalt) %>%
  group_by(jahr) %>%
  filter(!is.na(anzahl_haushalte)) %>%
  mutate(LQ_arbeitslose = arbeitslosenquote / weighted.mean(arbeitslosenquote, anzahl_haushalte, na.rm = TRUE),
         LQ_auslaender = anteil_auslaender  / weighted.mean(anteil_auslaender, anzahl_haushalte, na.rm = TRUE),
         .keep = c("unused")) %>%
  ungroup()

lm_mietekalt_LQ <- data_rent %>%
  left_join(data_LQ, by = c("jahr", "r1_id")) %>%
  filter(mietekalt_m2 > quantile(mietekalt_m2, probs = .01), 
         mietekalt_m2 < quantile(mietekalt_m2, probs = .99)) %>%
  mutate(jahr = jahr - 2007) %>%
  lm(log(mietekalt_m2) ~ jahr + LQ_arbeitslose + LQ_auslaender + LQ_arbeitslose:jahr + LQ_auslaender:jahr, data = .)

summary(lm_mietekalt_LQ)
```

    ## 
    ## Call:
    ## lm(formula = log(mietekalt_m2) ~ jahr + LQ_arbeitslose + LQ_auslaender + 
    ##     LQ_arbeitslose:jahr + LQ_auslaender:jahr, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.26013 -0.17991 -0.02092  0.16492  1.37576 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.0319257  0.0015652 1298.22   <2e-16 ***
    ## jahr                 0.0408227  0.0002431  167.92   <2e-16 ***
    ## LQ_arbeitslose      -0.3323799  0.0014136 -235.14   <2e-16 ***
    ## LQ_auslaender       -0.0161301  0.0007254  -22.24   <2e-16 ***
    ## jahr:LQ_arbeitslose  0.0081157  0.0002137   37.98   <2e-16 ***
    ## jahr:LQ_auslaender   0.0160260  0.0001225  130.80   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2599 on 1357292 degrees of freedom
    ##   (521311 observations deleted due to missingness)
    ## Multiple R-squared:  0.4278, Adjusted R-squared:  0.4278 
    ## F-statistic: 2.03e+05 on 5 and 1357292 DF,  p-value: < 2.2e-16

**Interpretation:** Die Konstante ist in diesem Modell schwierig zu
interpretieren. Sie würde die log Kaltmiete im Jahr 2007 in einem Gebiet
mit keinen Arbeitslosen oder Ausländern widerspiegeln. Die Variable
`jahr` zeigt wieder die mittlere Mietsteigerung pro Jahr, hier also
$\exp(0.066363)-1 = 6.86\%$. Der Koeffizient bei `LQ_arbeitslose` zeigt
durch das Vorzeichen, dass die Miete sinkt, je mehr Arbeitslosigkeit in
einem Gebiet konzentriert ist. Der Wert lässt sich wieder schlechter
interpretieren, er würde sagen, dass wenn der LQ Koeffizient um 1
steigt, die Miete um $\exp(-0.279016)-1 = -24.34\%$ steigt (bzw. sinkt).

Interessanterweise ist die Korrelation zwischen LQ_arbeitslose und
LQ_auslaender um einiges größer als zwischen den “Rohwerten”
arbeitslosenquote und anteil_auslaender.

``` r
data_LQ %>%
  select(LQ_arbeitslose, LQ_auslaender) %>%
  drop_na() %>%
  summarise(cor = cor(LQ_arbeitslose, LQ_auslaender))
```

    ## # A tibble: 1 × 1
    ##     cor
    ##   <dbl>
    ## 1 0.403

``` r
data_social %>%
  select(arbeitslosenquote, anteil_auslaender) %>%
  drop_na() %>%
  summarise(cor = cor(arbeitslosenquote, anteil_auslaender))
```

    ## # A tibble: 1 × 1
    ##     cor
    ##   <dbl>
    ## 1 0.115

``` r
lm_mietbelastung_LQ <- data_rent %>%
  left_join(data_LQ, by = c("jahr", "r1_id")) %>%
  filter(mietekalt_m2 > quantile(mietekalt_m2, probs = .01), mietekalt_m2 < quantile(mietekalt_m2, probs = .99)) %>%
  mutate(mietbelastung = mietekalt*12 / kaufkraft_pro_haushalt) %>%
  mutate(jahr = jahr - 2007) %>%
  lm(log(mietbelastung) ~ jahr + LQ_arbeitslose + LQ_auslaender + LQ_arbeitslose:jahr + LQ_auslaender:jahr, data = .)

summary(lm_mietbelastung_LQ)
```

    ## 
    ## Call:
    ## lm(formula = log(mietbelastung) ~ jahr + LQ_arbeitslose + LQ_auslaender + 
    ##     LQ_arbeitslose:jahr + LQ_auslaender:jahr, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8263 -0.3366 -0.0608  0.2853  3.3360 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -1.7433372  0.0030545 -570.74   <2e-16 ***
    ## jahr                 0.0521088  0.0004744  109.83   <2e-16 ***
    ## LQ_arbeitslose      -0.0749211  0.0027586  -27.16   <2e-16 ***
    ## LQ_auslaender       -0.0435210  0.0014157  -30.74   <2e-16 ***
    ## jahr:LQ_arbeitslose -0.0352928  0.0004170  -84.63   <2e-16 ***
    ## jahr:LQ_auslaender   0.0211725  0.0002391   88.55   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5073 on 1357292 degrees of freedom
    ##   (521311 observations deleted due to missingness)
    ## Multiple R-squared:  0.08543,    Adjusted R-squared:  0.08543 
    ## F-statistic: 2.536e+04 on 5 and 1357292 DF,  p-value: < 2.2e-16

``` r
# Stargazer Tabelle

stargazer(lm_mietekalt_LQ, lm_mietbelastung_LQ,
          dep.var.labels = c("log Kaltmiete (€/m\\textsuperscript{2})", "log Mietbelastung (\\%)"), 
          covariate.labels = c("Konstante", "Jahr", "LQ Arbeitslose", "LQ Ausländer", "LQ Arbeitslose*Jahr", "LQ Ausländer*Jahr"),
          omit.stat = c("ser", "f"), single.row = FALSE,
          intercept.bottom = FALSE,
          no.space = TRUE,
          dep.var.caption = "\\textit{Abhängige Variable}",
          notes.label = "\\textit{Anmerkung:}")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Thu, Feb 22, 2024 - 14:49:26
    ## \begin{table}[!htbp] \centering 
    ##   \caption{} 
    ##   \label{} 
    ## \begin{tabular}{@{\extracolsep{5pt}}lcc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{2}{c}{\textit{Abhängige Variable}} \\ 
    ## \cline{2-3} 
    ## \\[-1.8ex] & log Kaltmiete (€/m\textsuperscript{2}) & log Mietbelastung (\%) \\ 
    ## \\[-1.8ex] & (1) & (2)\\ 
    ## \hline \\[-1.8ex] 
    ##  Konstante & 2.032$^{***}$ & $-$1.743$^{***}$ \\ 
    ##   & (0.002) & (0.003) \\ 
    ##   Jahr & 0.041$^{***}$ & 0.052$^{***}$ \\ 
    ##   & (0.0002) & (0.0005) \\ 
    ##   LQ Arbeitslose & $-$0.332$^{***}$ & $-$0.075$^{***}$ \\ 
    ##   & (0.001) & (0.003) \\ 
    ##   LQ Ausländer & $-$0.016$^{***}$ & $-$0.044$^{***}$ \\ 
    ##   & (0.001) & (0.001) \\ 
    ##   LQ Arbeitslose*Jahr & 0.008$^{***}$ & $-$0.035$^{***}$ \\ 
    ##   & (0.0002) & (0.0004) \\ 
    ##   LQ Ausländer*Jahr & 0.016$^{***}$ & 0.021$^{***}$ \\ 
    ##   & (0.0001) & (0.0002) \\ 
    ##  \hline \\[-1.8ex] 
    ## Observations & 1,357,298 & 1,357,298 \\ 
    ## R$^{2}$ & 0.428 & 0.085 \\ 
    ## Adjusted R$^{2}$ & 0.428 & 0.085 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Anmerkung:} & \multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

Plot Regressionsmodelle

``` r
facet_names <- list(
  'mietekalt_m2'="Miete (€/m²)",
  'mietbelastung'="Mietbelastung (%)"
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}


expand.grid(jahr = (2007:2020) - 2007,
            LQ_arbeitslose = seq(from = 0, to = 2, by = 0.1) %>% round(1),
            LQ_auslaender = seq(from = 0, to = 2, by = 0.1) %>% round(1)) %>%
  mutate(mietekalt_m2 = exp(predict(lm_mietekalt_LQ, .)),
         mietbelastung = exp(predict(lm_mietbelastung_LQ, .)) * 100) %>%
  filter(LQ_arbeitslose %in% c(0.2, 1.2)) %>%
  filter(LQ_auslaender %in% c(0.2, 1.2)) %>%
  pivot_longer(cols = c(mietekalt_m2, mietbelastung)) %>%
  mutate(name = factor(name, levels = c("mietekalt_m2", "mietbelastung")),
         jahr = jahr + 2007,
         jahr = lubridate::ymd(paste(jahr, 01, 01, sep = "-"))) %>%
  ggplot(aes(x = jahr, y = value, color = as_factor(LQ_auslaender), linetype = as_factor(LQ_arbeitslose)))+
  geom_line()+
  facet_wrap(~name, scales = "free", labeller = facet_labeller)+
  scale_x_date(date_breaks = "2 years", date_labels = "%y")+
  #scale_color_brewer(palette = "Paired")+
  scale_color_manual(values = c("blue", "red", "green", "black")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash"))+
  theme_bw()+
  theme(legend.position = "top",
        #legend.box = "vertical",
        #legend.margin = margin(t = 2, b = -5, l = 0, r = 0)
        )+
  labs(x = "Jahr", y = element_blank(), color = "LQ-Ausländer", linetype = "LQ-Arbeitslose")
```

![](auswertung_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_lq_modell.png", width = 5, height = 2.5)
```

Plot Zusammenhang Miete, LQ für 2015

``` r
expand.grid(jahr = 2015 - 2007,
            LQ_arbeitslose = seq(from = 0, to = 2, by = 0.01) %>% round(2),
            LQ_auslaender = seq(from = 0, to = 2, by = 0.01) %>% round(2)) %>%
  mutate(mietekalt_m2 = exp(predict(lm_mietekalt_LQ, .)),
         mietbelastung = exp(predict(lm_mietbelastung_LQ, .)) * 100) %>%
  ggplot(aes(x = LQ_arbeitslose, y = LQ_auslaender, fill = mietekalt_m2))+
  geom_raster()+
  scale_fill_viridis_c()+
  coord_fixed()+
  theme_bw()+
  theme(legend.position = "top")+
  labs(x = "LQ-Arbeitslose", y = "LQ-Ausländer")
```

![](auswertung_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
data_rent %>%
  left_join(data_LQ, by = c("jahr", "r1_id")) %>%
  filter(mietekalt_m2 > quantile(mietekalt_m2, probs = .01), 
         mietekalt_m2 < quantile(mietekalt_m2, probs = .99)) %>%
  filter(jahr == 2015) %>%
  group_by(r1_id) %>%
  summarise(LQ_arbeitslose = mean(LQ_arbeitslose),
            LQ_auslaender = mean(LQ_auslaender),
            mietekalt_m2 = mean(mietekalt_m2)) %>%
  ungroup() %>%
  ggplot(aes(x = LQ_arbeitslose, y = LQ_auslaender, color = mietekalt_m2))+
  geom_point()+
  scale_color_viridis_c()+
  coord_fixed(xlim = c(0, 2), ylim = c(0, 2))+
  theme_bw()+
    theme(legend.position = "top")
```

    ## Warning: Removed 3 rows containing missing values (`geom_point()`).

![](auswertung_files/figure-gfm/unnamed-chunk-40-2.png)<!-- -->

``` r
data_rent %>%
  left_join(data_LQ, by = c("jahr", "r1_id")) %>%
  filter(mietekalt_m2 > quantile(mietekalt_m2, probs = .01), 
         mietekalt_m2 < quantile(mietekalt_m2, probs = .99)) %>%
  filter(jahr == 2015) %>%
  group_by(r1_id) %>%
  summarise(LQ_arbeitslose = mean(LQ_arbeitslose),
            LQ_auslaender = mean(LQ_auslaender),
            mietekalt_m2 = mean(mietekalt_m2)) %>%
  ungroup() %>%
  ggplot(aes(x = LQ_auslaender, y = mietekalt_m2, color = mietekalt_m2))+
  geom_point()+
  scale_color_viridis_c()+
  coord_cartesian(xlim = c(0.1, 2))+
  scale_y_continuous(trans = "log")+
  scale_x_continuous(trans = "log")+
  theme_bw()+
    theme(legend.position = "top")
```

    ## Warning: Removed 3 rows containing missing values (`geom_point()`).

![](auswertung_files/figure-gfm/unnamed-chunk-40-3.png)<!-- -->

Wir sollten noch die Güte der Regression überprüfen, indem wir die
Residuen betrachten.

``` r
facet_names <- list(
  'mietekalt_m2'="Miete",
  'mietbelastung'="Mietbelastung"
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}

# ------

set.seed(1)

tibble(model = "mietekalt_m2",
  fitted = fitted(lm_mietekalt_LQ),
       resids = resid(lm_mietekalt_LQ)) %>%
  rbind(
    tibble(model = "mietbelastung",
      fitted = fitted(lm_mietbelastung_LQ),
       resids = resid(lm_mietbelastung_LQ))
  ) %>%
  mutate(model = factor(model, levels = c("mietekalt_m2", "mietbelastung"))) %>%
  slice_sample(n = 10000, by = model) %>%
  ggplot(aes(x = fitted, y = resids))+
  geom_point(alpha = .05, size = .2)+
  facet_wrap(~model, scales = "free", labeller = facet_labeller)+
  scale_y_continuous(limits = function(x) c(quantile(x, 0.05, na.rm = TRUE), quantile(x, 0.95, na.rm = TRUE))) +
  theme_bw()+
  labs(x = "Fitted", y = "Residuals")
```

![](auswertung_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_lq.png", width = 5, height = 2)
```

# Ideen, Testen & Ausprobieren

``` r
1+1
```

    ## [1] 2

Dies ist ein Test. Das sollte nur auf dem test_branch auftauchen.

``` r
rnorm(1)
```

    ## [1] -0.1823902
