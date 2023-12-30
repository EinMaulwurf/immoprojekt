auswertung.Rmd
================
2023-12-28

# Libraries

``` r
library(ggpubr)
```

    ## Loading required package: ggplot2

``` r
library(vroom)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ readr::col_character()   masks vroom::col_character()
    ## ✖ readr::col_date()        masks vroom::col_date()
    ## ✖ readr::col_datetime()    masks vroom::col_datetime()
    ## ✖ readr::col_double()      masks vroom::col_double()
    ## ✖ readr::col_factor()      masks vroom::col_factor()
    ## ✖ readr::col_guess()       masks vroom::col_guess()
    ## ✖ readr::col_integer()     masks vroom::col_integer()
    ## ✖ readr::col_logical()     masks vroom::col_logical()
    ## ✖ readr::col_number()      masks vroom::col_number()
    ## ✖ readr::col_skip()        masks vroom::col_skip()
    ## ✖ readr::col_time()        masks vroom::col_time()
    ## ✖ readr::cols()            masks vroom::cols()
    ## ✖ readr::date_names_lang() masks vroom::date_names_lang()
    ## ✖ readr::default_locale()  masks vroom::default_locale()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ readr::fwf_cols()        masks vroom::fwf_cols()
    ## ✖ readr::fwf_empty()       masks vroom::fwf_empty()
    ## ✖ readr::fwf_positions()   masks vroom::fwf_positions()
    ## ✖ readr::fwf_widths()      masks vroom::fwf_widths()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ readr::locale()          masks vroom::locale()
    ## ✖ readr::output_column()   masks vroom::output_column()
    ## ✖ readr::problems()        masks vroom::problems()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(sf)
```

    ## Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1; sf_use_s2() is TRUE

# Importing data

Die Daten wurden im Dokument ‘cleaning.Rmd’ aufbereitet und
abgespeichert.

``` r
data_rent <- vroom("./daten/data_rent.csv")
```

    ## Rows: 1919320 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): r1_id
    ## dbl (3): mietekalt, wohnflaeche, jahr
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data_social <- vroom("./daten/data_social.csv")
```

    ## Rows: 11240 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): r1_id
    ## dbl (11): jahr, anzahl_haushalte, arbeitslosenquote, anteil_oberklassewagen,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
inspire_grid_berlin <- st_read("./daten/inspire_grid_berlin.gpkg")
```

    ## Reading layer `inspire_grid_berlin' from data source 
    ##   `/home/admin/workspace/immoprojekt/daten/inspire_grid_berlin.gpkg' 
    ##   using driver `GPKG'
    ## Simple feature collection with 1016 features and 17 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 4531000 ymin: 3253000 xmax: 4577000 ymax: 3291000
    ## Projected CRS: ETRS89-extended / LAEA Europe

``` r
bezirksgrenzen_berlin <- st_read("./daten/bezirksgrenzen_berlin/bezirksgrenzen.shp") %>%
  st_transform("EPSG:3035")
```

    ## Reading layer `bezirksgrenzen' from data source 
    ##   `/home/admin/workspace/immoprojekt/daten/bezirksgrenzen_berlin/bezirksgrenzen.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 12 features and 6 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 13.08835 ymin: 52.33825 xmax: 13.76116 ymax: 52.67551
    ## Geodetic CRS:  WGS 84

# Plotting

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
  theme_bw()+
  labs(x = "Jahr", y = "Kaltmiete pro m²")

plot2 <- data_rent %>%
  filter(jahr == 2010) %>%
  filter(mietekalt > 50, mietekalt < 10000) %>%
  ggplot(aes(x = mietekalt, y = after_stat(density))) +
  geom_histogram(binwidth = 20) +
  geom_vline(aes(xintercept = mean(mietekalt), color = "Mean"), linewidth = .8, linetype = "dashed") +
  geom_vline(aes(xintercept = median(mietekalt), color = "Median"), linewidth = .8, linetype = "dashed") +
  scale_color_manual("Statistics", values = c("Mean" = "red", "Median" = "blue"))+
  coord_cartesian(xlim = c(0, 2000)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1))+
  theme_bw() +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank(), legend.background = element_blank())+
  labs(x = "Kaltmiete", y = "Dichte in %")

plot3 <- data_rent %>%
  filter(jahr == 2020) %>%
  filter(mietekalt > 50, mietekalt < 10000) %>%
  ggplot(aes(x = mietekalt, y = after_stat(density))) +
  geom_histogram(binwidth = 20) +
  geom_vline(aes(xintercept = mean(mietekalt), color = "Mean"), linewidth = .8, linetype = "dashed") +
  geom_vline(aes(xintercept = median(mietekalt), color = "Median"), linewidth = .8, linetype = "dashed") +
  scale_color_manual("Statistics", values = c("Mean" = "red", "Median" = "blue"))+
  coord_cartesian(xlim = c(0, 2000)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1))+
  theme_bw() +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank(), legend.background = element_blank())+
  labs(x = "Kaltmiete", y = "Dichte in %")

ggarrange(plot1,
          ggarrange(plot2, plot3, nrow = 2),
          nrow = 1)
```

![](auswertung_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggsave("./plots/plot_miete.pdf")
```

    ## Saving 7.5 x 4.5 in image

## Social Daten

Wie sehen ausgewählte Social-Variablen auf einer Karte aus?

``` r
# arbeitslosenquote, kaufkraft_pro_haushalt, anteil_60_plus, anteil_auslaender

data_social_sf <- data_social %>%
  left_join(inspire_grid_berlin %>% select(r1_id, geom), by = "r1_id") %>%
  st_as_sf()
  
plot1 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = arbeitslosenquote, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Arbeitslosenquote")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5))

plot2 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = kaufkraft_pro_haushalt, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Kaufkraft")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5))

plot3 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  filter(anteil_60_plus < 80) %>%
  ggplot(aes(fill = anteil_60_plus, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Anteil 60+")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5))

plot4 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  ggplot(aes(fill = anteil_auslaender, geometry = geom), linewidth = .1)+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(subtitle = "Ausländeranteil")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle=element_text(hjust=0.5))

ggarrange(plot1, plot2, plot3, plot4, nrow = 1)
```

![](auswertung_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggsave("./plots/plot_data_social_2015.pdf")
```

    ## Saving 7.5 x 2.5 in image

# Clustering

k-means clustering mit den Variablen

- arbeitslosenquote
- anteil_oberklassewagen (NICHT MEHR, weil nicht für alle Jahre
  vorhanden)
- kaufkraft_pro_haushalt
- anteil_auslaender
- anteil_efh
- anteil_60_plus

Es werden 4 Cluster verwendet. *Hier Begründung einfügen*

``` r
data_clustering <- data_social %>%
  filter(jahr == 2015) %>%
  drop_na() %>%
  select(anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  as.matrix() %>%
  scale()

set.seed(1)
kmeans_result_2010 <- kmeans(data_clustering, centers = 4)

data_social %>%
  filter(jahr == 2015) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2010$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  left_join(inspire_grid_berlin %>% select(r1_id), by = "r1_id") %>%
  st_as_sf() %>%
  ggplot()+
  geom_sf(aes(fill = cluster))+
  theme_bw()
```

![](auswertung_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Cluster sind sehr ähnlich zu den Wahlergebnissen der Bundestagswahl in
Berlin 2016 ![Wahlergebnissen Berlin
2016](https://img.welt.de/img/politik/deutschland/mobile158256707/6751622257-ci23x11-w2000/DWO-IP-WahlergebnisBerlin-js-eps.jpg)

Wer steckt hinter den Clustern? Um diese Frage zu beantworten können wir
die Mittelwerte der jeweiligen Cluster betrachten.

``` r
kmeans_result_2010$centers %>% t() %>% round(2)
```

    ##                            1     2     3     4
    ## anzahl_haushalte       -0.63  0.29 -0.53  2.12
    ## arbeitslosenquote      -0.06  0.80 -1.02  0.79
    ## kaufkraft_pro_haushalt -0.28 -0.62  1.02 -0.67
    ## anteil_auslaender      -0.52  0.13 -0.42  2.10
    ## anteil_efh              1.03 -0.83  0.52 -1.11
    ## anteil_60_plus         -0.12 -0.21  0.72 -1.43

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

Zusammenfassend könnte man Cluster 3 also als Ort bezeichnen, wo
Reichtum, Ruhe und Rentner regieren!

## Zeitliche Stabilität der Cluster

Sind die Cluster über die Zeit stabil? Das ist wichtig falls wir
Regressionen basierend auf diesen Clusterungen vornehmen.

``` r
# Split data by year
data_clustering_year <- data_social %>%
  select(jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  group_by(jahr) %>%
  filter(jahr %in% c(2005, 2010, 2015, 2019)) %>%
  group_split(.keep = FALSE)

kmeans_result_2005 <- data_clustering_year[[1]] %>% as.matrix() %>% scale() %>%
  kmeans(., centers = 4)
#kmeans_result_2005

kmeans_result_2010 <- data_clustering_year[[2]] %>% as.matrix() %>% scale() %>%
  kmeans(., centers = 4)
#kmeans_result_2010

kmeans_result_2015 <- data_clustering_year[[3]] %>% as.matrix() %>% scale() %>%
  kmeans(., centers = 4)
#kmeans_result_2015

kmeans_result_2019 <- data_clustering_year[[4]] %>% as.matrix() %>% scale() %>%
  kmeans(., centers = 4)
#kmeans_result_2019
```

``` r
plot1 <- data_social_sf %>%
  filter(jahr == 2005) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2005$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  labs(subtitle = "Cluster in 2005")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank())

plot2 <- data_social_sf %>%
  filter(jahr == 2010) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2010$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  labs(subtitle = "Cluster in 2010")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank())

plot3 <- data_social_sf %>%
  filter(jahr == 2015) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2015$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  labs(subtitle = "Cluster in 2015")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank())

plot4 <- data_social_sf %>%
  filter(jahr == 2019) %>%
  select(r1_id, jahr, anzahl_haushalte, arbeitslosenquote, kaufkraft_pro_haushalt, anteil_auslaender, anteil_efh, anteil_60_plus) %>%
  drop_na() %>%
  cbind(cluster = kmeans_result_2019$cluster) %>%
  mutate(cluster = as_factor(cluster)) %>%
  ggplot()+
  geom_sf(aes(fill = cluster), linewidth = .1)+
  labs(subtitle = "Cluster in 2019")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggarrange(plot1, plot2, plot3, plot4, nrow = 1)
```

![](auswertung_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave("./plots/plot_cluster_jahr_vergleich.pdf")
```

    ## Saving 7.5 x 2.5 in image

**Ergebnis:** Die Cluster sind über die Zeit ziemlich stabil. Erst in
2019 lassen sich einige größere Unterschiede bzw. Abweichungen erkennen.
Wichtig ist, dass die Farbe der Cluster zwischen den Jahren nicht
konstant ist. Es sollte also die Form der Cluster über die Jahre
betrachtet werden. Statt 2020 wurde 2019 verwendet, da dies das letzte
Jahr aus dem Datensatz ist.

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
lm_miete_jahr <- data_rent %>%
  mutate(mietekalt_m2 = mietekalt / wohnflaeche) %>%
  filter(!is.na(mietekalt_m2)) %>%
  filter(mietekalt_m2 < quantile(mietekalt_m2, 0.99), mietekalt_m2 > quantile(mietekalt_m2, 0.01)) %>%
  lm(log(mietekalt_m2) ~ jahr, data = .)

summary(lm_miete_jahr)
```

    ## 
    ## Call:
    ## lm(formula = log(mietekalt_m2) ~ jahr, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.23383 -0.18964 -0.02218  0.17442  1.42756 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.190e+02  1.048e-01   -1135   <2e-16 ***
    ## jahr         6.015e-02  5.210e-05    1154   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2789 on 1880313 degrees of freedom
    ## Multiple R-squared:  0.4148, Adjusted R-squared:  0.4148 
    ## F-statistic: 1.333e+06 on 1 and 1880313 DF,  p-value: < 2.2e-16

Das Ergebnis der Regression spiegelt das wieder, was bereits im Boxplot
oben gezeigt wurde: Die Miete steigt im Durchschnitt pro Jahr. Konkret
wissen wir nun, dass die Miete pro Jahr im Schnitt um
$\exp(0.06015)-1=6.1\%$ steigt. Der Wert des $\beta_0$ (Intercept) lässt
sich nicht sinnvoll interpretieren, dieser würde die Miete im Jahr 0
angeben. Interessant ist für uns noch der Wert der t-Statistik
beziehungsweise der p-Wert. Dieser ist nahe 0 und zeigt somit starke
statistische Signifikanz. Dies impliziert, dass die Wahrscheinlichkeit,
dass der ermittelte Zusammenhang lediglich auf Zufall beruht, äußerst
gering ist. Das ist durch die schiere Größe des Datensatzes (knapp 1.9
Mio. Beobachtungen) jedoch nicht weiter überraschend. Umso wichtiger ist
es hingegen, auch auf die ökonomische Relevanz des Zusammenhangs zu
achten. Eine Steigerung von knapp 6% pro Jahr ist jedoch auch ökonomisch
relevant, liegt diese doch weit über dem zu dieser Zeit vorherrschenden
allgemeinen Inflationsniveau von unter 2%.

Zunächst wollen wir noch die Robustheit der Regression überprüfen um
Sicher zu gehen, dass der t-Test überhaupt ein sinnvolles Ergebnis
liefert. Dafür betrachten wir zuerst die Verteilung der Residuen, hier
der Einfachheit als Dichte dargestellt.

``` r
lm_miete_jahr$residuals %>%
  data.frame(residuals = .) %>%
  ggplot(aes(x = residuals))+
  geom_density(bw = .1)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  labs(x = "Residuen", y = "Dichte")
```

![](auswertung_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Im Ergebnis sehen wir eine fast perfekte Normalverteilung.

``` r
data_rent %>%
  mutate(mietekalt_m2 = mietekalt / wohnflaeche) %>%
  filter(!is.na(mietekalt_m2)) %>%
  filter(mietekalt_m2 < quantile(mietekalt_m2, 0.99), mietekalt_m2 > quantile(mietekalt_m2, 0.01)) %>%
  mutate(mietekalt_m2 = log(mietekalt_m2)) %>%
  group_by(jahr) %>%
  summarise(variance = var(mietekalt_m2)) %>%
  round(2)
```

    ## # A tibble: 15 × 2
    ##     jahr variance
    ##    <dbl>    <dbl>
    ##  1  2007     0.06
    ##  2  2008     0.06
    ##  3  2009     0.06
    ##  4  2010     0.07
    ##  5  2011     0.07
    ##  6  2012     0.08
    ##  7  2013     0.08
    ##  8  2014     0.09
    ##  9  2015     0.08
    ## 10  2016     0.09
    ## 11  2017     0.09
    ## 12  2018     0.09
    ## 13  2019     0.11
    ## 14  2020     0.15
    ## 15  2021     0.16

Ein Problem ist jedoch Heteroskedastizität. So nennt man das Phänomen
von nicht stabilen Varianzen. Für diesen Fall konkret sehen wir, dass
die Varianz über die Jahre steigt, die Preise für Wohnungen streuen also
immer weiter. Mathematisch bringt das Probleme mit der Robustheit der
Schätzung, insbesondere kann dies zu Verzerrungne in den Standartfehlern
führen. Diese werden wiederum für den t-Test und die Bestimmung der
statistischen Signifikanz benötigt.

Um dieses Problem zu umgehen, berechnen wir im folgenden robuste
Standartfehler und Vergleichen die Ergebnisse mit der ursprünglichen
Regression. Dafür wird eine “heteroskedasticity-consistent” (HC)
Kovarianzmatrix geschätzt.

``` r
library(sandwich)
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
coeftest(lm_miete_jahr, vcov = vcovHC(lm_miete_jahr, type = "HC3"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                Estimate  Std. Error t value  Pr(>|t|)    
    ## (Intercept) -1.1902e+02  1.1534e-01 -1031.9 < 2.2e-16 ***
    ## jahr         6.0150e-02  5.7350e-05  1048.8 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Wenn wir das Ergebnis dieser Schätzung mit dem des ursprünglichen
Modells vergleichen, sehen wir keine Unterschiede bei den Schätzwerten
der Betas. Die Standartfehler hingegen sind etwas größer. Das war zu
erwarten, da durch die vorliegende Heteroskedastizität die
Standartfehler in der ursprünglichen Regression verzerrt waren.
ALlerdings ist die Abweichung marginal, was sich auch in einem quasi
unverändert bei fast Null liegendem p-Wert zeigt.

## Miete in “Brennpunkten” und Clustern

Hat sich die Miete in Brennpunkten über die Jahre stärker erhöht?

Hierzu müssen wir neben `data_rent` auch `data_social` verwenden. Wir
definieren einen “Brennpunkt” als eine Gegend, in der die
Arbeitslosenquote 2015 über 10% liegt und wo die Kaufkraft im unteren
20% Quantil liegt.

Als Regressionsmodell verwenden wir **TODO…** mit einem Interaktionsterm
zwischen dem Jahr und der binären Ist-Brennpunkt Variable.

**TODO:** Erklären, wieso wir $jahr = jahr - 2007$ rechnen (um den
Effekt von ist_brennpunkt und dem Intercept interpretieren zu können…)

``` r
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
    ## -1.23880 -0.18592 -0.01919  0.17371  1.39658 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              1.732e+00  3.950e-04  4383.8   <2e-16 ***
    ## jahr                     5.755e-02  6.152e-05   935.6   <2e-16 ***
    ## ist_brennpunktTRUE      -1.006e-01  7.136e-04  -140.9   <2e-16 ***
    ## jahr:ist_brennpunktTRUE  8.079e-03  1.141e-04    70.8   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2772 on 1880133 degrees of freedom
    ## Multiple R-squared:  0.4221, Adjusted R-squared:  0.4221 
    ## F-statistic: 4.578e+05 on 3 and 1880133 DF,  p-value: < 2.2e-16

**TODO** Interpretieren

# Ideen, Testen & Ausprobieren

``` r
1+1
```

    ## [1] 2
