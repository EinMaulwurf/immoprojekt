auswertung.Rmd
================

# Libraries

``` r
# Für Korrelationsmatrizen
library(Hmisc)

# Für schönere Latex-Tabellen
library(stargazer)
library(kableExtra)
library(vtable)

# Für schönere Plots
library(ggpubr)
library(ggrepel)
library(gridExtra)
library(cowplot)

# Für schnelleres Einlesen von Daten
library(vroom)

# Für allgemeine Datenmanipulation und Plotten
library(tidyverse)

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

# Clustering

Anzahl der Cluster bestimmen

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

![](auswertung_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#ggsave("./plots/plot_cluster_elbow.png")
```

Es werden 4 Cluster verwendet.

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

![](auswertung_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#ggsave("./plots/plot_cluster_2015.png")
```

Cluster sind sehr ähnlich zu den Wahlergebnissen der Bundestagswahl in
Berlin 2016 ![Wahlergebnissen Berlin
2016](https://img.welt.de/img/politik/deutschland/mobile158256707/6751622257-ci23x11-w2000/DWO-IP-WahlergebnisBerlin-js-eps.jpg)

Wer steckt hinter den Clustern? Clustercenter betrachten.

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

Darstellen auf Karte für verschiedene Jahre

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

![](auswertung_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#ggsave("./plots/plot_cluster_jahr_vergleich.png", plot = plot_arranged)
```

# Regressionen

## Miete über die Jahre

Regresssion Miete

``` r
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

Regression Mietbelastung

``` r
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

Stargazer Tabelle

``` r
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
    ## % Date and time: Sat, Jan 18, 2025 - 09:55:43
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

Plot Modell

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

    ## Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    ## 3.5.0.
    ## ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](auswertung_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_jahr_modell.png", width = 5, height = 2)
```

Plot Residuen

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

![](auswertung_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_miete_mietbelastung_jahr.png", width = 5, height = 2)
```

## Miete und Mietbelastung in “Brennpunkten”

Plot Brennpunkte

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

![](auswertung_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
#ggsave("./plots/plot_brennpunkte.png", width = 5, height = 2)
```

Regression Miete

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

Regression Mietbelastung

``` r
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

Stargazer Tabelle

``` r
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
    ## % Date and time: Sat, Jan 18, 2025 - 09:55:47
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

![](auswertung_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_brennpunkte_modell.png", width = 5, height = 2.5)
```

Plot Residuen

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

![](auswertung_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_brennpunkte.png", width = 5, height = 2)
```

## Miete in Clustern

Regression Miete

``` r
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

Regression Mietbelastung

``` r
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

Stargazer Tabelle

``` r
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
    ## % Date and time: Sat, Jan 18, 2025 - 09:55:52
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

![](auswertung_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_cluster_modell.png", width = 5, height = 2.5)
```

Plot Residuen

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

![](auswertung_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_cluster.png", width = 5, height = 2)
```

## Regression LQ Quotienten

Daten vorbereiten

``` r
data_LQ <- data_social %>%
  select(r1_id, jahr, arbeitslosenquote, anteil_auslaender, anzahl_haushalte, kaufkraft_pro_haushalt) %>%
  group_by(jahr) %>%
  filter(!is.na(anzahl_haushalte)) %>%
  mutate(LQ_arbeitslose = arbeitslosenquote / weighted.mean(arbeitslosenquote, anzahl_haushalte, na.rm = TRUE),
         LQ_auslaender = anteil_auslaender  / weighted.mean(anteil_auslaender, anzahl_haushalte, na.rm = TRUE),
         .keep = c("unused")) %>%
  ungroup()
```

Regression Miete

``` r
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

Regression Mietbelastung

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

Stargazer Tabelle

``` r
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
    ## % Date and time: Sat, Jan 18, 2025 - 09:55:55
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

![](auswertung_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_lq_modell.png", width = 5, height = 2.5)
```

Plot Residuen

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

![](auswertung_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
#ggsave("./plots/plot_regression_lq.png", width = 5, height = 2)
```
