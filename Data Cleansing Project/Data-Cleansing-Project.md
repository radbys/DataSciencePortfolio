Data Cleansing Project
================
Radosław Bysewski
3 09 2021

``` r
library(tidyverse)
library(readxl) 
library(knitr)
library(kableExtra)
```

``` r
input_dir <- sprintf("%s/input", getwd())

for(file in list.files(input_dir)){
  name <- substring(file, 1, 1)
  df   <- readxl::read_excel(sprintf("%s/%s", input_dir, file))
  assign(name, df)
  rm(file, name, df)
}
```

``` r
main <- dplyr::left_join(g, t, by = c("x1" = "Team_ID")) %>% 
  dplyr::left_join(t, by = c("x2" = "Team_ID")) %>% 
  dplyr::select(Game_ID, Date, x2, x2, x1_N = Team_Short.x , x2_N = Team_Short.y) %>%
  dplyr::left_join(o, by = "Game_ID") %>% 
  dplyr::left_join(l, by = "Game_ID") %>%                         
  dplyr::left_join(p, by = "Game_ID") %>%                         
  dplyr::left_join(b, by = "Game_ID") %>%                         
  dplyr::left_join(s, by = "Game_ID")                         
```

``` r
for(col in 1:ncol(main)){
  if(anyNA(main[, col])) {

    row <- which(is.na(main[, col]))
    print(sprintf("%sth record of %s columns has NA value",
                  row,
                  names(main)[col]))
  }
} 
```

    ## [1] "12438th record of x1_N columns has NA value"
    ## [1] "12438th record of x1_B columns has NA value"

``` r
knitr::kable(main[12438, 1:13])
```

<table>
<thead>
<tr>
<th style="text-align:right;">
Game\_ID
</th>
<th style="text-align:left;">
Date
</th>
<th style="text-align:right;">
x2
</th>
<th style="text-align:left;">
x1\_N
</th>
<th style="text-align:left;">
x2\_N
</th>
<th style="text-align:right;">
x1\_Od
</th>
<th style="text-align:right;">
x2\_Od
</th>
<th style="text-align:right;">
Line
</th>
<th style="text-align:right;">
x1\_P
</th>
<th style="text-align:right;">
x2\_P
</th>
<th style="text-align:right;">
x1\_B
</th>
<th style="text-align:right;">
x2\_B
</th>
<th style="text-align:right;">
x1\_MIN
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
12438
</td>
<td style="text-align:left;">
2017-02-23
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CLE
</td>
<td style="text-align:right;">
2.2
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
217.5
</td>
<td style="text-align:right;">
141
</td>
<td style="text-align:right;">
119
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
130454892
</td>
<td style="text-align:right;">
199
</td>
</tr>
</tbody>
</table>
