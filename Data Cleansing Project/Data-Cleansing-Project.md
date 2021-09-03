Data Cleansing Project
================
Radosław Bysewski
3 09 2021

``` r
library(tidyverse)
library(readxl)  
```

``` r
input_dir <- sprintf("%s/input", getwd())
input_dir
```

    ## [1] "C:/Users/SirAnalyst/Desktop/DataSciencePortfolio/Data Cleansing Project/input"

``` r
for(file in list.files(input_dir)){
  name <- substring(file, 1, 1)
  df   <- readxl::read_excel(sprintf("%s/%s", input_dir, file))
  assign(name, df)
  rm(file, name, df)
}
```

``` r
dplyr::left_join(g, t, by = c("x1" = "Team_ID")) %>% 
  dplyr::left_join(t, by = c("x2" = "Team_ID")) %>% 
  dplyr::select(Game_ID, Date, x2, x2, x1_N = Team_Short.x , x2_N = Team_Short.y) %>%
  dplyr::left_join(o, by = "Game_ID") %>% 
  dplyr::left_join(l, by = "Game_ID") %>%                         
  dplyr::left_join(p, by = "Game_ID") %>%                         
  dplyr::left_join(b, by = "Game_ID") %>%                         
  dplyr::left_join(s, by = "Game_ID")                         
```

    ## # A tibble: 12,897 x 50
    ##    Game_ID Date                   x2 x1_N  x2_N  x1_Od x2_Od  Line  x1_P  x2_P
    ##      <dbl> <dttm>              <dbl> <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1       1 2007-10-30 00:00:00    25 SAS   POR    1.07 10     190.   106    97
    ##  2       2 2007-10-30 00:00:00    29 GSW   UTA    1.83  2     212     96   117
    ##  3       3 2007-10-30 00:00:00    11 LAL   HOU    2.9   1.43  199     93    95
    ##  4       4 2007-10-31 00:00:00    23 TOR   PHI    1.33  3.55  191    106    97
    ##  5       5 2007-10-31 00:00:00    30 IND   WAS    2.05  1.8   204.   119   110
    ##  6       6 2007-10-31 00:00:00    17 ORL   MIL    1.34  3.45  197    102    83
    ##  7       7 2007-10-31 00:00:00     5 BKN   CHI    1.8   2.05  186    112   103
    ##  8       8 2007-10-31 00:00:00     7 CLE   DAL    2.2   1.71  184     74    92
    ##  9       9 2007-10-31 00:00:00    27 MEM   SAS    3.1   1.4   201    101   104
    ## 10      10 2007-10-31 00:00:00    26 NOP   SAC    1.19  5.25  190.   104    90
    ## # ... with 12,887 more rows, and 40 more variables: x1_B <dbl>, x2_B <dbl>,
    ## #   x1_MIN <dbl>, x2_MIN <dbl>, x1_FGM <dbl>, x2_FGM <dbl>, x1_FGA <dbl>,
    ## #   x2_FGA <dbl>, x1_FG% <dbl>, x2_FG% <dbl>, x1_3PM <dbl>, x2_3PM <dbl>,
    ## #   x1_3PA <dbl>, x2_3PA <dbl>, x1_3P% <dbl>, x2_3P% <dbl>, x1_FTM <dbl>,
    ## #   x2_FTM <dbl>, x1_FTA <dbl>, x2_FTA <dbl>, x1_FT% <dbl>, x2_FT% <dbl>,
    ## #   x1_OREB <dbl>, x2_OREB <dbl>, x1_DREB <dbl>, x2_DREB <dbl>, x1_REB <dbl>,
    ## #   x2_REB <dbl>, x1_AST <dbl>, x2_AST <dbl>, x1_STL <dbl>, x2_STL <dbl>,
    ## #   x1_BLK <dbl>, x2_BLK <dbl>, x1_TOV <dbl>, x2_TOV <dbl>, x1_PF <dbl>,
    ## #   x2_PF <dbl>, x1_+/- <dbl>, x2_+/- <dbl>
