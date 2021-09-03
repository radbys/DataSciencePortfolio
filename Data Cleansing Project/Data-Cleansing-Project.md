Data Cleansing Project
================
Radosław Bysewski
3 09 2021

``` r
library(tidyverse)
library(readxl) 
library(knitr)
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
  dplyr::select(Game_ID, Date, x1, x2, x1_N = Team_Short.x , x2_N = Team_Short.y) %>%
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

| Game\_ID | Date       |  x1 |  x2 | x1\_N | x2\_N | x1\_Od | x2\_Od |  Line | x1\_P | x2\_P | x1\_B |     x2\_B |
|---------:|:-----------|----:|----:|:------|:------|-------:|-------:|------:|------:|------:|------:|----------:|
|    12438 | 2017-02-23 |   0 |   6 | NA    | CLE   |    2.2 |   1.71 | 217.5 |   141 |   119 |    NA | 130454892 |

``` r
#replace NAs
main[12438,3] <- 20
main[12438,5] <- "NYK"
main[12438,10] <- 104
main[12438,12] <-  103062240

#verryfing NA reduction
knitr::kable(main[12438, 1:13])
```

| Game\_ID | Date       |  x1 |  x2 | x1\_N | x2\_N | x1\_Od | x2\_Od |  Line | x1\_P | x2\_P |     x1\_B |     x2\_B |
|---------:|:-----------|----:|----:|:------|:------|-------:|-------:|------:|------:|------:|----------:|----------:|
|    12438 | 2017-02-23 |  20 |   6 | NYK   | CLE   |    2.2 |   1.71 | 217.5 |   104 |   119 | 103062240 | 130454892 |
