################################################################################
# Title:  Activities in Rugby - Exploratory Data Analysis
# Date:   2021-09-16
# Technologies:
#     * R
#     * Html (Kaggle only)
#     * CSS  (Kaggle only)
# Libraries:
#   * tidyverse
#   * stringi
#   * lubridate
#   * ggthemes
#   * gridExtra
#   * gtable
# Tasks:
#   * Data wrange
#   * Feature engineering
#   * EDA
# Exteral link:
#   * https://www.kaggle.com/radbys/activities-in-rugby-exploratory-data-analysis
################################################################################

# Environment settings
  # Loading libraries
  library(tidyverse)
  library(stringi)
  library(lubridate)
  library(ggthemes)
  library(gridExtra)
  library(gtable)

  # Plot settings
  ggplot2::theme_set(ggthemes::theme_fivethirtyeight())

  ggplot2::theme_update(text = ggplot2::element_text(size = 20),
                        plot.title = ggplot2::element_text(size = 26, face = "bold"),
                        strip.text = ggplot2::element_text(size = 20, face = "bold", colour = "black"),
                        strip.background = ggplot2::element_rect(fill = "darkgrey"),
                        legend.key.size = ggplot2::unit(0.5, "cm"),
                        legend.key.width = ggplot2::unit(0.5,"cm")
  )

  colors <- c("#0a1128", "#001f54", "#aa8f66", "#ffff82", "grey")

  # Default ggplot2 colors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }


# Data wranlge
  # Data Import
  zdf <- read.csv("C:/Users/SirAnalyst/Desktop/garmin/Zeke.csv")
  sdf <- read.csv("C:/Users/SirAnalyst/Desktop/garmin/Stachu.csv")
  wdf <- read.csv("C:/Users/SirAnalyst/Desktop/garmin/Wiaan.csv")

  # Data homogeneity check
  dim(zdf)
  dim(sdf)
  dim(wdf)

  cbind(head(names(zdf)),
        head(names(sdf)),
        head(names(wdf)))

  # Data unification
  sdf <- sdf %>% dplyr::select(1:21, 27:37)
  zdf <- zdf %>% dplyr::select(1:21,	24:30,	38:41)
  names(sdf) <- names(wdf)[-10]
  names(zdf) <- names(wdf)[-10]

  df <- rbind(
    wdf %>% dplyr::select(-10) %>% dplyr::mutate(Person = "WG"),
    sdf %>% dplyr::mutate(Person = "SPN"),
    zdf %>% dplyr::mutate(Person = "PZ")
  )

  dplyr::glimpse(df)

  # Simple conversions
  df$Date            <- as.POSIXct(df$Date)
  df$Favorite        <- as.logical(df$Favorite)
  df$Distance        <- as.numeric(ifelse(df$Distance > 200,
                                          as.numeric(df$Distance) / 1000,
                                          gsub(",", ".", df$Distance)))
  df$Calories        <- as.numeric(gsub(",", "", df$Calories))
  df$Avg.HR          <- as.numeric(df$Avg.HR)
  df$Max.HR          <- as.numeric(df$Max.HR)
  df$Avg.Run.Cadence <- as.numeric(df$Avg.Run.Cadence)
  df$Max.Run.Cadence <- as.numeric(df$Max.Run.Cadence)
  df$Total.Ascent    <- as.numeric(df$Total.Ascent)
  df$Total.Descent   <- as.numeric(df$Total.Descent)
  df$Number.of.Laps  <- as.numeric(df$Number.of.Laps)
  df$Min.Elevation   <- as.numeric(df$Min.Elevation)
  df$Max.Elevation   <- as.numeric(df$Max.Elevation)

  # Conversions into minutes
  df$Time             <- lubridate::period_to_seconds(lubridate::hms(df$Time))/60
  df$Best.Lap.Time    <- lubridate::period_to_seconds(lubridate::hms(df$Best.Lap.Time))/60
  df$Moving.Time      <- lubridate::period_to_seconds(lubridate::hms(df$Moving.Time))/60
  df$Elapsed.Time     <- lubridate::period_to_seconds(lubridate::hms(df$Elapsed.Time))/60
  df$Surface.Interval <- lubridate::period_to_seconds(lubridate::hms(df$Surface.Interval))/60

  # Speed-tempo conversions
  tempo_to_speed <- function(x) {
    rtn <- as.numeric(
      ifelse(grepl("\\:", x),
             round(60 / (lubridate::period_to_seconds(lubridate::ms(x)) / 60), 1),
             ifelse(grepl("\\.", x),
                    x,
                    NA
             )
      )
    )
  }

  speed_to_tempo <- function(x) {
    rtn <- as.numeric(ifelse(grepl("\\.", x),
                             round(60 / as.numeric(x), 1),
                             ifelse(grepl(":", x),
                                    round(lubridate::period_to_seconds(lubridate::ms(x)) / 60, 1),
                                    NA
                             )
    ))
  }

  df$Avg.Tempo <- speed_to_tempo(df$Avg.Speed)
  df$Min.Tempo <- speed_to_tempo(df$Max.Speed)

  df$Avg.Speed <- tempo_to_speed(df$Avg.Speed)
  df$Max.Speed <- tempo_to_speed(df$Max.Speed)

  # String manipulation
  df <- df %>%
    dplyr::mutate(Activity.Type = stringi::stri_replace_all_fixed(
      Activity.Type,
      c("Kardio", "Kolarstwo", "Bieganie", "Chodzenie", "PĹ‚ywanie w basenie", "WioĹ›larstwo", "Bieg na bieĹĽni", "Bieg przeĹ‚ajowy",
        "Jazda na rowerze treningowym", "SiĹ‚ownia i sprzÄ™t fitness", "Trening siĹ‚owy", "Oddech", "Inne", "Wykryto zdarzenie"),
      c("Cardio", "Cycling", "Running", "Walking", "Swimming", "Rowing", "Treadmill Running", "Trail Running",
        "Indoor Cycling", "Strength training", "Strength training", "Breathing","Other", "Other"),
      vectorize_all = FALSE
    ))

  df <- df %>%
    dplyr::mutate(Title = stringi::stri_replace_all_regex(
      Title,
      c(".*Bieg.*", ".*Run.*" ,"Kardio", ".*Cardio.*",".*SiĹ.*", ".*Strength.*", ".*Spacerowanie.*", ".*Walking.*", ".*Kolarstwo.*",
        ".*Cycl.*", ".*Rugby.*", ".*Hik.*", ".*Golf.*", ".*x.*", "Relaks.*", "PĹ‚ywanie.*", ".*Wykryto zdarzenie"),
      c("Running", "Running","Cardio", "Cardio","Strength training", "Strength training", "Walking", "Walking", "Cycling", "Cycling",
        "Rugby", "Hiking", "Golf", "Intervals", "Relax", "Swimming", "Event found"),
      vectorize_all = FALSE,
      case_insensitive = TRUE
    ))

  df <- df %>%
    dplyr::mutate(Decompression = stringi::stri_replace_all_fixed(Decompression, "Nie", "No"))

  # Excluding close to zero variance and Favorite column
  ival <- apply(df, 2, function(x){
    length(unique(x))
  })

  favourite_column <- df$Favorite
  idx <- which(ival == 1)
  df <- df[, -idx]


# Feature engineering
  # Date vars
  df <- df %>%
    dplyr::mutate(
      Month = lubridate::month(Date),
      Week  = lubridate::week(Date),
      Day   = lubridate::wday(Date, week_start = 1),
      Hour  = lubridate::hour(Date)
    )

  # Team practice
  df <- df %>%
      dplyr::mutate(Team_practice = ifelse(Day %in% c(1:2, 4:5) & Hour %in% c(18, 19), 1, 0)) %>%
      dplyr::mutate(Team_practice = dplyr::case_when(
        Person == "SPN" & Team_practice == 1 & Day < 4 ~ 0,
        Person == "WG" & Date < "2021-03-01" ~ 0,
        T ~ Team_practice
      ))

  #Grouping types and titles
  df <- df %>%
      dplyr::mutate(
        Activity.Type = stringi::stri_replace_all_regex(
          Activity.Type,
          c(".*running.*", ".*cycling*", "Breathing|Swimming|Golf|Joga|Pilates|Rowing|Stoper"),
          c("Running", "Cycling", "Other"),
          vectorize_all = FALSE,
          case_insensitive = TRUE)
      )

  out <- df$Title %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    as.data.frame() %>%
    dplyr::filter(Freq < 10) %>%
    dplyr::pull(1)

  df <- df %>%
    dplyr::mutate(Title = ifelse(Title %in% out, "Other", Title))


# Exploratory Data Analysis
Last but not least, proper analysis where we will by using all accumulated knowledge to present the taking place dependencies. The PDA has been divided into three parts.

* <b>Personal preferences</b> - In first part I will try to explain differences between out athletes that took part in a study. I will illustrated what are favorites of activities of our athletes, when do they like to take them and how often.
* <b>Season vs off season</b> - Secondly we will take a notice at differences in offseason practices in compare to in-season practices. Athletes always emphasize how tough the off season is, we will test it.
* <b>Practice characteristics</b> - Here, we will focus on characterizing most frequent types of practices.Let's find out if some of the are more difficult than the other.

## Personal preferences
Let's kick-off our analysis by looking at types of activity each of the player practice the most. First thing that comes to mind is <b> Why there is no Rugby practices at Wiaan's chart?</b>. Unfortunately sport watches are nowhere near being perfect, if I you don't set practice type manually they seam to miss them quite often. Second assumption which is quite obvious is than Stanislaw and Wiaan are having similar practice distribution. They activities are mostly 'foot oriented', which means that their favorites activities running, walking or in general doing cardio. On the Other side there is Piotr who's place to be is a gym when he spend most of his time doing strength tranings.

```{r message=FALSE, warning=FALSE}
# Jakie treningi robią
df %>%
  dplyr::select(Person, Title) %>%
  dplyr::group_by(Person, Title) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(position = rank(-n)) %>%
  ggplot2::ggplot(aes(Person, n, fill = Title, group = position)) +
  ggplot2::geom_bar(stat = "identity", position = "dodge", col = "black")+
  ggplot2::ggtitle("Number of activities performed by type")+
  ggplot2::scale_fill_manual(values = gg_color_hue(8))
```

<br>Again, I have to clarify some phenom at the beginning, <b> Why is Piotr chart so narrow? Does he only practice from february to september?</b> The answer is no, and the answer is actually quite simple. Piotr has just started using his watch, his is in his first year of collecting data so the record end with publishing date which is september. Even though Piotr set of data is not full we can see some similarities with the Stanislaw's chart. Both of the guys have been playing together for years so that make sens that their practice activities are related. What we can see is that much of work is being done before season starts and in between season, at summer break. In the first part of the season we can see steady decrease in number of practice which is which is caused by load management. Both of the players have also different approach in the final part of the season. Stanislaw likes to use precious time before most important game and squeeze in some extra practices while Piotr is rather taking things slowly. Relaying on form he has been build along the season. Wiaan's chart stands out, most of the time throughout collecting the data he has been playing outside Poland where season is scheduled differently.

```{r}
# Pora roku

my_colors <- colors[c(2, 5, 3, 1)]
names(my_colors) <- c('PZ', 'Season', 'SPN', 'WG')

df %>%
  dplyr::select(Person, Month) %>%
  ggplot2::ggplot(aes(Month))+
  ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_area(aes(fill = Person), stat = "count", alpha = 1, col = "black")+
  ggplot2::facet_grid(Person ~ .)+
  ggplot2::ggtitle("Number of performed activities by month")+
  ggplot2::scale_x_continuous(breaks = 1:12)+
  ggplot2::scale_fill_manual(
    "",
    values = my_colors,
    breaks = c( 'Season'))+
  ggplot2::theme(legend.text = ggplot2::element_text(size = 20))
```

<br> This time we can see more similarities than differences. All three players practice predominantly around 18:00. this seems to be the most obvious answer as this is the most common time for team practice. We start to see the differences when we look at the charts one by one. The second highest value for Wiaan is around 7 o'clock. He is also an early bird out of the three. On the other side, Stanisław is standing, and he starts training the most late among all of them. However, it must be admitted that he can train until late hours, he is the only person who trains regularly after 20. Piotr was in the middle of the stake again, his chart does not stand out from the rest.

```{r, warning=FALSE}
# Jakie godziny
df %>%
  dplyr::select(Person, Hour) %>%
  dplyr::mutate(Hour = substring(Hour, 1, 2)) %>%
  ggplot2::ggplot(aes(factor(Hour, levels = c(0:23)), group = Person, fill = Person)) +
    ggplot2::geom_histogram(stat = "count", position = "dodge") +
    ggplot2::facet_grid(Person ~ .)+
    ggplot2::theme(legend.position = "none")+
    ggplot2::scale_fill_manual(values = colors[c(2, 3, 1)])+
    ggplot2::ggtitle("Number of performed activities by hour of the day")
```

<br>The preferences related to training days again differ significantly from one another. We see three completely different approaches to the training week. Starting with Piotr, I think we can call his graph bimodal. Most trainings are held on Tuesdays, Thursdays and Fridays. Next is Stanislaw, for whom the days from Wednesday to Friday are the most dedicated to training. Finally, there is Wiaan, who likes to start the week hard and then begin to ease off the stresses later in the week. His activity is also the lowest on weekends of all.

```{r}
# W jakie dni
df %>%
  dplyr::select(Person, Day) %>%
  ggplot2::ggplot(aes(Day, group = Person, fill = Person)) +
    ggplot2::geom_density(kernel = "gaussian") +
    ggplot2::facet_grid(Person ~ .) +
    ggplot2::scale_x_continuous(breaks = 1:7,
                       labels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                  "Friday", "Saturday", "Sunday"))+
    ggplot2::theme(legend.position = "none")+
    ggplot2::scale_fill_manual(values = colors[c(2, 3, 1)])+
    ggplot2::ggtitle("Performed activities distribution across the week")
```

<br>By looking at the box plot we can again say it there are so visible similarities between Stanislaw and Wiaan. Both of them spend less time on average each week practicing with whole team. However, what is omitted in team practices is then made up in individual tranings. Their averages values increase drastically, what is impressive the most are extreme values which shows weeks with more than 15 individual practices each. In Piotr's boxplot, we can observe a much better balance between individual and team practices The average value in these two cases is very similar. Usually, his number of individual activities is slightly higher than those with the entire team.

```{r, warning=FALSE, message=FALSE}
# Ile razy w tygodniu (aktywność)
df %>%
  dplyr::select(Person, Week, Team_practice) %>%
  dplyr::group_by(Person, Week, Team_practice) %>%
  dplyr::summarise(n = n()) %>%
  ggplot2::ggplot(aes(Person, n, fill = Person)) +
  ggplot2::geom_boxplot(col = "grey", size = 1.08, alpha = 0.9)+
  ggplot2::scale_fill_manual(values = colors[c(2, 3, 1)])+
  ggplot2::scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2))+
  ggplot2::facet_grid( . ~ Team_practice, labeller = ggplot2::as_labeller(c("0" = "Individual", "1" = "Team")))+
  ggplot2::ggtitle("Box plot of activities durning the week")+
  ggplot2::theme(legend.position = "none")
```


## Season vs off season
How does off season differ from the regular season in terms of the types of activities grown? The first thing to say about rugby players is that they run, a lot. Practically all year round, running trainings constitute a significant percentage of all activities. If you include running, cycling and cardio as endurance training, you can safely say that it is the most important feature of a Rugby player. This seems to destroy the popular opinion that rugby is a sport for strongmen straight from the gym. Strength training is a small part of your overall preparation and gets suppressed before the season ends. Rugby training is practically absent at the end of the season where the main training goal seems to be increasing physical performance. The peak of rugby training takes place during the summer period, i.e. the finals of the national championship games. At the end of the year, when the season is over or has already ended, we see a sudden but temporary decrease in the amount of running training. The players then use this time for less demanding activities, such as walking.
```{r}
# Jakie
ord <- dimnames(table(df$Title))[[1]]

df %>%
  dplyr::select(Month, Title) %>%
  ggplot2::ggplot(aes(Month, fill = Title))+
  ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_bar(position = "fill", col = "black", alpha = 0.95)+
  ggplot2::scale_x_continuous(breaks = 1:12)+
  ggplot2::scale_y_continuous(limits = c(-0.1, 1.1))+
  ggplot2::scale_fill_manual("",
                             values = c(gg_color_hue(8), "grey"),
                             breaks = c(ord, "Season"))+
  ggplot2::theme(axis.text.y = ggplot2::element_blank())+
  ggplot2::ggtitle("Performed activities by months")
```

<br>When the activities of athletes take up the most time on average? Summer is again the most active period, the average duration of activity during this time fluctuates around 40 minutes.The exception to the rule here is July, which is a transition month between the end of the season and preparation for the next one. The length of training increases month by month from February to June. When analyzing the chart, you can also notice a clear difference between the length of training in both offsets. As the winter break is much longer than the summer training session, the training sessions are usually short, except in December and January.However, in a short summer break in activity, they achieve the greatest duration.The same amount of work should be done in a much shorter period of time making the ggplot2::units very long.

```{r, message=FALSE, warning=FALSE}
# Czy dluzsze
col <- c(gg_color_hue(12), "grey")
names(col) <- c(1:12, "Season")

plot <- df %>%
  dplyr::select(Month, Time) %>%
  ggplot2::ggplot(aes(Month, Time, group = Month, fill = factor(Month)))+
  ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_boxplot()+
  ggplot2::scale_x_continuous(breaks = 1:12)+
  ggplot2::scale_y_continuous(limits = c(0, 150))+
  ggplot2::scale_fill_manual("",
                             breaks = "Season",
                             values = col)+
  ggplot2::ggtitle("Duration of activities by months")


table <- df %>%
  dplyr::select(Month, Time) %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(Q1 = quantile(Time, 0.25),
                   Q2 = quantile(Time, 0.5),
                   Q3 = quantile(Time, 0.75)) %>%
  dplyr::ungroup() %>%
  round(., 3)


table <- table %>%
  t %>%
  .[-1,] %>%
  as.data.frame()

names(table) <- 1:12


theme <- gridExtra::ttheme_default(
  core = list(padding = ggplot2::unit(c(17, 8), "mm"),
              fg_params = list(fontsize = 15)
  ),
  colhead = list(padding = ggplot2::unit(c(10, 8), "mm"),
                 fg_params = list(fontsize = 20)
  ),
  rowhead = list(padding = ggplot2::unit(c(10, 8), "mm"),
                 fg_params = list(fontsize = 20)
  )
)

tbl <- gridExtra::tableGrob(table, theme = theme)

gridExtra::grid.arrange(plot, tbl,
                        nrow = 2, heights = c(5, 2))
```

<br> And how does this relate to the severity of training? To measure the effective weight of training, we will use three indicators: average heart rate, calories and maximum heart rate. In each of the three cases, a steady increase in value can be observed from the beginning of the spring round until its end. The reduction of training loads during the summer break is evident. Interestingly, the next phase of growth takes place around the middle of the autumn round, while the maximum HR starts to increase only after the round is over. One of the possible causes is that the body gets used to the training rhythm imposed on the current season. Average HR from March to April and September to October appear to be of similar heights. While the caloric supply is definitely lower when comparing these two periods. This can mean a good body adaptation to low and medium-intensity training.

```{r, message=FALSE, warning=FALSE}
col <- colors[c(2, 3, 1, 5)]
names(col) <- c("Avg.HR", "Calories", "Max.HR", "Season")

df %>%
  dplyr::select(Month, Avg.HR, Max.HR, Calories) %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(Avg.HR = mean(Avg.HR, na.rm = TRUE),
                   Max.HR = mean(Max.HR, na.rm = TRUE),
                   Calories = mean(Calories, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  tidyr::gather(key = "Var",
                value = "Value",
                -Month) %>%
  ggplot2::ggplot(aes(Month, Value))+
  ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_smooth(aes(col = Var, fill = Var))+
  ggplot2::facet_wrap(~Var, scales = "free_y")+
  ggplot2::scale_fill_manual(values = col)+
  ggplot2::scale_colour_manual("", values = col, breaks = "Season")+
  ggplot2::guides(fill = "none")+
  ggplot2::ggtitle("Average training difficulty")
```

<br>So far, we have indicated the differences in training taking place in the summer and during the break. We found out when, what and how hard the players train. So it remains to check the difference in the number of trainings. At first glance, you can see that the charts presented below have a different distribution. The greatest number of individual activities takes place at the beginning of the year and basically from that moment statically decreases up to the end of the year where it reaches its minimum. The exception here is August, where preparation for the autumn round forces increased activity among our athletes. The team training graph to the right is similar to the binomial distribution. It reaches the highest values in the first months of both rounds and then it starts to decline consistently.

```{r}
# Czy czestsze
col <- colors[c(2, 3, 5)]
names(col) <- c(0, 1, "Season")

df %>%
  dplyr::select(Month, Team_practice) %>%
  count(Team_practice, Month) %>%
  ggplot2::ggplot(aes(Month, n, fill = factor(Team_practice)))+
  ggplot2::geom_rect(aes(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_rect(aes(xmin = 8, xmax = 11, ymin = -Inf, ymax = Inf, fill = "Season"), alpha = 0.04)+
  ggplot2::geom_col(col = "black")+
  ggplot2::scale_x_continuous(breaks = 1:12)+
  ggplot2::scale_fill_manual("",
                             values = col,
                             breaks = "Season")+
  ggplot2::facet_wrap(Team_practice~.,
                      labeller = ggplot2::as_labeller(c("0" = "Individual", "1" = "Team")),
                      scales = "free_y")+
  ggplot2::theme(legend.position = "none")+
  ggplot2::ggtitle("Number of activities performed by month and type")


```


## Practice characteristics
As the last part of our analysis I am going to describe each type of activity with the help of below table:
  ```{r, message=FALSE, warning=FALSE, fig.height=17}
data <- df %>%
  dplyr::filter(Title != "Other") %>%
  dplyr::select("Title", "Distance", "Calories", "Time", "Avg.HR", "Max.HR",
                "Avg.Speed" , "Max.Speed") %>%
  dplyr::filter(Max.Speed < 90 | is.na(Max.Speed)) %>%
  dplyr::group_by(Title) %>%
  dplyr::summarise_all(mean, na.rm = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(vars(-Title), round, 2) %>%
  dplyr::arrange(desc(Title)) %>%
  dplyr::select(Title, Avg.HR, Avg.Speed, Calories,
                Distance, Max.HR, Max.Speed, Time) %>%
  dplyr::mutate(Title = ifelse(Title == "Strength training", "Strength", Title))

p <- data %>%
  tidyr::gather("var", "value", -Title) %>%
  dplyr::arrange(var, desc(value)) %>%
  ggplot2::ggplot(aes(Title, value, fill = var))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(.~var,
                      ncol = 3,
                      scales = "free_x"
  )+
  ggplot2::scale_fill_manual(values = colors[rep(c(2, 3, 1), 3)])+
  ggplot2::coord_flip()+
  ggplot2::theme(legend.position = "none")

gp <- ggplot2::ggplotGrob(p)

gp0 <- gtable::gtable_add_grob(
  x = gp,
  grobs = gridExtra::tableGrob(
    data,
    theme = gridExtra::ttheme_default(
      base_size = 20,
      core = list(padding = ggplot2::unit(c(5, 12), "mm"))),
    rows = NULL),
  t = 16,
  l = 10,
  b = 21,
  r = 12,
  name = "")
grid::grid.draw(gp0)

```
<br><h4>Walking</h4><br>
  Walking, as you can guess, belongs to the easiest training category. Most of the parameters oscillate around the lower values. The exception is the average speed achieved by athletes during this type of training. It exceeds the average speeds achieved during hiking activities and Rugby training. Walking is also characterized by the shortest average activity time. Keep in mind that sports watches automatically record an activity as a workout, even in situations where it shouldn't. It may therefore happen that for example morning shopping has been classified as full-fledged activity.

<br><h4>Strength training</h4><br>
Strength training is not a simple task to characterize. From the point of view of the indicators used by us, they are not distinguished by anything special. This activity is characterized by a moderate pace of training, which has a direct impact on the heart rate. In addition, in combination with a not too long duration of training, it results in one of the lower average calories burned during the unit.


<br><h4>Running</h4><br>
Running is the first activity that can be described as difficult. Both the maximum and average heart rate reach their maximum values for the entire study. However, because these are also quite short trainings, this translates into a small amount of calories burned. This relationship can also be seen by paying attention to the bar describing the training time, which is again one of the shortest. This is probably due to one of the highest speeds achieved by athletes during any activity. This may mean that the running category is primarily short-distance sprint runs.

<br><h4>Rugby</h4><br>
Rugby is another activity that can give you a hard time. These trainings last specifically long and are characterized by a high level of calories burned. The reason for this seems to be hidden in the specs of this activity. As you can see in the graph above, during rugby training, sportsmen achieve low to average values of average heart rate and average speed. However, it is interspersed with high values for maximum heart rate and speed. This may indicate that this training is mainly conducted in the form of interval training which can be described as changeable tempo training.

<br><h4>Hiking</h4><br>
If we decide that the difficulty of training is the calories burned, then hiking is an unbeatable winner. However, this is not due to the dizzying speeds achieved during this activity which are rather low, if not the lowest. The factors influencing the effectiveness of training in this case are time, and distance. The average hike takes twice as long as the rest of the workouts. During this time, athletes cover much longer distances than during rugby training, walking or running, for example.

<br><h4>Cycling</h4><br>
Another calorie-burning workout is cycling. In this case, however, the recipe for success is completely different than for hiking. In this training, the athletes cover more than three times longer distances than in the second longest activity. Moreover, they do it while maintaining the highest average and maximum speed. This is reflected in both the average and maximum heart rate which reach very high values.


<br><h4>Cardio</h4><br>
Last but not least there are cardio classes activities. Which, strangely enough, are characterized by the shortest distance traveled. This seems to read the basic premise behind this type of training. However, it should be remembered that sports watches are not able to examine the distance traveled on tredmills or stationary cycling stations. Therefore, the information about distance should be taken with a grain of salt. Moreover, as you would expect from cardio workouts, they are quite balanced. Both in terms of speed and heart rate they are in the middle of the field.
