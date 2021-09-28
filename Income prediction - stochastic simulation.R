################################################################################
# Title:  Income prediction with stochastic simulation
# Date:   2020-03-25
# Technologies:
#     * R
#     * Html (Kaggle only)
#     * CSS  (Kaggle only)
# Libraries:
#   * ggridges
#   * ggthemes
#   * ggpubr
#   * gganimate
#   * tidyverse
#   * knitr
#   * kableExtra
#   * assertthat
# Tasks:
#   * Monte Carlo simulation
# Exteral link:
#   * https://www.kaggle.com/radbys/activities-in-rugby-exploratory-data-analysis
################################################################################

# Environment settings
# Loading libraries
library(ggridges)
library(ggthemes)
library(ggpubr)
library(gganimate)
library(tidyverse)
library(knitr)
library(kableExtra)
library(assertthat)

# Plot options
theme_set(theme_fivethirtyeight())

theme_update(
  text = element_text(size = 20),
  plot.title = element_text(size = 26, face = "bold"),
  strip.text = element_text(size = 20, face = "bold", colour = "black"),
  strip.background = element_rect(fill = "darkgrey"),
  legend.key.size = unit(0.5, "cm"),
  legend.key.width = unit(0.5, "cm")
)

colors <- c("#0a1128", "#001f54", "#aa8f66", "#ffff82")

# Preparation
# Assumptions
w <- c(500, 1000, 1500) # Possible rent cost in three scenarios
k <- 0:4 # Possible number of clients for each day
c <- 30:50 # Cost per client
d <- 20:26 # Number of working days per month

# Propability-price df
co <- 0.8 # Percentage of regular clients
na <- 1 - co # Percentage of new clients
k4 <- c(0.01, 0.01, 0.3, 0.45, 0.3) # Probability of daily demand
dp <- c(0.3, 0.2, 0.15, 0.1, 0.1, 0.1, 0.05) # Probability of working days in month

pp <- data.frame(
  method = c("1to1", "2D", "3D", "4D", "5to7D", "8to10D"), # Method names
  co_prob = c(0.05, 0.2, 0.4, 0.25, 0.1, 0) * co, # Method probability by regulars
  na_prob = c(0.05, 0.2, 0.4, 0.25, 0.1, 0) * na, # Method probability by new clients
  co_price = c(100, 125, 140, 150, 160, 180), # Prices for regular clients
  na_price = c(130, 140, 160, 180, 200, 220) # Prices for new clients
)

pre <- pp
names(pre) <- c("method", "Regulars_prob", "New_clients_prob", "Regulars_prices", "New_clients_prices")

data.frame(pre) %>%
  kable() %>%
  kable_styling()

# User-made function

##########################################################################
#                               Disclaimer                               #
##########################################################################
# calc function has been recently vectorized.
# You can still view it raw for-loop form at kaggle's code section.

# rep: number of sampling iterations (default: 1000)
# wc:  rent cost (values: 500:1500, no default)
# dm:  working days in month (values: 20:26, default: sample)
# cc:  client cost (values: 30:50, default: sample)
# kk:  number of clients per day (values: 0:4, default: sample)

calc <- function(rep = 1000, wc, dm, cc, kk) {

  # Input validation
  assertthat::assert_that(
    wc %in% 500:1500,
    (missing(dm) || dm %in% 20:26),
    (missing(cc) || cc %in% 30:50),
    (missing(kk) || kk %in% 0:4)
  )

  # Sample data
  # Working days in a month
  m_Day <- if (missing(dm)) {
    message("Sampling number of working days per month")
    sample(d, rep, prob = dp, replace = TRUE)
  } else {
    rep(dm, rep)
  }

  # Clients in a month
  m_Cl <- if (missing(kk)) {
    message("Sampling number of clients per month")
    sapply(seq(m_Day), function(x) {
      sum(sample(k, size = m_Day[x], replace = TRUE, prob = k4))
    })
  } else {
    kk * m_Day
  }

  # Clients costs in a month
  co_Cl <- if (missing(cc)) {
    message("Sampling client cost")
    m_Cl * sample(c, 1)
  } else {
    m_Cl * cc
  }

  # Main Body
  # Costs
  cos <- wc + co_Cl

  # Income
  inc <- sapply(seq(m_Cl), function(x) {
    sum(sample(
      c(pp$co_price, pp$na_price),
      size = m_Cl[x],
      replace = TRUE,
      prob = c(pp$co_prob, pp$na_prob)
    ))
  })

  # True income
  inc <- (inc - cos) * 0.81

  # Return
  data.frame(
    "Rent" = wc,
    "Work_day" = m_Day,
    "Cl_Mon" = m_Cl,
    "Cl_Cost_Mon" = cos,
    "Income" = inc,
    "incCat" = ifelse(inc < 3000,
      "<3000",
      ifelse(inc > 5000,
        ">5000",
        "3000<x<5000"
      )
    )
  )
}


# Computation
# Scenarios
sce <- data.frame(
  Name = c("Min1", "Min2", "Min3", "Sim1", "Sim2", "Sim3", "Max1", "Max2", "Max3"),
  Rent = rep(c(500, 1000, 1500), 3),
  Working_day = c(rep(20, 3), rep("random", 3), rep(26, 3)),
  Client_cost = c(rep(50, 3), rep("random", 3), rep(40, 3)),
  Clients_per_day = c(rep(2, 3), rep("random", 3), rep(4, 3))
)

# Calculations
op <- rbind(
  # Minimal scenario
  # Rent: 500,1000,1500 - Working day: 20 - Client cost: 50 - Clients Per Day: 2
  calc(wc = 500, dm = min(d), cc = max(c), kk = 2),
  calc(wc = 1000, dm = min(d), cc = max(c), kk = 2),
  calc(wc = 1500, dm = min(d), cc = max(c), kk = 2),
  # Maximal scenario
  # Rent: 500,1000,1500 - Working day: 26 - Client cost: 40 - Clients Per Day, max: 4
  calc(wc = 500, dm = max(d), cc = min(c), kk = 4),
  calc(wc = 1000, dm = max(d), cc = min(c), kk = 4),
  calc(wc = 1500, dm = max(d), cc = min(c), kk = 4),
  # Stochastic symulation
  calc(wc = 500),
  calc(wc = 1000),
  calc(wc = 1500)
)

op$type <- factor(c(rep("Min", 3000), rep("Max", 3000), rep("Sim", 3000)),
  levels = c("Min", "Sim", "Max")
)

me <- op %>%
  group_by(type, Rent) %>%
  summarise(
    Mean_Monthly_Client_Cost = round(mean(Cl_Cost_Mon), 2),
    Mean_Monthly_Income = round(mean(Income), 2)
  )

data.frame(sce, me[, 3:4]) %>%
  kable() %>%
  kable_styling()


# Visualization
threshold <- 4500

ggplot(op, aes(y = factor(Rent))) +
  geom_density_ridges(
    aes(
      x = Income,
      fill = type,
      alpha = .8
    )
  ) +
  labs(
    title = "Income distribution",
    subtitle = "due to rent value and scenario issued"
  ) +
  scale_x_continuous("Income", labels = seq(0, 10000, by = 1000), breaks = seq(0, 10000, by = 1000)) +
  scale_y_discrete("Rent") +
  scale_alpha(guide = "none") +
  geom_vline(xintercept = threshold, linetype = "solid", color = "red", size = 1.5, alpha = 0.5) +
  annotate("text",
           x = threshold + 600,
           y = 4.5,
           label = paste(threshold, "PLN"),
           size = 6,
           col = "red",
           fontface = "bold",
           alpha = 0.5) +
  scale_fill_manual(values = c("#0a1128", "#001f54", "#aa8f66"))

op %>%
  group_by(Rent, type) %>%
  mutate(n = row_number()) %>%
  ungroup() %>%
  ggplot(aes(n, Income, group = type, fill = type, col = type), alpha = 0.8)+
  geom_line()+
  facet_grid(~Rent)+
  scale_color_manual(values = c(colors[2], "darkgrey", colors[3]))+
  labs(title = "Income line plot",
       subtitle = "Income samples plotted against income values grouped by rent value")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  theme(axis.text.x = element_blank())+
  transition_reveal(n)


  # Updating assumptions
  k  <- as.vector(c(0:3))    #Updated possible number of clients for each day
  k4 <- k4[1:4]+(k4[5]/4)    #Updated probability of daily demand
  d  <- c(20:21)             #Updated number of working days per month
  dp <- c(1,0)               #Updated probability of working days in month

  # Updating price-df
  pp <- data.frame(
    method = c("1to1", "2D", "3D", "4to6D", "6to8D"),                      #Method names
    co_prob = c(0.05, 0.2, 0.4, (0.25 +  (0.1/3) + (0.1/3)),(0.1/3))*co,   #Method probability by regulars
    na_prob = c(0.05, 0.2, 0.4, (0.25 +  (0.1/3) + (0.1/3)),(0.1/3))*na,   #Method probability by new clients
    co_price = c(130, 130, 150, 170, 190),                                 #Prices for regular clients
    na_price = c(130, 160, 180, 220, 240)                                  #Prices for new clients
  )

  new <- calc(wc = 800)

  p1 <- ggplot(new, aes(Income))+
    geom_density(alpha = 0.5, fill = "#0a1128")+
    labs(title = "Income distribution",
         subtitle = "for the offer")+
    scale_alpha(guide = "none")+
    scale_y_continuous("Density",labels=scales::percent)+
    scale_x_continuous("Income", limits = c(min(new$Income)-200, max(new$Income)+200))+
    geom_vline(xintercept = mean(new$Income), linetype="dotted", color = "black", size=1.5)+
    geom_vline(xintercept = threshold, linetype="solid", color = "red", size=1.5, alpha = 0.5)+
    annotate("text", x = mean(new$Income)-600, y=0.00055, label = paste(round(mean(new$Income),2), "PLN"), size = 6, col = "black",fontface = "bold")+
    annotate("text", x = threshold+600, y=0.0008, label = paste(threshold, "PLN"), size = 6, col = "red",fontface = "bold", alpha = 0.7)+
    theme(legend.position = "none")

  p2 <- new %>%
    ggplot(aes(Income))+
    geom_line(aes(y = 1 - ..y..), stat='ecdf', size = 1.5, alpha = 0.4)+
    labs(title = "Cumulative distribution",
         subtitle = "for the offer")+
    scale_y_continuous("Probability", labels = scales::percent_format())+
    scale_x_continuous("Income", limits = c(min(new$Income)-200, max(new$Income)+200))+
    geom_vline(xintercept = threshold, linetype="solid", color = "red", size=1.5, alpha = 0.5)+
    annotate("text", x = threshold+600, y=1, label = paste(threshold, "PLN"), size = 6, col = "red",fontface = "bold", alpha = 0.7)

  ggarrange(p1, p2)
