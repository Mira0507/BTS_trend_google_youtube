library(tidyverse)
library(ggplot2)
library(broom)
library(data.table)

h <- head
s <- summary   
g <- glimpse   
t <- tail

# usg: google US trend
# wdg: google world trend
# usy: youtube US trend
# wdy: youtube world trend
usg <- fread("time-us.csv")
wdg <- fread("time-world.csv")
usy <- fread("time-youtube-us.csv")
wdy <- fread("time-youtube-world.csv")

# data cleaning
name_columns <- function(df) {
        names(df) <- c("Date", "Trend")
        return(df)
}

usg1 <- name_columns(usg)
wdg1 <- name_columns(wdg)
usy1 <- name_columns(usy)
wdy1 <- name_columns(wdy)

trend <- usg1 %>%
        full_join(wdg1, by = "Date", suffix = c("_gus", "_gwd")) %>%
        full_join(usy1, by = "Date") %>%
        full_join(wdy1, by = "Date", suffix = c("_yus", "_ywd")) %>%
        mutate(Date = as.Date(Date))

names(trend) <- c("Date", "Google_US", "Google_World", "Youtube_US", "Youtube_World")

trend1 <- gather(trend, Website, Trend, -Date)

# regression models 
mod_usg <- lm(Google_US ~ Date, data = trend)
mod_wdg <- lm(Google_World ~ Date, data = trend)  
mod_usy <- lm(Youtube_US ~ Date, data = trend)
mod_wdy <- lm(Youtube_World ~ Date, data = trend)

stats <- trend1 %>%
        nest(-Website) %>%
        mutate(models = map(data, ~ lm(Trend ~ Date, .))) %>% 
        mutate(tidied = map(models,tidy)) %>% 
        unnest(tidied)

stats1 <- stats %>%
        filter(term == "Date")



# plots by time

merged_line_plot <- 
        ggplot(trend1, aes(x = Date, y = Trend, color = Website)) + 
        geom_line(size = 1) + 
        theme(panel.background = element_blank(),
              axis.line = element_line("black"),
              axis.text = element_text(size = 10, color = "black")) +
        ggtitle("Google and Youtube Trend of BTS for Recent Five Years") + 
        xlab("Year")
        
        

facet_line_plot <- ggplot(trend1, aes(x = Date, y = Trend, color = Website)) + 
        geom_line(size = 1) + 
        facet_grid(.~Website) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") + 
        theme(axis.text.x = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line("black"), 
              axis.text.y = element_text(size = 10, color = "black")) + 
        xlab("Year") + 
        ggtitle("Individual Trend Increase of BTS over Time")

# plots for relationship between websites

scatter_plot_function <- function(df, pointcolor, xcol, ycol, title, xtitle, ytitle) {
        ggplot(df, aes(x = xcol, y = ycol)) + 
                geom_jitter(size = 2, alpha = 0.4, color = pointcolor) +
                geom_smooth(method = "lm", se = FALSE, color = "blue") +
                theme(panel.background = element_blank(),
                      axis.line = element_line("black"), 
                      axis.text = element_text(size = 10, color = "black")) +
                ggtitle(title) + 
                xlab(xtitle) + 
                ylab(ytitle)
}

youtube_w_vs_youtube_us_scatterplot <-
        scatter_plot_function(trend,
                      "#993300",
                      trend$Youtube_World,
                      trend$Youtube_US, 
                      "Relationship of BTS' trends on Youtube World and Youtube US",
                      "Youtube World Trend",
                      "Youtube US Trend")

google_w_vs_google_us_scatterplot <-
        scatter_plot_function(trend,
                              "#006600",
                              trend$Google_World,
                              trend$Google_US, 
                              "Relationship of BTS' trends on Google World and Google US",
                              "Google World Trend",
                              "Google US Trend")

google_w_vs_youtube_w_scatterplot <-
        scatter_plot_function(trend,
                              "#FF3300",
                              trend$Google_World,
                              trend$Youtube_World, 
                              "Relationship of BTS' trends on Google World and Youtube World",
                              "Google World Trend",
                              "Youtube World Trend")

google_us_vs_youtube_us_scatterplot <-
        scatter_plot_function(trend,
                              "#FF3399",
                              trend$Google_US,
                              trend$Youtube_US, 
                              "Relationship of BTS' trends on Google US and Youtube US",
                              "Google US Trend",
                              "Youtube US Trend")

# correlation coefficient
cor(trend$Google_US, trend$Youtube_US)
cor(trend$Google_World, trend$Youtube_World)
cor(trend$Google_US, trend$Google_World)
cor(trend$Youtube_US, trend$Youtube_World)
