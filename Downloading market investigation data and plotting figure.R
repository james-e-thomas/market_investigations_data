rm(list = ls())

library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)

setwd("C:\\Users\\james\\Documents\\Market Investigations")                                         

# --------------------------------------------------------------------------- #

### Getting links to case pages --------------------------------------------- #

page1 <- read_html("https://www.gov.uk/cma-cases?case_type%5B%5D=markets&page=1")

links_to_cases1 <- page1 %>%
  html_nodes(".gem-c-document-list__item-link") %>%
  html_attr("data-ecommerce-path")

page2 <- read_html("https://www.gov.uk/cma-cases?case_type%5B%5D=markets&page=2")

links_to_cases2 <- page2 %>%
  html_nodes(".gem-c-document-list__item-link") %>%
  html_attr("data-ecommerce-path")

links_to_cases <- append(links_to_cases1, links_to_cases2)
rm(links_to_cases1, links_to_cases2)


### Getting data on cases --------------------------------------------------- #

master <- data.frame()

for (i in 1:length(links_to_cases)) {
  
  data <- read_html(paste0("https://www.gov.uk", links_to_cases[i]))
  
  case <- data %>%
    html_nodes(".govuk-heading-l") %>%
    html_text("govuk-heading-l")
  case <- str_trim(case)
  
  text1 <- data %>%
    html_nodes(".app-c-important-metadata__term") %>%
    html_text("app-c-important-metadata__term")
  text1 <- gsub(":", "", text1)
  text1 <- str_trim(text1)
  text1 <- c("Case", text1, "Link")
  
  text2 <- data %>%
    html_nodes(".app-c-important-metadata__definition") %>%
    html_text("app-c-important-metadata__definition")
  text2 <- c(case, text2, paste0("https://www.gov.uk", links_to_cases[i]))
  
  text <- as.data.frame(matrix(text2, ncol = length(text2)))
  colnames(text) <- text1
  
  master <- bind_rows(master, text)
  
}

rm(data, page1, page2, text, case, i, links_to_cases, text1, text2)


### Cleaning data ----------------------------------------------------------- #

master$Opened <- as.Date(master$Opened, format = "%d %b %Y")
master$Closed <- as.Date(master$Closed, format = "%d %b %Y")

master$"Market sector" <- str_trim(master$"Market sector")

# Filling in some missing data manually
master <- mutate(master, Opened = if_else(Case == "Electric vehicle charging market study", "2 December 2020", master$Opened))
master <- mutate(master, Closed = if_else(is.na(master$Close), today(), master$Close))
master <- mutate(master, Closed = if_else(master$Case == "'Loyalty penalty' super-complaint", as.Date("2018-12-19"), master$Close))
master <- mutate(master, Closed = if_else(master$Case == "Statutory audit market study", as.Date("2019-04-18"), master$Close))
master <- mutate(master, Closed = if_else(master$Case == "Review of the legal services market study in England and Wales", as.Date("2020-12-17"), master$Close))

# Simplifying the many market sectors of the misleading environmental claims case
master <- mutate(master, "Market sector" = if_else(master$Case == "Misleading environmental claims", "Various", master$"Market sector"))

# Creating a label for graph
master$Label <- paste0("<a style=\"color:#425563\";\" href=\"", master$Link, "\">", master$Case, "</a>")

# These pages are not actually cases
master <- filter(master, Case != "OFT / CC closed cases" & Case != "Markets cases before 1 April 2014")

master <- arrange(master, desc(Opened))

save(master, file = "market investigations.RData")


### Plotting graph ---------------------------------------------------------- #

# Choose colours
nera <- c('#469990', '#339900', '#e8e8e8', '#8f939f', '#002e63', '#cc0000',
          '#ffcc00', '#ffdf97', '#0056a6', '#b2c7e2', '#d28a7c', '#fabed4',
          '#f7c088', '#7a0c19', '#425563', '#be941b', '#000000', '#ff9900',
          '#aeca95')

# There is probably a more concise way of doing some parts of this, but it is
# bit fiddly as gantt charts aren't actually a proper plotly graph type
fig <- plot_ly() %>%
  config(displayModeBar = FALSE) %>%
  layout(yaxis = list(showline = F,
                      showticklabels = F,
                      showgrid = F,
                      range = c(1, nrow(master$Case) - 1),
                      fixedrange = T),
         xaxis = list(tickformat = "%Y",
                      dtick = "M12",
                      range = c(as.Date("2001-10-01"), today()),
                      fixedrange = T),
         shapes = list(type = "line",
                       x0 = as.Date("2014-04-01"), x1 = as.Date("2014-04-01"),
                       y0 = 0, y1 = nrow(master),
                       line = list(color = "#74a1ce",
                                   dash = "dot")),
         font = list(family = "Verdana",
                     color = "#425563")) %>%
  add_annotations(x = as.Date("2014-05-01"),
                  y = nrow(master)-2,
                  text = "CMA takes over <br>from OFT/CC",
                  align = "left",
                  xref = "x",
                  yref = "y",
                  xanchor = "left",
                  showarrow = F,
                  font = list(size = 10,
                              color = "#74a1ce"))

for(i in 1:nrow(filter(master, Opened > as.Date("2017-06-01")))){
  fig <- add_trace(fig,
                   type = "scatter",
                   x = c(master$Opened[i], master$Closed[i]),  # x0, x1
                   y = c(i, i),  # y0, y1
                   mode = "lines",
                   color = master$"Market sector"[i],
                   colors = nera,
                   line = list(width = 5),
                   showlegend = F,
                   texttemplate = master$Case[i],
                   textposition = "outside",
                   hoverinfo = "text",
                   text = paste(paste("<b>", master$Case[i], "</b>"), "<br>",
                                master$"Market sector"[i])
  ) %>%
    add_annotations(x = master$Opened[i],
                    y = i,
                    text = master$Label[i],
                    xref = "x",
                    yref = "y",
                    xanchor = "right",
                    showarrow = F,
                    font = list(size = 7))
  
}

for(i in (nrow(filter(master, Opened > as.Date("2017-06-01")))+1):(nrow(master) - 1)){
  fig <- add_trace(fig,
                   type = "scatter",
                   x = c(master$Opened[i], master$Closed[i]),  # x0, x1
                   y = c(i, i),  # y0, y1
                   mode = "lines",
                   color = master$"Market sector"[i],
                   colors = nera,
                   line = list(width = 5),
                   showlegend = F,
                   texttemplate = master$Case[i],
                   textposition = "outside",
                   hoverinfo = "text",
                   text = paste(paste("<b>", master$Case[i], "</b>"), "<br>",
                                master$"Market sector"[i])
  ) %>%
    add_annotations(x = master$Closed[i],
                    y = i,
                    text = master$Label[i],
                    xref = "x",
                    yref = "y",
                    xanchor = "left",
                    showarrow = F,
                    font = list(size = 7))
  
}

fig
