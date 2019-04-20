library(tidyverse)

# get vector of states and add DC alphabetically
states <- state.abb
which(states == "DE")
states <- append(states, "DC", after = 7)


# Create vector of file names
mortvect <- c()
for (i in states) {
  mortvect <- c(mortvect, paste0("sources/mort/", i, "_bltper_5x1.csv"))
}


# render length and list of file names
lst <- vector("list", length(mortvect))
names(lst) <- mortvect


# loop
le <- tibble()
for (i in 1:length(mortvect)) {
  tmp <- read_csv(mortvect[i]) %>%
    filter(Year == 2012 & Age == 0 | Year == 2016 & Age == 0) %>% 
    select(PopName, Year, ex) %>% 
    rename(st = PopName,
           year = Year,
           lexp = ex)
  le <- rbind(le, tmp)
}
rm(tmp)
