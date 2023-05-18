library("tidyverse")

df <- read_csv(readr_example("mtcars.csv"))
head(df)
is_tibble(df)
str(df)
colnames(df)

dat = df %>% select(wt,cyl,mpg) %>% 
              mutate(cyl = as.character(cyl)) %>%
              filter(cyl==4) %>%
              mutate(wt = wt*100) %>%
              mutate(CPW = mpg/wt)
dat
is.tibble(dat)

d = mtcars %>% select(wt,cyl,mpg) %>% 
                mutate(cyl = as.character(cyl)) %>%
                filter(cyl==4)%>%
                mutate(wt = wt*100) %>%
                mutate(CPW = mpg/wt)

d
is.tibble(d)
