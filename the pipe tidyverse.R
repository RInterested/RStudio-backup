library("tidyverse")

df <- read_csv(readr_example("mtcars.csv"))
head(df)
is_tibble(df)
str(df)
colnames(df)

dat = df %>% rownames_to_column("model") %>%
              select(model,wt,cyl,mpg) %>% 
              mutate(cyl = as.character(cyl)) %>%
              filter(cyl == 4) %>%
              mutate(wt = wt*100) %>%
              mutate(CPW = mpg/wt) %>%
              arrange(desc(mpg))
dat
is.tibble(dat)


d = mtcars %>% rownames_to_column("model") %>%
                select(model,wt,cyl,mpg) %>% 
                mutate(cyl = as.character(cyl)) %>%
                filter(cyl==4)%>%
                mutate(wt = wt*100) %>%
                mutate(CPW = mpg/wt) %>%
                arrange(desc(mpg))

d
is.tibble(d)
