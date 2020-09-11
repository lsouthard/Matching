# Matching
Options for creating a matched sample
This code is to show examples of different ways you can procure a matched sample for analysis using tidyverse. 

## The hard match:
**When to use:**
* you have a lot of data
* you want to isolate an effect, but have a lot of first-level variance

**How to prepare data:**
* You will need to have a grouping variable that's converted into numeric values. 
* You will need all of your matching variables to also be numeric. 
* GenMatch cannot run if there are any NA's in your dataframe.
* See my respirotry munging for more information. 


**How to do the match:**
Here is the entire code you will need. I've chosen to use comments next to each line to explain what's happening in the code.
```
library(Matching)

match.ready = GenMatch(data$group, #define your grouping variable 
                               Match.ready%>% #pipe in dataframe
                                 #You can do some manipulating of variables for the match that you don't
                                 #want saved in your dataframe here. 
                                 mutate(
                                 #if you haven't releveled your variables you can use case_when
                                   matchvar1 = case_when(matchvar1 == "level1" ~ 1,
                                                         matchvar2 == "level2" ~ 2,
                                                         T ~ 2), 
                                  #if your var only has 1 important level. Example = gender                               
                                   ,matchvar2 = ifelse(matchvar2 == "level1", 1, 0)
                                   #if you like the way your variables are leveled you can do this:
                                   matchvar3 = as.numeric(as.factor(matchvar3)))%>%
                                   #select all variables you want to match on
                                 dplyr::select(matchvar1, matchvar2, matchvar3,
                                               matchvar4, matchvar5, matchvar6),
                               #each true/false matches 1/0, respectively
                               #each one represents a parameter for each of the 6 variables
                               #see documentation on ?Matching for more information
                               exact = c(T,T,T,
                                         T,F,F),
                               caliper = c(0,0,0,
                                           0,1,1),
                               replace = F, ties = F) 

```
**How to pull out matched individuals:**
First you will define a new dataframe called matched.df. 
You want to bind it to your original dataframe so you can keep all your old information. 
You can do this with the bind_rows() function. 
Pull anyone with a unique match using the indexes. 
```
matched.df = bind_rows(
  Match.ready[unique(matched.students$matches[,1]),],
  Match.ready[unique(matched.students$matches[,2]),]
)
```
Now you have a dataframe called matched.df that you can use for analysis!


## The Fuzzy Match:
**How to prepare data**
Separate your grouping variable into two columns: I usually use yes and no.
You can do this a number of ways. Here's one:
1. Separate into two dataframes based on group
2. Create a new variable for each
3. Bind them back together
```
  yes <- data %>% 
    dplyr::select(c("id.yes")) %>% 
    mutate(Group = 1) %>% 
    rename(id = "id.yes") %>% 
    distinct_all()
  no <- data %>% 
    dplyr::select(c("id.no")) %>% 
    mutate(Group = 0) %>% 
    rename(id = "id.no") %>% 
    distinct_all()
  all.matched <- rbind(yes, no)
```
Now we are going to do a fuzzy join:
```
library(fuzzyjoin)
matched.df <- matched.df%>% 
  # Start with the group of interest
  filter(Group == 1) %>%
  inner_join(matched.df %>%
               filter(Group == 0),
             by = c("matchvar1", "matchvar2", "matchvar3", "matchvar4", "matchvar5", "matchvar6")) %>%
  # Filter out rows that don't meet our criteria for fuzzy matching.
  # if you are matching on a numeric value where you don't need to have an exact match you can use the code with abs()
  # an example of this is age. If it doesn't matter that much that one person is 38.88 years old vs 38.22
  # manipulate the <= 1 to make it more or less granular or you can bin your data before you do the match
  filter(abs(matchvar1.x - matchvar1.y) <= 1 & 
    abs(matchvar2.x - matchvar2.y) <= 1) %>% 
  # Keep IDs. Used in filter later
  dplyr::select(id.group = ID.yes, matchvar1.yes = matchvar1.x, 
                matchvar2.yes = matchvar2.x, 
                id.no = ID.y, 
                matchvar1.no = matchvar1.y,
                matchvar2.no = matchvar2.y)

# acceptable matches.
matched.terms.df = students.raw.df %>%
  semi_join(bind_rows(matches.df %>%
                        dplyr::select(ID = IDsacm, GPA = GPAsacm #, ft = term.code.SACM
                        ),
                      matches.df %>%
                        dplyr::select(ID = IDnot, GPA= GPAnot #, ft = term.code.not
                        )),
            by = c("ID")) 
```


## How to Assess your Match:
For any of the types of matches you want to do, you can create a visualization to see how "good" of a match you have. 
```
library(ggplot2)
ntiles = seq(0, 1, 0.01),
matched.df%>%
  mutate(Group = ifelse(Group == 1, "Yes", "No"),
         matchvar1 = as.numeric(as.factor(matchvar1)),
         matchvar2 = as.numeric(as.factor(matchvar2)),
         matchvar3 = as.numeric(as.factor(matchvar3)),
         matchvar4 = as.numeric(as.factor(matchvar4)),
         matchvar5 = as.numeric(as.factor(matchvar5)),
         matchvar6 = as.numeric(as.factor(matchvar6))
         ) %>%
  dplyr::select(Group, matchvar1, matchvar2, matchvar3,
                matchvar4, matchvar5, matchvar6) %>%
  gather(predictor, value, -Group) %>%
  mutate(predictor = case_when(predictor == "matchvar1" ~ "Matched Variable 1",
                                predictor == "matchvar2" ~ "Matched Variable 2",
                                 predictor == "matchvar3" ~ "Matched Variable 3" )) %>%
  group_by(predictor, Group) %>%
  summarize(ntile = list(ntiles),
            ntile.value = list(quantile(value, ntiles))) %>%
  unnest(cols = c(ntile, ntile.value)) %>%
  ungroup() %>%
  dplyr::select(ntile, Group, predictor, ntile.value) %>%
  spread(Group, ntile.value) %>%
  ggplot(aes(x = No, y = TW)) +
  geom_point(position = "jitter", alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~ predictor, ncol = 2, scales = "free") +
  scale_x_continuous("No") +
  scale_y_continuous("Yes")
  ```
  This will produce a figure that will roughly look like this. 
  
  ## Things to consider:
* How many people did you lose from your group of interest? For example, if you had 500 people in your treatment group you want your match to return as many of those people as possible. 
* If you are losing a lot of people or getting a paramertization error, you likely need to reduce the number of variables you are trying to match on. 
* The larger the population of your 'controls', the more you options you can match on. 
