# Matching
This code is to show examples of different ways you can procure a matched sample for analysis using tidyverse. 

## Why Match?
If you want to isolate an effect and compare groups, we need to get rid of any other noise. For example, say you have two people. One person gets tutored and the other does not, then they both take the same test. We want to know if the tutoring sessions were effective. We cannot simply compare the the test grades of these two individuals. We have to _control_ for person-level variables that may be responsible for the different test scores in order to isolate the effectiveness of the tutoring session. Some person-level differences that may influence a person's ability to perform on this test may include prior knowledge, GPA, time since they were a student, etc. We rarely can isolate an effect (that's why it's so important to have good research methodology, but we don't always get that option). Instead, we can approximate with a few different types of matching:

1. Hard Match 
2. Loose Match

You can also use the following, which are not discussed in this file.

3. Propensity Score Matching
4. Alternative: Stratified samples

I will go through each of these and how to perform them in R. 

**What to match on:**
This really is specific to your buisness problem or research question. However, I have described my approach below:
My approach, especially when working with a lot of data, is to start with looking at the variables I _might_ want to match on.
I will use the code below to count how many people I have per group per combination of the levels of each potential matching varaible. 
```
data %>%
group_by(group, matchvar1, matchvar2, matchvar3) %>% 
summarise(n=n()) %>%  
ungroup() 
```
If I notice that there are very few people in my __control__ group that have certain combinatons of levels then it'll be hard to match them to my __target__ group. 
This is especially true if there are more people in the __target__ group with a certain combination than the __control__ group, unless you can use replacement. Using replacement is usually specific to your hypothesis. 

Let's make this more concrete: say we are matching on gender and ethnicity. 

The breakdown of your groups looks like this:

|Group    | Gender | Ethnicity | Count 
| :---:   | :-:    | :-:       |  :-: |
| Yes | F | White | 290  
| Yes | F | Not White | 90  
| Yes | M | White | 286 
| Yes | M | Not White | 86 
| Yes | U | White  | 95 
|_Yes_ | _U_ | _Not White_  | _99_
| No | F | White | 1190  
| No | F | Not White | 980  
| No | M | White | 1086 
| No | M | Not White | 986 
| No | U | White  | 995 
| **No** | **U** | **Not White**  | **9** 

The last row suggests that there aren't many controls that have this particular combination of varaibles. It will be impossible to match the 99 people in the target group with the same combination of variables (italicized) without using replacement. I may consider condensing my Unknown Gender group into Female or Male in this case or not using a Gender at all. 

## 1. The Hard Match:
The hard or strict match looks at each person from a target group and finds someone most like them. For example, if one person in your target group is female, hispanic, age 22, this process iterates through each person in the control group until a match is found. This rigorous matching criterion creates a comparison group that is solely comprised of people that are as similar as possible to those in your target group.

**How to prepare data:**
* You will need to have a grouping variable that's converted into numeric values. 
* You will need all of your matching variables to also be numeric. 
* GenMatch cannot run if there are any NA's in your dataframe.
* The more people you have in your control, the higher likihood you can match and you can match on more variables! 
* See my repository munging for more information. 

**How to do the match:**
Here is the entire code you will need. I've chosen to use comments next to each line to explain what's happening in the code.
I use the [GenMatch](https://www.rdocumentation.org/packages/Matching/versions/4.9-7/topics/GenMatch) function as follows: 
```
matches.df = GenMatch(data$group, 
                   data[,c("var.1", "var.2", "var.3")], 
                   exact = c(T, F, F), 
                   caliper = c(0, 1, 0.5), 
                   replace = T, ties = F) 
```

However, this is what my code typically ends up looking like: 
```
library(Matching)

matches.df = GenMatch(data$group, #define your grouping variable 
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
                                         F,F,F),
                               caliper = c(0,1,.5,
                                           0,1,.5),
                               replace = F, ties = F) 

```
**How to pull out matched individuals:**
First you will define a new dataframe called matched.df. 
You want to bind it to your original dataframe so you can keep all your old information. 
You can do this with the bind_rows() function. 
Pull anyone with a unique match using the indexes. 
```
matches.df = bind_rows(
                        data[unique(maches.df$matches[,1]),],
                        data[unique(matches.df$matches[,2]),]
                       )
```

Another way to do it is like this:
```
library(dplyr) 
library(tibble) 

data = data %>% 
  rownames_to_column("row.id") %>% 
  filter(row.id %in% matches$matches[,1:2]) 
```

Now you have a dataframe called matched.df that you can use for analysis!

## The Loose Match:
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
Now we are going to do a loose match:
```
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

![MatchedDataViz](https://user-images.githubusercontent.com/51967620/92962151-c68c2380-f42d-11ea-8cbc-d9b5d6eb3856.png)

 A perfectly matched set will follow x=y line. Let's pretend that the first variable is age. We have Group == "Yes" on the y-axis and Group == "No" on the x-axis. This would mean that the people in Group == "Yes" are systematically _older_ than those in Group == "No".

## Things to consider:
* How many people did you lose from your group of interest? For example, if you had 500 people in your treatment group you want your match to return as many of those people as possible. 
* If you are losing a lot of people or getting a parameterization error, you likely need to reduce the number of variables you are trying to match on. 
* The larger the population of your 'controls', the more options you can match on. 
