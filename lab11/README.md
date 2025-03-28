Lab 11 - Interactive Visualization
================
Rayce Ramsay

# Learning Goals

- Read in and process Starbucks data.
- Create interactive visualizations of different types using `plot_ly()`
  and `ggplotly()`.
- Customize the hoverinfo and other plot features.
- Create a Choropleth map using `plot_geo()`.

# Lab Description

We will work with two Starbucks datasets, one on the store locations
(global) and one for the nutritional data for their food and drink
items. We will do some text analysis of the menu items.

# Deliverables

Upload an html file to Quercus and make sure the figures remain
interactive.

# Steps

### 0. Install and load libraries

### 1. Read in the data

- There are 4 datasets to read in, Starbucks locations, Starbucks
  nutrition, US population by state, and US state abbreviations. All of
  them are on the course GitHub.

``` r
sb_locs <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-locations.csv")

sb_nutr <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-menu-nutrition.csv")

usa_pop <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/us_state_pop.csv")

usa_states<-read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/states.csv")
```

### 2. Look at the data

- Inspect each dataset to look at variable names and ensure it was
  imported correctly.

``` r
head(sb_locs)
head(sb_nutr)
head(usa_pop)
head(usa_states)
```

### 3. Format and merge the data

- Subset Starbucks data to the US.
- Create counts of Starbucks stores by state.
- Merge population in with the store count by state.
- Inspect the range values for each variable.

``` r
sb_usa <- sb_locs |> filter(Country == "US")

sb_locs_state <- sb_usa |>
  group_by(`State/Province`) |>
  rename("Abbreviation" = `State/Province`) |>
  summarize(store_count = n())

# need state abbreviations
usa_pop_abbr <- usa_pop |> 
  rename("State" = "state") |> 
  full_join(usa_states, by = "State")

sb_locs_state <- usa_pop_abbr |> 
  full_join(sb_locs_state, by = "Abbreviation")

# Drop NA cols (it is USA territories such as Guam and Puerto Rico)
sb_locs_state <- sb_locs_state |> drop_na()

summary(sb_locs_state)
```

### 4. Use `ggplotly` for EDA

Answer the following questions:

- Are the number of Starbucks proportional to the population of a state?
  (scatterplot)

- Is the caloric distribution of Starbucks menu items different for
  drinks and food? (histogram)

- What are the top 20 words in Starbucks menu items? (bar plot)

``` r
p1 <- ggplot(sb_locs_state, aes(x = population, y = store_count)) +
  geom_point(aes(text = paste("State:", Abbreviation)), color = "blue", alpha = 0.6) +
  geom_smooth(formula = 'y ~ x', method = "lm", color = "red", se = FALSE) +
  labs(title = "Starbucks Stores vs. State Population",
       x = "State Population",
       y = "Number of Starbucks Stores") +
  theme_minimal()

ggplotly(p1)
```

- 4a) Answer: This plot suggests that, in general, the number of
  Starbucks locations in a state is proportional to its population.
  There are, however, a few states that have an unproportionate amount.

``` r
p2 <- ggplot(sb_nutr, aes(x = Calories, fill = Category)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("red", "steelblue")) +
  labs(title = "Caloric Distribution of Starbucks Menu Items",
       x = "Calories",
       y = "Count",
       fill = "Category") +
  theme_minimal()

ggplotly(p2)
```

- 4b) Answer: The distributions shapes are quite similar (both appear
  approximately Normal), but the calorie distribution for food exhibits
  a greater variance. Additionally, the mean of the food distribution is
  larger than the mean of the drinks distribution.

``` r
top_words <- sb_nutr |>
  unnest_tokens(word, Item) |>
  count(word, sort = TRUE) |>
  anti_join(stop_words, by = "word") |>
  slice_max(n, n = 20)

p3 <- ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Top 20 Words in Starbucks Menu Items",
       x = "Word",
       y = "Count") +
  theme_minimal()

ggplotly(p3)
```

- 4c) Answer: The top 3 most frequently occurring words are “iced,”
  “tazo,” and “bottled.” They mostly describe multiple items rather than
  one specific product.

### 5. Scatterplots using `plot_ly()`

- Create a scatterplot using `plot_ly()` representing the relationship
  between calories and carbs. Color the points by category (food or
  beverage). Is there a relationship, and do food or beverages tend to
  have more calories?

``` r
plot_ly(data = sb_nutr, 
              x = ~Calories, 
              y = ~`Carb. (g)`, 
              type = 'scatter', 
              mode = 'markers', 
              color = ~Category,
              colors = c("Food" = "red", "Drinks" = "steelblue"),
              marker = list(size = 8)) |> 
  layout(title = "Calories vs Carbs in Starbucks Menu Items",
         xaxis = list(title = "Calories"),
         yaxis = list(title = "Carbs (g)"),
         legend = list(title = list(text = "Category")))
```

- 5a) Answer: There does appear to be a positive linear relationship
  between calories and carbs. Additionally, food generally contains more
  calories, whereas beverages tend to have a higher carbohydrate content
  per calorie.

- Repeat this scatterplot but for the items that include the top 10
  words. Color again by category, and add hoverinfo specifying the word
  in the item name. Add layout information to title the chart and the
  axes, and enable `hovermode = "compare"`.

- What are the top 10 words and is the plot much different than above?

``` r
top_words <- top_words |> 
  slice_max(n, n=10)

top_words_pattern <- paste0("(?i)", paste(top_words$word, collapse = "|"))

sb_nutr_filtered <- sb_nutr |>
  filter(str_detect(Item, top_words_pattern)) |> 
  mutate(highlighted_word = str_extract(Item, top_words_pattern))

plot_ly(data = sb_nutr_filtered, 
              x = ~Calories, 
              y = ~`Carb. (g)`, 
              type = 'scatter', 
              mode = 'markers', 
              color = ~Category,
              colors = c("Food" = "red", "Drinks" = "steelblue"),
              text = ~paste("Item:", Item, "<br>Word:", highlighted_word), 
              hoverinfo = "text",
              marker = list(size = 8)) |> 
  layout(title = "Calories vs Carbs for Items with Top 10 Words",
         xaxis = list(title = "Calories"),
         yaxis = list(title = "Carbs (g)"),
         hovermode = "compare",
         legend = list(title = list(text = "Category")))
```

- 5b) Answer: This scatterplot shows the same trends as before: drinks
  generally have far fewer calories on average but contain significantly
  more grams of carbohydrates per calorie compared to food.

### 6. `plot_ly` Boxplots

- Create a boxplot of all of the nutritional variables in groups by the
  10 item words.
- Which top word has the most calories? Which top word has the most
  protein?

``` r
sb_nutr_long <- sb_nutr_filtered |>
  pivot_longer(cols = c(Calories, `Fat (g)`, `Carb. (g)`, `Protein (g)`, `Fiber (g)`), names_to = "Nutrient", values_to = "Value")

plot_list <- list()

colours <- c("Calories" = "red", 
             "Fat (g)" = "blue",
             "Carb. (g)" = "green", 
             "Protein (g)" = "purple", 
             "Fiber (g)" = "orange")

nutrients <- names(colours)

for (nutrient in nutrients) {
  p <- plot_ly(data = sb_nutr_long |> filter(Nutrient == nutrient), 
               x = ~highlighted_word, 
               y = ~Value, 
               type = "box",
               name = nutrient,
               marker = list(color = colours[[nutrient]])) |> 
    layout(title = paste("Distribution of", nutrient, "by Top 10 Words"),
           xaxis = list(title = "Top 10 Frequent Words in Item Name"),
           yaxis = list(title = paste(nutrient, "Value")),
           boxmode = "group")
  
  plot_list <- append(plot_list, list(p))
}

subplot(plot_list, nrows = length(nutrients), shareX = TRUE, titleX = TRUE)
```

- 6)  Answer: Items containing the words “protein” and “tea” appear to
      have the highest calorie counts. Similarly, these items also tend
      to have the most grams of protein.

### 7. 3D Scatterplot

- Create a 3D scatterplot between Calories, Carbs, and Protein for the
  items containing the top 10 words
- Do you see any patterns (clusters or trends)?

``` r
plot_ly(data = sb_nutr_filtered, 
              x = ~Calories, 
              y = ~`Carb. (g)`, 
              z = ~`Protein (g)`, 
              color = ~Category,  
              colors = c("steelblue", "red"),
              type = "scatter3d", 
              mode = "markers",
              marker = list(size = 6, opacity = 0.8),
              text = ~paste("Item:", Item, "<br>Word:", highlighted_word), 
              hoverinfo = "text") %>%
  layout(title = "3D Scatterplot: Calories, Carbs, and Protein",
         scene = list(
           xaxis = list(title = "Calories"),
           yaxis = list(title = "Carbs"),
           zaxis = list(title = "Protein")
         ))
```

- 7)  Answer: All three of these nutrition values show a positive
      correlation, generally increasing together, except for a few
      drinks that contain no protein.

### 8. `plot_ly` Map

- Create a map to visualize the number of stores per state, and another
  for the population by state. Add custom hover text. Use subplot to put
  the maps side by side.
- Describe the differences if any.

``` r
# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('steelblue')
)

# Make sure both maps are on the same color scale
shadeLimit <- 125

# Create hover text
sb_locs_state$hover <- with(sb_locs_state, paste("Number of Starbucks: ", store_count, '<br>', "State: ", Abbreviation, '<br>', "Population: ", population))

# Create the map
map1 <- plot_geo(sb_locs_state, locationmode = 'USA-states') |> 
  add_trace(
    z = ~store_count,
    locations = ~Abbreviation,
    text = ~hover,
    hoverinfo = "text",
    colorscale = 'Reds',
    zmin = 0, zmax = max(sb_locs_state$store_count, na.rm = TRUE)
  ) |> 
  layout(
    title = "Number of Starbucks Stores per State",
    geo = set_map_details
  )

map1


map2 <- plot_geo(sb_locs_state, locationmode = 'USA-states') |> 
  add_trace(
    z = ~population,
    locations = ~Abbreviation,
    text = ~hover,
    hoverinfo = "text",
    colorscale = "Reds",
    zmin = 0, zmax = max(sb_locs_state$population, na.rm = TRUE)
  ) |> 
  layout(
    title = "Population per State",
    geo = set_map_details
  )

map2

subplot(map1, map2)
```

- 8)  Answer: The two maps reveal a similar pattern, where states with
      larger populations typically have more Starbucks locations, as
      shown by the dark shading in California, Texas, and Florida.
      However, there are notable differences. Although high-population
      states tend to have more Starbucks stores, the distribution is not
      strictly proportional. For example, some densely populated states,
      like New York, have comparatively fewer locations, whereas less
      populated states, such as Washington (home to Starbucks’
      headquarters), have a higher concentration of stores relative to
      their population.
