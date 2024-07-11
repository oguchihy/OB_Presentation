library(tidyverse)
library(widyr)

store_list <- read_csv("https://raw.githubusercontent.com/zahiernasrudin/datasets/main/sample_store.csv")
# Perform k-means clustering using widely_kmeans

tst_wdr <- ob_data_chr %>%
  mutate(delivery_date = as.Date(delivery_date),
         week_start = floor_date(delivery_date, "week")) %>%
  group_by(week_start, zip) %>%
  summarise(deliveries = sum(event, na.rm = TRUE)) %>%
  ungroup()



cluster_group <-  store_list %>%
  widely_kmeans(item = storecode, 
                feature = year, 
                value = sales,
                k = 3)

cluster_group <-  tst_wdr %>%
  widely_kmeans(item = zip, 
                feature = week_start, 
                value = deliveries,
                k = 3)



# Join the clustering results back to the original data
store_list_with_cluster <- left_join(store_list, cluster_group)

tst_with_zip_cluster <- left_join(tst_wdr, cluster_group)

library(ggthemes)

# store_list_with_cluster |> 
#   ggplot(aes(x = year, y = sales, group = storecode, colour = cluster)) +
#   geom_line(show.legend = F) +
#   scale_y_continuous(labels = scales::comma) +
#   facet_wrap(vars(cluster)) +
#   scale_color_solarized()


tst_with_zip_cluster  %>%  
  ggplot(aes(x = week_start, y = deliveries, group = zip, colour = cluster)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(vars(cluster)) +
  scale_color_solarized()









cluster_group <-  store_list %>%
  widely_kmeans(item = storecode, 
                feature = year, 
                value = sales,
                k = 3)

# Join the clustering results back to the original data
store_list_with_cluster <- left_join(store_list, cluster_group)

library(ggthemes)

store_list_with_cluster |> 
  ggplot(aes(x = year, y = sales, group = storecode, colour = cluster)) +
  geom_line(show.legend = F) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(vars(cluster)) +
  scale_color_solarized()

