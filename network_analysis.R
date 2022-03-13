# Preparing for Network Analysis------------------------------------------------

## Create node list (nodes)
sources <- faculty %>% 
        distinct(home_category) %>%
        rename(label = home_category)

destinations <- faculty %>%
        distinct(job_category) %>%
        rename(label = job_category)

nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>% rowid_to_column("id")

## Create edge list (edges)            
per_route <- faculty %>%
        filter(home_category != job_category) %>%
        group_by(home_category, job_category) %>%
        summarize(weight = n()) %>%
        ungroup()

edges <- per_route %>%
        left_join(nodes, by = c("home_category" = "label")) %>%
        rename(from = id)

edges <- edges %>%
        left_join(nodes, by = c("job_category" = "label")) %>%
        rename(to = id)

edges <- select(edges, from, to, weight)