# Create simple example plots for vignettes
library(ggplot2)
library(dplyr)

# Create workflow diagram data
workflow_steps <- data.frame(
  step = 1:4,
  function_name = c("gna_verifier_pq()", "tax_gbif_occur_pq()", 
                   "tax_globi_pq()", "tax_info_pq()"),
  description = c("Verify Names", "Add GBIF Data", 
                 "Add Interactions", "Add Traits"),
  x = c(1, 2, 3, 4),
  y = c(1, 1, 1, 1)
)

# Basic workflow visualization
workflow_plot <- ggplot(workflow_steps, aes(x = x, y = y)) +
  geom_point(size = 12, color = "steelblue", alpha = 0.7) +
  geom_text(aes(label = step), color = "white", size = 6, fontface = "bold") +
  geom_text(aes(label = description, y = y - 0.15), size = 3) +
  geom_segment(data = workflow_steps[-4,], 
               aes(x = x + 0.1, y = y, xend = x + 0.9, yend = y),
               arrow = arrow(length = unit(0.2, "cm")), size = 1,
               color = "darkgray") +
  xlim(0.5, 4.5) + ylim(0.7, 1.3) +
  labs(title = "taxinfo Workflow",
       subtitle = "Basic steps for enriching phyloseq objects") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

ggsave("figures/workflow-diagram.png", workflow_plot, 
       width = 8, height = 3, dpi = 150)

# Example data completeness plot
data_sources <- c("GBIF", "Wikipedia", "GLOBI", "Traits", "OpenAlex")
completeness <- c(85, 60, 45, 70, 40)

completeness_data <- data.frame(source = data_sources, pct = completeness)

completeness_plot <- ggplot(completeness_data, aes(x = reorder(source, pct), y = pct)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Data Source Completeness",
       subtitle = "Percentage of taxa with available information",
       x = "Data Source", y = "Completeness (%)") +
  theme_minimal()

ggsave("figures/data-completeness.png", completeness_plot,
       width = 6, height = 4, dpi = 150)

# Example occurrence pattern
set.seed(123)
occurrence_example <- data.frame(
  taxon = paste("Species", LETTERS[1:10]),
  local = rpois(10, 15),
  global = rpois(10, 500) + 100
) %>%
  mutate(taxon = reorder(taxon, global))

occurrence_plot <- ggplot(occurrence_example, aes(x = taxon)) +
  geom_col(aes(y = global), fill = "lightgray", alpha = 0.7) +
  geom_col(aes(y = -local), fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Example: Global vs Local Occurrences",
       subtitle = "Global (gray) and local (blue, negative scale) occurrence counts",
       x = "Taxon", y = "Number of occurrences") +
  theme_minimal()

ggsave("figures/occurrence-example.png", occurrence_plot,
       width = 7, height = 5, dpi = 150)

cat("Figure files created successfully!\n")