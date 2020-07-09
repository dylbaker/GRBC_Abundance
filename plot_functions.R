

abundance_plot_isolate <- function(species_name, data, color_data, bar_color, genus_species) {
  plot <- data %>%
    filter(host_species == species_name) %>%
    #Reorder genus by average relative abundance to get a smooth curve
    ggplot( aes(x = genus, y = avg_rel_abund, fill = color)) +
    geom_bar(stat = "Identity") +
    theme_minimal() +
    scale_fill_manual(values = c(bar_color, "black"), name =  '', labels = c("Cultured Isolate", "Not Isolated")) +
    guides(fill = guide_legend(
      title = substitute(paste(italic(genus_species))),
      title.position = "top",
      nrow = 2, byrow = TRUE
    )) +
    labs( y = "Relative Abundance (Genus)", x = "", fill = '',
          title = "") +
    scale_y_continuous("Average Relative Abundance", limits = c(0, 0.32), breaks = c(0.0, 0.1, 0.2, 0.3)) +
    theme(axis.title.x = element_text(size = 12),
          axis.text.x = element_text(angle = 70, hjust = 1, size = 14,
                                     color = color_data$color),
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "bottom",
          legend.box.just = "center",
          legend.direction = "vertical",
          legend.title = element_text(vjust = .5, size = 14),
          legend.text = element_text(vjust = .5, size = 12),
          plot.title = element_text(hjust = .5, size = 16)) +
    scale_x_reordered()
  
  return(plot)
}