

abundance_plot_isolate <- function(species_name, data, bar_color, genus_species) {
  plot <- data |>
    filter(host_species == species_name) |>
    mutate(subset = ifelse(rel_abund > 0.01, "Abundant","Rare (<1% of Total Reads)")) |>
    #Reorder asv by relative abundance to get a smooth curve
    ggplot(aes(x = reorder(asv, -rel_abund), y = rel_abund, fill = color)) +
    geom_text(aes(label = significant), 
              position = position_dodge(width = .9), hjust = -0.2, vjust = .7, size = 20 / .pt) +
    geom_col() +
    coord_flip() +
    facet_wrap( ~ subset, scales = "free_x") +
    theme_pubclean() +
    scale_fill_manual(values = c(bar_color, "black"), name =  '', labels = c("ASVs with Cultured Representatives from Phycosphere", "ASVs not Cultured from Phycosphere")) +
    guides(fill = guide_legend(
      title = substitute(paste(italic(genus_species))),
      title.position = "top",
      nrow = 2, byrow = TRUE
    )) +
    labs( y = "Relative Abundance (ASV)", x = "Amplicon Sequence Variant", fill = '',
          title = "") 
  
  return(plot)
}

abundance_plot_combined <- function(species_name, data, data2, bar_color, genus_species) {
  plot1 <- data |>
    filter(host_species == species_name) |>
    #Reorder asv by relative abundance to get a smooth curve
    ggplot(aes(x = reorder(asv, -rel_abund), y = rel_abund, fill = class)) +
    scale_fill_brewer() +
    geom_text(aes(label = significant), 
              position = position_dodge(width = .9), hjust = -0.2, vjust = .7, size = 20 / .pt) +
    geom_col() +
    coord_flip() +
    theme_pubclean() +
    ylim(0.00,0.21) +
    labs( y = "", x = "Amplicon Sequence Variant", fill = '',
          title = "Phycosphere of a Green Alga 3 Days Post Introduction to Pond Water") 
  
  plot2 <- data2 |>
    filter(host_species == species_name) |>
    #Reorder asv by relative abundance to get a smooth curve
    ggplot(aes(x = reorder(asv, -rel_abund), y = rel_abund, fill = class)) +
    scale_fill_brewer() +
    geom_text(aes(label = significant), 
              position = position_dodge(width = .9), hjust = -0.2, vjust = .7, size = 20 / .pt) +
    geom_col() +
    coord_flip() +
    theme_pubclean() +
    ylim(0.00,0.21) +
    labs( y = "Relative Abundance (ASV)", x = "Amplicon Sequence Variant", fill = '',
          title = "Phycosphere of a Green Alga 31 Days Post Introduction to Pond Water") 
  
  patchwork <- (plot1 / plot2) 
  
  final_plot = patchwork +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = 'A') & theme(legend.position = "bottom")
    
  return(final_plot)
}

#Colors used for phylum classification
phyla.colors <- c(Acidobacteria = "grey26", 
                  Actinobacteria = "palevioletred2", 
                  Alphaproteobacteria = "steelblue4", 
                  Armatimonadetes = "red", 
                  Bacteroidetes = "darkorange", 
                  "BD1-5" = "chartreuse", 
                  Betaproteobacteria = "royalblue", 
                  Caldiserica = "black",
                  "WS1" = "red", 
                  "WPS-2" = "aquamarine1", 
                  "Candidate_division_OD1" = "#6DDE88", 
                  "Candidate_division_OP3" = "yellow1", 
                  "Candidate_division_OP8" = "goldenrod1", 
                  "Candidate_division_OP11" = "chocolate4", 
                  "Candidate_division_SR1" = "tan3", 
                  "Candidate_division_TM7" = "skyblue1", 
                  "Candidate_division_WS3" = "magenta", 
                  "Candidate_Division_CK_1C4_19" = "darkseagreen", 
                  "Candidate_Division_NPL_UPA2" = "darkmagenta", 
                  "Candidate_Division_RF3" = "bisque", 
                  "Candidate_Division_TA06" = "cadetblue",  
                  "Candidate_Division_TM6" = "plum", 
                  Chlamydiae="violet", 
                  Chlorobi="cyan2", 
                  Chloroflexi="darkgreen", 
                  Crenarchaeota = "khaki1", 
                  phycobacteria = "chartreuse3", 
                  Deferribacteres = "slateblue3", 
                  "Deinococcus-Thermus" = "violetred", 
                  Dictyoglomi = "cornsilk4", 
                  Deltaproteobacteria = "deepskyblue", 
                  "Deinococcus-Thermus" = "lightskyblue1", 
                  Elusimicrobia = "violetred4",
                  Epsilonproteobacteria = "lightskyblue", 
                  Euryarchaeota = "khaki2", 
                  Fibrobacteres = "hotpink", 
                  Firmicutes = "blue4", 
                  FGL7S = "palevioletred1", 
                  Fusobacteria = "slateblue1",
                  Euglenozoa = "#652926", 
                  Gammaproteobacteria = "plum2", 
                  Gemmatimonadetes="black", 
                  GOUTA4 = "plum1", 
                  "Hyd24-12" = "sienna2",
                  Ignavibacteriae = "sienna2", 
                  JTB23 = "seashell2", 
                  Lentisphaerae = "cyan4", 
                  "MVP_21" = "lightskyblue1", 
                  Nitrospirae = "yellow1", 
                  "NPL-UPA2"="#652926", 
                  Hydrogenedentes = "mediumpurple4", 
                  Planctomycetes = "mediumorchid3", 
                  Proteobacteria = "deepskyblue", 
                  Retaria ="lightsalmon3", 
                  Kiritimatiellaeota = "lightskyblue2", 
                  Patescibacteria = "orangered", 
                  Spirochaetes = "gold2", 
                  Spirochaetae = "gold3", 
                  Synergistetes = "olivedrab1", 
                  Tenericutes="pink", 
                  Thaumarchaeota = "khaki3", 
                  BRC1 = "chocolate1", 
                  Halanaerobiaeota = "lightsalmon3",
                  Dependentiae = "rosybrown3", 
                  TM6 = "olivedrab", 
                  unclassified = "grey", 
                  Verrucomicrobia = "purple4", 
                  Epsilonbacteraeota = "palegreen", 
                  Bacteria_unclassified = "darkgrey")

#Random color generator function. 
get_random_grid_colors <- function(ncolor,seed = sample_seq(1, 1000000, 1)) {
  require(uniformly)
  set.seed(seed)
  print(seed)
  ngrid <- ceiling(ncolor^(1/3))
  x <- seq(0,1,length=ngrid+1)[1:ngrid]
  dx <- (x[2] - x[1])/2
  x <- x + dx
  origins <- expand.grid(x,x,x)
  nbox <- nrow(origins) 
  RGB <- vector("numeric",nbox)
  for(i in seq_len(nbox)) {
    rgb <- runif_in_cube(n=1,d=3,O=as.numeric(origins[i,]),r=dx)
    RGB[i] <- rgb(rgb[1,1],rgb[1,2],rgb[1,3])
  }
  index <- sample(seq(1,nbox),ncolor)
  RGB[index]
} 