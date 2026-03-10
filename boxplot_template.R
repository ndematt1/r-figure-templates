##################################################################################################################################################

#Clear the environment
rm(list = ls())

#Import the libraries & apply the libraries 
pkgs <- c("ggplot2", "boot", "psych","ggforce","rempsyc","ggpubr") 
lapply(pkgs, library, character.only = TRUE)

#Enter path here
path<- ""

#Load csv with all the different concentrations
df_tot <- read.csv(path)


#Apply the filters for the required graph to the df
df_filtered <- df_tot[df_tot$Sample != "Sample A" &
                        df_tot$Sample != "Sample B" &
                        df_tot$Sample != "Sample C",]


#Choose the data from you want to use
df <- df_filtered 

#Assign levels to the df, so the graph is ordered
df$Sample <- factor(
  df$Sample,
  levels = c("Sample A","Sample B", "Sample C", "Sample D", "Sample D","Sample E", "Sample D", "Sample F", "Sample G","Sample H"))
##################################################################################################################################################
#Enter the names of the samples
sample_colors <- c(
  "Sample A" = "#F8766D",
  "Sample B" = "#D89000",
  "Sample C" = "#7CAE00",
  "Sample D" = "#00BE67",
  "Sample E" = "#00BFC4",
  "Sample F" = "#00A9FF",
  "Sample G" = "#C77CFF",
  "Sample H" = "#FF61C3",
  "Sample I" = "#8B4513",
  "Sample J" = "#FFC073",
  "Sample K" = "#FF3D00",
  "Sample L" = "#2e4057"
) 
#Write the colors the the samples

##################################################################################################################################################

samples <- levels(droplevels(df$Sample)) #Check the unique names of the samples

my_comparisons <- combn(samples, 2, simplify = FALSE) 
#Generates a combination of all elements;
#False returns it in a list
#This allows for me to automatically create all the combination for the statistical comparisons


y_max <- max(df$Values)
#get the maximum y-value

#Get the difference of y-max and y-min
data_range <- diff(range(df$Values))

#Assign step increase value
step_increase <- .1

#Calculate the height of the graph based on the step increase of the significance bars
max_height <- y_max + length(my_comparisons) * step_increase * data_range

##################################################################################################################################################
box <- ggplot(df, aes(x=Sample, y=Values, fill=Sample)) + 
  geom_boxplot(width=.75, alpha = .60)+ 
  geom_sina(alpha = .70, size = 1.5, aes(color = Sample) )  +
  scale_fill_manual(values = sample_colors) +
  scale_color_manual(values = sample_colors) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", step.increase = step_increase)+ 
  stat_compare_means(label.y = max_height) +     
  labs(title="Enter Title",x="Enter X-axis", y = "Enter Y axis") 

box + theme_classic() + theme(legend.position='none') + theme(plot.title = element_text(hjust = 0.5)) 


########################################
#Bibliography:

#(1)Add P-values and Significance Levels to ggplots - Articles - STHDA. Sthda.com. https://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/.
#(2)ggplot2 violin plot : Quick start guide - R software and data  visualization. Sthda.com. https://www.sthda.com/sthda/RDoc/figure/ggplot2/ggplot2-violin-plot-logo-data-visualization-1.png (accessed 2026-03-10).
