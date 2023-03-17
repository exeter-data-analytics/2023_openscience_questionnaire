# make some plots to get some insights from the Reproducibility Questionnaire we ran in 2023

#----------#
# setup ####
#----------#

# load in packages
librarian::shelf(tidyverse, likert, scales, padpadpadpad/BrewerUoE, showtext, tm, wordcloud, SnowballC)

# load in the dataset to begin with ####
d <- read_csv('data/reproducible_science_questionnaire_2023.csv')

colnames(d)

# the output from the data download is not great

# make a stacked likert plot
# borrowing much from the code here:
# https://scisley.github.io/articles/ggplot-likert-plot/

# check what fonts are available
showtext_auto()
showtext::showtext_opts(dpi = 300)
sysfonts::font_families()

# add Exeter font
# need to make sure cairo_pdf is enabled as the ggsave device for this to work: 
# see here: https://stackoverflow.com/questions/50767445/how-to-fix-failed-to-load-cairo-dll-in-r
# and here: https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/
font_add_google("Outfit")

# check fonts again
sysfonts::font_families()

#-----------------------------#
# custom ggplot2 functions ####
#-----------------------------#

# function to change text colour to white or black based on background
contrast <- function(colour) {
  out   <- rep("black", length(colour))
  light <- farver::get_channel(colour, "l", space = "hcl")
  out[light < 60] <- "white"
    out
}

autocontrast <- aes(colour = after_scale(contrast(fill)))

# change text size of geom_label to pts
pts <- function(x){
  as.numeric(grid::convertX(grid::unit(x, "points"), "mm"))
}

# custom label_wrap function
label_wrap_mod <- function (x, width){
  unlist(lapply(strwrap(x, width = width, simplify = FALSE), 
                paste0, collapse = "\n"))
}

#----------------------------------------------------------#
# first question to plot : Help Improve Reproducibility ####
#----------------------------------------------------------#

# Please use the scale below to indicate how likely you think the following factors would be to improve the reproducibility of research.

# set colours: see them, here https://brand.exeter.ac.uk/colour/
cols <- BrewerUoE::uoe_colours(c('Exeter Bright Green', 'Exeter Deep Green', 'Exeter Night Green', 'Morning Sky', 'Deep Sea'))
names(cols) <- rev(c('Not at all likely', 'Not very likely', "I don't know", 'Likely', "Very likely"))
cols[3] <- 'white'

# grab columns out and wrangle
d_temp <- select(d, 27:37) %>%
  rownames_to_column(var = 'id') %>%
  pivot_longer(-id, names_to = 'measure', values_to = 'score') %>%
  mutate(., measure = str_squish(measure),
         score = str_squish(score),
         score = str_to_sentence(score),
         score = replace_na(score, "I don't know")) %>%
  group_by(measure, score) %>%
  summarise(n = n(), .groups = 'drop') %>%
  complete(measure, score, fill = list(n=0)) %>%
  group_by(measure) %>%
  mutate(prop = n/sum(n),
         score_fac = factor(score, levels = c('Not at all likely', 'Not very likely', "I don't know", 'Likely', "Very likely"))) %>%
  arrange(score_fac)

unique(d_temp$measure)
unique(d_temp$score)

# make dataframe for likert plot
stage_one <- d_temp %>%
  group_by(measure) %>%
  mutate(text = paste0(formatC(100 * prop, format="f", digits=0), "%"),
         text2 = ifelse(prop <0.05, '', text),
         cs = cumsum(prop),
         offset = sum(prop[1:(floor(n()/2))]) + (n() %% 2)*0.5*(prop[ceiling(n()/2)]),
         xmax = -offset + cs,
         xmin = xmax-prop) %>%
  ungroup()

gap <- 0.2

stage_two <- stage_one %>%
  left_join(stage_one %>%
              group_by(measure) %>%
              summarise(max_xmax = max(xmax)) %>%
              mutate(r = row_number(max_xmax))) %>%
  arrange(desc(r)) %>%
  mutate(ymin = r - (1-gap)/2,
         ymax = r + (1-gap)/2)


# make plot
ggplot(stage_two, aes(fill = score_fac)) +
  geom_vline(aes(xintercept = 0)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill=score_fac), col = 'dark grey') +
  geom_text(aes(x= (xmin + xmax)/2, y=(ymin + ymax)/2, label=text2, !!!autocontrast),  size = MicrobioUoE::pts(9.5), family = 'Outfit') +
  scale_x_continuous(labels = percent, breaks=seq(-1, 1, len=9), limits=c(-1, 1)) +
  scale_y_continuous(breaks = 1:n_distinct(stage_two$measure),
                     labels = label_wrap_mod(rev(stage_two %>% distinct(measure) %>% pull(measure)), 40)) +
  scale_fill_manual('', values = cols) +
  theme_bw(base_size = 14, base_family = 'Outfit') +
  theme(axis.text.y = element_text(hjust = 0.5),
        plot.title.position = 'plot',
        panel.grid = element_blank()) +
  labs(title = 'How likely do you think the following measures would be in improving the reproducibility of research?',
       y = 'Measure',
       x = '% Respondents',
       caption = 'Data is comprised of 96 respondents')

ggsave('plots/what_measures_would_work.pdf', width = 12, height = 8, device = cairo_pdf)
ggsave('plots/what_measures_would_work.png', width = 12, height = 8, type = 'cairo')

#--------------------------------------------------------------------#
# second question to plot : What does reproducibility mean to you ####
#--------------------------------------------------------------------#

# grab correct column
d_temp <- select(d, 8) %>%
  pull(1) %>%
  # make it one continuous vector
  paste0()

# do some wrangling
# make corpus
d_temp <- VCorpus(VectorSource(d_temp))
# Strip unnecessary whitespace
d_temp <- tm_map(d_temp, stripWhitespace)
# Convert to lowercase
d_temp <- tm_map(d_temp, tolower)
# Remove conjunctions etc.
d_temp <- tm_map(d_temp, removeWords, stopwords("english"))
# Remove commas etc.
d_temp <- tm_map(d_temp, removePunctuation)
d_temp <- tm_map(d_temp, PlainTextDocument)

# make wordcloud
png(filename='plots/reproducible_wordcloud.png', height = 4, width = 4, units = 'in', res = 300)

wordcloud(d_temp,
          scale=c(2,0.5),
          max.words=100,      # Set top n words
          random.order=FALSE, # Words in decreasing freq
          rot.per=0.1,      # % of vertical words
          use.r.layout=FALSE, # Use C++ collision detection
          colors=BrewerUoE::uoe_colours()[1:5],
          random.color = TRUE)

dev.off()

#--------------------------------------------------------------------#
# second question to plot : What does Open Science mean to you ####
#--------------------------------------------------------------------#

# grab correct column
d_temp <- select(d, 10) %>%
  pull(1) %>%
  # make it one continuous vector
  paste0()

# do some wrangling
# make corpus
d_temp <- VCorpus(VectorSource(d_temp))
# Strip unnecessary whitespace
d_temp <- tm_map(d_temp, stripWhitespace)
# Convert to lowercase
d_temp <- tm_map(d_temp, tolower)
# Remove conjunctions etc.
d_temp <- tm_map(d_temp, removeWords, stopwords("english"))
# Remove commas etc.
d_temp <- tm_map(d_temp, removePunctuation)
d_temp <- tm_map(d_temp, PlainTextDocument)

# make wordcloud
png(filename='plots/openscience_wordcloud.png', height = 4, width = 4, units = 'in', res = 300)

wordcloud(d_temp,
          scale=c(2,0.5),
          max.words=100,      # Set top n words
          random.order=FALSE, # Words in decreasing freq
          rot.per=0.1,      # % of vertical words
          use.r.layout=FALSE, # Use C++ collision detection
          colors=BrewerUoE::uoe_colours()[1:5],
          random.color = TRUE)

dev.off()
