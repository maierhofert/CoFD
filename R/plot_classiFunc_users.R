# this file downloads and plots the daily number of downloads for the
# classiFunc and the mlr package
library("ggplot2")
library("installr")
mytheme = theme_bw(20)

# The first two functions might take a good deal of time to run (depending on the date range)
RStudio_CRAN_data_folder <- download_RStudio_CRAN_data(log_folder = "../Rlog/",
  START = '2017-05-29',
  END = '2017-12-09')

my_RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_data_folder, packages = c("mlr", "classiFunc"))

# # standard barplot
# barplot_package_users_per_day("classiFunc", my_RStudio_CRAN_data, remove_dups = TRUE)


# classiFunc ggplot barplot
lp = lineplot_package_downloads("classiFunc", my_RStudio_CRAN_data, remove_dups = TRUE)
bp = ggplot(data = lp, aes(time, V1, group = package)) +
  geom_col() + 
  ylab("downloads") + 
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  xlab("date") +
  mytheme + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
bp
ggsave(paste0("Grafiken/classiFunc_downloads.pdf"), bp, 
       width = 12, height = 7)

# number of countries
length(table(my_RStudio_CRAN_data[!duplicated(my_RStudio_CRAN_data$ip_id), ]$country))
# number of downloads (unique)
sum(lp$V1)

# mlr ggplot barplot
lp = lineplot_package_downloads("mlr", my_RStudio_CRAN_data, remove_dups = TRUE)
bp = ggplot(data = lp, aes(time, V1, group = package)) +
  geom_col() + 
  ylab("downloads") + 
  xlab("date") +
  mytheme + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
bp
ggsave(paste0("Grafiken/mlr_downloads.pdf"), bp, 
       width = 12, height = 7)
