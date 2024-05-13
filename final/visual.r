library(vcd)
library(ggplot2)
library(gridExtra)
data_path = "data/sleep_study"
data_p_path = paste(sep = "/", data_path, "T5 Parent Raw and Scored_5.12.2020.sav")
data_a_path = paste(sep = "/", data_path, "T5 Adolescent Raw and Scored_6.11.2020.sav")
data_csv_path = paste(sep = "/", data_path, "t5_data.csv")
data_item_parent_path = paste(sep = "/", data_path, "data_item_parent.csv")
data_item_adolescent_path = paste(sep = "/", data_path, "data_item_adolescent.csv")
data_sav_path = paste(sep = "/", data_path, "T5 Sleep Study Dataset.sav")

data_csv = read.csv(data_csv_path)
data_csv_clean = data_csv[!is.na(data_csv$Sex),]
data_csv_clean$DEM6 = factor(data_csv_clean$DEM6, levels = 1:6, labels = c("American Indian or Alaskan", "Asian", "Black or African American", "Native Asian or Pacific Islander", "White", "Biracial or Multiracial"))
data_csv_clean$Group = factor(data_csv_clean$Group, levels = 0:1, labels = c("Comparison", "ADHD"))
data_csv_clean$Sex = factor(data_csv_clean$Sex, levels = 1:2, labels = c("Male", "Female"))
#print(mosaic(~ Sex + Group + DEM6, data = data_csv_clean, rot_labels = c(0, 0, 0, 90)))

# Stacked + percent
bar_group = ggplot(data_csv_clean[data_csv_clean$Group == "Comparison",]) +
            geom_bar(aes(fill=DEM6, x=Sex, y = 1), position = "stack", stat = "identity") +
            labs(title = "Comparison") +
            scale_fill_manual("Race/Ethnicity", values = c("red", "orange", "yellow", "green", "blue", "purple"), breaks = levels(data_csv_clean$DEM6))
bar_sex = ggplot(data_csv_clean[data_csv_clean$Group == "ADHD",]) +
          geom_bar(aes(fill=DEM6, x=Sex, y = 1), position = "stack", stat = "identity") +
          labs(title = "ADHD") +
          scale_fill_manual("Race/Ethnicity", values = c("red", "orange", "yellow", "green", "blue", "purple"), breaks = levels(data_csv_clean$DEM6))
grid.arrange(bar_group, bar_sex, ncol = 2)
