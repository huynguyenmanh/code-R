library(corrplot)
library(rstatix)
library(psych)
library(vcd)
library(dplyr)
library(ggplot2)
# Read the CSV file
data <- read.csv("osteoporosis.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)

# Check the first few rows
head(data)

# Open it in a table view
View(data)

# See data structure
str(data)

# Basic summary
summary(data)

# Loại bỏ cột Id (giả sử cột tên là "Id")
data_no_id <- subset(data, select = -Id)

# Kiểm tra lại
head(data_no_id)

# kiểm tra rỗng
missing_summary <- data.frame(
  NA_Count = colSums(is.na(data_no_id)),
  N.A_String = colSums(data_no_id == "N/A", na.rm = TRUE),
  NULL_String = colSums(data_no_id == "NULL", na.rm = TRUE),
  Empty_String = colSums(data_no_id == "", na.rm = TRUE)
)

print(missing_summary)

# Thống kê mô tả cơ bản
summary(data_no_id$Age)

# Tính các thống kê chi tiết 
mean_age <- mean(data_no_id$Age, na.rm = TRUE)
median_age <- median(data_no_id$Age, na.rm = TRUE)
sd_age <- sd(data_no_id$Age, na.rm = TRUE)
var_age <- var(data_no_id$Age, na.rm = TRUE)
min_age <- min(data_no_id$Age, na.rm = TRUE)
max_age <- max(data_no_id$Age, na.rm = TRUE)
Q1_age <- quantile(data_no_id$Age, 0.25, na.rm = TRUE)
Q3_age <- quantile(data_no_id$Age, 0.75, na.rm = TRUE)
IQR_age <- IQR(data_no_id$Age, na.rm = TRUE)

# Tạo dataframe thống kê mô tả
desc_age <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Variance", 
                "Minimum", "Q1 (25%)", "Q3 (75%)", "Maximum", "IQR"),
  Value = c(mean_age, median_age, sd_age, var_age, 
            min_age, Q1_age, Q3_age, max_age, IQR_age)
)

# Hiển thị bảng
print(desc_age)

# === Histogram ===
# Opens a new plotting window
windows()  # optional if using RStudio; ensures a new window
hist(
  data_no_id$Age,
  main = "Biểu đồ tần số độ tuổi (Age)",
  xlab = "Tuổi",
  ylab = "Tần số",
  col = "skyblue",
  border = "white",
  breaks = 10
)

# Compute quartiles and IQR
quartiles <- quantile(data_no_id$Age, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
IQR_value <- IQR(data_no_id$Age, na.rm = TRUE)
lower_bound <- quartiles[1] - 1.5 * IQR_value
upper_bound <- quartiles[3] + 1.5 * IQR_value

# --- Tính outlier chỉ một lần cho toàn bộ Age ---
Q1 <- quantile(data_no_id$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(data_no_id$Age, 0.75, na.rm = TRUE)
IQR_val <- IQR(data_no_id$Age, na.rm = TRUE)
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

# Xác định outlier toàn cục
outliers <- data_no_id$Age[data_no_id$Age < lower_bound | data_no_id$Age > upper_bound]

# Thông báo kết quả
if (length(outliers) > 0) {
  cat("Phát hiện", length(outliers), "outlier(s):", outliers, "\n")
} else {
  cat("Không có outlier nào được phát hiện theo quy tắc 1.5×IQR.\n")
}

# --- Vẽ boxplot ---
windows()
boxplot(
  data_no_id$Age,
  main = "Boxplot mô tả biến Tuổi (Age)",
  ylab = "Tuổi",
  col = "lightblue",
  border = "darkblue"
)


# --- Đánh dấu outlier (nếu có) ---
if (length(outliers) > 0) {
  points(
    x = rep(1, length(outliers)),  # tất cả nằm cùng một cột
    y = outliers,
    col = "red",
    pch = 19,
    cex = 1.2
  )
  text(
    x = rep(1.1, length(outliers)),
    y = outliers,
    labels = round(outliers, 1),
    col = "red",
    pos = 4,
    cex = 0.8
  )
}

df <- read.csv("osteoporosis.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)



# Liệt kê các biến định tính (ngoại trừ biến mục tiêu)
cat_vars <- c("Hormonal.Changes","Gender", "Race.Ethnicity", "Body.Weight", "Calcium.Intake", "Vitamin.D.Intake","Physical.Activity","Smoking","Alcohol.Consumption","Medical.Conditions","Medications","Prior.Fractures")


for (var in cat_vars) {
  cat("===== ", var, " =====\n")
  
  # Tính bảng tần suất theo nhóm bệnh
  tab <- df %>%
    dplyr::group_by(.data[[var]], Osteoporosis) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    # Tính percent theo nhóm biến định tính (mỗi nhóm = 100%) 
    dplyr::group_by(.data[[var]]) %>%
    dplyr::mutate(percent = round(100 * n / sum(n), 1),
                  label = paste0(percent, "%"),
                  Osteoporosis = factor(Osteoporosis, labels = c("Không mắc", "Mắc bệnh"))) %>%
    dplyr::ungroup()
  
  print(tab)
  
  # Mở cửa sổ riêng
  #windows()  # MacOS: quartz(), Linux: X11()
  
  # Vẽ pie chart với % hiển thị trên từng phần
  p <- ggplot(tab, aes(x = "", y = percent, fill = Osteoporosis)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4, color = "black") +
    facet_wrap(vars(.data[[var]])) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = paste("Phân bố Osteoporosis theo", var))
  
  windows()
  print(p)
  Sys.sleep(0.5)  # hoặc 1 giây
}

num_vars <- c("Age")

summary_num <- df %>%
  group_by(Osteoporosis) %>%
  summarise(across(all_of(num_vars),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

print(summary_num)

windows()
ggplot(df, aes(x = factor(Osteoporosis), y = Age, fill = factor(Osteoporosis))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 19, width = 0.5) +
  scale_fill_manual(values = c("skyblue", "salmon"),
                    labels = c("Không mắc", "Mắc bệnh")) +
  labs(
    title = "Boxplot Tuổi theo nhóm Osteoporosis",
    x = "Nhóm Osteoporosis",
    y = "Tuổi",
    fill = "Nhóm"
  ) +
  theme_minimal()