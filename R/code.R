library(corrplot)
library(rstatix)
library(psych)
library(vcd)
library(dplyr)
library(ggplot2)
library(car)
library(effectsize)
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
cat_vars <- c("Family.History","Hormonal.Changes","Gender", "Race.Ethnicity", "Body.Weight", "Calcium.Intake", "Vitamin.D.Intake","Physical.Activity","Smoking","Alcohol.Consumption","Medical.Conditions","Medications","Prior.Fractures")


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


data <- read.csv("osteoporosis.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)
# === 1️⃣ Chuẩn bị dữ liệu ===
data$Race.Ethnicity <- as.factor(data$Race.Ethnicity)

cat("=== KIỂM TRA ĐIỀU KIỆN ÁP DỤNG ANOVA ===\n\n")

# --- 2️⃣ Kiểm tra cỡ mẫu ---
group_counts <- table(data$Race.Ethnicity)
print(group_counts)

small_groups <- names(group_counts[group_counts < 30])

if (length(small_groups) == 0) {
  cat("\n✅ Tất cả các nhóm có n ≥ 30.\n")
  cat("→ Có thể bỏ qua giả định chuẩn tính (theo Định lý giới hạn trung tâm - CLT).\n")
  normality_ok <- TRUE
} else {
  cat("\n⚠️ Có nhóm có n < 30:\n")
  print(small_groups)
  cat("→ Cần kiểm tra kỹ giả định chuẩn tính bằng Shapiro–Wilk.\n")
  normality_ok <- FALSE
}

# --- 3️⃣ Kiểm định Levene về phương sai đồng nhất ---
if (!require(car)) install.packages("car", dependencies = TRUE)
library(car)

levene_res <- leveneTest(Age ~ Race.Ethnicity, data = data, center = "median")
print(levene_res)

p_levene <- levene_res$`Pr(>F)`[1]

if (p_levene > 0.05) {
  cat("\n✅ Levene test: p =", round(p_levene, 4), "> 0.05\n")
  cat("→ Không bác bỏ H0, phương sai giữa các nhóm đồng nhất.\n")
  var_equal <- TRUE
} else {
  cat("\n⚠️ Levene test: p =", round(p_levene, 4), "< 0.05\n")
  cat("→ Bác bỏ H0, phương sai khác nhau giữa các nhóm.\n")
  var_equal <- FALSE
}

# --- 4️⃣ Tổng hợp kết luận về điều kiện ANOVA ---
cat("\n=== KẾT LUẬN VỀ GIẢ ĐỊNH ANOVA ===\n")

if (normality_ok & var_equal) {
  cat("✅ Dữ liệu thỏa mãn (hoặc xấp xỉ thỏa mãn) các giả định của ANOVA.\n")
  cat("→ Có thể tiến hành ANOVA thông thường.\n")
} else if (var_equal & !normality_ok) {
  cat("⚠️ Dữ liệu không chuẩn, nhưng các nhóm có cỡ mẫu lớn và phương sai đồng nhất.\n")
  cat("→ Có thể tiếp tục ANOVA (nhờ CLT), nhưng nên ghi chú trong báo cáo.\n")
} else if (!var_equal) {
  cat("⚠️ Phương sai không đồng nhất.\n")
  cat("→ Nên dùng Welch ANOVA hoặc Kruskal–Wallis test.\n")
}

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

cat("\n=== VẼ BIỂU ĐỒ PHÂN PHỐI TUỔI GIỮA CÁC NHÓM ===\n")

windows()
ggplot(data, aes(x = Age, fill = Race.Ethnicity)) +
  geom_histogram(aes(y = ..density..), bins = 25, alpha = 0.6, position = "identity") +
  geom_density(alpha = 0.7) +
  facet_wrap(~ Race.Ethnicity, scales = "free_y") +
  labs(title = "Phân bố tuổi theo từng nhóm chủng tộc",
       x = "Tuổi",
       y = "Mật độ (Density)") +
  theme_minimal(base_size = 13)

# Mô hình ANOVA một nhân tố
anova_model <- aov(Age ~ Race.Ethnicity, data = data)

# Hiển thị kết quả ANOVA
cat("=== Kết quả kiểm định ANOVA ===\n")
anova_result <- summary(anova_model)
print(anova_result)

# Lấy p-value từ kết quả ANOVA
p_value <- anova_result[[1]][["Pr(>F)"]][1]

# Viết kết luận tự động
cat("\n=== Kết luận ===\n")

if (p_value < 0.05) {
  cat(sprintf("Giá trị p = %.3f < 0.05 → Bác bỏ H0.\n", p_value))
  cat("→ Có sự khác biệt có ý nghĩa thống kê về độ tuổi trung bình giữa các nhóm chủng tộc.\n")
} else {
  cat(sprintf("Giá trị p = %.3f ≥ 0.05 → Không đủ bằng chứng để bác bỏ H0.\n", p_value))
  cat("→ Không có sự khác biệt đáng kể về độ tuổi trung bình giữa các nhóm chủng tộc.\n")
}

windows()
ggplot(data, aes(x = Race.Ethnicity, y = Age, fill = Race.Ethnicity)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "white", notch = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Phân bố độ tuổi giữa các nhóm chủng tộc",
       x = "Chủng tộc (Race.Ethnicity)", y = "Tuổi") +
  scale_fill_brewer(palette = "Pastel1")


#CODE HOI QUY LOGISTIC
#BUOC DAU TIEN ANH XA TUOI LA BIEN DINH LUONG -> BIEN DINH TINH
#THUONG THI CANG VE GIA TI LE MAC BENH LOANG XUONG CANG CAO
#NHOM CHON DO TUOI 60 LA DO TUOI SAU DO TI LE BI BENH CAO
# ===============================
# Hồi quy logistic với dummy variables
# ===============================
library(corrplot)
library(rstatix)
library(psych)
library(vcd)
library(dplyr)
library(ggplot2)
library(car)
library(effectsize)
data <- read.csv("osteoporosis_NoID.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)

View(data)

library(fastDummies)

# Danh sách các biến cần tạo dummy
categorical_vars <- c("Gender", "Hormonal.Changes", "Family.History", "Race.Ethnicity",
                      "Body.Weight", "Calcium.Intake", "Vitamin.D.Intake",
                      "Physical.Activity", "Smoking", "Alcohol.Consumption",
                      "Medical.Conditions", "Medications", "Prior.Fractures")

# Tạo biến dummy
data_dummy <- fastDummies::dummy_cols(data,
                                      select_columns = categorical_vars,
                                      remove_first_dummy = TRUE,  # loại bỏ cột đầu để tránh đa cộng tuyến
                                      remove_selected_columns = TRUE)  # loại bỏ cột gốc

# Xem kết quả
View(data_dummy)

quantitative_vars <- c("Age")

independent_vars <- c(quantitative_vars,
                      setdiff(colnames(data_dummy), c("Osteoporosis", quantitative_vars)))

# Thêm backticks cho tất cả tên cột
independent_vars_safe <- paste0("`", independent_vars, "`")

# Tạo công thức
formula <- as.formula(paste("Osteoporosis ~", paste(independent_vars_safe, collapse = " + ")))

# ============================
# 6️⃣ Chạy logistic regression
# ============================
model <- glm(formula, data = data_dummy, family = binomial)

# ============================
# 7️⃣ Xem kết quả chi tiết
# ============================
summary(model)

# Chọn các biến có ý nghĩa
selected_vars <- c("Age", "Medications_None")  # thêm biến bạn thấy phù hợp

# Thêm backticks để an toàn với tên cột
selected_vars_safe <- paste0("`", selected_vars, "`")

# Tạo công thức mới
formula_selected <- as.formula(paste("Osteoporosis ~", paste(selected_vars_safe, collapse = " + ")))

# Chạy logistic regression với biến đã chọn
model_selected <- glm(formula_selected, data = data_dummy, family = binomial)

# Xem kết quả
summary(model_selected)

# 1. Thống kê cơ bản
n  <- length(data$Osteoporosis)
x  <- sum(data$Osteoporosis == 1)
p_hat <- x / n

cat("p-hat =", p_hat, "\n")

# 2. Kiểm tra điều kiện xấp xỉ chuẩn
np <- n * p_hat
nq <- n * (1 - p_hat)

cat("np =", np, "\n")
cat("n(1-p) =", nq, "\n")

if (np >= 5 & nq >= 5) {
  cat("✔ Điều kiện xấp xỉ chuẩn thỏa mãn.\n")
} else {
  cat("✘ Điều kiện KHÔNG thỏa mãn → dùng Wilson hoặc Exact.\n")
}

# 3. Khoảng tin cậy 95% dùng chuẩn (nếu thỏa điều kiện)
z <- 1.96
se <- sqrt(p_hat * (1 - p_hat) / n)
CI <- c(p_hat - z * se, p_hat + z * se)

cat("Khoảng ước lượng 95% (Wald) =", CI, "\n")