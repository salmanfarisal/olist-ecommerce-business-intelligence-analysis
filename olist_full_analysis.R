# ==============================================================================
# PROJECT: Olist E-Commerce End-to-End Data Analysis
# AUTHOR: Salman Farisal
# OBJECTIVE: Extracting insights on sales, timing, geography, and business metrics
# DATA SOURCE: Brazilian E-Commerce Public Dataset (Olist)
# ==============================================================================

# 1. ENVIRONMENT SETUP
# --------------------
library(tidyverse)
library(DBI)
library(RPostgres)

# 2. DATABASE CONNECTION
# ----------------------
# Note: Security best practice - replace credentials with your own local setup
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'portfolio-brazilian-olist',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'your_password_here') #

# 3. ANALYSIS I: MOST POPULAR PRODUCT CATEGORIES
# ----------------------------------------------
query_top_kategori <- "
SELECT 
  p.product_category_name,
  COUNT(oi.order_id) as jumlah_terjual
FROM order_items oi
JOIN products p ON oi.product_id = p.product_id
WHERE p.product_category_name IS NOT NULL
GROUP BY p.product_category_name
ORDER BY jumlah_terjual DESC
LIMIT 10
"

df_top_produk <- dbGetQuery(con, query_top_kategori)

ggplot(df_top_produk, aes(x = reorder(product_category_name, jumlah_terjual), y = jumlah_terjual)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Popular Product Categories in Olist",
    subtitle = "Based on total order volume",
    x = "Product Category",
    y = "Total Orders",
    caption = "Source: Olist E-Commerce Dataset | Analyzed by: Salman Farisal"
  ) +
  theme_minimal() #

ggsave("1_top_product_categories.png", width = 10, height = 6, dpi = 300)

# 4. ANALYSIS II: TIME SERIES ANALYSIS (HOURLY & DAILY)
# -----------------------------------------------------
# A. Hourly Shopping Trends
query_hourly <- "
SELECT
  EXTRACT(HOUR FROM order_purchase_timestamp) AS purchase_hour,
  COUNT(order_id) AS total_orders
FROM orders
GROUP BY purchase_hour
ORDER BY purchase_hour
"

df_hourly <- dbGetQuery(con, query_hourly) %>% 
  mutate(total_orders = as.numeric(total_orders))

ggplot(df_hourly, aes(x = purchase_hour, y = total_orders)) +
  geom_line(color = "#2E86C1", linewidth = 1.2) +
  geom_point(color = "#1B4F72", size = 3) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Customer Purchasing Behavior by Hour",
    subtitle = "Identifying peak traffic hours for marketing optimization",
    x = "Hour of Day (24h Format)",
    y = "Total Number of Orders",
    caption = "Source: Olist E-Commerce Dataset | Analyzed by: Salman Farisal"
  ) +
  theme_minimal() #

ggsave("2_hourly_behavior.png", width = 10, height = 6, dpi = 300)

# B. Weekly Sales Distribution
query_daily <- "
SELECT
  EXTRACT(DOW FROM order_purchase_timestamp) AS day_of_week,
  COUNT(order_id) AS total_orders
FROM orders
GROUP BY day_of_week
ORDER BY day_of_week
"

df_daily <- dbGetQuery(con, query_daily) %>% 
  mutate(
    day_name = factor(day_of_week, levels = 0:6, 
                      labels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                 "Thursday", "Friday", "Saturday")),
    total_orders = as.numeric(total_orders)
  )

ggplot(df_daily, aes(x = day_name, y = total_orders, fill = day_name)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Weekly Sales Distribution",
    subtitle = "Comparing order volume across different days of the week",
    x = "Day of the Week",
    y = "Total Orders",
    caption = "Source: Olist E-Commerce Dataset | Analyzed by: Salman Farisal"
  ) +
  theme_minimal() #

ggsave("3_daily_trends.png", width = 10, height = 6, dpi = 300)

# 5. ANALYSIS III: GEOGRAPHIC REVENUE PERFORMANCE
# -----------------------------------------------
# A. Top 10 Cities by Revenue (GMV)
query_revenue_city <- "
SELECT
  c.customer_city AS city,
  SUM(oi.price) AS total_revenue
FROM orders o
JOIN order_items oi ON o.order_id = oi.order_id
JOIN customers c ON o.customer_id = c.customer_id
WHERE o.order_status = 'delivered'
GROUP BY city
ORDER BY total_revenue DESC
LIMIT 10
"

df_revenue_city <- dbGetQuery(con, query_revenue_city) %>% 
  mutate(total_revenue = as.numeric(total_revenue), city = str_to_title(city))

ggplot(df_revenue_city, aes(x = reorder(city, total_revenue), y = total_revenue)) +
  geom_col(fill = "#27AE60") + 
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ")) +
  labs(
    title = "Top 10 Cities by Total Revenue",
    subtitle = "Analysis of Gross Merchandise Value (GMV) per city",
    x = "City",
    y = "Total Revenue (BRL)",
    caption = "Source: Olist E-Commerce Dataset | Analyzed by: Salman Farisal"
  ) +
  theme_minimal() #

ggsave("4_revenue_per_city.png", width = 10, height = 6, dpi = 300)

# B. Average Order Value (AOV) per City
query_aov <- "
SELECT
  c.customer_city AS city,
  SUM(oi.price) / COUNT(DISTINCT o.order_id) AS aov
FROM orders o
JOIN order_items oi ON o.order_id = oi.order_id
JOIN customers c ON o.customer_id = c.customer_id
WHERE o.order_status = 'delivered'
GROUP BY city
HAVING COUNT(DISTINCT o.order_id) > 50 
ORDER BY aov DESC
LIMIT 10
"

df_aov <- dbGetQuery(con, query_aov) %>% 
  mutate(aov = as.numeric(aov), city = str_to_title(city))

ggplot(df_aov, aes(x = reorder(city, aov), y = aov)) +
  geom_point(size = 4, color = "#E67E22") + 
  geom_segment(aes(x = city, xend = city, y = 0, yend = aov), color = "gray") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ")) +
  labs(
    title = "Top 10 Cities by Average Order Value (AOV)",
    subtitle = "Identifying cities with the highest spending per transaction",
    x = "City",
    y = "Average Spend per Order (BRL)",
    caption = "Source: Olist E-Commerce Dataset | Analyzed by: Salman Farisal"
  ) +
  theme_minimal() #

ggsave("5_aov_per_city.png", width = 10, height = 6, dpi = 300)

# 6. WRAP UP
# ----------
dbDisconnect(con)
print("Analysis complete. All charts exported as PNG files.")