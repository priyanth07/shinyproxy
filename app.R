





library(RODBC)
library(DBI)
library(pivottabler)
library(kableExtra)
library(dplyr)
library(rlang)
library(tidyverse)
library(gt)
library(janitor)
library(paletteer)
library(ggplot2)
library(reshape2)
library(ggsci)
library(treemap)
#library(d3treeR)
library(devtools)
#library(hpackedbubble) # split packed bubble chart 
library(latticeExtra) # dual axis y-chart
library(lattice)
library(stats)
#library(tigerstats)
library(lubridate)
library(zoo) #as.yearmon()
library(formattable) #percent function
library(scales) #modify plot scales
library(ggalt) #geom_xspline - smooth line chart
#library(sunburstR) #sunburt chart
library(packcircles) #packed circle chart
library(viridis) #virdis pallete
library(ggiraph) #transform ggplot into interactive
library(ggstatsplot) #beautfiy violin plot on ticketsize
library(hrbrthemes)
library(forcats)
library(RColorBrewer)
library(cowplot)    # for theme_minimal_hgrid()
library(colorspace) # for darken()
library(ggrepel)    # for geom_text_repel()
library(ggridges)
library(heatmaply)
library(ggExtra)
library(wordcloud2)
library(data.tree)
library(ggraph)
library(igraph)
library(circlepackeR)  
library(geofacet)
library(ggh4x)
library(colorspace)
library(streamgraph)
library(leaflet)
library(rgdal)
library(networkD3)
library(DT)
library(ggnewscale)
library(reshape2)
library(data.table)
#library(guf)
library(gridExtra)
library(googleVis)
library(shinythemes)
library(shiny)
library(shinydashboard)
library(espnscrapeR)
library(stringr)
library(googleVis)
library(ggsankey)
library(prismatic)
library(ggallin)
library(rsconnect)
library(genderizeR)
# library(openNLP)
# library(NLP)
# library(rJava)
library(writexl)
library(leaflet.minicharts)
library(grid)



#------------------------------------------# Datasets #------------------------------------------#  

# salesrep_dist_total <- readRDS("salesrep_dist_total.rds")
# customer_gender <- readRDS("customer_gender.rds")
# circle_pkn_cust_segment_data <- readRDS("circle_pkn_cust_segment_data.rds")
# coeff <- readRDS("coeff.rds")
# sec_sales_discount <- readRDS("sec_sales_discount.rds")
# most_bought_products <- readRDS("most_bought_products.rds")
# product_cross_merched <- readRDS("product_cross_merched.rds")
# sec_cust_percent_top_manufacture <- readRDS("sec_cust_percent_top_manufacture.rds")
# sec_cust_percent_brand_loyal <- readRDS("sec_cust_percent_brand_loyal.rds")


#------------------------------------------# Shiny Server Code #------------------------------------------#  


server <- function(input, output, session) {
  
  
#------------------------------------------# Load RDS Files #------------------------------------------#
  
  # Removes Scientific Notation
  options(scipen=999)
  
  
  p1_sales <- readRDS("p1_sales.rds")
  p2_sales <- readRDS("p2_sales.rds")
  p1_profit <- readRDS("p1_profit.rds")
  p2_profit <- readRDS("p2_profit.rds")
  p1_profit_ratio <- readRDS("p1_profit_ratio.rds")
  p2_profit_ratio <- readRDS("p2_profit_ratio.rds")
  p1_total_orders <- readRDS("p1_total_orders.rds")
  p2_total_orders <- readRDS("p2_total_orders.rds")
  p1_profit_per_order <- readRDS("p1_profit_per_order.rds")
  p2_profit_per_order <- readRDS("p2_profit_per_order.rds")
  p1_avg_days_to_ship <- readRDS("p1_avg_days_to_ship.rds")
  p2_avg_days_to_ship <- readRDS("p2_avg_days_to_ship.rds")
  p1_corporate_sales <- readRDS("p1_corporate_sales.rds")
  p2_corporate_sales <- readRDS("p2_corporate_sales.rds")
  p1_profitcust <- readRDS("p1_profitcust.rds")
  p2_profitcust <- readRDS("p2_profitcust.rds")
  # p1_cust <- readRDS("p1_cust.rds")
  # p2_cust <- readRDS("p2_cust.rds")
  # p1_profitcust <- readRDS("p1_profitcust.rds")
  # p2_profitcust <- readRDS("p2_profitcust.rds")
  # p1_avgsalecust <- readRDS("p1_avgsalecust.rds")
  # p2_avgsalecust <- readRDS("p2_avgsalecust.rds")
  # p1_retn_percent <- readRDS("p1_retn_percent.rds")
  # p2_retn_percent <- readRDS("p2_retn_percent.rds")
  # p1_clv <- readRDS("p1_clv.rds")
  # p2_clv <- readRDS("p2_clv.rds")
  # p1_avg_recncy_mnth <- readRDS("p1_avg_recncy_mnth.rds")
  # p2_avg_recncy_mnth <- readRDS("p2_avg_recncy_mnth.rds")
  # p1_avg_freq <- readRDS("p1_avg_freq.rds")
  # p2_avg_freq <- readRDS("p2_avg_freq.rds")
  # p1_avg_monetary <- readRDS("p1_avg_monetary.rds")
  # p2_avg_monetary <- readRDS("p2_avg_monetary.rds")
  # customer_gender_plot <- readRDS("customer_gender_plot.rds")
  # customer_age_plot <- readRDS("customer_age_plot.rds")
  # customer_geo_plot2 <- readRDS("customer_geo_plot2.rds")
  # Circular_pkn_cust_segment_plot <- readRDS("Circular_pkn_cust_segment_plot.rds")
  # customer_income_plot <- readRDS("customer_income_plot.rds")
  # customer_occupn_income_plot <- readRDS("customer_occupn_income_plot.rds")
  # customer_edu_plot <- readRDS("customer_edu_plot.rds")
  # monthly_sales_plot <- readRDS("monthly_sales_plot.rds")
  # acqn_order_comparison_plot <- readRDS("acqn_order_comparison_plot.rds")
  # avg_sale_byorder_plot <- readRDS("avg_sale_byorder_plot.rds")
  # total_subscrpn_plot <- readRDS("total_subscrpn_plot.rds")
  # new_cust_acqn_plot <- readRDS("new_cust_acqn_plot.rds")
  # new_repeat_cust_plot <- readRDS("new_repeat_cust_plot.rds")
  # retention_cohort_plot <- readRDS("retention_cohort_plot.rds")
  # revenue_per_cohort_plot <- readRDS("revenue_per_cohort_plot.rds")
  # revenue_per_cust_plot <- readRDS("revenue_per_cust_plot.rds")
  # customer_worth_plot <- readRDS("customer_worth_plot.rds")
  # customer_worth_order_profit_plot <- readRDS("customer_worth_order_profit_plot.rds")
  # customer_worth_dist_tbl <- readRDS("customer_worth_dist_tbl.rds")
  # p1_rfm_segment <- readRDS("p1_rfm_segment.rds")
  # p2_rfm_segment <- readRDS("p2_rfm_segment.rds")
  # p3_rfm_geo <- readRDS("p3_rfm_geo.rds")
  # p1_segment_drill_r <- readRDS("p1_segment_drill_r.rds")
  # p2_segment_drill_f <- readRDS("p2_segment_drill_f.rds")
  # p3_segment_drill_m <- readRDS("p3_segment_drill_m.rds")
  # rfm_segment_sales_plot <- readRDS("rfm_segment_sales_plot.rds")
  # cust_in_segment_plot <- readRDS("cust_in_segment_plot.rds")
  # sales_in_segment_plot <- readRDS("sales_in_segment_plot.rds")
  # recency_in_segment_plot <- readRDS("recency_in_segment_plot.rds")
  # customer_purchasing_time <- readRDS("customer_purchasing_time.rds")
  # rfm_segm_cust_order_plot <- readRDS("rfm_segm_cust_order_plot.rds")
  # scatter_rec_freq__plot <- readRDS("scatter_rec_freq__plot.rds")
  # rfm_heatmap_gt <- readRDS("rfm_heatmap_gt.rds")
  # cust_segment_rfm_gt <- readRDS("cust_segment_rfm_gt.rds")
  # single_multi_categ_plot <- readRDS("single_multi_categ_plot.rds")
  # single_multi_categ_over_timeprd_plot <- readRDS("single_multi_categ_over_timeprd_plot.rds")
  # no_of_orders_plot <- readRDS("no_of_orders_plot.rds")
  # last_purchase_prod_category_plot <- readRDS("last_purchase_prod_category_plot.rds")
  # total_spend_plot <- readRDS("total_spend_plot.rds")
  # total_spend_detailed_plot <- readRDS("total_spend_detailed_plot.rds")
  # customer_spend_type_plot <- readRDS("customer_spend_type_plot.rds")
  # cust_spend_type_prod_categ_plot <- readRDS("cust_spend_type_prod_categ_plot.rds")
  # cust_spend_type_purch_behavr_monthyr_plot <- readRDS("cust_spend_type_purch_behavr_monthyr_plot.rds")
  # cust_spend_type_profit_discount_plot <- readRDS("cust_spend_type_profit_discount_plot.rds")
  # cust_spend_type_orderyr_plot <- readRDS("cust_spend_type_orderyr_plot.rds")
  # sales_by_cust_spend_type_plot <- readRDS("sales_by_cust_spend_type_plot.rds")
  # customer_spend_type_rfm_gt <- readRDS("customer_spend_type_rfm_gt.rds")
  # cust_spend_type_hedonic_util_plot <- readRDS("cust_spend_type_hedonic_util_plot.rds")
  # product_impulse_buys_plot <- readRDS("product_impulse_buys_plot.rds")
  # top_manufacturers_prod1_plot <- readRDS("top_manufacturers_prod1_plot.rds")
  # top_manufacturers_prod2_plot <- readRDS("top_manufacturers_prod2_plot.rds")
  # top_manufacturers_prod3_plot <- readRDS("top_manufacturers_prod3_plot.rds")
  # top_manufacturers_brand_loyal_prod1_plot <- readRDS("top_manufacturers_brand_loyal_prod1_plot.rds")
  # top_manufacturers_brand_loyal_prod2_plot <- readRDS("top_manufacturers_brand_loyal_prod2_plot.rds")
  # top_manufacturers_brand_loyal_prod3_plot <- readRDS("top_manufacturers_brand_loyal_prod3_plot.rds")
  # most_discount_brands_plot <- readRDS("most_discount_brands_plot.rds")
  # active_customers_layergraph <- readRDS("active_customers_layergraph.rds")
  # revenue_layergraph <- readRDS("revenue_layergraph.rds")
  # revenue_percustomer_layergraph <- readRDS("revenue_percustomer_layergraph.rds")
  # p1_retn_cycle <- readRDS("p1_retn_cycle.rds")
  # p2_retn_cycle <- readRDS("p2_retn_cycle.rds")
  # retn_ratio_plots <- readRDS("retn_ratio_plots.rds")
  # retention_rate_plot_facet <- readRDS("retention_rate_plot_facet.rds")
  # customer_journey_sankey_plot <- readRDS("customer_journey_sankey_plot.rds")
  # mom_churn_plot <- readRDS("mom_churn_plot.rds")
  # churn_nochurn_compare_plot <- readRDS("churn_nochurn_compare_plot.rds")
  # churn_inservice_month_segment_plot <- readRDS("churn_inservice_month_segment_plot.rds")
  # customer_dept_plot <- readRDS("customer_dept_plot.rds")
  # product_category_plot <- readRDS("product_category_plot.rds")
  # streamgraph_market_plot <- readRDS("streamgraph_market_plot.rds")
  # profit_loss_plot_dept1 <- readRDS("profit_loss_plot_dept1.rds")
  # profit_loss_plot_dept2 <- readRDS("profit_loss_plot_dept2.rds")
  # profit_loss_plot_dept3 <- readRDS("profit_loss_plot_dept3.rds")
  # profit_loss_plot_prod1 <- readRDS("profit_loss_plot_prod1.rds")
  # profit_loss_plot_prod2 <- readRDS("profit_loss_plot_prod2.rds")
  # profit_loss_plot_prod3 <- readRDS("profit_loss_plot_prod3.rds")
  # geo_map_leaflet <- readRDS("geo_map_leaflet.rds")
  # curr_prev_yr_sale_prod_sub_plot <- readRDS("curr_prev_yr_sale_prod_sub_plot.rds")
  # sales_distr_sales_rep_plot <- readRDS("sales_distr_sales_rep_plot.rds")
  # sales_country_year_timeseries_plot <- readRDS("sales_country_year_timeseries_plot.rds")
  # top_cust_heatmap_data <- readRDS("top_cust_heatmap_data.rds")
  # retention_cohort_plot <- readRDS("retention_cohort_plot.rds")
  # cust_segment_rfm_gt <- readRDS("cust_segment_rfm_gt.rds")
  # overall_sales_geo1_segment1 <- readRDS("overall_sales_geo1_segment1.rds")
  # overall_sales_geo1_segment2 <- readRDS("overall_sales_geo1_segment2.rds")
  # overall_sales_geo1_segment3 <- readRDS("overall_sales_geo1_segment3.rds")
  # overall_sales_geo1_segment4 <- readRDS("overall_sales_geo1_segment4.rds")
  # overall_sales_geo2_segment1 <- readRDS("overall_sales_geo2_segment1.rds")
  # overall_sales_geo2_segment2 <- readRDS("overall_sales_geo2_segment2.rds")
  # overall_sales_geo2_segment3 <- readRDS("overall_sales_geo2_segment3.rds")
  # overall_sales_geo2_segment4 <- readRDS("overall_sales_geo2_segment4.rds")
  # overall_sales_geo3_segment1 <- readRDS("overall_sales_geo3_segment1.rds")
  # overall_sales_geo3_segment2 <- readRDS("overall_sales_geo3_segment2.rds")
  # overall_sales_geo3_segment3 <- readRDS("overall_sales_geo3_segment3.rds")
  # overall_sales_geo3_segment4 <- readRDS("overall_sales_geo3_segment4.rds")
  # overall_sales_geo4_segment1 <- readRDS("overall_sales_geo4_segment1.rds")
  # overall_sales_geo4_segment2 <- readRDS("overall_sales_geo4_segment2.rds")
  # overall_sales_geo4_segment3 <- readRDS("overall_sales_geo4_segment3.rds")
  # overall_sales_geo4_segment4 <- readRDS("overall_sales_geo4_segment4.rds")
  # overall_sales_geo5_segment1 <- readRDS("overall_sales_geo5_segment1.rds")
  # overall_sales_geo5_segment2 <- readRDS("overall_sales_geo5_segment2.rds")
  # overall_sales_geo5_segment3 <- readRDS("overall_sales_geo5_segment3.rds")
  # overall_sales_geo5_segment4 <- readRDS("overall_sales_geo5_segment4.rds")
  # overall_sales_geo6_segment1 <- readRDS("overall_sales_geo6_segment1.rds")
  # overall_sales_geo6_segment2 <- readRDS("overall_sales_geo6_segment2.rds")
  # overall_sales_geo6_segment3 <- readRDS("overall_sales_geo6_segment3.rds")
  # overall_sales_geo6_segment4 <- readRDS("overall_sales_geo6_segment4.rds")
  # overall_sales_geo7_segment1 <- readRDS("overall_sales_geo7_segment1.rds")
  # overall_sales_geo7_segment2 <- readRDS("overall_sales_geo7_segment2.rds")
  # overall_sales_geo7_segment3 <- readRDS("overall_sales_geo7_segment3.rds")
  # overall_sales_geo7_segment4 <- readRDS("overall_sales_geo7_segment4.rds")
  # product_category_sales_geo1 <- readRDS("product_category_sales_geo1.rds")
  # product_category_sales_geo2 <- readRDS("product_category_sales_geo2.rds")
  # product_category_sales_geo3 <- readRDS("product_category_sales_geo3.rds")
  # product_category_sales_geo4 <- readRDS("product_category_sales_geo4.rds")
  # product_category_sales_geo5 <- readRDS("product_category_sales_geo5.rds")
  # product_category_sales_geo6 <- readRDS("product_category_sales_geo6.rds")
  # product_category_sales_geo7 <- readRDS("product_category_sales_geo7.rds")
  # product_subcateg_metric_geo1 <- readRDS("product_subcateg_metric_geo1.rds")
  # product_subcateg_metric_geo2 <- readRDS("product_subcateg_metric_geo2.rds")
  # product_subcateg_metric_geo3 <- readRDS("product_subcateg_metric_geo3.rds")
  # product_subcateg_metric_geo4 <- readRDS("product_subcateg_metric_geo4.rds")
  # product_subcateg_metric_geo5 <- readRDS("product_subcateg_metric_geo5.rds")
  # product_subcateg_metric_geo6 <- readRDS("product_subcateg_metric_geo6.rds")
  # product_subcateg_metric_geo7 <- readRDS("product_subcateg_metric_geo7.rds")
  # geo_sales_trend_geo1_map <- readRDS("geo_sales_trend_geo1_map.rds")
  # geo_sales_trend_geo2_map <- readRDS("geo_sales_trend_geo2_map.rds")
  # geo_sales_trend_geo3_map <- readRDS("geo_sales_trend_geo3_map.rds")
  # geo_sales_trend_geo4_map <- readRDS("geo_sales_trend_geo4_map.rds")
  # geo_sales_trend_geo5_map <- readRDS("geo_sales_trend_geo5_map.rds")
  # geo_sales_trend_geo6_map <- readRDS("geo_sales_trend_geo6_map.rds")
  # geo_sales_trend_geo7_map <- readRDS("geo_sales_trend_geo7_map.rds")
  # customer_geo_plot2 <- readRDS("customer_geo_plot2.rds")
  # sales_orders_by_geo <- readRDS("sales_orders_by_geo.rds")
  # sales_market_year_timeseries_plot <- readRDS("sales_market_year_timeseries_plot.rds")
  # plot_market_sales_prod_comparison <- readRDS("plot_market_sales_prod_comparison.rds")
  # plot_heatmap_metrics_market <- readRDS("plot_heatmap_metrics_market.rds")
  # plot_price_distr_market <- readRDS("plot_price_distr_market.rds")
  # plot_ticket_size_mkt <- readRDS("plot_ticket_size_mkt.rds")
  # p1_products_sold <- readRDS("p1_products_sold.rds")
  # p2_products_sold <- readRDS("p2_products_sold.rds")
  # p1_new_products <- readRDS("p1_new_products.rds")
  # p2_new_products <- readRDS("p2_new_products.rds")
  # p1_avg_unit_price <- readRDS("p1_avg_unit_price.rds")
  # p2_avg_unit_price <- readRDS("p2_avg_unit_price.rds")
  # p1_basket_size <- readRDS("p1_basket_size.rds")
  # p2_basket_size <- readRDS("p2_basket_size.rds")
  # p1_avg_discount <- readRDS("p1_avg_discount.rds")
  # p2_avg_discount <- readRDS("p2_avg_discount.rds")
  # p1_avg_margin <- readRDS("p1_avg_margin.rds")
  # p2_avg_margin <- readRDS("p2_avg_margin.rds")
  # p1_profit_per_order <- readRDS("p1_profit_per_order.rds")
  # p2_profit_per_order <- readRDS("p2_profit_per_order.rds")
  # p1_avg_days_to_ship <- readRDS("p1_avg_days_to_ship.rds")
  # p2_avg_days_to_ship <- readRDS("p2_avg_days_to_ship.rds")
  # prod_categ_monthly_plot <- readRDS("prod_categ_monthly_plot.rds")
  # prod_profit_cost_plot <- readRDS("prod_profit_cost_plot.rds")
  # prod_subcateg_monthly_plot <- readRDS("prod_subcateg_monthly_plot.rds")
  # prod_subcateg_plot <- readRDS("prod_subcateg_plot.rds")
  # prod_profit_cost_plot <- readRDS("prod_profit_cost_plot.rds")
  # prod_profit_sales_scatterplot <- readRDS("prod_profit_sales_scatterplot.rds")
  # sales_by_manufacturer_plot <- readRDS("sales_by_manufacturer_plot.rds")
  # prodcateg_subcateg_sales_plot <- readRDS("prodcateg_subcateg_sales_plot.rds")
  # prodcateg_subcateg_profit_plot <- readRDS("prodcateg_subcateg_profit_plot.rds")
  # most_bought_products_plot <- readRDS("most_bought_products_plot.rds")
  # product_purchase_combination_plot <- readRDS("product_purchase_combination_plot.rds")
  # cross_selling_prod_income_plot <- readRDS("cross_selling_prod_income_plot.rds")
  # cross_selling_prod_profit_plot <- readRDS("cross_selling_prod_profit_plot.rds")
  # product_cross_merched_plot <- readRDS("product_cross_merched_plot.rds")
  # shipping_mode_plot <- readRDS("shipping_mode_plot.rds")
  # shipping_mode_monthly_plot <- readRDS("shipping_mode_monthly_plot.rds")
  # shipping_status_plot <- readRDS("shipping_status_plot.rds")
  # shipping_status_monthly <- readRDS("shipping_status_monthly.rds")
  # shipping_efficiency_plot <- readRDS("shipping_efficiency_plot.rds")
  # shipping_commit_comply_plot <- readRDS("shipping_commit_comply_plot.rds")
  # geo_map_shipping_cost_leaflet <- readRDS("geo_map_shipping_cost_leaflet.rds")
  # 
  # leaflet_map_shipping_status <- readRDS("leaflet_map_shipping_status.rds")
  # prod_categ_plot <- readRDS("prod_categ_plot.rds")
  # 
  
  
    
#------------------------------------------# Main Shiny Server Plot Code #------------------------------------------#  

  
  #Executive Summary KPIs
  output$sales_kpi =
    renderPlot(
      {
        cowplot::ggdraw(grid.arrange(p1_sales, p2_sales, ncol = 1))+
          theme(panel.background=element_rect(fill = "#2d2e3e",
                                              colour = "#2d2e3e"),
                plot.background = element_rect(fill = "#2d2e3e",
                                               colour = "#2d2e3e"))
      }
    )  
  
  output$profit_kpi =
    renderPlot(
      {
        cowplot::ggdraw(grid.arrange(p1_profit, p2_profit, ncol = 1))+
          theme(panel.background=element_rect(fill = "#2d2e3e",
                                              colour = "#2d2e3e"),
                plot.background = element_rect(fill = "#2d2e3e",
                                               colour = "#2d2e3e"))
      }
    )  
  
  output$profit_ratio_kpi =
    renderPlot(
      {
        cowplot::ggdraw(grid.arrange(p1_profit_ratio, p2_profit_ratio, ncol = 1))+
          theme(panel.background=element_rect(fill = "#2d2e3e",
                                              colour = "#2d2e3e"),
                plot.background = element_rect(fill = "#2d2e3e",
                                               colour = "#2d2e3e"))
      }
    )
  
  output$total_orders_kpi =
    renderPlot(
      {
        cowplot::ggdraw(grid.arrange(p1_total_orders, p2_total_orders, ncol = 1))+
          theme(panel.background=element_rect(fill = "#2d2e3e",
                                              colour = "#2d2e3e"),
                plot.background = element_rect(fill = "#2d2e3e",
                                               colour = "#2d2e3e"))
      }
    )
  
  output$profit_perorder_kpi =
    renderPlot(
      {
        cowplot::ggdraw(grid.arrange(p1_profit_per_order, p2_profit_per_order, ncol = 1))+
          theme(panel.background=element_rect(fill = "#2d2e3e",
                                              colour = "#2d2e3e"),
                plot.background = element_rect(fill = "#2d2e3e",
                                               colour = "#2d2e3e"))
      }
    )
  
  output$avg_days_toship_kpi =
    renderPlot(
      {
        cowplot::ggdraw(grid.arrange(p1_avg_days_to_ship, p2_avg_days_to_ship, ncol = 1))+
          theme(panel.background=element_rect(fill = "#2d2e3e",
                                              colour = "#2d2e3e"),
                plot.background = element_rect(fill = "#2d2e3e",
                                               colour = "#2d2e3e"))
      }
    )
  
  output$corpsales_kpi =
    renderPlot(
      {
        cowplot::ggdraw(grid.arrange(p1_corporate_sales, p2_corporate_sales, ncol = 1))+
          theme(panel.background=element_rect(fill = "#2d2e3e",
                                              colour = "#2d2e3e"),
                plot.background = element_rect(fill = "#2d2e3e",
                                               colour = "#2d2e3e"))
      }
    )
  
  output$profitcust_kpi =
    renderPlot(
      {
        cowplot::ggdraw(grid.arrange(p1_profitcust, p2_profitcust, ncol = 1))+
          theme(panel.background=element_rect(fill = "#2d2e3e",
                                              colour = "#2d2e3e"),
                plot.background = element_rect(fill = "#2d2e3e",
                                               colour = "#2d2e3e"))
      }
    )
  # 
  # #Customer dashboard KPIs
  # output$plot1 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_cust, p2_cust, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$plot2 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_profitcust, p2_profitcust, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$plot3 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_avgsalecust, p2_avgsalecust, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$plot4 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_retn_percent, p2_retn_percent, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$plot5 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_clv, p2_clv, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$plot6 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_avg_recncy_mnth, p2_avg_recncy_mnth, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$plot7 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_avg_freq, p2_avg_freq, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$plot8 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_avg_monetary, p2_avg_monetary, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # 
  # # CUSTOMER DEMOGRAPHICS
  # output$customer_gender_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_gender_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_age_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_age_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_geo_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_geo_plot2)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$Circular_pkn_cust_segment_plot =
  #   renderPlot(
  #     {
  #       Circular_pkn_cust_segment_plot
  #       
  #     }
  #   )
  # 
  # output$customer_income_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_income_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_occupn_income_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_occupn_income_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_edu_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_edu_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # #CUSTOMER LOYALTY
  # output$monthly_sales_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(monthly_sales_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$acqn_order_comparison_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(acqn_order_comparison_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$avg_sale_byorder_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(avg_sale_byorder_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$total_subscrpn_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(total_subscrpn_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$new_cust_acqn_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(new_cust_acqn_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$new_repeat_cust_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(new_repeat_cust_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$retention_cohort_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(retention_cohort_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$revenue_per_cohort_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(revenue_per_cohort_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$revenue_per_cust_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(revenue_per_cust_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_worth_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_worth_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_worth_order_profit_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_worth_order_profit_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_worth_dist_tbl =
  #   render_gt(
  #     customer_worth_dist_tbl
  #   )
  # 
  # #RFM Overview
  # output$rfm_segment_overview =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_rfm_segment, p2_rfm_segment,p3_rfm_geo, ncol=3))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$rfm_segment_detailed =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_segment_drill_r, p2_segment_drill_f,p3_segment_drill_m,rfm_segment_sales_plot, ncol=4))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # #RFM Deep-Dive
  # output$cust_in_segment_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(cust_in_segment_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$sales_in_segment_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(sales_in_segment_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$recency_in_segment_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(recency_in_segment_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_purchasing_time =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_purchasing_time)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$rfm_segm_cust_order_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(rfm_segm_cust_order_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$scatter_rec_freq__plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(scatter_rec_freq__plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$rfm_heatmap_gt =
  #   render_gt(
  #     rfm_heatmap_gt
  #   )
  # 
  # output$cust_segment_rfm_gt =
  #   render_gt(
  #     cust_segment_rfm_gt
  #   )
  # 
  # #Customer Segmentation - Overview
  # output$single_multi_categ_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(single_multi_categ_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$single_multi_categ_over_timeprd_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(single_multi_categ_over_timeprd_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$no_of_orders_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(no_of_orders_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$last_purchase_prod_category_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(last_purchase_prod_category_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$total_spend_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(total_spend_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$total_spend_detailed_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(total_spend_detailed_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # #Customer Segmentation - Purchase Behavior
  # output$customer_spend_type_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_spend_type_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$cust_spend_type_prod_categ_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(cust_spend_type_prod_categ_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$cust_spend_type_purch_behavr_monthyr_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(cust_spend_type_purch_behavr_monthyr_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$cust_spend_type_profit_discount_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(cust_spend_type_profit_discount_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$cust_spend_type_orderyr_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(cust_spend_type_orderyr_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$sales_by_cust_spend_type_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(sales_by_cust_spend_type_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_spend_type_rfm_gt =
  #   render_gt(
  #     customer_spend_type_rfm_gt
  #   )
  # 
  # output$cust_spend_type_hedonic_util_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(cust_spend_type_hedonic_util_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # #Customer Segmentation - Brand Affiliation
  # output$product_impulse_buys_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_impulse_buys_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # grid_arrange_shared_legend2 <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  #   
  #   plots <- list(...)
  #   position <- match.arg(position)
  #   g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  #   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  #   lheight <- sum(legend$height)
  #   lwidth <- sum(legend$width)
  #   gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  #   gl <- c(gl, ncol = ncol, nrow = nrow)
  #   
  #   combined <- switch(position,
  #                      "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
  #                                             legend,
  #                                             ncol = 1,
  #                                             heights = unit.c(unit(1, "npc") - lheight, lheight)),
  #                      "right" = arrangeGrob(do.call(arrangeGrob, gl),
  #                                            legend,
  #                                            ncol = 2,
  #                                            widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  #   
  #   grid.newpage()
  #   grid.draw(combined)
  #   
  #   # return gtable invisibly
  #   invisible(combined)
  #   
  # }
  # 
  # 
  # output$top_manufacturers_overall =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw( grid_arrange_shared_legend2(top_manufacturers_prod1_plot, top_manufacturers_prod2_plot, top_manufacturers_prod3_plot, ncol = 3))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$top_manufacturers_brand_loyal_cust =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid_arrange_shared_legend2(top_manufacturers_brand_loyal_prod1_plot, 
  #                                                   top_manufacturers_brand_loyal_prod2_plot, 
  #                                                   top_manufacturers_brand_loyal_prod3_plot, ncol = 3))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$most_discount_brands_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(most_discount_brands_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # #Retention Plots
  # output$active_customers_layergraph =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(active_customers_layergraph)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$revenue_layergraph =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(revenue_layergraph)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$revenue_percustomer_layergraph =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(revenue_percustomer_layergraph)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$retn_rate_cycle =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_retn_cycle, p2_retn_cycle, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$retn_ratio_plots =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(retn_ratio_plots)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$retention_rate_plot_facet =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(retention_rate_plot_facet)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$customer_journey_sankey_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_journey_sankey_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$mom_churn_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(mom_churn_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$churn_inservice_month_segment_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(churn_inservice_month_segment_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$churn_nochurn_compare_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(churn_nochurn_compare_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$retn_churn_multi_layout =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(mom_churn_plot,churn_nochurn_compare_plot, churn_inservice_month_segment_plot, layout_matrix = retention_plot_lay))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # #Executive Summary - Overview
  # output$customer_dept_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_dept_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # output$product_category_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_category_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$streamgraph_market_plot =
  #   renderStreamgraph(streamgraph_market_plot)
  # 
  # 
  # grid_arrange_shared_legend <- function(...) {
  #   plots <- list(...)
  #   g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  #   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  #   lheight <- sum(legend$height)
  #   grid.arrange(
  #     do.call(arrangeGrob, lapply(plots, function(x)
  #       x + theme(legend.position="none"))),
  #     legend,
  #     ncol = 1,
  #     heights = unit.c(unit(1, "npc") - lheight, lheight))
  # }
  # 
  # 
  # output$profit_loss_plot_dept =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid_arrange_shared_legend(profit_loss_plot_dept1, profit_loss_plot_dept2,profit_loss_plot_dept3, ncol=1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # grid_arrange_shared_legend <- function(...) {
  #   plots <- list(...)
  #   g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  #   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  #   lheight <- sum(legend$height)
  #   grid.arrange(
  #     do.call(arrangeGrob, lapply(plots, function(x)
  #       x + theme(legend.position="none"))),
  #     legend,
  #     ncol = 1,
  #     heights = unit.c(unit(1, "npc") - lheight, lheight))
  # }
  # 
  # 
  # output$profit_loss_plot_prod =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid_arrange_shared_legend(profit_loss_plot_prod1, profit_loss_plot_prod2,profit_loss_plot_prod3, ncol=1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # 
  # #Executive Summary - Deep Dive
  # output$geo_map_leaflet =
  #   renderLeaflet(
  #     {
  #       # leaflet(world_spdf
  #       #         #,options = leafletOptions(minZoom = 1.5, maxZoom = 10)
  #       # ) %>% 
  #       #   addProviderTiles("Esri.WorldGrayCanvas",
  #       #                    options = providerTileOptions(minZoom=1.5, maxZoom=10))%>%
  #       #   #fitBounds(-90, 90, -90,90)%>%
  #       #   setView( lat=10, lng=0 , zoom=2) %>%
  #       #   addPolygons( 
  #       #     fillColor = ~mypalette(sales), 
  #       #     stroke=TRUE, 
  #       #     fillOpacity = 0.9, 
  #       #     color="white", 
  #       #     weight=0.3,
  #       #     label = mytext,
  #       #     labelOptions = labelOptions( 
  #       #       style = list("font-weight" = "normal", padding = "3px 8px"), 
  #       #       textsize = "13px", 
  #       #       direction = "auto"
  #       #     )
  #       #   ) %>%
  #       #   addLegend( pal=mypalette, values=~sales, opacity=0.9, title = "Sales", position = "bottomleft" )
  #       
  #       geo_map_leaflet
  #       
  #     }
  #   )
  # 
  # 
  # output$curr_prev_yr_sale_prod_sub_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(curr_prev_yr_sale_prod_sub_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$sales_distr_sales_rep_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(sales_distr_sales_rep_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$sales_country_year_timeseries_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(sales_country_year_timeseries_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$top_cust_heatmap_plot =
  #   renderPlot(
  #     {
  #       treemap(top_cust_heatmap_data,
  #               index=c("customer_name","product_category"),
  #               vSize="Order_Amount",
  #               vColor = "Total_Order_Amount",
  #               type="dens",
  #               algorithm="pivotSize",
  #               sortID = "-Total_Order_Amount",
  #               palette = "Purples", #YellowGreenBlue Palette
  #               bg.labels=0, # 0 to remove background labels
  #               align.labels=list(
  #                 c("left", "top"),
  #                 c("left", "bottom")
  #               ),
  #               fontsize.labels=8,
  #               lowerbound.cex.labels=.3,
  #               fontfamily.title="sans",
  #               fontfamily.labels="sans",
  #               position.legend="bottom",
  #               border.col="#7E57C2",
  #               title="",
  #               fontsize.title=10,
  #               title.legend="Customer Sales (in $)",
  #               fontsize.legend = 9
  #       )
  #     }
  #   )
  # 
  # # output$record <- renderTable({
  # #   getRecord()
  # # })
  # 
  # output$retention_cohort_plot2 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(retention_cohort_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$cust_segment_rfm_gt2 =
  #   render_gt(
  #     cust_segment_rfm_gt
  #   )
  # 
  # 
  # #Geography Analysis - Overview
  # 
  # #TILE 1
  # output$overall_sales_tile1_geo1 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(overall_sales_geo1_segment1,overall_sales_geo1_segment2,overall_sales_geo1_segment3,overall_sales_geo1_segment4, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$overall_sales_tile1_geo2 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(overall_sales_geo2_segment1,overall_sales_geo2_segment2,overall_sales_geo2_segment3,overall_sales_geo2_segment4, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$overall_sales_tile1_geo3 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(overall_sales_geo3_segment1,overall_sales_geo3_segment2,overall_sales_geo3_segment3,overall_sales_geo3_segment4, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$overall_sales_tile1_geo4 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(overall_sales_geo4_segment1,overall_sales_geo4_segment2,overall_sales_geo4_segment3,overall_sales_geo4_segment4, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$overall_sales_tile1_geo5 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(overall_sales_geo5_segment1,overall_sales_geo5_segment2,overall_sales_geo5_segment3,overall_sales_geo5_segment4, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$overall_sales_tile1_geo6 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(overall_sales_geo6_segment1,overall_sales_geo6_segment2,overall_sales_geo6_segment3,overall_sales_geo6_segment4, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$overall_sales_tile1_geo7 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(overall_sales_geo7_segment1,overall_sales_geo7_segment2,overall_sales_geo7_segment3,overall_sales_geo7_segment4, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # #TILE 2
  # output$product_category_sales_geo1 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_category_sales_geo1)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # output$product_category_sales_geo2 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_category_sales_geo2)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # output$product_category_sales_geo3 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_category_sales_geo3)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # output$product_category_sales_geo4 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_category_sales_geo4)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # output$product_category_sales_geo5 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_category_sales_geo5)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # output$product_category_sales_geo6 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_category_sales_geo6)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # 
  # output$product_category_sales_geo7 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_category_sales_geo7)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # #TILE 3
  # output$product_subcateg_metric_geo1 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_subcateg_metric_geo1)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$product_subcateg_metric_geo2 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_subcateg_metric_geo2)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$product_subcateg_metric_geo3 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_subcateg_metric_geo3)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$product_subcateg_metric_geo4 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_subcateg_metric_geo4)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$product_subcateg_metric_geo5 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_subcateg_metric_geo5)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$product_subcateg_metric_geo6 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_subcateg_metric_geo6)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$product_subcateg_metric_geo7 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_subcateg_metric_geo7)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # #TILE 4
  # output$geo_sales_trend_geo1_map =
  #   renderLeaflet(
  #     {
  #       geo_sales_trend_geo1_map
  #     }
  #   )
  # 
  # output$geo_sales_trend_geo2_map =
  #   renderLeaflet(
  #     {
  #       geo_sales_trend_geo2_map
  #     }
  #   )
  # 
  # output$geo_sales_trend_geo3_map =
  #   renderLeaflet(
  #     {
  #       geo_sales_trend_geo3_map
  #     }
  #   )
  # 
  # output$geo_sales_trend_geo4_map =
  #   renderLeaflet(
  #     {
  #       geo_sales_trend_geo4_map
  #     }
  #   )
  # 
  # output$geo_sales_trend_geo5_map =
  #   renderLeaflet(
  #     {
  #       geo_sales_trend_geo5_map
  #     }
  #   )
  # 
  # output$geo_sales_trend_geo6_map =
  #   renderLeaflet(
  #     {
  #       geo_sales_trend_geo6_map
  #     }
  #   )
  # 
  # output$geo_sales_trend_geo7_map =
  #   renderLeaflet(
  #     {
  #       geo_sales_trend_geo7_map
  #     }
  #   )
  # 
  # 
  # #Geography - DeepDive
  # output$customer_geo_plot2 =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(customer_geo_plot2)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$sales_orders_by_geo =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(sales_orders_by_geo)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$sales_market_year_timeseries_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(sales_market_year_timeseries_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$plot_market_sales_prod_comparison =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(plot_market_sales_prod_comparison)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$plot_heatmap_metrics_market =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(plot_heatmap_metrics_market)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$plot_price_distr_market =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(plot_price_distr_market)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$plot_ticket_size_mkt =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(plot_ticket_size_mkt)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # #PRODUCT KPIs
  # output$products_sold_kpi =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_products_sold, p2_products_sold, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$new_products_kpi =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_new_products, p2_new_products, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$avg_unit_price_kpi =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_avg_unit_price, p2_avg_unit_price, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$basket_size_kpi =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_basket_size, p2_basket_size, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$avg_discount_kpi =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_avg_discount, p2_avg_discount, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$avg_margin_kpi =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_avg_margin, p2_avg_margin, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$profit_per_order_kpi =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_profit_per_order, p2_profit_per_order, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # output$avg_days_to_ship_kpi =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(p1_avg_days_to_ship, p2_avg_days_to_ship, ncol = 1))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # #Product Analysis
  # output$prod_categ_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(prod_categ_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$prod_categ_monthly_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(prod_categ_monthly_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$prod_profit_cost_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(prod_profit_cost_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$prod_subcateg_monthly_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(prod_subcateg_monthly_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$prod_subcateg_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(prod_subcateg_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$prod_profit_cost_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(prod_profit_cost_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$prod_profit_sales_scatterplot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(prod_profit_sales_scatterplot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$sales_by_manufacturer_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(sales_by_manufacturer_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$prodcateg_subcateg_sales_profit_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(grid.arrange(prodcateg_subcateg_sales_plot, prodcateg_subcateg_profit_plot, ncol = 2))+
  #         theme(panel.background=element_rect(fill = "#2d2e3e",
  #                                             colour = "#2d2e3e"),
  #               plot.background = element_rect(fill = "#2d2e3e",
  #                                              colour = "#2d2e3e"))
  #     }
  #   )
  # 
  # #Product - Market Analysis
  # output$most_bought_products_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(most_bought_products_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )  
  # 
  # output$product_purchase_combination_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_purchase_combination_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$cross_selling_prod_income_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(cross_selling_prod_income_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$cross_selling_prod_profit_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(cross_selling_prod_profit_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )
  # 
  # output$product_cross_merched_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(product_cross_merched_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )  
  # #Shipping Analysis
  # 
  # #OVerview
  # output$shipping_mode_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(shipping_mode_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )  
  # 
  # output$shipping_mode_monthly_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(shipping_mode_monthly_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )  
  # 
  # output$shipping_status_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(shipping_status_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )  
  # 
  # output$shipping_status_monthly =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(shipping_status_monthly)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )  
  # 
  # #Shipping Efficiency
  # output$geo_map_shipping_cost_leaflet =
  #   renderLeaflet(
  #     {
  #       # leaflet(world_spdf) %>% 
  #       # addTiles()  %>% 
  #       # setView( lat=10, lng=0 , zoom=2) %>%
  #       # addPolygons( 
  #       #   fillColor = ~mypalette_shipping_cost(shipping_cost), 
  #       #   stroke=TRUE, 
  #       #   fillOpacity = 0.9, 
  #       #   color="white", 
  #       #   weight=0.3,
  #       #   label = label_shipping_cost,
  #       #   labelOptions = 
  #       #     labelOptions( 
  #       #       style = list("font-weight" = "normal", padding = "3px 8px"), 
  #       #       textsize = "13px", 
  #       #       direction = "auto"
  #       #     )
  #       # )%>%
  #       # addLegend(pal=mypalette_shipping_cost, values=~shipping_cost, opacity=0.9, title = "Shipping Cost", position = "bottomleft" )
  #       
  #       geo_map_shipping_cost_leaflet
  #     }
  #   )
  # 
  # output$shipping_efficiency_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(shipping_efficiency_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )  
  # 
  # #Shipping Commitment & Compliance
  # 
  # output$shipping_status_geomap_leaflet =
  #   renderLeaflet(
  #     {
  #       # leaflet()%>% 
  #       #   addTiles()%>% 
  #       #   setView( lat=30, lng=20 , zoom=1.5) %>%
  #       #   addMinicharts(world_spdf@data$LON,
  #       #                 world_spdf@data$LAT,
  #       #                 type = "pie", 
  #       #                 chartdata = minichart_data, 
  #       #                 colorPalette = minichart_colors, 
  #       #                 legend = TRUE, 
  #       #                 legendPosition = "bottomleft")
  #       
  #       leaflet_map_shipping_status
  #     }
  #   )
  # 
  # output$shipping_commit_comply_plot =
  #   renderPlot(
  #     {
  #       cowplot::ggdraw(shipping_commit_comply_plot)+
  #         theme(panel.background=element_rect(fill = "white",
  #                                             colour = "white"),
  #               plot.background = element_rect(fill = "white",
  #                                              colour = "white"))
  #     }
  #   )  
  # 
}


#------------------------------------------# Shiny UI Code #------------------------------------------#  

ui=navbarPage(
  theme=shinytheme("slate"),
  "Customer Analytics",
  tabPanel("Executive Summary",
           
           tabsetPanel(
             
             tabPanel("KPI",
                      fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
                      fluidRow(
                        column(3,plotOutput("sales_kpi")),
                        column(3,plotOutput("profit_kpi")),
                        column(3,plotOutput("profit_ratio_kpi")),
                        column(3,plotOutput("total_orders_kpi"))
                      ),
                      fluidRow(column(width = 6, offset = 0, style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:25px')
                      ),
                      fluidRow(
                        column(3,plotOutput("profit_perorder_kpi")),
                        column(3,plotOutput("avg_days_toship_kpi")),
                        column(3,plotOutput("corpsales_kpi")),
                        column(3,plotOutput("profitcust_kpi"))
                      ),
                      fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
             )#,# End of Executive Summary KPI tabPanel()
             
             # 
             # tabPanel("Overview",
             #          fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
             #          
             #          fluidRow(
             #            column(3,
             #                   box(title = "Sales By Customer Segment", width=12,
             #                       solidHeader = TRUE,
             #                       plotOutput("customer_dept_plot")
             #                   )
             #            ),
             #            column(3,
             #                   box(title = "Sales By Product Category", width=12,
             #                       solidHeader = TRUE,
             #                       plotOutput("product_category_plot")
             #                   )
             #            ),
             #            column(6,
             #                   box(title = "Sales By Target Market", width=12,
             #                       solidHeader = TRUE,
             #                       style = "background-color: white;",
             #                       streamgraphOutput("streamgraph_market_plot")
             #                   )
             #            )
             #          ),
             #          
             #          fluidRow(
             #            column(6,
             #                   box(title = "Profit, Loss By Customer Segment", width=12,
             #                       solidHeader = TRUE,
             #                       plotOutput("profit_loss_plot_dept")
             #                   )
             #            ),
             #            column(6,
             #                   box(title = "Profit, Loss By Product Category", width=12,
             #                       solidHeader = TRUE,
             #                       plotOutput("profit_loss_plot_prod")
             #                   )
             #            )
             #          ),
             #          
             #          fluidRow(
             #            column(5,
             #                   box(title = "Current Vs. Previous Yr Sales", width=12,
             #                       solidHeader = TRUE,
             #                       plotOutput("curr_prev_yr_sale_prod_sub_plot")
             #                   )
             #            ),
             #            column(7,
             #                   box(title = "Sales Generated By Sales Reps", width=12,
             #                       solidHeader = TRUE,
             #                       plotOutput("sales_distr_sales_rep_plot")
             #                   )
             #            )
             #          ),
             #          
             #          fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
             # ),# End of Executive Summary Overview tabPanel()
             # 
             # tabPanel("Sales, Customer",
             #          fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
             #          
             #          fluidRow(
             #            column(width = 7,
             #                   fluidRow(
             #                     column(12,
             #                            box(title = "Sales Distribution - Worldwide", width=12,
             #                                style = "height:855px; background-color: white;",
             #                                solidHeader = TRUE,
             #                                leafletOutput("geo_map_leaflet",height=855)
             #                            )
             #                     )
             #                   )
             #            ),
             #            
             #            column(width=5,
             #                   fluidRow(style = "height:300px;",
             #                            column(12,
             #                                   box(title = "Monthly Sales By Country", width=12,
             #                                       solidHeader = TRUE,
             #                                       plotOutput("sales_country_year_timeseries_plot")
             #                                   )
             #                            )
             #                   ),
             #                   fluidRow(style = "height:300px;",
             #                            column(12,
             #                                   box(title = "Top 10 Customers", width=12,
             #                                       solidHeader = TRUE,
             #                                       plotOutput("top_cust_heatmap_plot")
             #                                   )
             #                            )
             #                   )
             #            )
             #          ),
             #          
             #          fluidRow(
             #            column(8,
             #                   box(title = "Customer Retention Cohort", width=12,
             #                       style = "height:470px; background-color: white;",
             #                       solidHeader = TRUE,
             #                       plotOutput("retention_cohort_plot2",height=470)
             #                   )
             #            ),
             #            column(4,
             #                   box(title = "Customer Segment Vs Avg RFM", width=12,
             #                       style = "height:470px; background-color: white;",
             #                       solidHeader = TRUE,
             #                       gt_output("cust_segment_rfm_gt2")
             #                   )
             #            )
             #          ),
             #          
             #          fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
             #          
             # )# End of Executive Summary Deepdive tabPanel()
             
           )# End of Executive Summary tabsetPanel()
           
  )#,# End of Executive Summary Main Header tabPanel() - #---# Exec Summary Ends Here #---#
  
  # 
  # tabPanel("Customer",
  #          tabsetPanel(
  #            
  #            tabPanel("KPI",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     fluidRow(
  #                       column(3,plotOutput("plot1")),
  #                       column(3,plotOutput("plot2")),
  #                       column(3,plotOutput("plot3")),
  #                       column(3,plotOutput("plot4"))
  #                     ),
  #                     fluidRow(column(width = 6, offset = 0, style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:25px')
  #                     ),
  #                     fluidRow(
  #                       column(3,plotOutput("plot5")),
  #                       column(3,plotOutput("plot6")),
  #                       column(3,plotOutput("plot7")),
  #                       column(3,plotOutput("plot8"))
  #                     ),
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            tabPanel("Demographics",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Gender", width=12,
  #                                  status = "primary",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_gender_plot")
  #                              )
  #                       ),
  #                       column(8,
  #                              box(title = "Age", width=12,
  #                                  status = "warning",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_age_plot")
  #                              )
  #                       )
  #                       
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Geography", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_geo_plot")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Business Segment", width=12,
  #                                  plotOutput("Circular_pkn_cust_segment_plot")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Income", width=12,
  #                                  plotOutput("customer_income_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Education", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_edu_plot")
  #                              )
  #                       ),
  #                       column(8,
  #                              box(title = "Occupation, Income", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_occupn_income_plot")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            tabPanel("Customer Value",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Monthly Sales", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("monthly_sales_plot")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "New Customer Acqn.", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("new_cust_acqn_plot")
  #                              )
  #                       ),
  #                       # column(4,
  #                       #        box(title = "Running Total of Customers", width=12,
  #                       #            solidHeader = TRUE,
  #                       #            plotOutput("total_subscrpn_plot")
  #                       #        )
  #                       #        )
  #                       column(4,
  #                              box(title = "New, Repeat Customers", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("new_repeat_cust_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(3,
  #                              box(title = "Yearly Sales By Acqn. Year", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("acqn_order_comparison_plot")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(title = "Avg. Sales By No. of Orders", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("avg_sale_byorder_plot")
  #                              )
  #                       ),
  #                       column(6,
  #                              box(title = "Revenue Per Cohort", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("revenue_per_cohort_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(6,
  #                              box(title = "Avg Revenue/Customer Cohort", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("revenue_per_cust_plot")
  #                              )
  #                       ),
  #                       column(6,
  #                              box(title = "Customer Retention Cohort", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("retention_cohort_plot")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(
  #                       column(3,
  #                              box(title = "Customer Worth", width=12,
  #                                  style = "height:450px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_worth_plot",height=450)
  #                              )
  #                       ),
  #                       column(3,
  #                              box(title = "Customer Worth, Profit & Order", width=12,
  #                                  style = "height:450px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_worth_order_profit_plot",height=450)
  #                              )
  #                       ),
  #                       column(6,
  #                              box(title = "Customer Worth Summary", width=12,
  #                                  style = "height:450px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  gt_output("customer_worth_dist_tbl")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            tabPanel("Segmentation - Overview",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(3,
  #                              box(title = "Number of Categories Purchased", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("single_multi_categ_plot")
  #                              )
  #                       ),
  #                       column(5,
  #                              box(title = "Categories Purchased Over Time", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("single_multi_categ_over_timeprd_plot")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Number of Orders By Customer", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("no_of_orders_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(3,
  #                              box(title = "Last Purchase Date Bucket", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("last_purchase_prod_category_plot")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Total Spend Bucket", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("total_spend_plot")
  #                              )
  #                       ),
  #                       column(5,
  #                              box(title = "Total Spend Bucket Over Time", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("total_spend_detailed_plot")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            tabPanel("Segmentation - Purchase Behaviour",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(3,
  #                              box(title = "Customer Spend Type", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_spend_type_plot")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(title = "Spend Type by Order Year", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("cust_spend_type_orderyr_plot")
  #                              )
  #                       ),
  #                       column(6,
  #                              box(title = "Sales by Customer Spend Type Over Time", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("sales_by_cust_spend_type_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Profit Vs Discount% by Customer Spend Type", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("cust_spend_type_profit_discount_plot")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Customer Spend Type by AVG RFM", width=12,
  #                                  style = "height:400px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  gt_output("customer_spend_type_rfm_gt")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Customer Spend Type by Hedonic/Util. Split ", width=12,
  #                                  style = "height:400px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("cust_spend_type_hedonic_util_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Spend Type by Product Category", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("cust_spend_type_prod_categ_plot")
  #                              )
  #                       ),
  #                       column(8,
  #                              box(title = "Spend Type by Product Category Over Time", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("cust_spend_type_purch_behavr_monthyr_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            tabPanel("Segmentation - Brand Affiliation",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Products Bought on Impulse", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_impulse_buys_plot")
  #                              )
  #                       ),
  #                       column(8,
  #                              box(title = "Top Brands by Product Category", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("top_manufacturers_overall")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(
  #                       column(7,
  #                              box(title = "Top Brands Bought by Brand Loyal Customers", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("top_manufacturers_brand_loyal_cust")
  #                              )
  #                       ),
  #                       column(5,
  #                              box(title = "Top Brand's Discount Vs Profit Comparison", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("most_discount_brands_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            tabPanel("RFM Analysis - Overview",
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(12,
  #                              box(title = "RFM Analysis Overview", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("rfm_segment_overview")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(12,
  #                              box(title = "RFM Customer Base", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("rfm_segment_detailed")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            
  #            tabPanel("RFM Analysis - DeepDive",
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(width=7,
  #                              fluidRow(style = "height:300px;",
  #                                       column(5,
  #                                              box(title = "Customers In Each Segment", width=12,
  #                                                  solidHeader = TRUE,
  #                                                  plotOutput("cust_in_segment_plot")
  #                                              )
  #                                       ),
  #                                       column(7,
  #                                              box(title = "Customer Purchasing Periods", width=12,
  #                                                  solidHeader = TRUE,
  #                                                  plotOutput("customer_purchasing_time")
  #                                              )
  #                                       )
  #                              ),
  #                              fluidRow(style = "height:300px;",
  #                                       column(5,
  #                                              box(title = "Sales In Each Segment", width=12,
  #                                                  solidHeader = TRUE,
  #                                                  plotOutput("sales_in_segment_plot")
  #                                              )
  #                                       ),
  #                                       column(7,
  #                                              box(title = "No. of Customers Vs No. of Orders", width=12,
  #                                                  solidHeader = TRUE,
  #                                                  plotOutput("rfm_segm_cust_order_plot")
  #                                              )
  #                                       )
  #                              )
  #                       ),
  #                       column(width = 5,
  #                              fluidRow(
  #                                column(12,
  #                                       box(title = "RFM Heatmap", width=12, 
  #                                           style = "height:855px; background-color: white;",
  #                                           solidHeader = TRUE,
  #                                           gt_output("rfm_heatmap_gt")
  #                                       )
  #                                )
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(3,
  #                              box(title = "Days Since Last Purchase", width=12,
  #                                  style = "height:470px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("recency_in_segment_plot",height=470)
  #                              )
  #                       ),
  #                       column(5,
  #                              box(title = "Segment Performance R Vs F", width=12,
  #                                  style = "height:470px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("scatter_rec_freq__plot",height=470)
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Customer Segment Vs Avg RFM", width=12,
  #                                  style = "height:470px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  gt_output("cust_segment_rfm_gt")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            tabPanel("Retention",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Active Customer by Cohort ", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("active_customers_layergraph")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Total Revenue By Cohort", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("revenue_layergraph")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Avg Revenue by Cohort", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("revenue_percustomer_layergraph")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     # fluidRow(
  #                     #   column(6,
  #                     #          box(title = "Customer Retention Rate Cycle", width=12,
  #                     #              solidHeader = TRUE,
  #                     #              plotOutput("retn_rate_cycle")
  #                     #              )
  #                     #          ),
  #                     #   column(6,
  #                     #          box(title = "Cohort Retention Rate Dynamics", width=12,
  #                     #              solidHeader = TRUE,
  #                     #              plotOutput("retn_ratio_plots")
  #                     #              )
  #                     #          )
  #                     #   ),
  #                     
  #                     fluidRow(
  #                       column(6,
  #                              box(title = "Customer Journey", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_journey_sankey_plot")
  #                              )
  #                       ),
  #                       column(6,
  #                              box(title = "Cohort Retention Rate Dynamics", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("retention_rate_plot_facet")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       # column(12,
  #                       #        box(title = "Churn Analysis", width=12,
  #                       #            style = "height:550px; background-color: white;",
  #                       #            solidHeader = TRUE,
  #                       #            plotOutput("retn_churn_multi_layout",height=550)
  #                       #        )
  #                       # )
  #                       column(4,
  #                              box(title = "Customers - No order Last 3M", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("mom_churn_plot")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Tenure Comparison - Active Vs Churned", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("churn_nochurn_compare_plot")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Churned Customers by Tenure Segments ", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("churn_inservice_month_segment_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            )
  #          )
  # ),
  # tabPanel("Product",
  #          tabsetPanel(
  #            
  #            tabPanel("KPI",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     fluidRow(
  #                       column(3,plotOutput("products_sold_kpi")),
  #                       column(3,plotOutput("new_products_kpi")),
  #                       column(3,plotOutput("avg_unit_price_kpi")),
  #                       column(3,plotOutput("basket_size_kpi"))
  #                     ),
  #                     fluidRow(column(width = 6, offset = 0, style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:25px')
  #                     ),
  #                     fluidRow(
  #                       column(3,plotOutput("avg_discount_kpi")),
  #                       column(3,plotOutput("avg_margin_kpi")),
  #                       column(3,plotOutput("profit_per_order_kpi")),
  #                       column(3,plotOutput("avg_days_to_ship_kpi"))
  #                     ),
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),# End of Product KPI tabPanel()
  #            
  #            tabPanel("Category Analysis",
  #                     
  #                     fluidRow(column(width = 12, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(12,
  #                              box(title = "Sales By Products Category", width=12,
  #                                  solidHeader = TRUE,
  #                                  
  #                                  splitLayout(cellWidths = c("33%", "67%"), plotOutput("prod_categ_plot"), plotOutput("prod_categ_monthly_plot"))
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(12,
  #                              box(title = "Sales By Product Sub-Category", width=12,
  #                                  solidHeader = TRUE,
  #                                  splitLayout(cellWidths = c("67%", "33%"), plotOutput("prod_subcateg_monthly_plot"), plotOutput("prod_subcateg_plot"))
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;'))
  #                     
  #            ),# End of Product -> Category Analysis tabPanel()
  #            tabPanel("Sales, Profit Analysis",
  #                     
  #                     fluidRow(column(width =12 , offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(4,
  #                              box(title = "Product Sales & Profit Analysis", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("prod_profit_cost_plot")
  #                              )
  #                       ),
  #                       
  #                       column(4,
  #                              box(title = "Sales Vs Profit", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("prod_profit_sales_scatterplot")
  #                              )
  #                       ),
  #                       
  #                       column(4,
  #                              box(title = "Sales By Manufacturer", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("sales_by_manufacturer_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(12,
  #                              box(title = "Sales By Product Sub-Category", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("prodcateg_subcateg_sales_profit_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;'))
  #                     
  #            ),# End of Product -> Sales,Profit Analysis tabPanel()
  #            
  #            tabPanel("Market Analysis",
  #                     
  #                     fluidRow(column(width = 12, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(6,
  #                              box(title = "Most Bought Products", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("most_bought_products_plot")
  #                              )
  #                       ),
  #                       
  #                       column(6,
  #                              box(title = "product that can be cross merchandised.", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_cross_merched_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(6,
  #                              box(title = "Products Bought in Combination", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_purchase_combination_plot")
  #                              )
  #                       ),
  #                       
  #                       column(6,
  #                              box(title = "Cross-selling product generating most income", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("cross_selling_prod_income_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(6,
  #                              box(title = "Cross-selling product generating most Profit", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("cross_selling_prod_profit_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;'))
  #                     
  #            )# End of Product -> Market Analysis tabPanel()
  #          )# End of Product tabsetPanel()
  # ),# End of Product Main tabPanel()
  # 
  # tabPanel("Shipping",
  #          tabsetPanel(
  #            tabPanel("Overview",
  #                     
  #                     fluidRow(column(width = 12, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(12,
  #                              box(title = "Shipping Mode", width=12,
  #                                  # style = "height:450px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  splitLayout(cellWidths = c("30%", "70%"), plotOutput("shipping_mode_plot"), plotOutput("shipping_mode_monthly_plot"))
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(12,
  #                              box(title = "Shipping Status", width=12,
  #                                  # style = "height:450px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  splitLayout(cellWidths = c("30%", "70%"), plotOutput("shipping_status_plot"), plotOutput("shipping_status_monthly"))
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            tabPanel("Shipping Efficiency",
  #                     
  #                     fluidRow(column(width = 12, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(width = 7,
  #                              box(title = "Shipping Cost Distribution", width=12,
  #                                  style = "height:855px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("geo_map_shipping_cost_leaflet",height=855)
  #                              )
  #                       ),
  #                       
  #                       column(width = 5,
  #                              box(title = "Shipping Efficiency", width=12,
  #                                  style = "height:855px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("shipping_efficiency_plot",height=855)
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            ),
  #            
  #            tabPanel("Shipping Commitment & Compliance",
  #                     
  #                     fluidRow(column(width = 12, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(width = 7,
  #                              box(title = "Shipping Delivery Status - Geographical", width=12,
  #                                  style = "height:855px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("shipping_status_geomap_leaflet",height=855)
  #                              )                        ),
  #                       column(5,
  #                              box(title = "Shipping Commitment Compliance", width=12,
  #                                  style = "height:855px; background-color: white;",
  #                                  solidHeader = TRUE,
  #                                  plotOutput("shipping_commit_comply_plot",height=855)
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #            )# End of Shipping Commitment & Compliance tabPanel()
  #          )# End of Shipping Main tabsetPanel()
  # ),# End of Shipping main tabPanel()
  # 
  # 
  # tabPanel("Geography",
  #          tabsetPanel(
  #            tabPanel("Overview",
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     
  #                     #geo1
  #                     fluidRow(
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("overall_sales_tile1_geo1")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_category_sales_geo1")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_subcateg_metric_geo1")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("geo_sales_trend_geo1_map")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;')),
  #                     
  #                     #geo2
  #                     fluidRow(
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("overall_sales_tile1_geo2")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_category_sales_geo2")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_subcateg_metric_geo2")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("geo_sales_trend_geo2_map")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;')),
  #                     
  #                     #geo3
  #                     fluidRow(
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("overall_sales_tile1_geo3")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_category_sales_geo3")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_subcateg_metric_geo3")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("geo_sales_trend_geo3_map")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;')),
  #                     
  #                     #geo4
  #                     fluidRow(
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("overall_sales_tile1_geo4")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_category_sales_geo4")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_subcateg_metric_geo4")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("geo_sales_trend_geo4_map")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;')),
  #                     
  #                     #geo5
  #                     fluidRow(
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("overall_sales_tile1_geo5")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_category_sales_geo5")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_subcateg_metric_geo5")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("geo_sales_trend_geo5_map")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;')),
  #                     
  #                     #geo6
  #                     fluidRow(
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("overall_sales_tile1_geo6")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_category_sales_geo6")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_subcateg_metric_geo6")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("geo_sales_trend_geo6_map")
  #                              )
  #                       )
  #                     ),
  #                     fluidRow(column(width = 12, offset = 0, style='padding-bottom:15px;')),
  #                     
  #                     #geo7
  #                     fluidRow(
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("overall_sales_tile1_geo7")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_category_sales_geo7")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("product_subcateg_metric_geo7")
  #                              )
  #                       ),
  #                       column(3,
  #                              box(width=12,
  #                                  solidHeader = TRUE,
  #                                  leafletOutput("geo_sales_trend_geo7_map")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #                     
  #            ),
  #            tabPanel("DeepDive",
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-top:15px;')),
  #                     
  #                     fluidRow(
  #                       column(3,
  #                              box(title = "Geography", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("customer_geo_plot2")
  #                              )
  #                       ),
  #                       column(5,
  #                              box(title = "Sales, Orders By Region", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("sales_orders_by_geo")
  #                              )
  #                       ),
  #                       column(4,
  #                              box(title = "Daily Sales By Target Market", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("sales_market_year_timeseries_plot")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(7,
  #                              box(title = "Sales Comparison - Tech Vs furniture", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("plot_market_sales_prod_comparison")
  #                              )
  #                       ),
  #                       column(5,
  #                              box(title = "Sales distribution by Market, Product", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("plot_heatmap_metrics_market")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(
  #                       column(5,
  #                              box(title = "Price distribution by Market", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("plot_price_distr_market")
  #                              )
  #                       ),
  #                       column(7,
  #                              box(title = "Number of Txns at Price point ", width=12,
  #                                  solidHeader = TRUE,
  #                                  plotOutput("plot_ticket_size_mkt")
  #                              )
  #                       )
  #                     ),
  #                     
  #                     fluidRow(column(width = 6, offset = 0, style='padding-bottom:15px;'))
  #                     
  #            )# End of Geography Deepdive tabPanel()
  #            
  #          )# End of Geography tabsetPanel()
  #          
  # )# End of Geography Main Header tabPanel() - #---# Geography Ends Here #---#
  # 
)# End of Shiny UI Function




shinyApp(ui = ui, server = server)

