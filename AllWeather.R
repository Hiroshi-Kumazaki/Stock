install.packages("tidyquant")
library(tidyquant)

stock_prices <- c("VTI", "VGIT", "EDV", "IAU") %>%
    tq_get(get="stock.prices")
stock_prices

stock_returns_monthly <- stock_prices %>% group_by(symbol) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                 period = "monthly", col_rename = "Ra")
stock_returns_monthly

wts <- c(0.3, 0.15, 0.4, 0.15)
portfolio_returns_monthly <- stock_returns_monthly %>%
    tq_portfolio(assets_col = symbol, returns_col = Ra, 
                 weights = wts, col_rename = "Ra", 
                 rebalance_on = "years")

portfolio_returns_monthly %>%
    ggplot(aes(x=date, y=Ra)) + 
    geom_bar(stat="identity", fill=palette_light()[[1]]) +
    labs(title="Portfolio Returns", 
         subtitle="30% VTI, 15% VGIT, 40% EDV, 15% IAU", 
         caption = "Shows an above-zero trend meaning positive returns", 
         x="", y="Monthly Returns") +
    geom_smooth(method = "lm") + theme_tq() +
    scale_color_tq() + scale_y_continuous(labels=scales::percent)


portfolio_growth_monthly <- stock_returns_monthly %>%
    tq_portfolio(assets_col   = symbol, 
                 returns_col  = Ra, 
                 weights      = wts, 
                 col_rename   = "investment.growth",
                 wealth.index = TRUE) %>%
    mutate(investment.growth = investment.growth * 10000)

portfolio_growth_monthly %>%
    ggplot(aes(x = date, y = investment.growth)) +
    geom_line(size = 2, color = palette_light()[[1]]) +
    labs(title = "Portfolio Growth",
         subtitle = "30% VTI, 15% VGIT, 40% EDV, 15% IAU",
         caption = "Now we can really visualize performance!",
         x = "", y = "Portfolio Value") +
    geom_smooth(method = "loess") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar)


