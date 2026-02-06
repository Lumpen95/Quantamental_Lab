hc_stock_basic <- function(x, title) {
  
  if (!is.xts(x)) stop("x must be an xts object.")
  
  # 전역 변수 설정
  hc_lang <- getOption("highcharter.lang")
  hc_lang$thousandsSep <- ","
  options(
    highcharter.lang = hc_lang
  )
  
  updated.t <- attr(x, "updated")
  sub_title <- format(updated.t, "%Y-%m-%d %H:%M")
  
  x <- x |> na.omit()
  
  highchart(
    type = "stock"
  ) |> 
    hc_rangeSelector(
      selected = 0,
      dropdown = "always",
      buttonTheme = list(
        r = 3,
        states = list(
          hover = list(
            fill = "#4a4a4a", # 배경색
            style = list(color = "#FFFFFF") # 글자색
          ),
          select = list(
            fill = "#E0E0E0",
            style = list(
              color = "#2b2b2b",
              fontWeight = "bold"
            )
          )
        )
      ),
      x = 20
    ) |> 
    hc_add_series(type = "candlestick", name = "price", data = OHLC(x)) |> 
    hc_add_series(type = "column", name = "Volume", data = Vo(x), yAxis = 1) |> 
    hc_plotOptions(
      candlestick = list(
        color = "blue",
        upColor = "red"
      )
    ) |> 
    hc_tooltip(
      shape = "rect",
      headerShape = "callout",
      shadow = F,
      fixed = TRUE,
      borderWidth = 0.1,
      valueDecimals = 2,
      style = list(
        fontSize = "13px"
      )
    ) |> 
    hc_yAxis_multiples(
      list(
        title = list(text = "Price"),
        lineWidth = 2,
        labels = list(x = -3, 
                      style = list(fontSize = "13px")),
        height = "68%"),
      list(
        title = list(text = "Volume"),
        lineWidth = 2,
        labels = list(x = -3,
                      style = list(fontSize = "13px")),
        top = "70%",
        height = "30%",
        offset = 0)
    ) |> 
    hc_title(text = title,
             style = list(fontSize = "25px", fontWeight = "bold"),
             align = "left", x = 20, y = 30) |> 
    hc_subtitle(text = sub_title,
                style = list(fontSize = "18px"),
                align = "left",
                x = 20, y = 55) |> 
    hc_legend(
      enabled = TRUE,
      layout = "vertical",
      align = "left",
      verticalAlign = "top",
      floating = TRUE,
      x = 10,
      y = 10,
      backgroundColor = "rgba(0,0,0,0)",
      itemStyle = list(
        color = "#E0E0E0",
        fontSize = "15px"
      ),
      symbolHeight = 10,
      symbolWidth = 10,
      symbolRadius = 2
    ) |> 
    hc_add_theme(hc_theme_darkunica())
}

hc_stock_lp <- function(x, title) {
  
  if (!is.xts(x)) stop("x must be an xts object.")

  # 전역 변수 설정
  hc_lang <- getOption("highcharter.lang")
  hc_lang$thousandsSep <- ","
  options(
    highcharter.lang = hc_lang
  )
  
  updated.t <- attr(x, "updated")
  sub_title <- format(updated.t, "%Y-%m-%d %H:%M")
  
  x <- x |> na.omit()
  
  x.sma.5 <- TTR::SMA(Cl(x), n = 5)
  x.sma.20 <- TTR::SMA(Cl(x), n = 20)
  x.sma.60 <- TTR::SMA(Cl(x), n = 60)
  x.sma.200 <- TTR::SMA(Cl(x), n = 200)
  x.v.sma.10 <- TTR::SMA(Vo(x), n = 10)
  
  x.macd <- TTR::MACD(Cl(x))
  x.macd$hist <- x.macd$macd - x.macd$signal
  
  highchart(
    type = "stock"
  ) |> 
  hc_rangeSelector(
    selected = 0,
    dropdown = "always",
    buttonTheme = list(
      r = 3,
      states = list(
        hover = list(
          fill = "#4a4a4a", # 배경색
          style = list(color = "#FFFFFF") # 글자색
        ),
        select = list(
          fill = "#E0E0E0",
          style = list(
            color = "#2b2b2b",
            fontWeight = "bold"
          )
        )
      )
    ),
    x = 20
    ) |> 
    hc_add_series(type = "candlestick", name = "price", data = OHLC(x)) |> 
    hc_add_series(name = "SMA 5", data = x.sma.5) |> 
    hc_add_series(name = "SMA 20", data = x.sma.20) |> 
    hc_add_series(name = "SMA 60", data = x.sma.60) |> 
    hc_add_series(name = "SMA 200", data = x.sma.200) |> 
    hc_add_series(type = "column", name = "Volume", data = Vo(x), yAxis = 1) |> 
    hc_add_series(name = "SMA 10", data = x.v.sma.10, yAxis = 1) |> 
    hc_add_series(type = "column", name = "MACD HIST", data = x.macd$hist, yAxis = 2,
                  color = "red", negativeColor = "blue") |> 
    hc_plotOptions(
      candlestick = list(
        color = "blue",
        upColor = "red"
      )
    ) |> 
    hc_tooltip(
      shape = "rect",
      headerShape = "callout",
      shadow = F,
      fixed = TRUE,
      borderWidth = 0.1,
      valueDecimals = 2,
      style = list(
        fontSize = "13px"
      )
    ) |> 
    hc_yAxis_multiples(
      list(
        title = list(text = "Price"),
        lineWidth = 2,
        labels = list(x = -3, 
                      style = list(fontSize = "13px")),
        height = "48%"),
      list(
        title = list(text = "Volume"),
        lineWidth = 2,
        labels = list(x = -3,
                      style = list(fontSize = "13px")),
        top = "50%",
        height = "30%",
        offset = 0),
      list(
        title = list(text = "MACD"),
        lineWidth = 2,
        labels = list(x = -3,
                      style = list(fontSize = "13px")),
        top = "82%",
        height = "18%",
        offset = 0)
    ) |> 
    hc_title(text = title,
             style = list(fontSize = "25px", fontWeight = "bold"),
             align = "left", x = 20, y = 30) |> 
    hc_subtitle(text = sub_title,
                style = list(fontSize = "18px"),
                align = "left",
                x = 20, y = 55) |> 
    hc_legend(
      enabled = TRUE,
      layout = "vertical",
      align = "left",
      verticalAlign = "top",
      floating = TRUE,
      x = 10,
      y = 10,
      backgroundColor = "rgba(0,0,0,0)",
      itemStyle = list(
        color = "#E0E0E0",
        fontSize = "15px"
      ),
      symbolHeight = 10,
      symbolWidth = 10,
      symbolRadius = 2
    ) |> 
    hc_add_theme(hc_theme_darkunica())
}

hc_basic <- function(x, 
                     title,
                     sub_title = NULL,
                     tooltip_nm,
                     rg_num = 0) {
  
  if (!is.xts(x)) stop("x must be an xts object.")
  
  # 전역 변수 설정
  hc_lang <- getOption("highcharter.lang")
  hc_lang$thousandsSep <- ","
  options(
    highcharter.lang = hc_lang
  )
  
  highchart(
    type = "stock"
  ) |> 
    hc_rangeSelector(
      selected = rg_num,
      dropdown = "always",
      buttonTheme = list(
        r = 3,
        states = list(
          hover = list(
            fill = "#4a4a4a", # 배경색
            style = list(color = "#FFFFFF") # 글자색
          ),
          select = list(
            fill = "#E0E0E0",
            style = list(
              color = "#2b2b2b",
              fontWeight = "bold"
            )
          )
        )
      ),
      x = 20
    ) |> 
    hc_add_series(type = "line", name = tooltip_nm, data = x) |> 
    hc_tooltip(
      shape = "rect",
      headerShape = "callout",
      shadow = F,
      fixed = TRUE,
      borderWidth = 0.1,
      valueDecimals = 2,
      style = list(
        fontSize = "13px"
      )
    ) |> 
    hc_title(text = title,
             style = list(fontSize = "25px", fontWeight = "bold"),
             align = "left", x = 20, y = 30
    ) |> 
    hc_subtitle(text = sub_title,
                style = list(fontSize = "18px"),
                align = "left",
                x = 20, y = 55
    ) |> 
    hc_legend(
      enabled = TRUE,
      layout = "vertical",
      align = "left",
      verticalAlign = "top",
      floating = TRUE,
      x = 10,
      y = 10,
      backgroundColor = "rgba(0,0,0,0)",
      itemStyle = list(
        color = "#E0E0E0",
        fontSize = "15px"
      ),
      symbolHeight = 10,
      symbolWidth = 10,
      symbolRadius = 2
    ) |> 
    hc_add_theme(hc_theme_darkunica())
  
}
