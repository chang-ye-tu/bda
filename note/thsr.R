# -*- coding: utf-8 -*-
# encoding: UTF-8
## ============================================================
## 台灣高鐵訂票 / 取消訂位
## 後端: httr + W3C WebDriver（直接與 chromedriver 通訊，不依賴 RSelenium）
##
## ── Windows 前置作業 ──────────────────────────────────────────
##   1. 確認 Chrome 版本：chrome://version
##   2. 下載對應 chromedriver（以 145 版為例）：
##      https://storage.googleapis.com/chrome-for-testing-public/
##             145.0.7632.159/win64/chromedriver-win64.zip
##   3. 解壓縮，把 chromedriver.exe 放到任意資料夾（例如 ~/chromedriver/）
##   4. 修改下方 CHROMEDRIVER_PATH 為該路徑
##
## ── Linux/macOS 前置作業 ─────────────────────────────────────
##   wget "https://storage.googleapis.com/chrome-for-testing-public/\
##         145.0.7632.159/linux64/chromedriver-linux64.zip"
##   unzip chromedriver-linux64.zip
##   sudo mv chromedriver-linux64/chromedriver /usr/local/bin/
##   sudo chmod +x /usr/local/bin/chromedriver
##   # 此時 CHROMEDRIVER_PATH 設 "chromedriver" 即可（在 PATH 裡）
##
## ── 安裝 R 套件 ──────────────────────────────────────────────
##   install.packages(c("httr", "jsonlite", "magick", "base64enc"))
## ============================================================
library(httr); library(jsonlite); library(magick); library(base64enc)

## ── chromedriver 路徑設定 ────────────────────────────────────────────────────
## Windows：填入 chromedriver.exe 的完整路徑（可用 / 或 \\ 作為路徑分隔符）
##   CHROMEDRIVER_PATH <- "C:/Users/YourName/chromedriver/chromedriver.exe"
## Linux/macOS：若已安裝到 /usr/local/bin/，直接用指令名稱即可
##   CHROMEDRIVER_PATH <- "chromedriver"
CHROMEDRIVER_PATH <- "chromedriver"   # ← 請依實際環境修改

PID        <- "D121350349"
MOBILE     <- "0922825743"
MEMBER_PID <- "D121350349"
SEAT       <- "1"
NUM_TRIALS   <- 10
WAIT_TIMEOUT <- 20
POLL_FREQ    <- 0.4

train_id  <- "0818"; start_sta <- "11"; dest_sta <- "7"  # 台南→台中
# train_id <- "0841"; start_sta <- "7";  dest_sta <- "11" # 台中→台南

THSR_BOOK_URL   <- "https://irs.thsrc.com.tw/IMINT/?locale=tw"
THSR_CANCEL_URL <- paste0("https://irs.thsrc.com.tw/IMINT/",
  "?wicket:bookmarkablePage=:tw.com.mitac.webapp.thsr.viewer.History")

# ============================================================
# 薄型 W3C WebDriver client（httr 實作）
# ============================================================

.wd_base <- function(sess) {
  sprintf("http://localhost:%d/session/%s", sess$port, sess$id)
}

wd_cmd <- function(sess, method, path, body = NULL) {
  url <- paste0(.wd_base(sess), path)
  res <- if (method == "GET") {
    GET(url)
  } else if (method == "POST") {
    POST(url, body = body, encode = "json")
  } else if (method == "DELETE") {
    DELETE(url)
  }
  parsed <- fromJSON(rawToChar(res$content))
  parsed$value
}

start_driver <- function(headless = TRUE, port = 4444L,
                        driver_path = CHROMEDRIVER_PATH) {
  # ── 清除殘存的 chromedriver 行程（Windows / Unix 分支）────────────────────
  if (.Platform$OS.type == "windows") {
    system2("taskkill", args = c("/F", "/IM", "chromedriver.exe"),
            wait = TRUE, stdout = NULL, stderr = NULL)
  } else {
    system2("pkill", args = "chromedriver",
            wait = TRUE, stdout = NULL, stderr = NULL)
  }
  Sys.sleep(0.5)

  # ── 在背景啟動 chromedriver ───────────────────────────────────────────────
  system2(driver_path,
          args = sprintf("--port=%d", port),
          wait = FALSE, stdout = NULL, stderr = NULL)
  Sys.sleep(2)

  chrome_args <- c(
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--window-size=1920,1080",
    "--disable-blink-features=AutomationControlled",
    if (headless) "--headless=new"
  )

  body <- list(
    capabilities = list(
      alwaysMatch = list(
        browserName      = "chrome",
        "goog:chromeOptions" = list(args = as.list(chrome_args))
      )
    )
  )

  res  <- POST(sprintf("http://localhost:%d/session", port),
               body = body, encode = "json")
  val  <- fromJSON(rawToChar(res$content))
  sess <- list(id = val$value$sessionId, port = port)
  if (is.null(sess$id) || is.na(sess$id))
    stop("WebDriver session 建立失敗：", toJSON(val, auto_unbox = TRUE))
  message(sprintf("Session 建立成功：%s", sess$id))
  sess
}

stop_driver <- function(sess) {
  tryCatch(DELETE(sprintf("http://localhost:%d/session/%s",
                          sess$port, sess$id)), error = function(e) NULL)
  if (.Platform$OS.type == "windows") {
    system2("taskkill", args = c("/F", "/IM", "chromedriver.exe"),
            wait = FALSE, stdout = NULL, stderr = NULL)
  } else {
    system2("pkill", args = "chromedriver",
            wait = FALSE, stdout = NULL, stderr = NULL)
  }
  invisible(NULL)
}

wd_navigate <- function(sess, url) {
  wd_cmd(sess, "POST", "/url", list(url = url))
  invisible(NULL)
}

wd_js <- function(sess, script, args = list()) {
  # list() in R serialises to {} not [] — build raw JSON to guarantee array
  body_json <- sprintf('{"script":%s,"args":%s}',
                       toJSON(script, auto_unbox = TRUE),
                       toJSON(args))
  url <- paste0(.wd_base(sess), "/execute/sync")
  res <- POST(url, body = body_json,
              add_headers("Content-Type" = "application/json"))
  fromJSON(rawToChar(res$content))$value
}

js_str <- function(x) toJSON(x, auto_unbox = TRUE)

# ============================================================
# DOM 工具函式
# ============================================================

wait_for <- function(sess, css, timeout = WAIT_TIMEOUT, poll = POLL_FREQ) {
  deadline <- Sys.time() + timeout
  script   <- sprintf("return document.querySelector(%s) !== null;",
                      js_str(css))
  repeat {
    found <- tryCatch(isTRUE(wd_js(sess, script)), error = function(e) FALSE)
    if (found) return(invisible(TRUE))
    if (Sys.time() > deadline) stop(sprintf("等待逾時：%s", css))
    Sys.sleep(poll)
  }
}

wait_click <- function(sess, css, timeout = WAIT_TIMEOUT) {
  wait_for(sess, css, timeout)
  wd_js(sess, sprintf("document.querySelector(%s).click();", js_str(css)))
  invisible(NULL)
}

fill <- function(sess, css, text) {
  wait_for(sess, css)
  wd_js(sess, sprintf('
    (function() {
      var el = document.querySelector(%s);
      el.focus(); el.value = ""; el.value = %s;
      el.dispatchEvent(new Event("input",  {bubbles: true}));
      el.dispatchEvent(new Event("change", {bubbles: true}));
    })();
  ', js_str(css), js_str(text)))
  invisible(NULL)
}

js_select <- function(sess, css, value) {
  wait_for(sess, css)
  wd_js(sess, sprintf('
    (function() {
      var el = document.querySelector(%s);
      el.value = %s;
      el.dispatchEvent(new Event("change", {bubbles: true}));
    })();
  ', js_str(css), js_str(value)))
  invisible(NULL)
}

click_link_text <- function(sess, text, timeout = WAIT_TIMEOUT) {
  deadline <- Sys.time() + timeout
  script   <- sprintf('
    (function() {
      var a = Array.from(document.querySelectorAll("a"))
                   .find(function(el) {
                     return el.textContent.trim() === %s;
                   });
      if (a) { a.click(); return true; }
      return false;
    })();
  ', js_str(text))
  repeat {
    ok <- tryCatch(isTRUE(wd_js(sess, script)), error = function(e) FALSE)
    if (ok) return(invisible(TRUE))
    if (Sys.time() > deadline) stop(sprintf("等待逾時：連結「%s」", text))
    Sys.sleep(POLL_FREQ)
  }
}

element_screenshot <- function(sess, css, filename = "captcha.png") {
  wait_for(sess, css)
  rect <- wd_js(sess, sprintf('
    var r = document.querySelector(%s).getBoundingClientRect();
    return {x: r.left, y: r.top, width: r.width, height: r.height};
  ', js_str(css)))
  raw  <- wd_cmd(sess, "GET", "/screenshot")
  img  <- image_read(base64decode(raw))
  img  <- image_crop(img, sprintf("%dx%d+%d+%d",
            round(rect$width), round(rect$height),
            round(rect$x),     round(rect$y)))
  image_write(img, filename)
  invisible(filename)
}

page_screenshot <- function(sess, filename) {
  raw <- wd_cmd(sess, "GET", "/screenshot")
  writeBin(base64decode(raw), filename)
  invisible(filename)
}

ocr_captcha <- function(filename = "captcha.png", manual = FALSE) {
  if (manual) {
    # ── 手動模式：顯示圖片後等待使用者輸入 ──────────────────────
    if (interactive()) {
      # RStudio Viewer / X11 顯示
      tryCatch(print(image_scale(image_read(filename), "400")), error = function(e) NULL)
    }
    cat(sprintf("\n[驗證碼] 請查看圖片 %s，輸入後按 Enter: ", filename))
    code <- trimws(readline())
    return(gsub("[^a-zA-Z0-9]", "", code))
  }
  # manual = FALSE 時預留給未來 reticulate + ddddocr 自動辨識
  ""
}

# ============================================================
# 訂票
# ============================================================

book <- function(date, headless = FALSE, manual = FALSE) {
  sess <- start_driver(headless)
  on.exit(stop_driver(sess), add = TRUE)

  wd_navigate(sess, THSR_BOOK_URL)
  wait_for(sess, "#BookingS1Form", timeout = 30)
  tryCatch(wait_click(sess, "#cookieAccpetBtn", timeout = 5), error = function(e) NULL)

  reached <- function()
    tryCatch(isTRUE(wd_js(sess,
      'return document.querySelector("#idNumber") !== null;')),
      error = function(e) FALSE)

  trials <- 0L
  while (!reached() && trials < NUM_TRIALS) {
    trials <- trials + 1L
    message(sprintf("  驗證碼 #%d", trials))
    tryCatch({
      js_select(sess, "#BookingS1Form_seatCon_seatRadioGroup", SEAT)
      js_select(sess, "[name='selectStartStation']",           start_sta)
      js_select(sess, "[name='selectDestinationStation']",     dest_sta)
      wd_js(sess, sprintf('
        var el = document.querySelector("#toTimeInputField");
        el.value = %s;
        el.dispatchEvent(new Event("change", {bubbles: true}));
      ', js_str(date)))
      wait_click(sess, "input[data-target='search-by-trainNo']")
      fill(sess, "[name='toTrainIDInputField']", train_id)
      element_screenshot(sess, "#BookingS1Form_homeCaptcha_passCode",
                         "captcha.png")
      code <- ocr_captcha("captcha.png", manual = manual)
      message(sprintf("    OCR: '%s'", code))
      fill(sess, "[name='homeCaptcha:securityCode']", code)
      Sys.sleep(0.5)
      wait_click(sess, "#SubmitButton")
      tryCatch(
        wait_for(sess, "#idNumber", timeout = 8),
        error = function(e) {
          message("    未通過，重試...")
          wd_navigate(sess, THSR_BOOK_URL)
          wait_for(sess, "#BookingS1Form", timeout = 30)
          tryCatch(wait_click(sess, "#cookieAccpetBtn", timeout = 5), error = function(e2) NULL)
        }
      )
    }, error = function(e)
      message(sprintf("    例外: %s", conditionMessage(e))))
  }

  if (!reached()) {
    message(sprintf("訂票失敗：%d 次皆未成功", NUM_TRIALS))
    page_screenshot(sess, "thsr_fail.png")
    return(FALSE)
  }

  message("  通過！填寫旅客資料...")
  fill(sess, "#idNumber",    PID)
  fill(sess, "#mobilePhone", MOBILE)
  tryCatch(
    fill(sess, paste0("#BookingS3Form_TicketPassengerInfoInputPanel_",
      "passengerDataView_0_passengerDataView2_passengerDataIdNumber"), PID),
    error = function(e) message("  不須輸入乘客資訊"))
  tryCatch({
    wait_click(sess, "#memberSystemRadio1", timeout = 3)
    fill(sess, "#msNumber", MEMBER_PID)
  }, error = function(e) NULL)
  wait_click(sess, "[name='agree']")
  wait_click(sess, "#isSubmit")
  tryCatch({
    wait_for(sess, "[name='SubmitButton']", timeout = 8)
    wait_click(sess, "[name='SubmitButton']")
  }, error = function(e) NULL)
  Sys.sleep(2)
  page_screenshot(sess, sprintf("thsr_ok_%s.png", gsub("/", "", date)))
  message("訂票成功！截圖已存。")
  TRUE
}

# ============================================================
# 取消訂位
# ============================================================

cancel <- function(order_id, headless = FALSE, manual = FALSE) {
  sess <- start_driver(headless)
  on.exit(stop_driver(sess), add = TRUE)

  wd_navigate(sess, THSR_CANCEL_URL)
  wait_for(sess, "#rocId", timeout = 30)

  reached <- function()
    tryCatch(isTRUE(wd_js(sess,
      'return document.querySelector(".btn-edit") !== null;')),
      error = function(e) FALSE)

  trials <- 0L
  while (!reached() && trials < NUM_TRIALS) {
    trials <- trials + 1L
    message(sprintf("  驗證碼 #%d", trials))
    fill(sess, "#rocId",   PID)
    fill(sess, "#orderId", order_id)
    element_screenshot(sess, "#HistoryForm_divCaptcha_passCode", "captcha.png")
    code <- ocr_captcha("captcha.png", manual = manual)
    message(sprintf("    OCR: '%s'", code))
    fill(sess, "#securityCode", code)
    Sys.sleep(0.5)
    wait_click(sess, "[name='SubmitButton']")
    tryCatch(wait_for(sess, ".btn-edit", timeout = 5),
             error = function(e) message("    未通過"))
  }

  tryCatch({
    wait_for(sess, "div.uk-modal.uk-open", timeout = 3)
    wd_js(sess, '
      var btn = document.querySelector(
        "div.uk-modal.uk-open input.uk-modal-close");
      if (btn) btn.click();
    ')
    Sys.sleep(1)
  }, error = function(e) message("  無查詢彈窗，略過"))

  wait_click(sess, ".btn-edit")
  # 等「取消訂位」連結出現（btn-edit 點後頁面需短暫載入）
  wait_for(sess, 'a[href*="Cancel"], a', timeout = 10)
  Sys.sleep(0.5)
  # 用 JS 找完全符合文字的 <a> 並點擊（對應 Python EC.element_to_be_clickable LINK_TEXT）
  wd_js(sess, '
    (function() {
      var links = Array.from(document.querySelectorAll("a"));
      var target = links.find(function(a) {
        return a.textContent.trim() === "\u53d6\u6d88\u8a02\u4f4d";
      });
      if (target) { target.click(); return true; }
      return false;
    })();
  ')
  Sys.sleep(0.5)
  wait_click(sess, "[name='agree']")
  wait_click(sess, "[name='SubmitButton']")
  Sys.sleep(3)
  message(sprintf("訂位 %s 取消完成", order_id))
  invisible(TRUE)
}

# ============================================================
# 範例
# ============================================================
if (FALSE) {
  book(date = "2026/04/02", headless = FALSE)
  cancel(order_id = "01901879", headless = FALSE)
}
