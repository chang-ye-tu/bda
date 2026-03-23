## ============================================================
## 台灣高鐵訂票 / 取消訂位
## 後端: httr + W3C WebDriver（直接與 chromedriver 通訊）
##
## ── 前置作業 ────────────────────────────────────────────────
##   R 套件與 chromedriver 皆會在首次執行時自動安裝，無須手動處理。
## ============================================================

## ── 套件自動安裝 ────────────────────────────────────────────
local({
  pkgs <- c("httr", "jsonlite", "magick", "base64enc")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message(sprintf("安裝缺少的套件：%s", paste(missing, collapse = ", ")))
    install.packages(missing, quiet = TRUE)
    still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still))
      stop(sprintf("以下套件安裝失敗，程式無法執行：%s", paste(still, collapse = ", ")))
  }
})
library(httr); library(jsonlite); library(magick); library(base64enc)

## 設定目前工作目錄為本程式所在處
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## ── chromedriver 自動下載 / 路徑設定 ───────────────────────────────────────────
ensure_chromedriver <- function(path = "./chromedriver") {
  # Windows 下補上 .exe
  exe <- if (.Platform$OS.type == "windows" && !grepl("\\.exe$", path))
    paste0(path, ".exe") else path
  if (file.exists(exe)) { message("chromedriver 已存在"); return(invisible(exe)) }

  message("chromedriver 不存在，自動下載中...")

  # ── 偵測平台 ──────────────────────────────────────────────
  sysname <- Sys.info()[["sysname"]]
  machine <- Sys.info()[["machine"]]
  platform <- if (sysname == "Windows") {
    if (.Machine$sizeof.pointer == 8L) "win64" else "win32"
  } else if (sysname == "Darwin") {
    if (grepl("arm|aarch", machine, ignore.case = TRUE)) "mac-arm64" else "mac-x64"
  } else {
    "linux64"
  }
  message(sprintf("  平台: %s", platform))

  # ── 查詢 Stable channel 最新版本 ──────────────────────────
  api_url <- paste0("https://googlechromelabs.github.io/",
                    "chrome-for-testing/last-known-good-versions-with-downloads.json")
  info  <- fromJSON(content(GET(api_url), "text", encoding = "UTF-8"))
  stable <- info$channels$Stable
  message(sprintf("  Stable 版本: %s", stable$version))

  urls <- stable$downloads$chromedriver
  dl   <- urls[urls$platform == platform, "url"]
  if (length(dl) == 0L) stop(sprintf("找不到平台 %s 的下載連結", platform))
  message(sprintf("  下載: %s", dl))

  # ── 下載並解壓縮 ──────────────────────────────────────────
  zip_file <- tempfile(fileext = ".zip")
  download.file(dl, zip_file, mode = "wb", quiet = TRUE)
  tmp_dir  <- tempfile()
  unzip(zip_file, exdir = tmp_dir)

  # zip 內結構: chromedriver-<platform>/chromedriver[.exe]
  found <- list.files(tmp_dir, pattern = "^chromedriver(\\.exe)?$",
                      recursive = TRUE, full.names = TRUE)
  if (length(found) == 0L) stop("解壓縮後找不到 chromedriver 執行檔")
  file.copy(found[1], exe, overwrite = TRUE)

  # Linux/macOS 加上執行權限
  if (.Platform$OS.type != "windows") Sys.chmod(exe, "755")

  unlink(zip_file); unlink(tmp_dir, recursive = TRUE)
  message(sprintf("  已安裝至 %s", normalizePath(exe)))
  invisible(exe)
}

CHROMEDRIVER_PATH <- ensure_chromedriver()

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

  # ── 等待圖片實際載入完成（對 <img> 元素而言）──────────────────
  wd_js(sess, sprintf('
    (function() {
      var el = document.querySelector(%s);
      if (el && el.tagName === "IMG" && !el.complete) {
        return new Promise(function(resolve) {
          el.addEventListener("load",  function() { resolve(true); });
          el.addEventListener("error", function() { resolve(false); });
          setTimeout(function() { resolve(false); }, 5000);
        });
      }
      return true;
    })();
  ', js_str(css)))
  Sys.sleep(0.3)

  # ── 取得 BoundingClientRect 並乘以 devicePixelRatio ─────────
  rect <- wd_js(sess, sprintf('
    var dpr = window.devicePixelRatio || 1;
    var r   = document.querySelector(%s).getBoundingClientRect();
    return {x: r.left * dpr, y: r.top * dpr,
            width: r.width * dpr, height: r.height * dpr, dpr: dpr};
  ', js_str(css)))
  message(sprintf("    [截圖] DPR=%.1f  rect: %dx%d+%d+%d",
          rect$dpr, round(rect$width), round(rect$height),
          round(rect$x), round(rect$y)))

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

book <- function(date, headless = FALSE, manual = TRUE) {
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

cancel <- function(order_id, headless = FALSE, manual = TRUE) {
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

  if (!reached()) {
    message(sprintf("取消失敗：%d 次皆未成功", NUM_TRIALS))
    page_screenshot(sess, "thsr_cancel_fail.png")
    return(FALSE)
  }

  message("  通過！開始取消訂位...")

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
if (TRUE) {
  book(date = "2026/04/13")
  #cancel(order_id = "01901879")
}
