;;; spreadsheet-test.scm --- Spreadsheet backend and discovery tests.

(deftest 'spreadsheet-backend-is-pluggable
  "a registered backend serves the grid without file assumptions"
  (lambda ()
    (let ((buffer "*zz-sheet-backend*")
          (backend 'zz-sheet-memory)
          (stored "{\"version\":1,\"sheets\":[{\"name\":\"Memory\",\"data\":[[1]]}]}")
          (replacement "{\"version\":1,\"sheets\":[{\"name\":\"Changed\",\"data\":[[2]]}]}"))
      (spreadsheet-register-backend!
        backend
        (lambda (source) stored)
        (lambda (source text) (set! stored text) #t))
      (buffer-create buffer)
      (buffer-set-local! buffer 'mode-name "spreadsheet-mode")
      (buffer-set-local! buffer 'spreadsheet-backend backend)
      (buffer-set-local! buffer 'spreadsheet-source "memory:one")
      (check-contains! (cadr (spreadsheet-app-request buffer "read" ""))
                       "Memory" "the reader supplies workbook JSON")
      (check-equal! (car (spreadsheet-app-request buffer "write" replacement))
                    200 "the writer accepts workbook JSON")
      (check-contains! stored "Changed" "the registered writer owns storage")
      (buffer-kill! buffer)
      (set! *spreadsheet-backends*
        (alist-delete *spreadsheet-backends* backend)))))

(deftest 'spreadsheet-agent-api-reads-and-writes-without-display
  "agents can change backend data by source without selecting a grid"
  (lambda ()
    (let ((backend 'zz-sheet-agent-memory)
          (stored "{\"version\":1,\"sheets\":[{\"name\":\"Before\",\"data\":[[1]]}]}"))
      (spreadsheet-register-backend!
        backend
        (lambda (source) stored)
        (lambda (source text) (set! stored text) #t))
      (check-equal!
        (plist-get (car (plist-get (spreadsheet-read-source backend "memory:agent") 'sheets)) 'name)
        "Before" "the source API reads without a buffer")
      (check-true!
        (spreadsheet-write-source!
          backend "memory:agent"
          (json-parse "{\"version\":1,\"sheets\":[{\"name\":\"Agent wrote this\",\"data\":[[2]]}]}"))
        "the source API writes through the registered backend")
      (check-contains! stored "Agent wrote this" "the agent write reached storage")
      (set! *spreadsheet-backends*
        (alist-delete *spreadsheet-backends* backend)))))

(deftest 'spreadsheet-agent-api-manages-embedded-charts
  "agents can add, update, list, and delete persistent charts"
  (lambda ()
    (let ((backend 'zz-sheet-chart-memory)
          (buffer "*zz-sheet-chart-api*")
          (stored
            "{\"version\":1,\"sheets\":[{\"name\":\"Budget\",\"data\":[[\"Month\",\"Spend\"],[\"Jan\",10],[\"Feb\",12]]}]}"))
      (spreadsheet-register-backend!
        backend
        (lambda (source) stored)
        (lambda (source text) (set! stored text) #t))
      (buffer-create buffer)
      (buffer-set-local! buffer 'spreadsheet-backend backend)
      (buffer-set-local! buffer 'spreadsheet-source "memory:charts")
      (check-true!
        (spreadsheet-add-chart!
          buffer "Budget" "monthly-spend" "line" "A1:B3" "D2:K18" "Monthly spending")
        "one call adds a chart")
      (check-equal! (plist-get (spreadsheet-chart-status buffer) 'state) "not-reported"
                    "a saved descriptor does not claim that its chart is visible")
      (let ((charts (spreadsheet-charts buffer)))
        (check-equal! (length charts) 1 "the workbook has one chart")
        (check-equal! (plist-get (car charts) 'anchor) "D2:K18"
                      "the chart keeps its sheet anchor"))
      (check-true!
        (spreadsheet-add-chart!
          buffer 1 "monthly-spend" "column" "A1:B3" "E3:L19" "Spending by month")
        "the same ID updates the chart")
      (let ((charts (spreadsheet-charts buffer)))
        (check-equal! (length charts) 1 "an update does not duplicate the chart")
        (check-equal! (plist-get (car charts) 'type) "column"
                      "the updated chart has the new type")
        (check-equal! (plist-get (car charts) 'sheet) "Budget"
                      "a numbered sheet stores its stable name"))
      (check-true! (spreadsheet-set-cell! buffer "Budget" "B3" 14)
                   "a cell edit keeps the workbook writable")
      (check-equal! (length (spreadsheet-charts buffer)) 1
                    "a cell edit preserves chart descriptors")
      (check-true!
        (spreadsheet--error?
          (spreadsheet-add-chart!
            buffer "Budget" "bad" "radar" "A1:B3" "D2:K18" "Bad chart"))
        "an unsupported chart type returns an error")
      (check-true!
        (spreadsheet--error?
          (spreadsheet-add-chart!
            buffer "Budget" "bad" "line" "not-a-range" "D2:K18" "Bad range"))
        "an invalid source range returns an error")
      (check-true! (spreadsheet-delete-chart! buffer "monthly-spend")
                   "the chart can be deleted")
      (check-equal! (spreadsheet-charts buffer) '()
                    "deletion leaves no chart descriptors")
      (check-true! (spreadsheet--error? (spreadsheet-delete-chart! buffer "missing"))
                   "a missing chart returns an error")
      (buffer-kill! buffer)
      (set! *spreadsheet-backends*
        (alist-delete *spreadsheet-backends* backend)))))

(deftest 'spreadsheet-agent-api-creates-a-chart-with-safe-defaults
  "agents can create and verify a chart without choosing drawing internals"
  (lambda ()
    (let ((backend 'zz-sheet-simple-chart-memory)
          (buffer "*zz-sheet-simple-chart-api*")
          (stored
            "{\"version\":1,\"sheets\":[{\"name\":\"Rain\",\"data\":[[\"Month\",\"Rainfall\"],[\"Jan\",2]]}]}"))
      (spreadsheet-register-backend!
        backend
        (lambda (source) stored)
        (lambda (source text) (set! stored text) #t))
      (buffer-create buffer)
      (buffer-set-local! buffer 'mode-name "spreadsheet-mode")
      (buffer-set-local! buffer 'spreadsheet-backend backend)
      (buffer-set-local! buffer 'spreadsheet-source "memory:simple-chart")
      (check-true!
        (spreadsheet-chart! buffer "Rain" "A1:B2" "column" "Rainfall")
        "the simple API creates a chart")
      (let* ((chart (car (spreadsheet-charts buffer)))
             (status (spreadsheet-chart-status buffer)))
        (check-equal! (plist-get chart 'id) "chart-1" "the API creates an ID")
        (check-equal! (plist-get chart 'anchor) "D1:K16"
                      "the API puts the chart beside its data")
        (check-equal! (plist-get status 'configured) '("chart-1")
                      "status lists configured charts")
        (check-equal! (plist-get status 'mounted) '()
                      "status does not infer browser success")
        (check-equal! (plist-get status 'drawn) '()
                      "status does not infer ECharts success"))
      (check-equal!
        (car
          (spreadsheet-app-request
            buffer "chart-status"
            "{\"state\":\"ready\",\"mounted\":[\"chart-1\"],\"drawn\":[\"chart-1\"],\"failed\":[]}"))
        200 "the grid can report rendering status")
      (check-equal! (plist-get (spreadsheet-chart-status buffer) 'mounted)
                    '("chart-1") "the agent reads the grid report")
      (check-equal! (plist-get (spreadsheet-chart-status buffer) 'drawn)
                    '("chart-1") "the agent verifies that ECharts drew the chart")
      (buffer-kill! buffer)
      (set! *spreadsheet-backends*
        (alist-delete *spreadsheet-backends* backend)))))

(deftest 'spreadsheet-agent-api-edits-one-cell
  "agents can inspect sheets and change one A1 cell without rebuilding lists"
  (lambda ()
    (let ((backend 'zz-sheet-cell-memory)
          (buffer "*zz-sheet-cell-api*")
          (stored
            "{\"version\":1,\"activeSheet\":1,\"sheets\":[{\"name\":\"One\",\"data\":[[\"x\"]]},{\"name\":\"Two\",\"data\":[[\"\",\"\"],[\"\",\"1\"],[\"\",\"2\"],[\"\",\"w\"]]}]}"))
      (spreadsheet-register-backend!
        backend
        (lambda (source) stored)
        (lambda (source text) (set! stored text) #t))
      (buffer-create buffer)
      (buffer-set-local! buffer 'spreadsheet-backend backend)
      (buffer-set-local! buffer 'spreadsheet-source "memory:cells")
      (check-equal! (spreadsheet-sheet-names buffer) '("One" "Two")
                    "the agent sees sheet names")
      (check-equal! (spreadsheet-read-cell buffer 2 "B4") "w"
                    "a 1-based sheet number and A1 cell read directly")
      (check-equal! (length (plist-get (spreadsheet-read-sheet buffer "Two") 'data)) 4
                    "compact sheet reads omit unused trailing rows")
      (check-true! (spreadsheet-set-cell! buffer 2 "B4" "=SUM(B2:B3)")
                   "one call replaces the cell")
      (check-equal! (spreadsheet-read-cell buffer "Two" "B4") "=SUM(B2:B3)"
                    "the formula reached the named sheet")
      (check-equal! (plist-get (spreadsheet-read buffer) 'activeSheet) 1
                    "a cell edit preserves the selected sheet")
      (check-true! (spreadsheet--error? (spreadsheet-read-cell buffer 2 "B0"))
                   "invalid A1 notation returns a useful error")
      (buffer-kill! buffer)
      (set! *spreadsheet-backends*
        (alist-delete *spreadsheet-backends* backend)))))

(deftest 'the-app-bridge-reaches-a-spreadsheet-through-its-handler
  "app-request hands the HTTP method to the spreadsheet handler, and an app no package owns gets a 404"
  (lambda ()
    (let ((buffer "*zz-sheet-bridge*")
          (other "*zz-sheet-bridge-other*")
          (backend 'zz-sheet-bridge-memory)
          (stored "{\"version\":1,\"sheets\":[{\"name\":\"Bridge\",\"data\":[[1]]}]}"))
      (spreadsheet-register-backend! backend
        (lambda (source) stored)
        (lambda (source text) (set! stored text) #t))
      (buffer-create buffer)
      (buffer-create other)
      (buffer-set-local! buffer 'mode-name "spreadsheet-mode")
      (buffer-set-local! buffer 'spreadsheet-backend backend)
      (buffer-set-local! buffer 'spreadsheet-source "memory:bridge")
      (check-contains! (cadr (app-request buffer "GET" "")) "Bridge" "GET reads the workbook")
      (check-equal! (car (app-request buffer "PUT" "{\"version\":1,\"sheets\":[{\"name\":\"Put\",\"data\":[[2]]}]}"))
                    200 "PUT writes it")
      (check-contains! stored "Put" "the backend holds the write")
      (check-equal! (car (app-request other "GET" "")) 404 "no handler answers for another buffer")
      (buffer-kill! buffer)
      (buffer-kill! other)
      (set! *spreadsheet-backends*
        (alist-delete *spreadsheet-backends* backend)))))

(deftest 'the-grid-page-loads-its-script-from-the-package-directory
  "the page has no inline script; the app-directory holds spreadsheet.js"
  (lambda ()
    (let ((dir (spreadsheet--app-directory)))
      (check-true! (string? dir) "the package directory is on load-path")
      (check-true! (file-exists? (string-append dir "/spreadsheet.js")) "the script is there")
      (check-contains! (spreadsheet--app-html) "<script src=\"spreadsheet.js\"></script>" "the page names it")
      (check-false! (string-contains? (spreadsheet--app-html) "<script>") "and holds no inline script"))))


;;; --- a workbook on disk -------------------------------------------------------
;;; These were spreadsheet_mode_test.exs: the text backend, the page, the
;;; mode setup and the reload command, over a real file.

(define t--sheet-book "{\"version\":1,\"sheets\":[{\"name\":\"Safe\",\"data\":[[\"value\"]]}]}\n")

(define (t--sheet-path)
  (string-append (compos-temp-dir) "/zz-sheet-" (number->string (monotonic-ms)) ".sheet.json"))

(define (t--sheet-done! path)
  (let ((stem (car (string-split (file-name-nondirectory path) "."))))
    (for-each (lambda (b) (when (string-contains? b stem) (buffer-kill! b))) (buffer-list)))
  (when (file-exists? path) (delete-file! path)))

;; spreadsheet-open! switches to its buffer, so each test has a frame
(define (t--sheet! thunk)
  (let ((path (t--sheet-path)))
    (with-frame (lambda (frame) (thunk path)))
    (t--sheet-done! path)))

(define (t--sheet-charts path)
  (plist-get (plist-get (plist-get (json-parse (read-file path)) 'extensions) 'compos) 'charts))

(deftest 'spreadsheet-open-makes-a-json-workbook-and-runs-it-as-an-app
  "a new path gets a version 1 workbook; the page loads the grid script and follows the theme"
  (lambda ()
   (t--sheet! (lambda (path)
    (let* ((buf (spreadsheet-open! path))
           (theme (theme-current)))
      (check-true! (file-exists? path) "the file is written")
      (check-equal! (plist-get (json-parse (read-file path)) 'version) 1 "a version 1 workbook")
      (check-equal! (buffer-local-value 'mode-name buf) "spreadsheet-mode" "the mode")
      (check-equal! (buffer-local-value 'render-mode buf) "app" "drawn as an app")
      (let ((page (buffer-text buf))
            (script (read-file (string-append (buffer-local-value 'app-directory buf) "/spreadsheet.js"))))
        (check-contains! page "<script src=\"spreadsheet.js\"></script>" "the page names the script")
        (check-false! (string-contains? page "<script>") "no inline script")
        (for-each (lambda (s) (check-contains! page s "the page loads univer"))
                  '("@univerjs/presets@0.25.1" "@univerjs/preset-sheets-core@0.25.1"
                    "@univerjs/preset-sheets-drawing@0.25.1" ".compos-chart>*{pointer-events:none}"
                    "id=\"app\" tabindex=\"0\""))
        (for-each (lambda (s) (check-contains! script s "the grid script"))
                  '("UniverSheetsCorePreset" "UniverSheetsDrawingPreset"
                    "api.registerComponent('ComposChart',ComposChart)"
                    "var dark=document.documentElement.dataset.theme==='dark'"
                    "if(dark&&api.toggleDarkMode)api.toggleDarkMode(true)"
                    "addFloatDomToPosition" "initialChartPosition" "allowTransform:true"
                    "eventPassThrough:true" "api.Enum.DrawingType.DRAWING_CHART"
                    "ResizeObserver" "markChartDrawn(spec.id)" "univerSnapshot" "book.save()"
                    "compos:'request-focus'" "var endpoint='_compos/app'"))
        (check-false! (string-contains? script "addFloatDomToRange") "charts float by position")
        (check-false! (string-contains? script "allowTransform:false") "charts can move"))
      (load-theme "compos-dark")
      (check-contains! (buffer-text buf) "<html data-theme=\"dark\">" "a dark theme makes a dark page")
      (check-contains! (buffer-text buf) "color-scheme:dark" "and a dark color scheme")
      (check-false! (string-contains? (buffer-text buf) "prefers-color-scheme") "the editor decides, not the OS")
      (load-theme "paper")
      (check-contains! (buffer-text buf) "<html data-theme=\"light\">" "a light theme makes a light page")
      (check-contains! (buffer-text buf) "color-scheme:light" "and a light color scheme")
      (load-theme theme)
      (check-equal! (car (app-request buf "GET" "")) 200 "the app bridge reaches it")
      (let ((stored (read-file path)))
        (with-current-buffer buf (lambda () (run-command "spreadsheet-save")))
        (check-false! (minibuffer-active?) "a save asks nothing")
        (check-equal! (read-file path) stored "and writes no page over the data")))))))

(deftest 'spreadsheet-mode-on-the-json-source-keeps-its-data
  "the source stays a JSON buffer; the mode does not draw the app over it"
  (lambda ()
   (t--sheet! (lambda (path)
    (let ()
      (write-file! path t--sheet-book)
      (let ((src (find-file path)))
        (buffer-set-local! src 'render-mode "app")
        (buffer-set-local! src 'preview-renderer "html")
        (with-current-buffer src (lambda () (set-mode! "spreadsheet-mode")))
        (check-equal! (buffer-local-value 'mode-name src) "json-mode" "the source stays JSON")
        (check-false! (buffer-local-value 'render-mode src) "no app view")
        (check-false! (buffer-local-value 'preview-renderer src) "no renderer")
        (check-equal! (json-parse (buffer-text src)) (json-parse t--sheet-book) "the data stays")
        (check-equal! (read-file path) t--sheet-book "and the file")))))))

(deftest 'save-buffer-refuses-html-in-a-workbook-source
  "the page of the app never lands in the workbook file"
  (lambda ()
   (t--sheet! (lambda (path)
    (let ()
      (write-file! path t--sheet-book)
      (let ((src (find-file path)))
        (buffer-replace! src t--sheet-book "<!doctype html><html><body>internal app</body></html>")
        (check-false! (ignore-errors
                        (lambda () (with-current-buffer src (lambda () (run-command "save-buffer"))) 'saved))
                      "the save raises")
        (check-equal! (read-file path) t--sheet-book "the file keeps the workbook")))))))

(deftest 'spreadsheet-charts-persist-in-the-workbook-file
  "a chart is a descriptor in extensions.compos.charts; a new grid reports loading"
  (lambda ()
   (t--sheet! (lambda (path)
    (let* ((buf (spreadsheet-open! path)))
      (check-equal! (car (spreadsheet-app-request buf "write"
                           "{\"version\":1,\"sheets\":[{\"name\":\"Budget\",\"data\":[[\"Month\",\"Spend\"],[\"Jan\",10],[\"Feb\",12]]}]}"))
                    200 "the workbook is written")
      (check-true! (spreadsheet-add-chart! buf "Budget" "monthly-spend" "line" "A1:B3" "D2:K18" "Monthly spending")
                   "a chart is added")
      (let ((chart (car (t--sheet-charts path))))
        (check-equal! (plist-get chart 'id) "monthly-spend" "the file holds its id")
        (check-equal! (plist-get chart 'anchor) "D2:K18" "its anchor")
        (check-equal! (plist-get chart 'title) "Monthly spending" "and its title"))
      (check-true! (spreadsheet-delete-chart! buf "monthly-spend") "it is deleted")
      (check-equal! (t--sheet-charts path) '() "and gone from the file")
      (check-true! (spreadsheet-chart! buf "Budget" "A1:B2" "column" "Spend") "a chart with defaults")
      (check-equal! (plist-get (spreadsheet-chart-status buf) 'state) "loading"
                    "an open grid has not drawn it yet"))))))

(deftest 'spreadsheet-writes-formulas-and-rejects-a-bad-workbook
  "a formula is kept as written; a write that is not a workbook changes nothing"
  (lambda ()
   (t--sheet! (lambda (path)
    (let* ((buf (spreadsheet-open! path)))
      (check-equal! (car (spreadsheet-app-request buf "write"
                           "{\"version\":1,\"univerSnapshot\":{\"styles\":{},\"resources\":[]},\"sheets\":[{\"name\":\"Budget\",\"data\":[[\"Total\",\"=SUM(B2:B3)\"],[\"Tea\",2],[\"Coffee\",3]]}]}"))
                    200 "the workbook is written")
      (check-equal! (spreadsheet-read-cell buf 1 "B1") "=SUM(B2:B3)" "the formula is stored")
      (check-contains! (read-file path) "=SUM(B2:B3)" "in the file")
      (let ((before (read-file path))
            (answer (spreadsheet-app-request buf "write" "{\"bad\":true}")))
        (check-equal! (car answer) 400 "a bad workbook is refused")
        (check-contains! (cadr answer) "not valid" "and says why")
        (check-equal! (read-file path) before "the file is unchanged")))))))

(deftest 'spreadsheet-write-and-read-by-buffer-name
  "an agent writes a workbook, the app view takes a new generation, and a cell edit keeps the rest"
  (lambda ()
   (t--sheet! (lambda (path)
    (let* ((buf (spreadsheet-open! path))
           (gen (buffer-local-value 'app-generation buf))
           (book (json-parse "{\"version\":1,\"activeSheet\":0,\"univerSnapshot\":{\"id\":\"agent-book\"},\"sheets\":[{\"name\":\"Agent data\",\"data\":[[\"Task\",\"Done\"],[\"QA\",true]]}]}")))
      (check-true! (spreadsheet-write! buf book) "the write answers #t")
      (check-equal! (buffer-local-value 'app-generation buf) (+ gen 1) "the app view reloads")
      (check-equal! (spreadsheet-read buf) book "the read answers what was written")
      (check-equal! (spreadsheet-sheet-names buf) '("Agent data") "the sheet names")
      (check-true! (spreadsheet-set-cell! buf 1 "B2" "=COUNTIF(B1:B1,\"Done\")") "one cell")
      (check-equal! (spreadsheet-read-cell buf 1 "B2") "=COUNTIF(B1:B1,\"Done\")" "reads back")
      (let ((stored (json-parse (read-file path))))
        (check-equal! (plist-get stored 'activeSheet) 0 "the active sheet stays")
        (check-equal! (plist-get stored 'univerSnapshot) (plist-get book 'univerSnapshot)
                      "the grid snapshot stays")))))))

(deftest 'spreadsheet-mode-setup-rebuilds-the-app-and-reload-takes-a-new-generation
  "set-mode! draws the page again; spreadsheet-reload bumps the generation"
  (lambda ()
   (t--sheet! (lambda (path)
    (let* ((buf (spreadsheet-open! path)))
      (buffer-replace! buf (buffer-text buf) "stale")
      (buffer-set-local! buf 'render-mode #f)
      (with-current-buffer buf (lambda () (set-mode! "spreadsheet-mode")))
      (check-contains! (buffer-text buf) "<script src=\"spreadsheet.js\"></script>" "the page is back")
      (check-equal! (buffer-local-value 'render-mode buf) "app" "drawn as an app")
      (let ((gen (buffer-local-value 'app-generation buf)))
        (with-current-buffer buf (lambda () (run-command "spreadsheet-reload")))
        (check-equal! (buffer-local-value 'app-generation buf) (+ gen 1) "reload takes a new generation")))))))
