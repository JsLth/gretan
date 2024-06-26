# helpBox works

    Code
      helpBox(help_id = "test", title = "test title", footer = "test footer", status = "primary",
        solidHeader = TRUE, background = "primary", width = 2, height = "400px",
        collapsed = TRUE, closable = TRUE, maximizable = TRUE, icon = shiny::icon(
          "refresh"), gradient = TRUE, boxToolSize = "lg", elevation = 1,
        headerBorder = FALSE, id = "testbox")
    Output
      <div class="col-sm-2">
        <div class="card bs4Dash card-primary collapsed-card elevation-1 bg-gradient-primary" id="testbox">
          <div class="card-header border-0">
            <h3 class="card-title">
              <i class="fas fa-arrows-rotate" role="presentation" aria-label="arrows-rotate icon"></i>
              test title
            </h3>
            <div class="card-tools float-right">
              <button id="test" class="btn btn-tool btn-sm action-button" type="button">
                <i class="fas fa-question" role="presentation" aria-label="question icon"></i>
              </button>
              <button class="btn btn-tool btn-lg btn-primary" type="button" data-card-widget="collapse">
                <i class="fas fa-plus" role="presentation" aria-label="plus icon"></i>
              </button>
              <button class="btn btn-tool btn-lg btn-primary" data-card-widget="remove" type="button">
                <i class="fas fa-xmark" role="presentation" aria-label="xmark icon"></i>
              </button>
              <button type="button" class="btn btn-tool btn-lg btn-primary" data-card-widget="maximize">
                <i class="fas fa-up-right-and-down-left-from-center" role="presentation" aria-label="up-right-and-down-left-from-center icon"></i>
              </button>
            </div>
          </div>
          <div class="card-body" style="height: 400px"></div>
          <div class="card-footer">test footer</div>
        </div>
        <script type="application/json" data-for="testbox">{"title":"test title","status":"primary","solidHeader":true,"background":"primary","width":2,"height":"400px","collapsible":true,"closable":true,"maximizable":true,"gradient":true}</script>
      </div>

---

    Code
      helpBox(help_id = "test", collapsible = FALSE)
    Output
      <div class="col-sm-6">
        <div class="card bs4Dash">
          <div class="card-header">
            <h3 class="card-title"></h3>
            <div class="card-tools float-right">
              <button id="test" class="btn btn-tool btn-sm action-button" type="button">
                <i class="fas fa-question" role="presentation" aria-label="question icon"></i>
              </button>
            </div>
          </div>
          <div class="card-body"></div>
        </div>
        <script type="application/json">{"solidHeader":true,"width":6,"collapsible":false,"closable":false,"maximizable":false,"gradient":false}</script>
      </div>

# leafletPanel works

    Code
      leafletPanel("test", "this is a panel", title = "title", position = "bottomleft",
        width = 400, top = 10)
    Output
      <div class="leaflet-bottomleft">
        <div class="leaflet-info draggable" style="top:10px;width:400px;position:absolute;cursor:move;">
          <div class="leaflet-info-header">
            <span>
              <h5 style="display: inline-block; margin: 0.2rem;">title</h5>
              <div class="card-tools float-right">
                <button class="btn btn-tool btn-sm" data-toggle="collapse" data-target="#test" type="button">
                  <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
                </button>
              </div>
            </span>
          </div>
          <div class="collapse show leaflet-info-body" id="test">this is a panel</div>
        </div>
        <script>$(".draggable").draggable({cancel: ".shiny-input-container,.html-widget,input,textarea,button,select,option"});</script>
      </div>

---

    Code
      leafletPanel("test", position = "topleft")
    Output
      <div class="leaflet-topleft">
        <div class="leaflet-info draggable" style="top:150px;left:10px;width:350px;position:absolute;cursor:move;">
          <div class="leaflet-info-header">
            <span>
              <h5 style="display: inline-block; margin: 0.2rem;"></h5>
              <div class="card-tools float-right">
                <button class="btn btn-tool btn-sm" data-toggle="collapse" data-target="#test" type="button">
                  <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
                </button>
              </div>
            </span>
          </div>
          <div class="collapse show leaflet-info-body" id="test"></div>
        </div>
        <script>$(".draggable").draggable({cancel: ".shiny-input-container,.html-widget,input,textarea,button,select,option"});</script>
      </div>

---

    Code
      leafletPanel("test", position = "bottomleft")
    Output
      <div class="leaflet-bottomleft">
        <div class="leaflet-info draggable" style="left:10px;bottom:150px;width:350px;position:absolute;cursor:move;">
          <div class="leaflet-info-header">
            <span>
              <h5 style="display: inline-block; margin: 0.2rem;"></h5>
              <div class="card-tools float-right">
                <button class="btn btn-tool btn-sm" data-toggle="collapse" data-target="#test" type="button">
                  <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
                </button>
              </div>
            </span>
          </div>
          <div class="collapse show leaflet-info-body" id="test"></div>
        </div>
        <script>$(".draggable").draggable({cancel: ".shiny-input-container,.html-widget,input,textarea,button,select,option"});</script>
      </div>

---

    Code
      leafletPanel("test", position = "topright")
    Output
      <div class="leaflet-topright">
        <div class="leaflet-info draggable" style="top:150px;right:10px;width:350px;position:absolute;cursor:move;">
          <div class="leaflet-info-header">
            <span>
              <h5 style="display: inline-block; margin: 0.2rem;"></h5>
              <div class="card-tools float-right">
                <button class="btn btn-tool btn-sm" data-toggle="collapse" data-target="#test" type="button">
                  <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
                </button>
              </div>
            </span>
          </div>
          <div class="collapse show leaflet-info-body" id="test"></div>
        </div>
        <script>$(".draggable").draggable({cancel: ".shiny-input-container,.html-widget,input,textarea,button,select,option"});</script>
      </div>

---

    Code
      leafletPanel("test", position = "bottomright")
    Output
      <div class="leaflet-bottomright">
        <div class="leaflet-info draggable" style="right:10px;bottom:150px;width:350px;position:absolute;cursor:move;">
          <div class="leaflet-info-header">
            <span>
              <h5 style="display: inline-block; margin: 0.2rem;"></h5>
              <div class="card-tools float-right">
                <button class="btn btn-tool btn-sm" data-toggle="collapse" data-target="#test" type="button">
                  <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
                </button>
              </div>
            </span>
          </div>
          <div class="collapse show leaflet-info-body" id="test"></div>
        </div>
        <script>$(".draggable").draggable({cancel: ".shiny-input-container,.html-widget,input,textarea,button,select,option"});</script>
      </div>

# groupRadioButtons works

    Code
      awesome
    Output
      <div id="test" class="form-group shiny-input-radiogroup awesome-bootstrap-radio shiny-input-container">
        <label class="control-label" id="test-label" for="test" style="margin-bottom: 5px;"></label>
        <div style="height: 7px;"></div>
        <div class="shiny-options-group">
          <div class="awesome-radio radio-primary">
            <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">1st</div>
            <input name="test" id="test1" value="A" type="radio" checked="checked"/>
            <label for="test1">A</label>
          </div>
          <div class="awesome-radio radio-primary">
            <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">2nd</div>
            <input name="test" id="test2" value="B" type="radio"/>
            <label for="test2">B</label>
          </div>
          <div class="awesome-radio radio-primary">
            <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">3rd</div>
            <input name="test" id="test3" value="C" type="radio"/>
            <label for="test3">C</label>
          </div>
        </div>
      </div>

---

    Code
      pretty
    Output
      <div id="test" class="form-group shiny-input-radiogroup shiny-input-container">
        <label class="control-label" id="test-label" for="test"></label>
        <div class="shiny-options-group">
          <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">1st</div>
          <div class="pretty p-default p-round">
            <input type="radio" name="test" value="A" checked="checked"/>
            <div class="state p-primary">
              <label>
                <span>A</span>
              </label>
            </div>
          </div>
          <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">2nd</div>
          <div class="pretty p-default p-round">
            <input type="radio" name="test" value="B"/>
            <div class="state p-primary">
              <label>
                <span>B</span>
              </label>
            </div>
          </div>
          <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">3rd</div>
          <div class="pretty p-default p-round">
            <input type="radio" name="test" value="C"/>
            <div class="state p-primary">
              <label>
                <span>C</span>
              </label>
            </div>
          </div>
          <div style="height:3px;"></div>
        </div>
      </div>

---

    Code
      classic
    Output
      <div id="test" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="test-label">
        <label class="control-label" id="test-label" for="test"></label>
        <div class="shiny-options-group">
          <div class="radio">
            <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">1st</div>
            <label>
              <input type="radio" name="test" value="A" checked="checked"/>
              <span>A</span>
            </label>
          </div>
          <div class="radio">
            <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">2nd</div>
            <label>
              <input type="radio" name="test" value="B"/>
              <span>B</span>
            </label>
          </div>
          <div class="radio">
            <div style="margin-bottom: 10px; font-size: 16px; font-weight: bold;">3rd</div>
            <label>
              <input type="radio" name="test" value="C"/>
              <span>C</span>
            </label>
          </div>
        </div>
      </div>

# loadingButton works

    Code
      button
    Output
      <span class="sf-loading-button" id="sf-loading-buttontest">
        <button id="test" type="button" class="btn action-button btn-success btn-lg">
          <i class="fas fa-wand-magic-sparkles" role="presentation" aria-label="wand-magic-sparkles icon"></i>
          button
        </button>
      </span>

# make_header works

    Code
      make_header("title", c("author 1", "author 2"), affil = "institution", date = "YYYY/MM/DD")
    Output
      <div id="header" align="center">
        <h2 class="title toc-ignore">
          <p style="font-family: Literata; margin-bottom: 0px;">title</p>
        </h2>
        <h5 class="author">Prepared by: author 1<sup>1</sup>, author 2<sup>1</sup></h5>
        <h5 class="date">YYYY/MM/DD</h5>
        <h6 class="affil"><sup>1</sup>institution</h6>
      </div>

---

    Code
      make_header("title", c("author 1", "author 2"), affil = c(`author 1` = "institution 1",
        `author 2` = "institution 2"), date = "YYYY/MM/DD")
    Output
      <div id="header" align="center">
        <h2 class="title toc-ignore">
          <p style="font-family: Literata; margin-bottom: 0px;">title</p>
        </h2>
        <h5 class="author">Prepared by: author 1<sup>1</sup>, author 2<sup>2</sup></h5>
        <h5 class="date">YYYY/MM/DD</h5>
        <h6 class="affil"><sup>1</sup>institution 1; <sup>2</sup>institution 2</h6>
      </div>

---

    Code
      make_header("title", c("author 1", "author 2"), date = "YYYY/MM/DD")
    Output
      <div id="header" align="center">
        <h2 class="title toc-ignore">
          <p style="font-family: Literata; margin-bottom: 0px;">title</p>
        </h2>
        <h5 class="author">Prepared by: author 1, author 2</h5>
        <h5 class="date">YYYY/MM/DD</h5>
        <h6 class="affil"></h6>
      </div>

