// inst/app/www/scripts.js

$(document).on("shiny:inputchanged", function atualizarAltura() {
  var painelAtivo = document.querySelector(".tab-pane.container-fluid.active");
  var divSuperior = painelAtivo ? painelAtivo.getElementsByTagName("div")[0] : null;
  var divDependente = document.querySelector(".div-dependente");

  if (divSuperior && divDependente) {
    divDependente.style.height = divSuperior.clientHeight + "px";
  }
});

var openTab = function(tabName){
  $('a', $('.sidebar')).each(function() {
    if(this.getAttribute('data-value') == tabName) {
      this.click()
    };
  });
};

/*
 * Estado estável da barra lateral.
 *
 * O PushMenu do AdminLTE abre a barra automaticamente quando a viewport volta
 * a ultrapassar 992px. Como o painel usa uma barra mais larga (400px), essa
 * alternância também pode deixar uma margem vazia em larguras intermediárias.
 * Mantemos o componente recolhido ao iniciar e deixamos mudanças posteriores
 * exclusivamente a cargo do hover (desktop) ou do botão de alternância.
 */
(function () {
  "use strict";

  var compactSidebarQuery = window.matchMedia("(max-width: 991.98px)");
  var sidebarResizeTimer = null;

  function sidebarToggle() {
    return document.querySelector('[data-widget="pushmenu"]');
  }

  function sidebarIsOpen() {
    return document.body && !document.body.classList.contains("sidebar-collapse");
  }

  function updateSidebarAccessibility(open) {
    var toggle = sidebarToggle();
    if (!toggle) return;

    toggle.setAttribute("aria-expanded", open ? "true" : "false");
    toggle.setAttribute(
      "aria-label",
      open ? "Recolher menu lateral" : "Abrir menu lateral"
    );
  }

  function syncSidebarClasses() {
    if (!document.body) return;

    var open = sidebarIsOpen();
    var compact = compactSidebarQuery.matches;

    document.body.classList.toggle("sidebar-open", compact && open);
    document.body.classList.toggle("sidebar-closed", compact && !open);

    if (!compact) {
      document.body.classList.remove("sidebar-open", "sidebar-closed");
    }

    updateSidebarAccessibility(open);
  }

  function disableAutomaticSidebarResize() {
    var toggle = sidebarToggle();
    if (!toggle) return;

    /*
     * O atributo é lido pelo AdminLTE ao criar o PushMenu. A atualização da
     * instância cobre também páginas em que o componente já foi inicializado.
     */
    toggle.setAttribute("data-auto-collapse-size", "false");

    var pushMenu = $(toggle).data("lte.pushmenu");
    if (pushMenu && pushMenu._options) {
      pushMenu._options.autoCollapseSize = false;
    }
  }

  function setInitialSidebarState() {
    if (!document.body) return;

    document.body.classList.add("sidebar-collapse");
    document.body.classList.remove(
      "sidebar-open",
      "sidebar-is-opening",
      "sidebar-closed"
    );
    syncSidebarClasses();
  }

  function prepareSidebar() {
    disableAutomaticSidebarResize();
    setInitialSidebarState();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", prepareSidebar);
  } else {
    prepareSidebar();
  }

  window.addEventListener("load", function () {
    disableAutomaticSidebarResize();
    setInitialSidebarState();
  });

  $(document).on("shown.lte.pushmenu collapsed.lte.pushmenu", function () {
    syncSidebarClasses();
  });

  window.addEventListener("resize", function () {
    window.clearTimeout(sidebarResizeTimer);
    sidebarResizeTimer = window.setTimeout(function () {
      disableAutomaticSidebarResize();
      syncSidebarClasses();
    }, 80);
  });
})();

/*
 * Camada responsiva global.
 *
 * Os módulos continuam definindo a organização de desktop. Este código apenas
 * identifica linhas que contêm visualizações, resumos ou filtros para que o
 * CSS possa reorganizá-las de forma consistente em viewports menores.
 */
(function () {
  "use strict";

  var resizeTimer = null;
  var observedWidgets = new WeakSet();
  var adaptiveLayoutQuery = window.matchMedia("(max-width: 1439.98px)");
  var visualSelector = [
    ".js-plotly-plot",
    ".plotly.html-widget",
    ".highchart",
    ".leaflet",
    ".reactable",
    ".datatables",
    ".dataTable"
  ].join(",");
  var mortalitySeriesSelector = [
    "[id$='-plot_n_obitos']",
    "[id$='-plot_rmm']",
    "[id$='-plot_pct_diretas']",
    "[id$='-plot_pct_especificas']",
    "[id$='-plot_pct_indiretas']",
    "[id$='-plot_pct_indiretas_especificas']"
  ].join(",");
  var assistIndicatorSelector = [
    "[id$='-grafico_nascimentos']",
    "[id$='-grafico_pp']",
    "[id$='-grafico_pc']",
    "[id$='-grafico_an']",
    "[id$='-grafico_cpn']",
    "[id$='-grafico_robson']",
    "[id$='-grafico_rc']"
  ].join(",");
  var obitosTableSelector = [
    "[id$='-tabela_oficiais']",
    "[id$='-tabela_nao']"
  ].join(",");

  function directBootstrapColumn(element) {
    if (!element || !element.closest) return null;
    var selector = ".col, [class*='col-sm-'], [class*='col-md-'], [class*='col-lg-'], [class*='col-xl-']";
    var column = element.closest(selector);
    var fallback = column;

    while (column) {
      if (column.parentElement && column.parentElement.classList.contains("row")) {
        return column;
      }
      column = column.parentElement ? column.parentElement.closest(selector) : null;
    }

    return fallback;
  }

  function directRow(column) {
    if (!column || !column.parentElement) return null;
    return column.parentElement.classList.contains("row")
      ? column.parentElement
      : column.closest(".row");
  }

  function clearResponsiveLayoutMarks() {
    document.querySelectorAll(".responsive-series-header").forEach(function (element) {
      element.style.removeProperty("min-height");
      element.classList.remove("responsive-series-header");
    });
    document.querySelectorAll(".responsive-assist-layout").forEach(function (element) {
      element.classList.remove("responsive-assist-layout");
    });
    document.querySelectorAll(".responsive-assist-filter").forEach(function (element) {
      element.classList.remove("responsive-assist-filter");
    });
    document.querySelectorAll(".responsive-assist-content").forEach(function (element) {
      element.classList.remove("responsive-assist-content");
    });
    document.querySelectorAll(".obitos-table-top-scroll").forEach(function (element) {
      if (element._resizeObserver) element._resizeObserver.disconnect();
      element.remove();
    });
    document.querySelectorAll(".responsive-visual").forEach(function (element) {
      element.classList.remove("responsive-visual");
    });
    document.querySelectorAll(".responsive-visual-col").forEach(function (element) {
      element.classList.remove("responsive-visual-col");
    });
    document.querySelectorAll(".responsive-visual-row").forEach(function (element) {
      element.classList.remove("responsive-visual-row");
    });
    document.querySelectorAll(".responsive-visual-card").forEach(function (element) {
      element.classList.remove("responsive-visual-card");
    });
    document.querySelectorAll(".responsive-summary-col").forEach(function (element) {
      element.classList.remove("responsive-summary-col");
    });
    document.querySelectorAll(".responsive-summary-row").forEach(function (element) {
      element.classList.remove("responsive-summary-row");
    });
    document.querySelectorAll(".responsive-filter-row").forEach(function (element) {
      element.classList.remove("responsive-filter-row");
    });
  }

  function equalizeMortalitySeriesHeaders(scope) {
    var root = scope && scope.querySelectorAll ? scope : document;
    var headers = [];

    root.querySelectorAll(mortalitySeriesSelector).forEach(function (output) {
      var card = output.closest(".card, .box");
      var header = card ? card.querySelector(":scope > .card-header, :scope > .box-header") : null;

      if (header && headers.indexOf(header) === -1) {
        header.classList.add("responsive-series-header");
        header.style.removeProperty("min-height");
        headers.push(header);
      }
    });

    var visibleHeaders = headers.filter(function (header) {
      return header.offsetParent !== null;
    });
    if (visibleHeaders.length < 2) return;

    var naturalHeights = visibleHeaders.map(function (header) {
      return Math.ceil(header.getBoundingClientRect().height);
    });
    var maxHeight = Math.max.apply(Math, naturalHeights);
    var minHeight = Math.min.apply(Math, naturalHeights);

    if (maxHeight - minHeight > 1) {
      visibleHeaders.forEach(function (header) {
        header.style.minHeight = maxHeight + "px";
      });
    }
  }

  function markAssistIndicatorLayouts(scope) {
    var root = scope && scope.querySelectorAll ? scope : document;

    root.querySelectorAll(assistIndicatorSelector).forEach(function (output) {
      var contentColumn = directBootstrapColumn(output);
      var row = directRow(contentColumn);
      if (!contentColumn || !row) return;

      contentColumn.classList.add("responsive-assist-content");
      row.classList.add("responsive-assist-layout");

      Array.prototype.forEach.call(row.children, function (column) {
        if (column !== contentColumn && /(^|\s)col-(sm|md|lg|xl)-\d+($|\s)/.test(column.className || "")) {
          column.classList.add("responsive-assist-filter");
        }
      });
    });
  }

  function ensureObitosTopScrollbars(scope) {
    var root = scope && scope.querySelectorAll ? scope : document;

    root.querySelectorAll(obitosTableSelector).forEach(function (output) {
      var reactTable = output.querySelector(".ReactTable");
      var scrollTarget = output.querySelector(".rt-table");
      if (!reactTable || !scrollTarget) return;

      var existing = reactTable.querySelector(":scope > .obitos-table-top-scroll");
      if (existing && existing._scrollTarget === scrollTarget) {
        if (typeof existing._updateMetrics === "function") {
          window.requestAnimationFrame(existing._updateMetrics);
        }
        return;
      }
      if (existing) {
        if (existing._resizeObserver) existing._resizeObserver.disconnect();
        existing.remove();
      }

      var topScroll = document.createElement("div");
      var spacer = document.createElement("div");
      topScroll.className = "obitos-table-top-scroll";
      spacer.className = "obitos-table-top-scroll-spacer";
      topScroll.tabIndex = 0;
      topScroll.setAttribute("role", "region");
      topScroll.setAttribute("aria-label", "Rolagem horizontal da tabela");
      topScroll.appendChild(spacer);
      reactTable.insertBefore(topScroll, scrollTarget);

      var syncing = false;
      var updateMetrics = function () {
        if (!topScroll.isConnected || !scrollTarget.isConnected) return;

        var targetViewportWidth = scrollTarget.clientWidth;
        var targetMaxScroll = Math.max(
          0,
          scrollTarget.scrollWidth - targetViewportWidth
        );
        var proxyViewportWidth = topScroll.clientWidth || reactTable.clientWidth;
        var proxyContentWidth = proxyViewportWidth + targetMaxScroll;

        spacer.style.width = proxyContentWidth + "px";
        topScroll.classList.toggle("is-needed", targetMaxScroll > 1);
        topScroll.scrollLeft = Math.min(
          scrollTarget.scrollLeft,
          targetMaxScroll
        );
      };

      topScroll._scrollTarget = scrollTarget;
      topScroll._updateMetrics = updateMetrics;

      topScroll.addEventListener("scroll", function () {
        if (syncing) return;
        syncing = true;
        scrollTarget.scrollLeft = topScroll.scrollLeft;
        syncing = false;
      });

      topScroll.addEventListener("keydown", function (event) {
        var step = Math.max(48, Math.round(topScroll.clientWidth * 0.2));
        var nextLeft = topScroll.scrollLeft;

        if (event.key === "ArrowRight") nextLeft += step;
        else if (event.key === "ArrowLeft") nextLeft -= step;
        else if (event.key === "PageDown") nextLeft += topScroll.clientWidth;
        else if (event.key === "PageUp") nextLeft -= topScroll.clientWidth;
        else if (event.key === "Home") nextLeft = 0;
        else if (event.key === "End") nextLeft = topScroll.scrollWidth;
        else return;

        event.preventDefault();
        topScroll.scrollLeft = nextLeft;
      });

      scrollTarget.addEventListener("scroll", function () {
        if (syncing) return;
        syncing = true;
        topScroll.scrollLeft = scrollTarget.scrollLeft;
        syncing = false;
      });

      if (window.ResizeObserver) {
        var tableResizeObserver = new ResizeObserver(updateMetrics);
        tableResizeObserver.observe(scrollTarget);
        tableResizeObserver.observe(reactTable);
        topScroll._resizeObserver = tableResizeObserver;
      }

      window.requestAnimationFrame(updateMetrics);
    });
  }

  function markVisualLayouts(root) {
    if (!adaptiveLayoutQuery.matches) {
      clearResponsiveLayoutMarks();
      equalizeMortalitySeriesHeaders(document);
      return;
    }

    var scope = root && root.querySelectorAll ? root : document;

    scope.querySelectorAll(visualSelector).forEach(function (widget) {
      var column = directBootstrapColumn(widget);
      var row = directRow(column);
      var card = widget.closest(".card, .box");

      widget.classList.add("responsive-visual");
      if (column) column.classList.add("responsive-visual-col");
      if (row) row.classList.add("responsive-visual-row");
      if (card) card.classList.add("responsive-visual-card");

      if (window.ResizeObserver && !observedWidgets.has(widget)) {
        observedWidgets.add(widget);
        responsiveResizeObserver.observe(widget);
      }
    });

    scope.querySelectorAll(".custom-box").forEach(function (summary) {
      var column = directBootstrapColumn(summary);
      var row = directRow(column);

      if (column) column.classList.add("responsive-summary-col");
      if (row) row.classList.add("responsive-summary-row");
    });

    scope.querySelectorAll(".row").forEach(function (row) {
      var directColumns = Array.prototype.filter.call(row.children, function (child) {
        return /(^|\s)col($|\s)|(^|\s)col-(sm|md|lg|xl)-\d+($|\s)/.test(child.className || "");
      });
      var filterColumns = directColumns.filter(function (column) {
        return column.querySelector(".form-group, .shiny-input-container");
      });

      if (directColumns.length > 1 && filterColumns.length >= 2) {
        row.classList.add("responsive-filter-row");
      }
    });

    markAssistIndicatorLayouts(scope);
    equalizeMortalitySeriesHeaders(scope);
    ensureObitosTopScrollbars(scope);
  }

  function resizeVisuals() {
    if (!adaptiveLayoutQuery.matches) return;

    window.requestAnimationFrame(function () {
      document.querySelectorAll(".js-plotly-plot").forEach(function (plot) {
        if (window.Plotly && window.Plotly.Plots) {
          window.Plotly.Plots.resize(plot);
        }
      });

      window.dispatchEvent(new Event("resize"));
    });
  }

  function scheduleResponsiveRefresh(root) {
    markVisualLayouts(root);
    window.clearTimeout(resizeTimer);
    resizeTimer = window.setTimeout(resizeVisuals, 90);
  }

  var responsiveResizeObserver = window.ResizeObserver
    ? new ResizeObserver(function (entries) {
        if (!adaptiveLayoutQuery.matches) return;

        entries.forEach(function (entry) {
          var plot = entry.target;
          if (plot.classList.contains("js-plotly-plot") &&
              window.Plotly && window.Plotly.Plots) {
            window.requestAnimationFrame(function () {
              window.Plotly.Plots.resize(plot);
            });
          }
        });
      })
    : { observe: function () {} };

  function startResponsiveLayout() {
    scheduleResponsiveRefresh(document);

    if (window.MutationObserver && document.body) {
      new MutationObserver(function () {
        window.clearTimeout(resizeTimer);
        resizeTimer = window.setTimeout(function () {
          markVisualLayouts(document);
        }, 90);
      }).observe(document.body, { childList: true, subtree: true });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", startResponsiveLayout);
  } else {
    startResponsiveLayout();
  }

  window.addEventListener("resize", function () {
    window.clearTimeout(resizeTimer);
    resizeTimer = window.setTimeout(function () {
      markVisualLayouts(document);
      if (!adaptiveLayoutQuery.matches) return;

      document.querySelectorAll(".js-plotly-plot").forEach(function (plot) {
        if (window.Plotly && window.Plotly.Plots) {
          window.Plotly.Plots.resize(plot);
        }
      });
    }, 120);
  });

  $(document).on(
    "shiny:connected shiny:value shiny:recalculated shown.bs.tab shown.lte.pushmenu collapsed.lte.pushmenu",
    function () {
      scheduleResponsiveRefresh(document);
    }
  );
})();
