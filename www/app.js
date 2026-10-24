// ---- Map sizing: an Enlarge button and a drag handle on every map.
// Enlarge: the map card spans the full width and most of the screen height
// (click again to shrink). Drag the handle at the bottom right of a map card
// to change its height, and its width where it shares a row with another
// panel. Double-click the handle to go back to the original size.
window.grMapEnhance = function (el, map) {
  var card = el.closest('.gr-card'); if (!card) return;
  var explore = el.closest('.explore-grid');
  var grid = explore || (card.parentElement && card.parentElement.classList.contains('gr-grid') ? card.parentElement : null);
  var sizeEl = explore || el;                       // element whose height sets the map height
  var h0 = sizeEl.style.height || '', cols0 = grid ? (grid.style.gridTemplateColumns || '') : '';
  var raf = null;
  var refresh = function () { if (raf) return; raf = requestAnimationFrame(function () { raf = null; map.invalidateSize({ pan: false }); }); };
  var later = function () { setTimeout(function () { map.invalidateSize({ pan: false }); }, 260); };
  var ICON_BIG = '<svg viewBox="0 0 24 24" width="16" height="16"><path d="M14 4h6v6M10 20H4v-6M20 4l-7 7M4 20l7-7" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/></svg>';
  var ICON_SMALL = '<svg viewBox="0 0 24 24" width="16" height="16"><path d="M20 10h-6V4M4 14h6v6M14 10l7-7M10 14l-7 7" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/></svg>';
  var big = false, link = null;
  function setBig(on) {
    big = on;
    var holder = explore || card;
    holder.classList.toggle('map-wide', on);
    if (grid && !explore) grid.classList.toggle('grid-stack', on);
    if (grid) grid.style.gridTemplateColumns = on ? (explore ? 'minmax(0, 1fr) 340px' : '') : cols0;
    sizeEl.style.height = on ? (explore ? '86vh' : '76vh') : h0;
    if (link) { link.innerHTML = on ? ICON_SMALL : ICON_BIG; link.title = on ? 'Shrink map back' : 'Enlarge map (full width, most of the screen)'; }
    if (on) setTimeout(function () { holder.scrollIntoView({ behavior: 'smooth', block: 'start' }); }, 50);
    setTimeout(function () {
      map.invalidateSize({ pan: false });
      if (on) { saved = { c: map.getCenter(), z: map.getZoom() }; if (map.getZoom() < 3) map.fitBounds([[-56, -168], [74, 180]], { animate: false }); }
      else if (saved) { map.setView(saved.c, saved.z, { animate: false }); saved = null; }
    }, 280);
  }
  var saved = null;
  var Big = L.Control.extend({ options: { position: 'topright' }, onAdd: function () {
    var box = L.DomUtil.create('div', 'leaflet-bar gr-big');
    link = L.DomUtil.create('a', '', box); link.href = '#'; link.setAttribute('role', 'button');
    link.innerHTML = ICON_BIG; link.title = 'Enlarge map (full width, most of the screen)';
    L.DomEvent.disableClickPropagation(box);
    L.DomEvent.on(link, 'click', function (e) { L.DomEvent.preventDefault(e); setBig(!big); });
    return box; } });
  new Big().addTo(map);

  // Drag handle
  var handle = document.createElement('div');
  handle.className = 'map-resize'; handle.title = 'Drag to resize the map; double-click to reset';
  handle.innerHTML = '<svg viewBox="0 0 16 16" width="14" height="14"><path d="M14 6L6 14M14 10l-4 4" stroke="currentColor" stroke-width="1.8" stroke-linecap="round"/></svg>';
  var old = card.querySelector(':scope > .map-resize'); if (old) old.remove();
  card.appendChild(handle);
  var start = null;
  handle.addEventListener('pointerdown', function (e) {
    e.preventDefault(); handle.setPointerCapture(e.pointerId);
    var cols = grid ? getComputedStyle(grid).gridTemplateColumns.split(' ').length : 1;
    start = { x: e.clientX, y: e.clientY, h: sizeEl.getBoundingClientRect().height, w: card.getBoundingClientRect().width,
              gw: grid ? grid.getBoundingClientRect().width : 0, two: grid && cols >= 2 && !card.classList.contains('map-wide'),
              first: grid ? grid.firstElementChild === card || (explore && true) : true };
    document.body.classList.add('resizing');
  });
  handle.addEventListener('pointermove', function (e) {
    if (!start) return;
    var h = Math.max(260, Math.min(window.innerHeight * 0.95, start.h + (e.clientY - start.y)));
    sizeEl.style.height = h + 'px';
    if (start.two) {
      var gap = parseFloat(getComputedStyle(grid).columnGap) || 16;
      var w = Math.max(320, Math.min(start.gw - 300 - gap, start.w + (start.first ? 1 : -1) * (e.clientX - start.x)));
      grid.style.gridTemplateColumns = start.first ? (w + 'px minmax(0, 1fr)') : ('minmax(0, 1fr) ' + w + 'px');
    }
    refresh();
  });
  var stop = function () { if (!start) return; start = null; document.body.classList.remove('resizing'); later(); };
  handle.addEventListener('pointerup', stop); handle.addEventListener('pointercancel', stop);
  handle.addEventListener('dblclick', function () { if (big) setBig(false); sizeEl.style.height = h0; if (grid) grid.style.gridTemplateColumns = cols0; later(); });
};

// Navigation, foldable cards and compact layout.
$(function () {
  document.body.classList.add('mode-analyst');
  function resizeSoon() { setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 260); }
  function store(k, v) { try { if (v === undefined) return window.localStorage.getItem(k); window.localStorage.setItem(k, v); } catch (e) { return null; } }

  // ---- page navigation
  $(document).on('click', '.gr-pill', function () {
    var bar = $(this).closest('.pill-bar');
    bar.find('.gr-pill').removeClass('active');
    $(this).addClass('active');
    Shiny.setInputValue('nav', $(this).data('target'), { priority: 'event' });
    window.scrollTo({ top: 0, behavior: 'smooth' });
    resizeSoon();
  });
  // Recolour a map that is already drawn (only colours and labels are sent).
  Shiny.addCustomMessageHandler('gr-restyle', function (msg) {
    var w = HTMLWidgets.find('#' + msg.id); if (!w || !w.getMap) return;
    var map = w.getMap(); if (!map || !map.layerManager) return;
    for (var i = 0; i < msg.ids.length; i++) {
      var layer = map.layerManager.getLayer('shape', msg.ids[i]); if (!layer) continue;
      layer.setStyle({ fillColor: msg.fill[i] });
      if (msg.labels && layer.getTooltip && layer.getTooltip()) layer.setTooltipContent(msg.labels[i]);
    }
  });

  Shiny.addCustomMessageHandler('goto', function (target) {
    $('.gr-pill[data-target="' + target + '"]').trigger('click');
  });

  // ---- foldable cards: every card with a header gets a fold button
  function addFolders(root) {
    $(root).find('.gr-card').each(function () {
      var head = $(this).children('.gr-card-head');
      if (!head.length || head.find('.fold-btn').length) return;
      var tools = head.children('.gr-card-tools');
      if (!tools.length) { tools = $('<div class="gr-card-tools"></div>'); head.append(tools); }
      tools.append('<button type="button" class="fold-btn" title="Fold or unfold this panel" aria-label="Fold panel"><svg viewBox="0 0 24 24" width="16" height="16"><path d="M6 9l6 6 6-6" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/></svg></button>');
    });
  }
  addFolders(document);
  // Shiny adds cards when pages render; check at most once per frame, and ignore
  // map tiles and chart internals, which change constantly.
  var pending = false;
  new MutationObserver(function (m) {
    if (pending) return;
    for (var i = 0; i < m.length; i++) {
      var t = m[i].target;
      if (t.closest && t.closest('.leaflet-container, .js-plotly-plot, .dataTables_wrapper')) continue;
      pending = true;
      window.requestAnimationFrame(function () { pending = false; addFolders(document); });
      return;
    }
  }).observe(document.body, { childList: true, subtree: true });

  $(document).on('click', '.fold-btn', function (e) {
    e.stopPropagation();
    $(this).closest('.gr-card').toggleClass('folded');
    resizeSoon();
  });
  $(document).on('click', '.gr-card-head', function (e) {
    if ($(e.target).closest('.gr-card-tools, a, button, details, input, select').length) return;
    $(this).closest('.gr-card').toggleClass('folded'); resizeSoon();
  });
  $(document).on('click', '#fold-all', function () { $('.view:visible .gr-card').has('.fold-btn').addClass('folded'); resizeSoon(); });
  $(document).on('click', '#unfold-all', function () { $('.gr-card.folded').removeClass('folded'); resizeSoon(); });

  // ---- compact layout, remembered in this browser
  function setCompact(on) { document.body.classList.toggle('compact', on); $('#compact-toggle').toggleClass('on', on); store('georisk-compact', on ? '1' : '0'); resizeSoon(); }
  if (store('georisk-compact') === '1') setCompact(true);
  $(document).on('click', '#compact-toggle', function () { setCompact(!document.body.classList.contains('compact')); });

  // Close an open download menu when clicking elsewhere.
  $(document).on('click', function (e) {
    $('details.dl-menu[open]').each(function () { if (!this.contains(e.target)) this.removeAttribute('open'); });
  });
});
