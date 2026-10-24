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
    if (on) setTimeout(function () {
      var bar = document.querySelector('.topbar'); var off = bar ? bar.getBoundingClientRect().bottom + 12 : 16;
      window.scrollTo({ top: window.scrollY + holder.getBoundingClientRect().top - off, behavior: 'smooth' });
    }, 50);
  }
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

  // Watch the map's frame. Whenever it changes size (Enlarge, dragging, full
  // screen, folding panels, resizing the window) the map redraws at once, and
  // in the world view it zooms so the world fills the frame with no empty band.
  var WORLD_VIEW = 3;
  function fillWorld() {
    var s = map.getSize(); if (!s.x || !s.y) return;
    var snap = function (z) { return Math.ceil(z * 4) / 4; };
    var zFill = snap(Math.max(Math.log2(s.x / 256), Math.log2(s.y / 256)));
    map.setMinZoom(Math.max(1, snap(Math.log2(s.y / 256))));
    // In the world view, keep the whole world centred in the frame.
    if (map.getZoom() < WORLD_VIEW) map.setView([18, 10], zFill, { animate: false });
  }
  var pending = null;
  var onResize = function () {
    if (pending) cancelAnimationFrame(pending);
    pending = requestAnimationFrame(function () { pending = null; map.invalidateSize({ animate: false }); fillWorld(); });
  };
  if (window.ResizeObserver) { if (el._grRO) el._grRO.disconnect(); el._grRO = new ResizeObserver(onResize); el._grRO.observe(el); }
  else window.addEventListener('resize', onResize);
  map.whenReady(function () { setTimeout(onResize, 50); });
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

// ---- Help and first-visit guide ---------------------------------------
$(function () {
  function get(k) { try { return window.localStorage.getItem(k); } catch (e) { return null; } }
  function set(k, v) { try { window.localStorage.setItem(k, v); } catch (e) {} }
  function go(target) {
    var pill = $('.gr-pill[data-target="' + target + '"]');
    if (pill.length) pill.first().trigger('click');
  }

  // Any element with data-goto="page" opens that page.
  $(document).on('click', '[data-goto]', function (e) {
    e.preventDefault();
    var t = $(this).data('goto');
    if ($(this).closest('#guide').length) closeGuide();
    go(t);
  });

  // Welcome guide: slides, dots, back and next.
  var $g = $('#guide'), slides = $g.find('.guide-slide'), cur = 0, lastFocus = null;
  slides.each(function (i) { $g.find('.guide-dots').append('<button type="button" class="guide-dot" aria-label="Slide ' + (i + 1) + '"></button>'); });
  function show(i) {
    cur = Math.max(0, Math.min(slides.length - 1, i));
    slides.removeClass('on').eq(cur).addClass('on');
    $g.find('.guide-dot').removeClass('on').eq(cur).addClass('on');
    $g.find('.guide-back').css('visibility', cur === 0 ? 'hidden' : 'visible');
    var last = cur === slides.length - 1;
    var ARROW = '<svg viewBox="0 0 24 24" width="14" height="14"><path d="M5 12h14M13 6l6 6-6 6" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round" stroke-linejoin="round"/></svg>';
    var TICK = '<svg viewBox="0 0 24 24" width="14" height="14"><path d="M5 12.5l4.5 4.5L19 7.5" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round" stroke-linejoin="round"/></svg>';
    $g.find('.guide-next').html(last ? 'Close ' + TICK : 'Next ' + ARROW);
  }
  function openGuide() { lastFocus = document.activeElement; show(0); $g.addClass('open'); document.body.classList.add('guide-on'); setTimeout(function () { $g.find('.guide-next').trigger('focus'); }, 50); }
  function closeGuide() { $g.removeClass('open'); document.body.classList.remove('guide-on'); set('gr-guide-seen', '1'); if (lastFocus && lastFocus.focus) lastFocus.focus(); }
  window.grOpenGuide = openGuide;
  $g.on('click', '.guide-next', function () { if (cur === slides.length - 1) closeGuide(); else show(cur + 1); });
  $g.on('click', '.guide-back', function () { show(cur - 1); });
  $g.on('click', '.guide-dot', function () { show($(this).index()); });
  $g.on('click', '.guide-close, .guide-skip', closeGuide);
  $g.on('click', function (e) { if (e.target === this) closeGuide(); });
  $(document).on('keydown', function (e) {
    if (!$g.hasClass('open')) return;
    if (e.key === 'Escape') closeGuide();
    else if (e.key === 'ArrowRight') show(cur + 1);
    else if (e.key === 'ArrowLeft') show(cur - 1);
  });
  $(document).on('click', '#open-guide', openGuide);
  // Welcome screen: shown every time the app opens. Continue (or Enter / Esc)
  // hides it and shows the Overview; on a first visit the tour opens next.
  var $w = $('#welcome');
  document.body.classList.add('welcome-on');
  setTimeout(function () { $('#welcome-go').trigger('focus'); }, 100);
  function closeWelcome() {
    if (!$w.hasClass('gone')) {
      $w.addClass('gone'); document.body.classList.remove('welcome-on');
      go('overview');
      if (get('gr-guide-seen') !== '1') setTimeout(openGuide, 500);
    }
  }
  function openWelcome() { $w.removeClass('gone'); document.body.classList.add('welcome-on'); setTimeout(function () { $('#welcome-go').trigger('focus'); }, 100); }
  $(document).on('click', '#welcome-go', closeWelcome);
  $(document).on('keydown', function (e) {
    if ($w.hasClass('gone') || $g.hasClass('open')) return;
    if (e.key === 'Enter' || e.key === 'Escape' || e.key === 'ArrowRight') { e.preventDefault(); closeWelcome(); }
  });
  $(document).on('click keydown', '.topbar .brand', function (e) { if (e.type === 'click' || e.key === 'Enter') openWelcome(); });

  // View menu: close after a choice or when clicking elsewhere.
  $(document).on('click', '.view-list button', function () { $(this).closest('details').removeAttr('open'); });
  $(document).on('click', function (e) {
    $('details.view-menu[open]').each(function () { if (!this.contains(e.target)) this.removeAttribute('open'); });
  });

  // Help page: copy buttons and the section list (scroll below the top bar).
  $(document).on('click', '.copy-btn[data-copy]', function () {
    var btn = $(this), text = btn.attr('data-copy'), label = btn.find('span'), old = label.text();
    var done = function () { label.text('Copied'); btn.addClass('copied'); setTimeout(function () { label.text(old); btn.removeClass('copied'); }, 1600); };
    if (navigator.clipboard && window.isSecureContext) navigator.clipboard.writeText(text).then(done, function () { fallback(); });
    else fallback();
    function fallback() {
      var ta = document.createElement('textarea'); ta.value = text; ta.style.position = 'fixed'; ta.style.opacity = '0';
      document.body.appendChild(ta); ta.select(); try { document.execCommand('copy'); done(); } catch (e) {} ta.remove();
    }
  });
  $(document).on('click', '.help-toc-link', function (e) {
    e.preventDefault();
    var el = document.querySelector($(this).attr('href')); if (!el) return;
    el.classList.remove('folded');
    var bar = document.querySelector('.topbar'), off = bar ? bar.getBoundingClientRect().bottom + 12 : 16;
    window.scrollTo({ top: window.scrollY + el.getBoundingClientRect().top - off, behavior: 'smooth' });
  });
});
