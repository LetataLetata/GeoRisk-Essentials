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
  new MutationObserver(function (m) { m.forEach(function (r) { r.addedNodes.forEach(function (n) { if (n.nodeType === 1) addFolders(n); }); }); })
    .observe(document.body, { childList: true, subtree: true });

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
