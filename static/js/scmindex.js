const controlsId = 'search-controls';

function collapseFacet(event, id) {
    event.preventDefault();
    document.getElementById(id).classList.add('search-page__facet--collapsed');
}

function expandFacet(event, id) {
    event.preventDefault();
    document.getElementById(id).classList.remove('search-page__facet--collapsed');
}

function searchFacet(event, id) {
    for (let option of document.getElementById(id).querySelectorAll('.search-page__facet-option')) {
        let text = option.querySelector('.search-page__facet-value');
        let visible = !event.target.value || text.textContent.indexOf(event.target.value) != -1;
        if (visible) {
            option.classList.remove('search-page__facet-option--filtered');
        } else {
            option.classList.add('search-page__facet-option--filtered');
        }
    }
}

function handleControlsVisibilityOnLoad() {
    if (localStorage.getItem('collapsed') == 'true' && document.getElementById(controlsId)) {
        hideSearchControls();
    }
}

function hideSearchControls() {
    document.getElementById(controlsId).classList.add('search-page__controls--hidden');
    localStorage.setItem('collapsed', 'true');
}

function showSearchControls() {
    document.getElementById(controlsId).classList.remove('search-page__controls--hidden');
    localStorage.setItem('collapsed', 'false');
}

function collapseControlsForSmallScreen() {
    if (window.innerWidth <= 600) {
        hideSearchControls();
    }
}

function applyTheme() {
    let theme = window.localStorage.getItem('theme');
    let themeCls;
    if (theme == null) {
        if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
            themeCls = 'dark';
        } else {
            themeCls = 'light';
        }
    } else {
        themeCls = theme;
    }
    document.documentElement.className = '';
    document.documentElement.classList.add('theme-' + themeCls);
}

//******************************************************

window.addEventListener('load', () => {
    handleControlsVisibilityOnLoad();
    let openBtn = document.getElementById('search-controls-open');
    if (openBtn) {
        openBtn.addEventListener('click', showSearchControls);
    }
    let closeBtn = document.getElementById('search-controls-close');
    if (closeBtn) {
        closeBtn.addEventListener('click', hideSearchControls);
    }
    let themecb = document.getElementById('theme-cb');
    themecb.value = window.localStorage.getItem('theme') || 'default';
    themecb.addEventListener('change', e => {
        let selected = e.target.value;
        if (selected == 'default') {
            window.localStorage.removeItem('theme');
        } else {
            window.localStorage.setItem('theme', selected);
        }
        applyTheme();
    });
    applyTheme();
    document.getElementById('theme-cb-wrapper').classList.remove('hidden');
    setTimeout(() => {
        document.body.classList.remove('preload');
    }, 0);
});

window.addEventListener('keydown', e => {
    if (e.ctrlKey && e.keyCode == 70) { 
        let el = document.getElementById('query-field');
        if (el) {
            e.preventDefault();
            el.select();
        }
    }
});
document.documentElement.classList.add('js-enabled');
