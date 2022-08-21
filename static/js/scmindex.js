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

window.addEventListener('load', () => {
    handleControlsVisibilityOnLoad();
    setTimeout(() => {
        document.body.classList.remove('preload');
    }, 0);
});
