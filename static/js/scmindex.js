let suggestTimeout = null;
function updateSuggestions(text, datalistId) {
    clearTimeout(suggestTimeout);
    suggestTimeout = setTimeout(() => {
        let base = window.location.protocol + '//' + window.location.host;
        let url = new URL('/suggest', base);
        url.searchParams.append('text', text)
        fetch(url)
            .then(resp => resp.json())
            .then(data => {
                let newOptions = data.map(d => {
                    let e = document.createElement('option');
                    e.setAttribute('value', d);
                    return e;
                });
                let datalist = document.getElementById(datalistId);
                datalist.replaceChildren(...newOptions);
            });
    }, 200);
}

function collapseFacet(event, id) {
    event.preventDefault();
    document.getElementById(id).classList.add('collapsed');
}

function expandFacet(event, id) {
    event.preventDefault();
    document.getElementById(id).classList.remove('collapsed');
}

function searchFacet(event, id) {
    for (let option of document.getElementById(id).querySelectorAll('.facet-option')) {
        let text = option.querySelector('.facet-value');
        let visible = !event.target.value || text.textContent.indexOf(event.target.value) != -1;
        if (visible) {
            option.classList.remove('filtered');
        } else {
            option.classList.add('filtered');
        }
    }
}

function hideSearchControls(id) {
    document.getElementById(id).classList.add('hidden');
}

function showSearchControls(id) {
    document.getElementById(id).classList.remove('hidden');
}
