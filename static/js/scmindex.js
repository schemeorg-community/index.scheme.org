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
