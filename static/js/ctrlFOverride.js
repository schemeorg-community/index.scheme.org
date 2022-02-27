window.addEventListener("keydown", e => {
    if (e.ctrlKey && e.keyCode == 70) { 
        let el = document.getElementById('query-field');
        if (el) {
            e.preventDefault();
            el.select();
        }
    }
})
