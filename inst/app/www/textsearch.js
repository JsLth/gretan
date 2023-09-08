$('html').on('click', function(e) {
    if (e.target.id != 'listbox' || e.target.id != 'textSearch') {
        $('#listbox').hide();
    }
})

$('html').on('click', function(e) {
    if (e.target.id == 'textSearch') {
        $('#listbox').show()
    }
})