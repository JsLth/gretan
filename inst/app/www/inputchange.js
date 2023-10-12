$(document).on('shiny:inputchanged', function(event) {
    if (event.name != 'main-stakeholder-changed' && event.name.startsWith('main-stakeholder-') && !event.name.endsWith('hidden') && !event.name.endsWith('open') && !event.name.startsWith('.') && !event.name.startsWith('waiter')) {
      Shiny.setInputValue('main-stakeholder-changed', event.name);
    }
});
