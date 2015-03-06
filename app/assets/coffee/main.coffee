class Main

  handle_add_timeslot: (form) ->


  init_document: () ->
    $('add-timeslot-form').on('click', @handle_add_timeslot)

  constructor: () ->
    document.onload()