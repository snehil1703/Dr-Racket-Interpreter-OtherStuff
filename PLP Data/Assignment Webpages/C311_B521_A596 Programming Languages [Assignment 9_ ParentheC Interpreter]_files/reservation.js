function Reservation(options) {
  var log = function(logItem) {
    if( window.console && console.log ){
      console.log(logItem)
    }
  }

  this.options = options || new Object();
  log(this.options);

  var reservationType, 
      reservationTypeMaxSeats,
      year, 
      month, 
      day, 
      startingTime, 
      firstName, 
      lastName, 
      phone, 
      email, 
      comments, 
      group,
      hasTags,
      tag,
      datepicker,
      data,
      availabilityDates,
      selectedDates,
      unSelectableDates,
      maximumDays,
      numWholeDays,
      jq_populateDate,
      jq_populateString,
      populateData,
      isWidget,
      slotsToReschedule,
      directBooking,
      hasSubcategories,
      passedSubcatStep,
      numApplicableSubcats,
      providerLocale,
      serverOffset;
      
  var stepOffset = 1;
  var currentStep = 1;
  var currentDate = $("#startDate").val();
  var self = $(this);

  var DOM = {
    reservationTypeLinks: $('#availReservationTypes a'),
    datepicker: $('#jq_datePicker'),
    clockSpinner: $('img#clockSpinner'),
    getCurrentDate: function(){
      return $('#startDate').val();
    }

  };


  function sortLis(a, b) {
    var x = $(a).find('a').html();
    var y = $(b).find('a').html();
    return ((x < y) ? -1 : ((x > y) ? 1 : 0));
  }

  function sortTimestampLis(a, b){
    var x = $(a).find('a').attr('id');
    var y = $(b).find('a').attr('id');
    return ((x < y) ? -1 : ((x > y) ? 1 : 0));
  }

  function sortedLis(visibleLis, stepId){
    if(stepId == 'availTimeslots'){
      return visibleLis.sort(sortTimestampLis);      
    } else {
      return visibleLis.sort(sortLis);    
    }
  }

  function reChunk(stepId) {
    //must be called after the step is shown b/c it looks for visible lis
    var stepWrap    = $('#' + stepId);
    var columnCount = isWidget ? 2 : $("#"+ stepId + " div > ul").length;
    var visibleLis  = stepWrap.find("li:visible");
    var LisPerColumn = Math.ceil(visibleLis.length / columnCount);
    //var LisPerColumn = Math.floor(visibleLis.length / columnCount);
    //var LisPerColumn = visibleLis.length;

    visibleLis = sortedLis(visibleLis, stepId);

    visibleLis.each(function(index, element){
      element = $(element);
      var currentColumn = Math.floor(index/LisPerColumn);

      $(stepWrap.find('ul')[currentColumn]).append(element);
    })
  }

  var insertStepNumbers = function(){
    //this recounts the steps and numbers them in their h tags appropriately
   $.each($.find('.step h2'), function(i, item) {
   $(item).find('span').text((i+stepOffset) + '. ');
  });
  }
   
  var insertRecalculatedStepNumbers = function(){
    //this recounts the steps and numbers them in their h tags appropriately
   stepOffset = 0;
   $.each($.find('.step h2'), function(i, item) {
   $(item).find('span').text((i+stepOffset) + '. ');
  });
  }

  var clearAdditionalReservationTypes = function() {
    var otherReservationTypes = $("#availReservationTypes a[id!='" + reservationType + "']");
    otherReservationTypes.each(function(index, element){
      $("a[id='" + element.id + "']'").parent().hide();
    });
    if(otherReservationTypes.length > 0){
      var currentContent = $('#availReservationTypes h2').html();
      currentContent = currentContent + "<a href='#' class='moreoptions' id='showAllReservationTypes'>(Show More Options)</a>";
      $('#availReservationTypes h2').html(currentContent);
      $('#showAllReservationTypes').click(function(){
        otherReservationTypes.each(function(index, element){
      $("a[id='" + element.id + "']'").parent().show();
        });
      });
    }
  }

  var initialHide = function(){
    if(isWidget){
      initialHideInWidget();
    } else {
      initialHideNoWidget();
    }
  };

  var initialHideNoWidget = function(){
    //just hides the form elements that should not appear on page load
    $('#reservationTypeSubcategories').hide();
    if (hasTags == true) {
      $('#availReservationTypes').hide();
    } else {
      //currentStep = 2;
      currentStep = stepNumberFromId('availReservationTypes');//JWH:  Might be superfluous
      $('#availReservationTypes').addClass('used');
    }

    $('#customerForm, #startDatetime h2').hide();      
  };

  var initialHideInWidget = function(){
    //just hides the form elements that should not appear on page load
    if (hasTags == true) {
      hideFromThisStepOn('providerTags');
    } else if (directBooking) {
      if(hasSubcategories){
        hideFromThisStepOn("reservationTypeSubcategories");
        $('#reservationTypeSubcategories a.jq_stepBack').hide();
        $('#startDatetime a.jq_stepBack').hide();
        currentStep = stepNumberFromId('reservationTypeSubcategories'); 
        reservationType = directBooking; 
        $('.used').removeClass('used');
      } else {
        hideFromThisStepOn("availResources");
        $('#startDatetime a.jq_stepBack').hide();
        reservationType = directBooking; 
        $('.used').removeClass('used');
      }
    } else {
      //currentStep = 2;
      currentStep = stepNumberFromId('availReservationTypes'); //JWH: This might be superfluous
      hideFromThisStepOn('availReservationTypes');
      //hideFromThisStepOn("availResources");
      $('#availReservationTypes').addClass('used');
    }
  };

  var hideFromThisStepOn = function(selector) {
    //called when a previous step is clicked and form must cleared from that point on
     var steps = [
      "providerTags",
      "availReservationTypes", 
      "reservationTypeSubcategories",
      "availResources", 
      "availTimeslots", 
      "customerForm", 
      "startDatetime"
    ]

    var startingStep = null; //steps.indexOf(selector); // IE doesn't support this.
    for (var i=0; i<steps.length; i++) {
      if(steps[i] == selector) {
        startingStep = i;
        break;
      }
    }

    for (i=startingStep + 1;i<=steps.length;i++){
      $('#' + steps[i]).hide();
    }
  }

  var destroyDatePicker = function(){
    if(!directBooking){
      DOM.datepicker.datepicker('destroy');
    }
  }

  var removeAndAddSelectedClass = function(className, element) {
    $(className).removeClass('selected');
    element.addClass('selected');
  };

  var clearDatesList = function(){
    $('#datesList').html(''); 
    selectedDates = new Array();
    unSelectableDates = new Array();
  };

  var hidePreviousStep = function(currentStepId){
    if(isWidget){
      var stepToHide = $('#' + currentStepId).parent().find('div.step:visible').first();
      log('hiding');
      log(stepToHide);
      stepToHide.hide();
    }
  };

  var showStepBackLink = function(currentStepId){
    $('#' + currentStepId + ' a.jq_stepBack').show();
  };

  var stepIds = [
    "providerTags",
    "availReservationTypes", 
    "reservationTypeSubcategories",
    "startDatetime",
    "availResources", 
    "availTimeslots", 
    "customerForm"
  ];

  var stepNumberFromId = function(stepId){
    var step;

    $(stepIds).each(function(index, id){
      if(stepId == id){
        step = index + 1;
      }
    })
log('step' + step);
    return step;
  };

  var idFromStepNumber = function(number){
    return stepIds[number -1];
  };

  var setCurrentStep = function(step){
    var stepsSkipped = step - currentStep - 1;

    if(stepsSkipped > 0){
      for(skippedSteps = stepsSkipped; skippedSteps > 0; skippedSteps--){
        var skippedDiv = $('#' + idFromStepNumber(step - skippedSteps));
        log(skippedDiv);
        skippedDiv.removeClass('used');
      }
    }

    log('step number to mark used' + step);
    var stepToMarkUsed = $('#' + idFromStepNumber(step));
    log('setused')
    log(stepToMarkUsed);
    stepToMarkUsed.addClass('used');
    currentStep = step;
  };

  var moveToStep = function(stepId){
    //hides previous, shows current
    hidePreviousStep(stepId);
    if(isWidget){
      $('#' + stepId).show('slide', {direction: 'right'});
    } else {
      $('#' + stepId).show();
    }
  };

  var showAvailReservationTypes = function(){ //step 2
    log('show step2')
      setCurrentStep(stepNumberFromId('availReservationTypes'));
      $('#availReservationTypes a').removeClass('selected');
      destroyDatePicker();
      hideReservationTypes();
      hideFromThisStepOn('availReservationTypes');
      insertStepNumbers();
      showReservationTypesByCurrentTag();

      moveToStep('availReservationTypes');    
      reChunk("availReservationTypes");

      showStepBackLink('availReservationTypes');

  };

  //JWH: Added 2011-11-10
  var showSubcategoriesStep = function(){ //substep of step 2
      var applicableSubcats = $('.subcat-'+reservationType);
      if(applicableSubcats.length > 0){
        passedSubcatStep = true;
        $('#reservationTypeSubcategories').addClass('step');//necessary to ensure display if user clicks after we're in the date step
        $('#reservationTypeSubcategories').addClass('used');//necessary to ensure backlinks work
        $('#startDatetime input.jq_nextStep').hide();
        destroyDatePicker();
        hideReservationTypeSubcategories();
        //$('#reservationTypeSubcategories').show();
        //hideFromThisStepOn('availResources');
        hideFromThisStepOn('reservationTypeSubcategories');
        insertStepNumbers();
        showSubcategoriesByCurrentReservationType();

        moveToStep('reservationTypeSubcategories');
        reChunk("reservationTypeSubcategories");

        showStepBackLink('availReservationTypes');
      } else {
        //$('#reservationTypeSubcategories').removeClass('step');//Necessary to skip this step in numbering if there are no applicable subcategories
        showDateStep();
      }

  };

  var showDateStep = function(){ //step 3
    log('show step3');

    $('#startDatetime input.jq_nextStep').hide();

      setCurrentStep(stepNumberFromId('startDatetime'));
      destroyDatePicker();
      hideFromThisStepOn('availResources');
      $('#startDatetime h2').show();
      if(!directBooking || (directBooking && passedSubcatStep)){
        showStepBackLink('startDatetime');
      }
      DOM.clockSpinner.show();
      $('#availResources, #availTimeslots').empty();

      //alert('Showdate');
      clearDatesList();
      showChooseDateMessage();
      //$("#startDate").val(currentDate);//JWH: This call is probably superfluous
      //alert($("#startDate").val());
      insertStepNumbers();
      ajaxUpdateCalendar(typeof populateData != 'undefined' ? true : false);

      moveToStep('startDatetime');
      //$('#ui-datepicker-div').css('visibility', 'visible');//Firefox workaround
      //$('#ui-datepicker-div').css('display', 'block');//Firefox workaround
      //$('.ui-datepicker').css('display', 'block');//Firefox workaround
      //$('.ui-datepicker').css('visibility', 'visible');//Firefox workaround
      //$('.ui-helper-clearfix:after').attr('visibility', 'visible');//Firefox workaround
  };

  function showAvailableResources(){//step 4
        log('show step4')
    $('#availResources .jq_nextStep').hide();

    if($('#availResources').html().length > 0){
      setCurrentStep(stepNumberFromId('availResources'));
    };

    showStepBackLink('availResources');
    if ($('#availResources').find('a').length > 0 && populateData) {
      $.each(populateData.resources, function(key, resourceId) {
        $('#availResources a.' + resourceId).click();          
      });
      populateData.resources = null; // in case the customer selects a different date.  //AJC
    }

    moveToStep('availResources');

    $('#availResources .group').each(function(index, element){
      reChunk($(element).attr('id'));
    });

    
    //$('#availResources .group li a').tooltip({offset: [-20, 0]});
    $('#availResources .group li a:not([title=""])').tooltip({offset: [-20, 0]});

    $.scrollTo('#availResources');
  }

  function showAvailableTimeslots(data) {//step 5
        log('show step5')
      $('#availTimeslots input.jq_nextStep').hide();

      setCurrentStep(stepNumberFromId('availTimeslots'));
      showStepBackLink('availTimeslots');
      $.scrollTo('#availTimeslots');
      $('#availTimeslots').html(data);
      //reset the form so there are no lingering selected timeslots
      log('starting the check check');

      $.each($.find('#availTimeslots .hideCheckboxes input'), function(i, item) {
        $(item).removeAttr('checked');
        log ('removed check for '+item);
      });

      // If only one timeslot is available, select it.
      if ($('#availTimeslots a').length == 1) {
        $('#availTimeslots a').click();
      } else if (populateData && !isRescheduling()) {
        $.each(populateData.numDurations, function(key, timestamp) {
          $('#availTimeslots a#' + timestamp).click();
        });
      }

      insertStepNumbers();
      setChooseTimeslotsMessage();
      moveToStep('availTimeslots');
      reChunk('availTimeslots');
      includeContinueButtonForMultiSelect();

      $.scrollTo('#availTimeslots');
  }; 
  

  var setChooseTimeslotsMessage = function(){
    if(slotsToReschedule > 0){
      $('#availTimeslots h2').html(
        $('#availTimeslots h2').html().replace(
          'Select a Time',
          'Select ' + slotsToReschedule + ' Timeslots'
        )
      )
    }
  } 

  function getShortFormattedDate(ln, dt) {
    var msg = $.evalJSON($.ajax({
      url: reservationPath('formatdate'),
      type: 'GET',
      dataType: 'json',
      data: {'language': ln, 'startdate': dt},
      async: false
    }).responseText);
    return msg;
  }

  function fetchCustomQuestions() {
    return $.evalJSON($.ajax({
      url: reservationPath('customquestions'),
      type: 'GET',
      dataType: 'json',
      data: { 'id': reservationType },
      async: false
    }).responseText);
  }

  function makeFormTextField(fld) {
    //alert(JSON.stringify(fld));
    return '<tr><th><label for="' 
      + 'cqid-' + fld['qid'] + 
      '">' 
      + fld['question'] + 
      '</label></th><td><input type="text" name="'
      + 'cqid-' + fld['qid'] + 
      '" id="' 
      + 'cqid-' + fld['qid'] + 
      '" class="text required"/></td></tr>';
  }

  function addCustomerFormFields(flds) {
    $('input[id^="cqid-"]').parent().parent().remove();
    for( fld in flds ){
      var newelemid = '#cqid-' + flds[fld]['qid'];
      if(!$(newelemid).length){
        $('#customerFormTable').prepend($(makeFormTextField(flds[fld])));
      } 
      //alert(makeFormField(flds[fld]));
    }
  }

  function showCustomerForm(){//step 6
        log('show step6')
    if (hasCustomQuestions) {
      addCustomerFormFields(fetchCustomQuestions());
    }

    setCurrentStep(stepNumberFromId('customerForm'));
    populateCustomerForm();
    showStepBackLink('customerForm');
    insertStepNumbers();
    moveToStep('customerForm');

    // If the customer can only select one day or timeslot, scroll to the customer form.
    if (($('a.selected.allDay').length > 0 && maximumDays == 1) ||
      ($('a.selected.allDay').length == 0 && data.maximumDurations == 1)) {
        if(!isWidget){
          $.scrollTo('#customerForm');
        }
    }
  }

  var includeContinueButtonForResources = function(){
    if(isWidget){
      $('#availResources input.jq_nextStep').show();      
    }
  }

  var includeContinueButtonForMultiSelect = function(){
    if(data.allowMultiSelect == true && isWidget){
      $('#availTimeslots input.jq_nextStep').show();
    }
  }

  var includeContinueButtonForMultiDay = function(){
    if(maximumDays > 1 && isWidget){
      $('#startDatetime input.jq_nextStep').show();
    } else {
      showCustomerForm();
    }
  }

  var getReservationTypeInfo = function(id) {
      log('res type: ' + reservationType)

    return $.evalJSON($.ajax({
      url: reservationPath('info'),
      type: 'GET',
      dataType: 'json',
      data: { 'id': reservationType },
      async: false
    }).responseText);
  };

  var showChooseDateMessage = function() {
      data = getReservationTypeInfo(reservationType);
      var html;
      numWholeDays = data.numWholeDays;

      //alert(data.startDate);
      if(data.startDate){
        currentDate = data.startDate; 
        $('#startDate').val(currentDate);
      }
      //alert(currentDate);

      if ($('a.selected.allDay').length > 0) {
        if (data.maximumDurations > 1 && numWholeDays == 1) {
          html = '<span>2. </span>Choose up to ' + data.maximumDurations + ' Dates';
        } else if (numWholeDays > 1 && data.maximumDurations == 1) {
          html = '<span>2. </span>This Reservation is ' + numWholeDays + ' Days Long, Select The First Date.';
        } else if (numWholeDays > 1 && data.maximumDurations > 1) {
          html = '<span>2. </span>This Reservation is ' + numWholeDays + ' Days Long, You May Select Up to ' + data.maximumDurations + ' Reservations.';
        } else {
          //html = '<span>2. </span>Choose a Date';
        }
        maximumDays = data.maximumDurations;
      }else{
          //html = '<span>2. </span>Choose a Date';
          maximumDays = 1;
      }      

      if($('#startDatetime h2').html == 'Choose a Date'){
        $('#startDatetime h2').html(html);
      }
  };

  var reservationPath = function(path){
    var fullpath = '/customer/index/' + path;
    if(isWidget){
      if(path.match('\\?')){
        fullpath += '&is-widget=true';
      }
      else {
        fullpath += "?is-widget=true";
      }
    }

    return fullpath;
  };


  var showRefreshedDatePicker = function(){
    $('#jq_datePicker').datepicker("refresh");
    if(numWholeDays > 1) {
      bindMultiDayHover();
      multiDayCalendarSelectedDays();
    }
    $('img#clockSpinner').hide();
    $('#jq_datePicker').show();
  };

  // function expects an array of dates in YYYY-MM-DD format
  function updateDatesList (selectedDatesPrivate){ 
      if (!selectedDatesPrivate) {
          var selectedDatesPrivate = selectedDates; // use a default variable assignment so we can re-use this code.
      }
      var dateText = '';
	  
      for (var i in selectedDatesPrivate){
          selectedArray = selectedDatesPrivate[i].split('-');
          var date = new Date(selectedArray[0], selectedArray[1]-1, selectedArray[2]); // supplying only the date does 00:00:00 UTC, which is the day before in many time zones
		  var thisDate = date.format('M jS, Y');
		  log('unformatted: '+dateText);
          dateText += getShortFormattedDate(providerLocale, thisDate);
		  //log('formatted '+date);
          if(numWholeDays > 1) {
            var startTS = date.getTime() / 1000; //convert to unix timestamp
            var endDate = new Date();
            endDate.setTime((startTS + ((numWholeDays - 1) * 86400)) * 1000);
            dateText += " through " + endDate.format('M jS, Y');
          }
          dateText += "<br>";
      }
    $('#datesList').html('<strong>' + selectionMessage + ':</strong><br><em>' + dateText + '</em>');
  }

  function addDate(date, timestamp) {
    if (selectedDates.length >= maximumDays) {
      if (maximumDays == 1) {
        // TODO: reselect this for them
        alert('You may only select 1 date.');
      } else {
        alert('You may only select ' + maximumDays + ' dates.');
      }
    } else {
      //stick the selected date into a hidden form field
      $('#availTimeslots').append ('<input type="hidden" name="numDurations[]" id="numDurations-'+timestamp+'" value="'+timestamp+'">');

      if ($.inArray(date, selectedDates) < 0) {
        selectedDates.push(date);
        selectedDates.sort();
		//log('selected'+selectedDates);
        if(numWholeDays > 1) {
          //Populate the unselectable days array.
          var addDates = getDatePlusNMinusN(date, numWholeDays);
          $.each(addDates, function(index, value) {
            unSelectableDates.push(value);
          });
          unSelectableDates.sort();
        }
        updateDatesList();
      }
      //log(unSelectableDates);
    }
  }

  // Given a start date and an integer, returns an array of dates formatted Y-m-d
  var getDatePlusN = function(date, plusDays) {
    var YMDarray = date.split('-');

    // Create a date object, use Date(y,m,d) to make IE happy
    var beginningDate = new Date(YMDarray[0], YMDarray[1] - 1 , YMDarray[2]);
    var beginningTS = beginningDate.getTime() / 1000;
    var returnDates = new Array();
    for(var ts = beginningTS; ts < beginningTS + (plusDays * 86400); ts += 86400) {
      //Convert timestamp back to a date object
      var tempDate = new Date();
      tempDate.setTime(ts * 1000);
      var formattedDate = tempDate.format('Y-m-d');
      returnDates.push(formattedDate);
    }
    return returnDates;
  }

  // Given a start date and an integer, returns an array of dates formatted Y-m-d, N plus the date, N minus the date, not the date itself.
  var getDatePlusNMinusN = function(date, plusDays) {
    // Get date plus N
    var returnDates = getDatePlusN(date, plusDays);

    // Take out the start date.
    var indexToRemove = $.inArray(date, returnDates);
    returnDates.splice(indexToRemove, 1);

    // Now get the previous N dates
    var YMDarray = date.split('-');
    // Create a date object, use Date(y,m,d) to make IE happy
    var beginningDate = new Date(YMDarray[0], YMDarray[1] - 1, YMDarray[2]);
    var beginningTS = beginningDate.getTime() / 1000;
    for(var ts = beginningTS - 86400; ts > beginningTS - (plusDays * 86400); ts -= 86400) {
      //Convert timestamp back to a date object
      var tempDate = new Date();
      tempDate.setTime(ts * 1000);
      var formattedDate = tempDate.format('Y-m-d');
      returnDates.push(formattedDate);
    }
    returnDates.sort();
    return returnDates;
  }


  function removeDate(index) { 

    if(numWholeDays > 1) {
      //Remove from the unselectable days array.
      var removeDates = getDatePlusNMinusN(selectedDates[index], numWholeDays);
      log(removeDates);
      $.each(removeDates, function(index, value) {
        var removeIndex = $.inArray(value, unSelectableDates);
        if(removeIndex >= 0) {
          unSelectableDates.splice(removeIndex, 1);
        }
      });
      unSelectableDates.sort();
    }
    
    selectedDates.splice(index, 1); 

    updateDatesList ();
  }

  function addOrRemoveDate(date) {
    var index = $.inArray(date, selectedDates);
    dateArray = date.split('-');
    // Get the right timestamp for the beginning of this date.  Get the UTC 0 timestamp then offset by server time.
    dateObject = new Date(Date.UTC(dateArray[0], dateArray[1] - 1, dateArray[2], 0, 0, 0));
    var timestamp = (dateObject.getTime() / 1000) - serverOffset;

      if (index >= 0) {
        removeDate(index);
        //remove the hidden form field for that date
        $('#numDurations-'+timestamp).remove();
      } else {
        addDate(date, timestamp);
      }
  }

  // Shows the correct selected days on the calendar for multiday reservation types.
  var multiDayCalendarSelectedDays = function() {
    // Get current month & year off datepicker month/year select.  Not great, hopefully there is a property in datepicker for this in the future.
    var currentMonth = parseInt($('.ui-datepicker-month').val()) + 1;
    currentMonth = currentMonth.toString();
    if (currentMonth.length == 1) {
      // Pad a 0
      currentMonth = '0' + currentMonth;
    }
    //log("current month is " + currentMonth);
    var currentYear = $('.ui-datepicker-year').val();
    $.each(selectedDates, function(index, cDate) {
      daysToModify = getDatePlusN(cDate, numWholeDays)
      // Loop through each date
      $.each(daysToModify, function(index, mDate) {
          
        // split it down to day/month year
        checkDate = mDate.split('-');

        // Make sure these are on the current month
        if(checkDate[0] == currentYear && checkDate[1] == currentMonth) {

          // Update style for this day
          $('td > .ui-state-default').filter( function() {
            return parseInt($(this).html()) == checkDate[2];
          }).addClass('ui-state-active').parent().addClass('st-state-active-override-opacity');
        }
      });

    });


  }

  function padNumber(number) {
    var ret = new String(number);
      if (ret.length == 1) {
        ret = "0" + ret;
      }
      return ret;
  }

  function fullDayBeforeShowDay(date) {
    //do this if it IS a full day reservation type
    var dateString = date.getFullYear() + "-" + padNumber(date.getMonth()+1) + "-" + padNumber(date.getDate());
    if ($.inArray(dateString, selectedDates) >= 0) {
      return [true, 'st-state-active'];
    } else if ($.inArray(dateString, unSelectableDates) >= 0) {
      return [false, 'disabled'];
    } else if ($.inArray(dateString, availabilityDates) >= 0) {
      return [true, 'enabled'];
    }
    
    return [false, 'disabled'];
  };

  function partialDayBeforeShow(date){
    //do all this if it's NOT a full day reservation type
    if (availabilityDates.length!=0) {
      if (availabilityDates==undefined) {
        return [false, 'disabled'];
      } else {
        if (!$.isArray(availabilityDates)) {
          return [false, 'disabled'];
        } else {
          var checkDate = $.datepicker.formatDate('yy-mm-dd', new Date(date));
          if (availabilityDates[0] && checkDate == availabilityDates[0]){
          //if (checkDate == $("#startDate").val()) {
          //  log("date checked " + checkDate + " matched date selected " + $("#startDate").val());
          //  return[true, 'st-state-active'];
            log("date checked " + checkDate + " matched date selected " + availabilityDates[0]);
            return[true, 'st-state-active'];
          } else if ($.inArray(checkDate, availabilityDates) >= 0) { 
            return [true, 'enabled'];
          } else {
            return [false, 'disabled'];
          }
        }
      }
    } else {
      return [false, 'disabled'];
    }
  }

  function getTimeslots(resourceIds) {
    $.ajax({
      url:     reservationPath('timeslots'),
      type:     'GET',
      dataType: 'html',
      data: {
        'resources[]': resourceIds,
        'startDate': $('#startDate').val(),
        'reservationTypeId': reservationType
      },
      success: function(data) {
        showAvailableTimeslots(data);
      }
    });
  };

  function showStepAfterDateStep(that){
    //bad naming.. that can be the xml response or it can be the input button clicked
    if ($('#availResources a').length < 1) {
      if ($('a.selected.allDay').length > 0) {
        if(!isWidget || $(that).hasClass('jq_nextStep')){
          showCustomerForm();              
        } else {
          includeContinueButtonForMultiDay();
        }
        // Because datepicker refreshes at inconvenient moments, do this here:
        if(numWholeDays > 1) {
          bindMultiDayHover();
          multiDayCalendarSelectedDays();
        }
      } else {
        getTimeslots($('#availResources input:checked').map(function() { return $(this).val(); }).get());
      }
    } else {
      showAvailableResources();
    }
  }

  function getResources(date) {
    //alert(reservationPath('resources'));
    $.ajax({
      url:     reservationPath('resources'),
      dataType: 'html',
      type:     'GET',
      data: {
        reservationTypeId: reservationType,
        startDate: date
      },
      success: function(returnData) {
        //alert(returnData);
        $('#availResources').html(returnData);
        insertStepNumbers();
        showStepAfterDateStep($(this))
      },
      error: function(xhr, stat, e){
        alert(stat + e);
      }
    });
  }

  function STbeforeShowDay(date){
    if ($('a.selected.allDay').length) { 
      return fullDayBeforeShowDay(date);
    } else { 
      return partialDayBeforeShow(date);
    }
  };

  function STonSelect(date) {
    $('#availResources').hide();
    $('#availTimeslots').hide();
    $('#customerForm').hide();
    $('#startDate').val($("#jq_datePicker").val());
    if ($('a.selected.allDay').length) {
      $('#allDay').val(1);
      addOrRemoveDate(date);
      showRefreshedDatePicker();
      getResources($(this).val());
    } else {
     getResources($(this).val());
      var dateArray = new Array(date);
      updateDatesList(dateArray);
    }
  }

  var autoClickDay = function(withPopulateData){
    if(withPopulateData){
      $('.ui-datepicker-calendar a').each(function(index, element){
        if($(element).html() == populateData.day){
          $(element).click();
          //clear populate data so it is not used again if other days are selected
          populateData.year = null;
          populateData.month = null;
          populateData.day = null;
        }
      });
    }   
  };

  var getDateFromString = function(stdt){
    var input = stdt.split('-');
    var yr = input[0].substring(2);
    var mn = input[1];
    var dy = input[2];
    var newdt = [yr, mn, dy];
    return newdt.join('-');

    
  }

  var ajaxUpdateCalendarSuccess = function(responseData, withPopulateData){
    availabilityDates = responseData;
    if(withPopulateData){
      var defaultDate = jq_populateDate;
    } else {
      var defaultDate = null;
      if(currentDate){
        defaultDate = getDateFromString(currentDate);
      }
      if(responseData){
          if(responseData.length == 0){
            defaultDate = getDateFromString(currentDate);
          } else {
            defaultDate = getDateFromString(responseData[0]);
          }
      }
    }

    $('#jq_datePicker').datepicker({
      defaultDate: defaultDate,
      dateFormat: 'yy-mm-dd',
      minDate:  '-0d',
      maxDate:  '+2y',
      changeMonth: 'true',
      changeYear: 'true',
      beforeShowDay: STbeforeShowDay,
      onSelect: STonSelect,
      onChangeMonthYear: function(newYear, newMonth) {
        //run it again when changing to a different month
        $("#startDate").val(newYear + "-" + newMonth + "-" + "01");
        $('#jq_datePicker').hide();
        $("img#clockSpinner").show();
        populateData = null;
        ajaxUpdateCalendar();
      }
    });


    showRefreshedDatePicker();

    insertStepNumbers();
    autoClickDay(withPopulateData);   
  };

  var ajaxUpdateCalendar = function(withPopulateData){
    withPopulateData = withPopulateData || false;

    if(withPopulateData && populateData && populateData.month && populateData.year){
      $('#startDate').val(jq_populateDateString);
    }  else {
      withPopulateData = false;
    }
    //if this reservation type has a fixed date range that starts in the past, just use today as the startdate
	var today = $.datepicker.formatDate('yy-mm-dd', new Date());
    var startDate         = $('#startDate').val();
	  if (startDate < today){
		  startDate = today;
	  }
    //alert(startDate);
    log('today:' + today);
	  log('startDate:' + startDate);
    log('withPopulate:' + withPopulateData);
    log('reservationType:' + reservationType);
    log('url:' + reservationPath('ajax-timeslots'));

    $.ajax({
      url: reservationPath('ajax-timeslots'),
      data: {
      'startDate': startDate,
      'reservationTypeId': reservationType
      },
      dataType: 'json',
      success: function(responseData) {
        //alert("SUCCESS" + JSON.stringify(responseData));
        console.log("SUCCESS" + JSON.stringify(responseData));
        if (!responseData) {
          console.log('calerror');
          showCalendarError();
        }
        log(responseData);

        //AJC: if reservation lasts only 1 day then skip calendar, auto display chosen date, and go straight to selecting time slot
        //if (responseData.length < 2) {  
          //alert("gooooooood");
        //}
        //else {
        ajaxUpdateCalendarSuccess(responseData, withPopulateData);
        //}
        },
      error: function(responseData) {
        //alert("ERROR" + JSON.stringify(responseData));
        log(responseData);
        showCalendarError();
      }
    });
  };

  var bindMultiDayHover = function() {
    $('#jq_datePicker td.enabled > a').hover(
      function() {
        // Get date hovered over and parse as an int
        hoveredDate = parseInt($(this).html());

        // Change style for the next X days
        for(var i = hoveredDate; i <= hoveredDate+numWholeDays-1; i++) {
          $('td > .ui-state-default').filter( function() {
            return parseInt($(this).html()) == i;
          }).addClass('ui-state-hover').parent().addClass('datepicker-remove-opacity');
        }
      },
      function() {
        $('.ui-state-hover').removeClass('ui-state-hover').parent().removeClass('datepicker-remove-opacity');
      }
    );
  }

  var showCalendarError = function() {
    // The datepicker calendar failed to load for some reason.  Show an error and offer the ability to try again.
    $('img#clockSpinner').hide();
    $('#datepicker_error').show();

    $('#datepicker_error a').click(function() {
      $('#datepicker_error').hide();
      // Reclick the already-clicked reservation type.
      $('#availReservationTypes a.selected').click();
      return false;
    });
  }

  var makeSeatsSelectArray = function(num) {
    var obj = {}
    for(ii = 1; ii < num + 1; ii++){
      obj[ii.toString()] = ii.toString();
    }
    return obj;
  }

  var setSeatsArrayValues = function(num) {
    $('#numSeats').find('option').remove();
    $.each(makeSeatsSelectArray(num), function(key, value){
      $('#numSeats').append($("<option></option>").attr("value",key).text(value));
    });
    $('#numSeats').val("1");
  }

  var bindReservationTypeOnClicks = function(){
    $('#availReservationTypes ul a').live('click', function() {
      hideFromThisStepOn('availReservationTypes');
      $('#reservationTypeSubcategories').removeClass('step');//necessary to ensure display if user clicks after we're in the date step
      $('#datepicker_error').hide();
      reservationType = $(this).attr('id');
      reservationTypeMaxSeats = parseInt($(this).attr('seatsperbooking'));
      setSeatsArrayValues(reservationTypeMaxSeats);
      if(reservationTypeMaxSeats <= 1 || !reservationTypeMaxSeats){
        $('#numSeatsElement').hide();
      }  else {
        $('#numSeatsElement').show();
      }
      $('#reservationTypeId').val(reservationType);
      removeAndAddSelectedClass("#availReservationTypes a", $(this));

      //JWH: Added 2011-11-10
      if(hasSubcategories){ 
        showSubcategoriesStep();
      } else {
        showDateStep();
      }

      return false;
    });
  };

  //JWH: Added 2011-11-10, infrastructure for subcats
  var bindReservationSubcategoryOnClicks = function(){
    $('#reservationTypeSubcategories ul a').live('click', function() {
      $('#datepicker_error').hide();
      reservationSubcategory = $(this).attr('id');
      $('#reservationSubcategoryId').val(reservationSubcategory);
      removeAndAddSelectedClass("#reservationTypeSubcategories a", $(this));

      showDateStep();

      return false;
    });
  };

  var hideReservationTypes = function(){
   // hide all individual resTypes, we'll show them individually based on selected tag.
    $('#availReservationTypes li').each( function() {
      $(this).hide();

    });
  };

  //JWH:  Added 2011-11-11
  var hideReservationTypeSubcategories = function(){
   // hide all individual resTypes, we'll show them individually based on selected tag.
    $('#reservationTypeSubcategories li').each( function() {
      $(this).hide();

    });
  };


  var showReservationTypesByCurrentTag = function(){
    //individually shows reservation types with specified tag
    $('.tag-'+tag).each( function(index, element) {
      $(this).parent().show();
    });
  }

  //JWH: Added 2011-11-09 as first step toward adding subcategories
  var showSubcategoriesByCurrentReservationType = function(){
    //individually shows reservation types with specified tag
    $('.subcat-'+reservationType).each( function(index, element) {
      $(this).parent().show();
    });
  }


  var bindTagLinkOnClicks = function(){
    $('#providerTags ul a, #Show_All').live('click', function(){
      tag = $(this).attr('id');

      removeAndAddSelectedClass('#providerTags a', $(this));

      showAvailReservationTypes();

      return false;
    });
  }

  var checkResourceRequirements = function(resourceLink){
      var id = resourceLink.attr('class').replace(/-/g, '').replace('selected', '');
      var input = $('#resources-'+id);
      var crewsize = resourceLink.parents(".group").data("crewsize");
      var numSelected = resourceLink.closest('div.group').find('.selected').length;
    
      if ($('.'+id).hasClass('selected')) {
        alert('You have already selected this resource in a different group');
        return false;
      }

      if (resourceLink.hasClass('selected')) {
        resourceLink.removeClass('selected');
        $(input).removeAttr('checked');
        $('#availTimeslots').hide();
        return false;
      } else {
        if (crewsize == 1) {
          resourceLink.closest('div.group').find('a').each(function(i, el) {
            var resource = '#resources-' + $(el).attr('class').replace(/-/g, '').replace('selected', '');
            $(el).removeClass('selected');
            $(resource).removeAttr('checked');
          });
        } else if (crewsize > 1 && numSelected == crewsize) {
          alert("You have already selected " + numSelected + " from this group, please deselect one to select a new resource.");
          return false;
        }

        resourceLink.addClass('selected');
        $(input).attr('checked', true);
      }
      return true;
  }

  function checkCrewsizeAllGroups() {
    satisfied = true;
    $("#availResources .group").each( function(index, element) {
      //autoselected groups are autoselected and don't need this validation
      //weeding them out by checking presence of h3      
      if($(element).find('h3').length > 0){  
        crewsize = $(element).data("crewsize");  
        numSelected = $(element).find('.selected').length;

        if (numSelected < crewsize) {
          // they haven't selected enough resources in this option group yet.
          satisfied = false;
        }
      } else {
        satisfied = false;
      }
    });
    return satisfied;
  }

  var bindResourcesOnClicks = function(){
    $('#availResources ul a, #availResources .jq_nextStep').live('click', function() {
      if (
        //weird to call hasclass twice, but i need checkResourceRequirements not to run if the class isn't jq_nextStep
        (!$(this).hasClass('jq_nextStep') && checkResourceRequirements($(this)) && checkCrewsizeAllGroups()) ||
        (isWidget && $(this).hasClass('jq_nextStep'))
      ) {
        if(!isWidget){
          $('#availTimeslots').hide();
          if ($('a.selected.allDay').length) {
            showCustomerForm();
          } else {
            getTimeslots($('#availResources input:checked').map(function() { return $(this).val(); }).get());
          }
          insertStepNumbers();
        } else {
          includeContinueButtonForResources();
        }
      } else {
        $('#availResources .jq_nextStep').hide();
      }
      if($(this).hasClass('jq_nextStep')){
        if ($('a.selected.allDay').length) {
          showCustomerForm();
        } else {
          getTimeslots($('#availResources input:checked').map(function() { return $(this).val(); }).get());
        }
      }

      return false;
    });
  };

  var bindNextStepInputs = function(){
    $('#startDatetime input.jq_nextStep').live('click', function(){
      showStepAfterDateStep($(this));
    })
  };


  $.toggleChecked = function(id) {
    if ($(id).attr('checked')) {
      $(id).removeAttr('checked');
    } else {
      $(id).attr('checked', true);
    }
  }

  $.fn.toggleSelected = function(inputId) {
    if ($(this).hasClass('selected')) {
      $(this).removeClass('selected');
    } else {
      $(this).addClass('selected');
    }

    $.toggleChecked(inputId);
  }

//alert if no date is selected // if id="availTimeslots" has no input
  var noneSelected = function(event) {
      //if (!$.find('#availTimeslots a.selected').length && !$.find('#numDurations')) {
      //if (((!$('#availTimeslots').find('[id~="numDurations"]')).length) == 0) {
      //if ($("#availTimeslots").exists();) {
      //if (timestamp === null) {
      //if ('[id="availTimeslots"]' === null) {
      //if (typeof '#datesList' == 'undefined') {  //&& image_array.length > 0) {
      //if ('#datesList' == undefined || '#datesList' == null || '#datesList'.length == 0){
      //if($('.step').val()) {
      if (!$('#availTimeslots').children().length > 0 ) {  //AJC
      //if ($('step').length == 0) {
       // console.log('numDurations Found');
        event.preventDefault();
        alert('Please select a timeslot to continue.');
        return false;
    }
  }

  $('#submit').live('click', noneSelected);

  var handleTimeSlotsClass = function(link) {
	  //if there's only one timeslot, and it's already selected, you can't unselect it
	if ($.find('#availTimeslots a').length == 1 && $.find('#availTimeslots a.selected').length == 1) {
	 	alert('This is the only timeslot available on this date. It\'s automatically selected for you. Try another date if you don\'t want this time slot.');
		return false;
		
	// If durationSeconds is part of the link's data, this is likely a carry-over slot and we don't want this slot combined with another timeslot yet.
    } else if (maximumTimeslots() == 1 || data.allowMultiSelect == false || link.data('durationSeconds') > 0) {
      $("#availTimeslots a.selected").removeClass("selected");
      $("#availTimeslots input:checkbox").removeAttr("checked");
      link.toggleSelected('#numDurations-' + link.attr('id'));
      return true;
    } else {
		
      link.toggleSelected('#numDurations-' + link.attr('id'));
      if ($.find('#availTimeslots a.selected').length > maximumTimeslots()) {
        link.toggleSelected('#numDurations-' + link.attr('id'));
        alert('You may only select ' + maximumTimeslots() + ' timeslots.');
        return false;
      } else if(isRescheduling() && $.find('#availTimeslots a.selected').length < slotsToReschedule && $("#customerForm:visible").length > 0) {
        $('#customerForm').hide();
      }

      return true;
    }      
  };

  var maximumTimeslots = function(){
    if(isRescheduling()){
      return slotsToReschedule;
    } else {
      return data.maximumDurations;
    }
  }

  var populateCustomerForm = function(){
    if(populateData){
      $('#firstName').val(populateData.firstName);
      $('#lastName').val(populateData.lastName);
      $('#numSeats').val(1);
      $('#phone').val(populateData.phone);
      $('#email').val(populateData.email);
      $('#comments').val(populateData.comments);
      $('#address1').val(populateData.address1);
      $('#address2').val(populateData.address2);
      $('#city').val(populateData.city);
      $('#state').val(populateData.state);
      $('#zip').val(populateData.zip);
      if(populateData.rememberMe == 1) {
        $('#rememberMe').attr("checked", true);
      }
      populateData = null;
    }
  };

  var bindTimeSlotClicks = function(){
    $('#availTimeslots ul a, #availTimeslots .jq_nextStep').live('click', function() {
log('click');   
      if(!handleTimeSlotsClass($(this))){
        log('a')
        return false;
      }

      if (0 == data.allowNonConcurrency) {

        var nonConcurrent = false;
        var timeslots    = new Array();
        $.each($.find('#availTimeslots .hideCheckboxes input[type=checkbox]:checked'), function(i, item) {
          timeslots.push($(item).val());
        });
        if (timeslots.length > 1) {

          var result = $.evalJSON($.ajax({
            url: reservation_path('check-concurrency'),
            data: { duration: data.durationMinimum, 'times[]': timeslots },
            dataType: 'json',
            type: 'GET',
            async: false
          }).responseText);
          if (result.message == 'notValid') {
            $(this).toggleSelected('#numDurations-' + $(this).attr('id'));
            alert('You must select adjacent time slots.');
            return false;
          }
        }
      }

      var lunchSeconds = 0;
      $("#availTimeslots a.selected").each(function () {
        lunchSeconds += $(this).data("lunchSeconds");
        if($(this).data("durationSeconds") > 0) {
          $("#durationSeconds").val($(this).data("durationSeconds"));
        }
      });
      $("#lunchSeconds").val(lunchSeconds);

      if (($.find('#availTimeslots a.selected').length > 0 && !(data.allowMultiSelect == true && isWidget)) ||
      $(this).hasClass('jq_nextStep') && $.find('#availTimeslots a.selected').length > 0) {
        if(
          (isRescheduling() && $.find('#availTimeslots a.selected').length >= slotsToReschedule) ||
          !isRescheduling()
        ){
          showCustomerForm();          
        }
      } else if($(this).hasClass('jq_nextStep') && $.find('#availTimeslots a.selected').length < 1){
        alert('Select a timeslot to continue.');
      }


      return false;
    });
  }

  var bindStepBackLinks = function(){
    $('a.jq_stepBack').live('click', function(){
      var lcurrentStep = $(this).parents('div.step');
      lcurrentStep.removeClass('used');

      var prevStep = lcurrentStep.prevAll('div.step.used:first');
      currentStep = stepNumberFromId(prevStep.attr('id'));

      lcurrentStep.hide();
      if(isWidget){
        prevStep.show('slide');   
      } else {
        prevStep.show();
      }
      return false;
    });
  };

  var bindOnClicks = function(){
    bindTagLinkOnClicks();
    bindReservationTypeOnClicks();
    bindReservationSubcategoryOnClicks();//JWH: Added 2011-11-10
    bindResourcesOnClicks();
    bindTimeSlotClicks();
    bindStepBackLinks();
    bindNextStepInputs();
  };
    
  var handleAndDisplayErrors = function(){
    var errorCount = 0;
    $.each(options.errors, function() {
      errorCount++;
    });
    if(errorCount > 0){
      $('#customerForm').show();
      $('#startDatetime h2').show();
      $.scrollTo('.errors:first');
    }
  };

  var populateForms = function(){
    if(options.populateData) {
      populateData = options.populateData;
      jq_populateDate = new Date(populateData.year, parseInt(populateData.month, 10) - 1, 1);
      jq_populateDateString = populateData.year + '-' + populateData.month + '-' + '1';
      $("a#Show_All").click();
      $('#' + populateData['reservationTypeId']).click();
      if(isRescheduling()){
        $("#providerTags").hide();
        $('#availReservationTypes').hide();
      }
    }
  };

  var isRescheduling = function(){
    return slotsToReschedule > 0;
  }

  var initialReChunk = function(){
    if(hasTags){
      reChunk('providerTags');
    } else {
      reChunk('availReservationTypes');
    }    
  }

  var handleDirectBooking = function(){
    passedSubcatStep = false;
    if(directBooking){
      reservationType = directBooking;
      numApplicableSubcats = $('.subcat-'+directBooking).length;
      ajaxUpdateCalendar(typeof populateData != 'undefined' ? true : false);
      hasTags = false;
    }
  }

  var initialize = function(){
    hasCustomQuestions  = options.hasCustomQuestions;//JWH:  Added 2012-06-12
    hasTags             = options.hasTags;
    hasSubcategories    = options.hasSubcategories;//JWH:  Added 2011-11-10
    isWidget            = options.isWidget;
    slotsToReschedule   = options.slotsToReschedule;
    serverOffset        = options.serverOffset;
    directBooking       = options.directBooking;
    regional            = options.regional;
    selectionMessage    = options.selectionMessage;
    providerLocale      = options.providerLocale;
    //handleDirectBooking();
    initialReChunk();
    initialHide();
    bindOnClicks();
    insertStepNumbers();
    //handleAndDisplayErrors();
    populateForms();
    $.datepicker.regional[regional];
    if(directBooking){
      reservationType = directBooking;
      //$('#availReservationTypes').hide();
      $('#availReservationTypes').show();
      $('#providerTags').hide(); //Fix for firefox
      insertRecalculatedStepNumbers();
      clearAdditionalReservationTypes();
      $('#' + directBooking).click()
      //$('#' + directBooking).trigger('click');
      numApplicableSubcats = $('.subcat-'+directBooking).length;
      if(numApplicableSubcats > 0){
        showSubcategoriesStep();
      } else {
        showDateStep();
      }
    }
  };
  
  initialize();

  return {
    tag: tag,
    reservationType: reservationType
  }
}
