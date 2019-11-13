***********************************************************************
**
** Handler class for managing travels
**
***********************************************************************
*CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
*
*  PRIVATE SECTION.
*
*    TYPES:
*         tt_travel_update TYPE TABLE FOR UPDATE /dmo/i_travel_u.
*
*    METHODS:
*"! Comment layers:
*"! 1. Update + Delete
*"! 2. Create by Association (CBA) for Bookings
*"! 3. Set status to Booked (set_travel_status)
*"! 4. Lock Travel -> NOT THERE!!!
*"! 5. Read Travel

*      update_travel     FOR MODIFY
*        IMPORTING it_travel_update FOR UPDATE travel

*      ,delete_travel     FOR MODIFY
*        IMPORTING it_travel_delete FOR DELETE travel

*      ,set_travel_status FOR MODIFY
*        IMPORTING it_travel_set_status_booked FOR ACTION travel~set_status_booked
*        RESULT    et_travel_set_status_booked

*      ,cba_booking       FOR MODIFY
*        IMPORTING it_booking_create_ba FOR CREATE travel\_booking,


*      ,_fill_travel_inx
*        IMPORTING is_travel_update     TYPE LINE OF tt_travel_update
*        RETURNING VALUE(rs_travel_inx) TYPE /dmo/if_flight_legacy=>ts_travel_inx.

*ENDCLASS.
*
*
*CLASS lhc_travel IMPLEMENTATION.
*
*
***********************************************************************
**
** Update data of existing travel instances
**
***********************************************************************
*  METHOD update_travel.
*
*    DATA lt_messages    TYPE /dmo/if_flight_legacy=>tt_message.
*    DATA ls_travel      TYPE /dmo/travel.
*    DATA ls_travelx TYPE /dmo/if_flight_legacy=>ts_travel_inx. "refers to x structure (> BAPIs)
*
*    LOOP AT it_travel_update ASSIGNING FIELD-SYMBOL(<fs_travel_update>).
*
*      ls_travel = CORRESPONDING #( <fs_travel_update> MAPPING FROM ENTITY ).
*
*      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
*        EXPORTING
*          is_travel   = CORRESPONDING /dmo/if_flight_legacy=>ts_travel_in( ls_travel )
*          is_travelx  = _fill_travel_inx( <fs_travel_update> )
*        IMPORTING
*          et_messages = lt_messages.
*
*       zmhp_cl_travel_auxiliary_XXX=>handle_travel_messages(
*         EXPORTING
*           iv_cid       = <fs_travel_update>-%cid_ref
*           iv_travel_id = <fs_travel_update>-travelid
*           it_messages  = lt_messages
*         CHANGING
*           failed   = failed-travel
*           reported = reported-travel ).

*
**      /dmo/cl_travel_auxiliary=>handle_travel_messages(
**        EXPORTING
**          iv_cid       = <fs_travel_update>-%cid_ref
**          iv_travel_id = <fs_travel_update>-travelid
**          it_messages  = lt_messages
**        CHANGING
**          failed   = failed-travel
**          reported = reported-travel ).
*
*    ENDLOOP.
*
*  ENDMETHOD.
*
***********************************************************************
** Helper method:
** Indicates the travel fields that have been changed by the client
**
***********************************************************************
*  METHOD _fill_travel_inx.
*
*    CLEAR rs_travel_inx.
*    rs_travel_inx-travel_id = is_travel_update-TravelID.
*
*    rs_travel_inx-agency_id     = xsdbool( is_travel_update-%control-agencyid     = if_abap_behv=>mk-on ).
*    rs_travel_inx-customer_id   = xsdbool( is_travel_update-%control-customerid   = if_abap_behv=>mk-on ).
*    rs_travel_inx-begin_date    = xsdbool( is_travel_update-%control-begindate    = if_abap_behv=>mk-on ).
*    rs_travel_inx-end_date      = xsdbool( is_travel_update-%control-enddate      = if_abap_behv=>mk-on ).
*    rs_travel_inx-booking_fee   = xsdbool( is_travel_update-%control-bookingfee   = if_abap_behv=>mk-on ).
*    rs_travel_inx-total_price   = xsdbool( is_travel_update-%control-totalprice   = if_abap_behv=>mk-on ).
*    rs_travel_inx-currency_code = xsdbool( is_travel_update-%control-currencycode = if_abap_behv=>mk-on ).
*    rs_travel_inx-description   = xsdbool( is_travel_update-%control-memo         = if_abap_behv=>mk-on ).
*    rs_travel_inx-status        = xsdbool( is_travel_update-%control-status       = if_abap_behv=>mk-on ).
*  ENDMETHOD.
*
*
***********************************************************************
**
** Delete of travel instances
**
***********************************************************************
*  METHOD delete_travel.
*
*    DATA lt_messages TYPE /dmo/if_flight_legacy=>tt_message.
*
*    LOOP AT it_travel_delete ASSIGNING FIELD-SYMBOL(<fs_travel_delete>).
*
*      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_DELETE'
*        EXPORTING
*          iv_travel_id = <fs_travel_delete>-travelid
*        IMPORTING
*          et_messages  = lt_messages.
*
*       zmhp_cl_travel_auxiliary_XXX=>handle_travel_messages(
*         EXPORTING
*           iv_cid       = <fs_travel_delete>-%cid_ref
*           iv_travel_id = <fs_travel_delete>-travelid
*           it_messages  = lt_messages
*         CHANGING
*           failed   = failed-travel
*           reported = reported-travel ).
*
**      /dmo/cl_travel_auxiliary=>handle_travel_messages(
**        EXPORTING
**          iv_cid       = <fs_travel_delete>-%cid_ref
**          iv_travel_id = <fs_travel_delete>-travelid
**          it_messages  = lt_messages
**        CHANGING
**          failed       = failed-travel
**          reported     = reported-travel ).
*
*    ENDLOOP.
*
*  ENDMETHOD.
*
*
*
***********************************************************************
**
** Implement travel action(s) (in our case: for setting travel status)
**
***********************************************************************
*
*  METHOD set_travel_status.
*
*    DATA lt_messages TYPE /dmo/if_flight_legacy=>tt_message.
*    DATA ls_travel_out TYPE /dmo/travel.
*
*    CLEAR et_travel_set_status_booked.
*
*    LOOP AT it_travel_set_status_booked ASSIGNING FIELD-SYMBOL(<fs_travel_set_status_booked>).
*
*      DATA(lv_travelid) = <fs_travel_set_status_booked>-travelid.
*
*      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_SET_BOOKING'
*        EXPORTING
*          iv_travel_id = lv_travelid
*        IMPORTING
*          et_messages  = lt_messages.
*
*      IF lt_messages IS INITIAL.
*        APPEND VALUE #( travelid        = lv_travelid
*                        %param-travelid = lv_travelid )
*               TO et_travel_set_status_booked.
*      ELSE.
*
*       zmhp_cl_travel_auxiliary_XXX=>handle_travel_messages(
*          EXPORTING
*            iv_cid       = <fs_travel_set_status_booked>-%cid_ref
*            iv_travel_id = lv_travelid
*            it_messages  = lt_messages
*          CHANGING
*            failed   = failed-travel
*            reported = reported-travel ).
*
*
**        /dmo/cl_travel_auxiliary=>handle_travel_messages(
**          EXPORTING
**            iv_cid       = <fs_travel_set_status_booked>-%cid_ref
**            iv_travel_id = lv_travelid
**            it_messages  = lt_messages
**          CHANGING
**            failed       = failed-travel
**            reported     = reported-travel ).
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDMETHOD.
*
*
***********************************************************************
**
** Create associated booking instances
**
***********************************************************************
*  METHOD cba_booking.
*    DATA lt_messages        TYPE /dmo/if_flight_legacy=>tt_message.
*    DATA lt_booking_old     TYPE /dmo/if_flight_legacy=>tt_booking.
*    DATA ls_booking         TYPE /dmo/booking.
*    DATA lv_last_booking_id TYPE /dmo/booking_id VALUE '0'.
*
*    LOOP AT it_booking_create_ba ASSIGNING FIELD-SYMBOL(<fs_booking_create_ba>).
*
*      DATA(lv_travelid) = <fs_booking_create_ba>-travelid.
*
*      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
*        EXPORTING
*          iv_travel_id = lv_travelid
*        IMPORTING
*          et_booking   = lt_booking_old
*          et_messages  = lt_messages.
*
*      IF lt_messages IS INITIAL.
*
*        IF lt_booking_old IS NOT INITIAL.
*          lv_last_booking_id = lt_booking_old[ lines( lt_booking_old ) ]-booking_id.
*        ENDIF.
*
*        LOOP AT <fs_booking_create_ba>-%target ASSIGNING FIELD-SYMBOL(<fs_booking_create>).
*          ls_booking = CORRESPONDING #( <fs_booking_create> MAPPING FROM ENTITY USING CONTROL ) .
*
*          lv_last_booking_id += 1.
*          ls_booking-booking_id = lv_last_booking_id.
*
*          CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
*            EXPORTING
*              is_travel   = VALUE /dmo/if_flight_legacy=>ts_travel_in( travel_id = lv_travelid )
*              is_travelx  = VALUE /dmo/if_flight_legacy=>ts_travel_inx( travel_id = lv_travelid )
*              it_booking  = VALUE /dmo/if_flight_legacy=>tt_booking_in( ( CORRESPONDING #( ls_booking ) ) )
*              it_bookingx = VALUE /dmo/if_flight_legacy=>tt_booking_inx( ( booking_id  = ls_booking-booking_id
*                                                                           action_code = /dmo/if_flight_legacy=>action_code-create ) )
*            IMPORTING
*              et_messages = lt_messages.
*
*          IF lt_messages IS INITIAL.
*            INSERT VALUE #( %cid = <fs_booking_create>-%cid  travelid = lv_travelid  bookingid = ls_booking-booking_id )
*                INTO TABLE mapped-booking.
*          ELSE.
*
*            LOOP AT lt_messages INTO DATA(ls_message) WHERE msgty = 'E' OR msgty = 'A'.
*              INSERT VALUE #( %cid = <fs_booking_create>-%cid ) INTO TABLE failed-booking.
*
*              INSERT VALUE #(
*                    %cid     = <fs_booking_create>-%cid
*                    travelid = <fs_booking_create>-TravelID
*                    %msg     = new_message(
*                              id       = ls_message-msgid
*                              number   = ls_message-msgno
*                              severity = if_abap_behv_message=>severity-error
*                              v1       = ls_message-msgv1
*                              v2       = ls_message-msgv2
*                              v3       = ls_message-msgv3
*                              v4       = ls_message-msgv4
*                        )
*               )
*              INTO TABLE reported-booking.
*
*            ENDLOOP.
*
*          ENDIF.
*
*        ENDLOOP.
*
*      ELSE.
*
*        zmhp_cl_travel_auxiliary_XXX=>handle_travel_messages(
*          EXPORTING
*            iv_cid       = <fs_booking_create_ba>-%cid_ref
*            iv_travel_id = lv_travelid
*            it_messages  = lt_messages
*          CHANGING
*            failed   = failed-travel
*            reported = reported-travel ).
*
**        /dmo/cl_travel_auxiliary=>handle_travel_messages(
**         EXPORTING
**           iv_cid       = <fs_booking_create_ba>-%cid_ref
**           iv_travel_id = lv_travelid
**           it_messages  = lt_messages
**         CHANGING
**           failed       = failed-travel
**           reported     = reported-travel ).
*
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDMETHOD.
*
*
*ENDCLASS.
*
*
***********************************************************************
**
** Saver class implements the save sequence for data persistence
**
***********************************************************************
*CLASS lsc_saver DEFINITION INHERITING FROM cl_abap_behavior_saver.
*  PROTECTED SECTION.
*    METHODS finalize          REDEFINITION.
*    METHODS check_before_save REDEFINITION.
*    METHODS save              REDEFINITION.
*    METHODS cleanup           REDEFINITION.
*ENDCLASS.
*
*CLASS lsc_saver IMPLEMENTATION.
*  METHOD finalize.
*  ENDMETHOD.
*
*  METHOD check_before_save.
*  ENDMETHOD.
*
*  METHOD save.
*    CALL FUNCTION '/DMO/FLIGHT_TRAVEL_SAVE'.
*  ENDMETHOD.
*
*  METHOD cleanup.
*    CALL FUNCTION '/DMO/FLIGHT_TRAVEL_INITIALIZE'.
*  ENDMETHOD.
*ENDCLASS.
