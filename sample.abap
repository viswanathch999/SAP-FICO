*----------------------------------------------------------------------*
*                                                                      *
*  RFITEMGL      Line ITEMs General Ledger Accounts                    *
*                     ----  -       -                                  *
*                                                                      *
*                This report selects data to be displayed by ALV       *
*----------------------------------------------------------------------*

REPORT rfitemgl MESSAGE-ID msitem NO STANDARD PAGE HEADING.

*... general data definitions:
INCLUDE rfitem_def.
*... icons and symbols:
INCLUDE <list>.

TABLES: skat, skb1, bsis, admi_files.
TABLES: t001.

DATA: it_h_t001      TYPE tpit_t_vt001 WITH HEADER LINE,
      it_h_skat      TYPE tpit_t_vskat WITH HEADER LINE,
      it_h_skb1      TYPE tpit_t_vskb1 WITH HEADER LINE.

* if report is called from balcance display.
DATA:  gt_saknr TYPE RANGE OF ska1-saknr,
       gd_saknr LIKE LINE OF gt_saknr.
DATA:  gt_bukrs TYPE RANGE OF skb1-bukrs,
       gd_bukrs LIKE LINE OF gt_bukrs.
DATA:  gt_gjahr TYPE RANGE OF bsis-gjahr,
       gd_gjahr LIKE LINE OF gt_gjahr.
DATA:  gt_monat TYPE RANGE OF bsis-monat,
       gd_monat LIKE LINE OF gt_monat.
DATA:  gt_yrper TYPE RANGE OF rfposx-jamon,
       gd_yrper LIKE LINE OF gt_yrper.
TYPES: BEGIN OF gty_acccoco,
         saknr LIKE ska1-saknr,
         bukrs LIKE skb1-bukrs,
         gvtyp LIKE ska1-gvtyp,
       END OF gty_acccoco.
DATA:  gt_accoco TYPE STANDARD TABLE OF gty_acccoco,
       gt_accoco_bs TYPE STANDARD TABLE OF gty_acccoco,
       gd_accoco TYPE gty_acccoco.


*... selection screen layout:

* button worklists on/off:
SELECTION-SCREEN FUNCTION KEY 1.
* selections for processing of worklists:
* selections for processing of worklists:
SELECTION-SCREEN BEGIN OF BLOCK glaccount WITH FRAME TITLE text-w02.
PARAMETERS: pa_wlsak LIKE rf42b-idnts MODIF ID wkl.
SELECT-OPTIONS: so_wlsak FOR skb1-saknr MODIF ID wkl NO DATABASE
SELECTION.
SELECTION-SCREEN END OF BLOCK glaccount.
SELECTION-SCREEN BEGIN OF BLOCK company WITH FRAME TITLE text-w03.
PARAMETERS: pa_wlbuk LIKE rf42b-idntb MODIF ID wkl.
SELECT-OPTIONS: so_wlbuk FOR skb1-bukrs MODIF ID wkl NO DATABASE
SELECTION.
SELECTION-SCREEN END OF BLOCK company.

* outer frame for item selection:
SELECTION-SCREEN BEGIN OF BLOCK items WITH FRAME TITLE text-007.
* inner frame 1:
SELECTION-SCREEN BEGIN OF BLOCK status WITH FRAME TITLE text-002.
*   open items:
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS x_opsel LIKE itemset-xopsel RADIOBUTTON GROUP rad1
                                                   DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(20) text-003 FOR FIELD x_opsel.
SELECTION-SCREEN END OF LINE.
PARAMETERS pa_stida LIKE rfpdo-allgstid DEFAULT sy-datlo.
SELECTION-SCREEN SKIP.
*   cleared items:
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS x_clsel LIKE itemset-xclsel RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 3(25) text-004 FOR FIELD x_clsel.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS so_augdt FOR bsis-augdt NO DATABASE SELECTION.
PARAMETERS pa_stid2 LIKE rfpdo-allgstid.
SELECTION-SCREEN SKIP.
*   all items:
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS x_aisel LIKE itemset-xaisel RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 3(20) text-005 FOR FIELD x_aisel.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS so_budat FOR bsis-budat NO DATABASE SELECTION.
SELECTION-SCREEN END OF BLOCK status.
* inner frame 2:
SELECTION-SCREEN BEGIN OF BLOCK type WITH FRAME TITLE text-006.
PARAMETERS: x_norm LIKE itemset-xnorm AS CHECKBOX DEFAULT 'X',
            x_shbv LIKE itemset-xshbv NO-DISPLAY,
            x_merk LIKE itemset-xmerk AS CHECKBOX,
            x_park LIKE itemset-xpark AS CHECKBOX,
            x_apar LIKE itemset-xarit NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK type.
* end of inner frames.
SELECTION-SCREEN END OF BLOCK items.
* end of outer frame.
* list layout frame:
SELECTION-SCREEN BEGIN OF BLOCK list WITH FRAME TITLE text-001.
PARAMETERS: pa_vari TYPE slis_vari,
            pa_nmax LIKE itemset-nmax.
SELECTION-SCREEN END OF BLOCK list.
*... end of selection screen layout.

*... dark parameters:
* branch: read items posted on central office? (not used for GL)
*   Values: (Y)es, (N)o, ( ) prompt user
PARAMETERS: pa_cent TYPE c NO-DISPLAY.
* net due date selection:
SELECT-OPTIONS so_faedt FOR it_pos-faedt NO-DISPLAY.
* combined fiscal year/posting period
DATA:           gp_yrper TYPE rwcoom-fiscper.
SELECT-OPTIONS: so_yrper FOR gp_yrper NO-DISPLAY.
* internet transaction mode?
PARAMETERS: pa_inet TYPE c NO-DISPLAY.
* grid control display?
*   Values: (Y)es, (N)o, ( ) get parameter
PARAMETERS: pa_grid TYPE c NO-DISPLAY.

*... include internal select-options for derived item fields:
INCLUDE rfitem_sel.

INITIALIZATION.
  g_repid  = sy-repid.
  sd_noaut = 'X'.
  sd_nooap = 'X'.
* expiring currencies:
  PERFORM init_expcur.
* deactivate user commands:
  PERFORM change_status.
* initialize worklist button:
  PERFORM wl_flag_and_button.
* get T021S and make field administration tables:
  PERFORM init_admin_tables.
* make field catalog, exclude all unnecessary fields:
  PERFORM make_fieldcatalog.
* prepare KKBER currency lookup table:
  PERFORM init_kkcurr_table.
* get parameters:
  PERFORM get_general_param.
* set account and company code:
  PERFORM set_acct_and_ccode.
* set fields for transfer prices
  PERFORM insert_tp_fields.
  PERFORM rri_init.                                         "n1800500

AT SELECTION-SCREEN OUTPUT.
* check for "suppress dialog":
  gd_dynp_fun = 7.
  gd_dynp_val = 1.
  PERFORM dynp_get_status USING    gd_dynp_fun
                          CHANGING gd_dynp_val.
  LOOP AT SCREEN.
    PERFORM wl_modify_screen.
    MODIFY SCREEN.
  ENDLOOP.
  IF x_aisel IS NOT INITIAL                                 "n1800500
  OR x_clsel IS NOT INITIAL.                                "n1800500
    CLEAR x_opsel.                                          "n1800500
  ENDIF.                                                    "n1800500


AT SELECTION-SCREEN ON BLOCK glaccount.
  IF NOT gd_wl_on IS INITIAL.        " worklists on >
    IF gd_dynp_val EQ 0.             " rebuild sd_bukrs and sd_saknr >
*       GL accounts:
      REFRESH sd_saknr.
      IF NOT pa_wlsak IS INITIAL.    " account worklist given >
        PERFORM resolve_worklist TABLES gt_cosel
                                 USING  pa_wlsak
                                        'SAKNR'.
*         worklist overrides account select-option:
        REFRESH so_wlsak.
        CLEAR so_wlsak.
        LOOP AT gt_cosel.
          MOVE-CORRESPONDING gt_cosel TO sd_saknr.
          APPEND sd_saknr.
        ENDLOOP.
      ELSE.
        APPEND LINES OF so_wlsak TO sd_saknr.
      ENDIF.                         " account worklist given <
    ENDIF.                           " rebuild sd_bukrs and sd_saknr <
  ENDIF.                             " worklists on <


AT SELECTION-SCREEN ON BLOCK company.
  IF NOT gd_wl_on IS INITIAL.        " worklists on >
    IF gd_dynp_val EQ 0.             " rebuild sd_bukrs and sd_saknr >
*       company codes:
      REFRESH sd_bukrs.
      IF NOT pa_wlbuk IS INITIAL.    " ccode worklist given >
        PERFORM resolve_worklist TABLES gt_cosel
                                 USING  pa_wlbuk
                                        'BUKRS'.
*         worklist overrides company code select-option:
        REFRESH so_wlbuk.
        CLEAR so_wlbuk.
        LOOP AT gt_cosel.
          MOVE-CORRESPONDING gt_cosel TO sd_bukrs.
          APPEND sd_bukrs.
        ENDLOOP.
      ELSE.
        APPEND LINES OF so_wlbuk TO sd_bukrs.
      ENDIF.                         " ccode worklist given <
    ENDIF.                           " rebuild sd_bukrs and sd_saknr <
  ENDIF.                             " worklists on <


AT SELECTION-SCREEN.
  PERFORM rri_init.                                         "n2496078

  CASE sy-ucomm.

*...switch worklist on or off:
    WHEN 'FC01'.
      PERFORM worklist_on_off.
      PERFORM set_acct_and_ccode.

*...process worklists and check selections:
    WHEN 'ONLI' OR 'PRIN' OR 'INIT' OR 'PICK' OR 'ENTR' OR space.
      CALL FUNCTION 'BUKRS_AUTHORITY_CHECK'
        EXPORTING
          xdatabase = 'S'
        TABLES
          xbukreis  = sd_bukrs.
      CALL FUNCTION 'BUKRS_AUTHORITY_CHECK'
        EXPORTING
          xdatabase = 'B'
        TABLES
          xbukreis  = sd_bukrs.
*     check input:
      PERFORM sel_account_check.
*     check date (tpc)
      PERFORM check_date TABLES sd_bukrs
                         USING  sd_tpc.

  ENDCASE.

AT SELECTION-SCREEN ON BLOCK type.
* check item type only when action = execute:
  IF sy-ucomm = 'ONLI'.
    PERFORM sel_type_check.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_vari.
  PERFORM alv_variant_f4 CHANGING pa_vari.

*** start of selection
START-OF-SELECTION.
***

  CLEAR: x_stop.
* set parameters:
  PERFORM set_parameters.

* set dynamic selections of Log. Database according to the dyn.
* selections of this programm for BSIS, SKA1, and SKB1
  PERFORM set_dyn_sel_ldb.                                  "n2199162

* map selections to LDB logic:
  PERFORM map_sel_to_ldb.

* determine company codes which use transfer prices
  SELECT * FROM t001a INTO TABLE it_tp_auth WHERE bukrs IN sd_bukrs.
  LOOP AT it_tp_auth.
    IF it_tp_auth-curtp+1(1) = '1' OR it_tp_auth-curtp+1(1) = '2'
      OR it_tp_auth-curtp2+1(1) = '1' OR it_tp_auth-curtp2+1(1) = '2'.
    ELSE.
      DELETE it_tp_auth.
    ENDIF.
  ENDLOOP.
* check tp authority and set 'tech'-attribute if authority is missing
  PERFORM make_fieldcatalog2.

* if report called from balance display:
* Exclude PL accounts now (will be included later).
  PERFORM exclude_pl_accounts.

GET skb1.
* company master data:
  PERFORM t001_info_fill USING skb1-bukrs.
* GL account text:
  PERFORM skat_info_fill.
* company account master data:
  PERFORM skb1_info_fill.

GET bsis.
* recon. account + line item display => open item management:
  IF ( skb1-mitkz EQ 'D' OR skb1-mitkz EQ 'K' ) AND skb1-xkres NE space.
    bsis-xopvw = 'X'.
  ENDIF.
* importing BKPF and BSEG data from archive
  PERFORM import_arch_from_memory.
* relevant basically if report is called from balance display.
  gp_yrper(4) = bsis-gjahr.
  gp_yrper+5  = bsis-monat.
  CHECK gp_yrper IN so_yrper.
* writing items into it_pos.
  IF x_stop IS INITIAL.                                        "2342763
    perform pos_table_fill  changing  x_stop.
  ENDIF.                                                       "2342763
END-OF-SELECTION.
* set variant:
  gs_variant-report   = g_repid.
  gs_variant-username = sy-uname.
  gs_variant-variant  = pa_vari.
* if report is called from balance display:
  PERFORM include_pl_accounts.
* save all selection criteria for refresh:
  PERFORM save_all_selections.
* determine list display mode:
  PERFORM display_grid_or_classic.
* read special fields if necessary:
  PERFORM special_fields_init USING gs_variant.
* number of selected items:
  IF x_stop = 'X'.
    MESSAGE i023 WITH pa_nmax.
  ELSE.
    DESCRIBE TABLE it_pos LINES n_lines.
    IF n_lines > 0.
      MESSAGE s024 WITH n_lines.
    ELSEIF pa_inet IS INITIAL AND gd_dynp_val IS INITIAL.
      MESSAGE s033.
      EXIT.
    ELSE.
      MESSAGE i033.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.
* export general data used in header info:
  PERFORM export_filitexts_data.

* authority display or change
  PERFORM authority_tcode USING 'FB02' subrc.
  IF subrc EQ 0.
    x_change = 'X'.
  ELSE.
    x_change = space.
  ENDIF.

* leave to ALV:
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = text-009.
  CALL FUNCTION 'FI_ITEMS_DISPLAY'
    EXPORTING
      caller_repid  = c_repid_gl
      acctype       = c_koart_gl
      x_opvw        = x_gl_opvw
      x_change      = x_change
      i_u_save      = gd_alvsave
      is_u_variant  = gs_variant
      it_u_fieldcat = gt_fieldcat[]
      it_kontab     = it_accts[]
      it_slbtab     = it_comps[]
      it_t001       = it_h_t001[]
      it_skat       = it_h_skat[]
      it_skb1       = it_h_skb1[]
      x_grid        = x_grid
      x_inet        = pa_inet
    TABLES
      it_items      = it_pos.

  FREE it_h_t001.                                           "1236371
  FREE it_h_skat.                                           "1236371
  FREE it_h_skb1.                                           "1236371
  FREE it_pos.                                              "1236371

*----------------------------------------------------------------------*
*  FORM SEL_ACCOUNT_CHECK
*----------------------------------------------------------------------*
FORM sel_account_check.

  DATA: ld_lines LIKE sy-index,
        ld_single_account TYPE c,
        ld_single_bukrs   TYPE c.


  READ TABLE sd_saknr INDEX 1.
  IF sy-subrc NE 0.
    READ TABLE sd_bukrs INDEX 1.
    IF sy-subrc NE 0.
      MESSAGE w019.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE sd_saknr LINES ld_lines.
  LOOP AT sd_saknr TRANSPORTING NO FIELDS
       WHERE sign NE 'I' OR
             option NE 'EQ'.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0 OR ld_lines = 0.
    SELECT SINGLE saknr bukrs FROM skb1
           INTO (gd_saknr_msg, gd_bukrs_msg)
           WHERE saknr IN sd_saknr
             AND bukrs IN sd_bukrs.
    IF sy-subrc NE 0.
      READ TABLE sd_bukrs INDEX 1 INTO sd_bukrs.            "1569654
      MESSAGE e030(msitem).
    ENDIF.
ENHANCEMENT-SECTION     EHP603_RFITEMGL_1 SPOTS ES_RFITEMGL.
    SELECT SINGLE saknr FROM skb1 INTO skb1-saknr
           WHERE saknr IN sd_saknr
             AND bukrs IN sd_bukrs
             AND xkres = 'X'.
    IF sy-subrc NE 0.
      MESSAGE e430(f4) WITH gd_saknr_msg gd_bukrs_msg.
    ENDIF.
END-ENHANCEMENT-SECTION.
*$*$-Start: EHP603_RFITEMGL_1-------------------------------------------------------------------$*$*
ENHANCEMENT 1  FREP_RFITEMGL_1.    "active version
    select single saknr from skb1 into skb1-saknr
           where saknr in sd_saknr
             and bukrs in sd_bukrs
             and xkres = 'X'.
    if sy-subrc ne 0.
*      if no entry found, check if this account is a ledger group specific op-managed account
       select single saknr from skb1 into skb1-saknr
              where saknr in sd_saknr
              and   bukrs in sd_bukrs
              and   xlgclr = 'X'.
       if sy-subrc = 0.
          message e026(fagl_posting) with gd_saknr_msg gd_bukrs_msg.
       else.
          message e430(f4) with gd_saknr_msg gd_bukrs_msg.
       endif.
    endif.
ENDENHANCEMENT.
*$*$-End:   EHP603_RFITEMGL_1-------------------------------------------------------------------$*$*
  ELSE.  "worklist
    SELECT saknr bukrs FROM skb1
       INTO (gd_saknr_msg, gd_bukrs_msg)
       FOR ALL ENTRIES IN sd_saknr
       WHERE saknr = sd_saknr-low
         AND bukrs IN sd_bukrs.
      EXIT.
    ENDSELECT.
    IF sy-subrc NE 0.
      READ TABLE sd_bukrs INDEX 1 INTO sd_bukrs.            "1569654
      MESSAGE e030(msitem).
    ENDIF.
    SELECT saknr FROM skb1 INTO skb1-saknr
           FOR ALL ENTRIES IN sd_saknr
           WHERE saknr = sd_saknr-low
             AND bukrs IN sd_bukrs
             AND xkres = 'X'.
      EXIT.
    ENDSELECT.
ENHANCEMENT-SECTION     EHP603_RFITEMGL_2 SPOTS ES_RFITEMGL.
    IF sy-subrc NE 0.
      MESSAGE e430(f4) WITH gd_saknr_msg gd_bukrs_msg.
    ENDIF.
END-ENHANCEMENT-SECTION.
*$*$-Start: EHP603_RFITEMGL_2-------------------------------------------------------------------$*$*
ENHANCEMENT 1  FREP_RFITEMGL_2.    "active version
    if sy-subrc ne 0.
*      if no entry found, check if this account is a ledger group specific op-managed account
       select single saknr from skb1 into skb1-saknr
              where saknr in sd_saknr
              and   bukrs in sd_bukrs
              and   xlgclr = 'X'.
       if sy-subrc = 0.
          message e026(fagl_posting) with gd_saknr_msg gd_bukrs_msg.
       else.
          message e430(f4) with gd_saknr_msg gd_bukrs_msg.
       endif.
    endif.
ENDENHANCEMENT.
*$*$-End:   EHP603_RFITEMGL_2-------------------------------------------------------------------$*$*
  ENDIF.
*...write sd_saknr und sd_bukrs into memory
*...(important later for Dispute-Management).

  EXPORT sd_saknr TO MEMORY ID 'FILITEXTS_SAKNR'.
  EXPORT sd_bukrs TO MEMORY ID 'FILITEXTS_BUKRS'.

*...find out, if single accounts and company codes have been selected..*
*...(important for header display).....................................*

  CLEAR ld_single_account.
  DESCRIBE TABLE sd_saknr LINES ld_lines.
  IF ld_lines EQ 1.
    READ TABLE sd_saknr INDEX 1.
    IF ( sd_saknr-sign EQ 'I' AND sd_saknr-option EQ 'EQ' ) OR
       ( sd_saknr-sign EQ 'I' AND sd_saknr-option EQ 'BT' AND
         sd_saknr-low  EQ sd_saknr-high ).
      ld_single_account = 'X'.
    ENDIF.
  ENDIF.
  CLEAR ld_single_bukrs.
  DESCRIBE TABLE sd_bukrs LINES ld_lines.
  IF ld_lines EQ 1.
    READ TABLE sd_bukrs INDEX 1.
    IF ( sd_bukrs-sign EQ 'I' AND sd_bukrs-option EQ 'EQ' ) OR
       ( sd_bukrs-sign EQ 'I' AND sd_bukrs-option EQ 'BT' AND
         sd_bukrs-low  EQ sd_bukrs-high ).
      ld_single_bukrs = 'X'.
    ENDIF.
  ENDIF.
  EXPORT ld_single_bukrs ld_single_account TO MEMORY ID
                                      'FILITEXTS_SINGLE'.

ENDFORM.                    "sel_account_check

*----------------------------------------------------------------------*
*  FORM SEL_TYPE_CHECK
*----------------------------------------------------------------------*
FORM sel_type_check.
  IF x_norm IS INITIAL AND x_merk IS INITIAL AND x_park IS INITIAL.
    MESSAGE e020.
  ENDIF.
ENDFORM.                    "sel_type_check

*----------------------------------------------------------------------*
*      Form  POS_TABLE_FILL
*----------------------------------------------------------------------*
FORM pos_table_fill  CHANGING  p_stop.
  DATA: text_index TYPE i,
        okay       TYPE c VALUE space.


  CLEAR wa_pos.
  MOVE-CORRESPONDING bsis  TO wa_pos.
*... check item against selection flags:
  PERFORM check_item_ok  USING x_norm
                               'X'
                               x_merk
                               x_park
                               wa_pos
                         CHANGING okay.
  CHECK okay = 'X'.
*
  wa_pos-koart = c_koart_gl.
  wa_pos-konto = bsis-hkont.
  wa_pos-dmshb = bsis-dmbtr.
  wa_pos-wrshb = bsis-wrbtr.
  wa_pos-vbewa = bsis-bewar.                                "944067
  wa_bsegp-bdiff = bsis-bdiff.
  wa_bsegp-bdif2 = bsis-bdif2.
  wa_bsegp-bdif3 = bsis-bdif3.
* derive nontrivial rfpos fields:
  CALL FUNCTION 'ITEM_DERIVE_FIELDS'
    EXPORTING
      s_t001    = it_h_t001
      s_bsegp   = wa_bsegp
      key_date  = p_keydate
      xopvw     = bsis-xopvw
    CHANGING
      s_item    = wa_pos
    EXCEPTIONS
      bad_input = 1
      OTHERS    = 2.
  IF sy-subrc NE 0.
    MESSAGE a022.
  ENDIF.
* fill currency fields:
  PERFORM item_currency_fields.
* check dark select-options and add item to table:
  PERFORM item_check_append.
* check max number:
  DESCRIBE TABLE it_pos LINES n_lines.
  IF pa_nmax > 0 AND n_lines GE pa_nmax.
    p_stop = 'X'.
    IF  sy-calld IS INITIAL and NOT                            "2342763
      (    sy-tcode = 'FS10N'                                  "2342763
        or sy-tcode cp 'S+38'                                  "2342763
        or sy-tcode eq 'SE80' ).                               "2342763
      STOP.
    ENDIF.                                                     "2342763
  ENDIF.
ENDFORM.                    "pos_table_fill

*&---------------------------------------------------------------------*
*&      Form  T001_INFO_FILL
*&---------------------------------------------------------------------*
FORM t001_info_fill USING ip_bukrs TYPE skb1-bukrs.

  READ TABLE it_h_t001 WITH KEY mandt = sy-mandt
                                bukrs = ip_bukrs
                       BINARY SEARCH.
  IF sy-subrc NE 0.
    CLEAR: it_h_t001.
    SELECT SINGLE * FROM t001 INTO it_h_t001
        WHERE bukrs = ip_bukrs.
    INSERT TABLE it_h_t001.

*    table of companies:
    PERFORM fill_comp_table  USING  ip_bukrs
                              it_h_t001-waers
                              it_h_t001-kkber.
  ENDIF.

  IF wa_x001-bukrs NE skb1-bukrs.
    CLEAR: wa_x001.
    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs                = ip_bukrs
      IMPORTING
        e_x001                 = wa_x001
      EXCEPTIONS
        currency_2_not_defined = 1
        currency_3_not_defined = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.
* ?
    ENDIF.
  ENDIF.

ENDFORM.                    "t001_info_fill

*&---------------------------------------------------------------------*
*&      Form  SKAT_INFO_FILL
*&---------------------------------------------------------------------*
FORM skat_info_fill.
  DATA: langu   LIKE sy-langu.
  CLEAR it_h_skat.
  SELECT SINGLE * FROM skat INTO it_h_skat
       WHERE spras = sy-langu
       AND   ktopl = it_h_t001-ktopl
       AND   saknr = skb1-saknr.
  IF sy-subrc = 0.
    INSERT TABLE it_h_skat.
    EXIT.
  ENDIF.
  SELECT SINGLE * FROM skat INTO it_h_skat
       WHERE spras = it_h_t001-spras
       AND   ktopl = it_h_t001-ktopl
       AND   saknr = skb1-saknr.
  IF sy-subrc = 0.
    INSERT TABLE it_h_skat.
    EXIT.
  ENDIF.
  SELECT SINGLE * FROM skat INTO it_h_skat
       WHERE ktopl = it_h_t001-ktopl
       AND   saknr = skb1-saknr.
  IF sy-subrc = 0.
    INSERT TABLE it_h_skat.
    EXIT.
  ENDIF.
ENDFORM.                    "skat_info_fill

*&---------------------------------------------------------------------*
*&      Form  SKB1_INFO_FILL
*&---------------------------------------------------------------------*
FORM skb1_info_fill.
  DATA: sak_name   LIKE kna1-name1.
  CLEAR it_h_skb1.
  MOVE-CORRESPONDING skb1 TO it_h_skb1.
  INSERT TABLE it_h_skb1.
* table of accounts:
  WRITE skat-txt50 TO sak_name.
  PERFORM fill_acct_table  USING  c_koart_gl
                                  skb1-saknr
                                  skb1-bukrs
                                  space
                                  sak_name
                                  skb1-saknr.
ENDFORM.                    "skb1_info_fill


*&---------------------------------------------------------------------*
*&      Form  MAP_SEL_TO_LDB
*&---------------------------------------------------------------------*
FORM map_sel_to_ldb.

  DATA: ld_stida LIKE sy-datum.

  CLEAR:   sd_augdt, sd_budat, sd_opopt, sd_apopt, sd_stida.
  REFRESH: sd_augdt, sd_budat.

* Determine whether parked (xstav) and/or normal items
* (xstan) must be selected by the log. database
  IF x_park = 'X'.
    b0sg-xstav = 'X'.
  ELSE.
    CLEAR b0sg-xstav.
  ENDIF.
  IF ( x_norm = 'X' OR x_shbv = 'X' OR x_merk = 'X' ).
    b0sg-xstan = 'X'.
  ELSE.
    CLEAR b0sg-xstan.
  ENDIF.

* Determine relevant keydates for the selection by the log. database
* (sd_stida)
  GET PARAMETER ID 'FI_STIDA' FIELD ld_stida.
  IF NOT ld_stida IS INITIAL AND sy-subrc EQ 0.
    pa_stida = ld_stida.
    SET PARAMETER ID 'FI_STIDA' FIELD '00000000'.
  ENDIF.
  CASE 'X'.
    WHEN x_opsel.
      IF pa_stida IS INITIAL.
        p_keydate         = '99991231'.
        sd_stida          = '99991231'.
      ELSE.
        p_keydate         = pa_stida.
        sd_stida          = pa_stida.
      ENDIF.
      sd_opopt          = 'X'.
    WHEN x_clsel.
      IF pa_stid2 IS INITIAL.
        p_keydate       = '99991231'.
        sd_stida        = '00010101'.
        sd_apopt        = 'X'.                              "992643
      ELSE.
        p_keydate       = pa_stid2.
        sd_stida        = pa_stid2.
        sd_budat-sign   = 'I'.                              "1107225
        sd_budat-option = 'LE'.                             "1107225
        sd_budat-low    = pa_stid2.                         "1107225
        APPEND sd_budat.                                    "1107225
      ENDIF.
      sd_augdt[]        = so_augdt[].
    WHEN x_aisel.
      p_keydate         = '99991231'.
      sd_stida          = '99991231'.
      sd_budat[]        = so_budat[].
      sd_opopt          = 'X'.
      sd_apopt          = 'X'.
  ENDCASE.
  pa_stida_default = sd_stida.

* For reading from archives:
* number of archived items as i-message:
  sd_iarch = 'X'.
  sd_memor = 'X'.

ENDFORM.                    "map_sel_to_ldb

*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETERS
*&---------------------------------------------------------------------*
FORM set_parameters.
* set general parameters:
  PERFORM set_general_param.
* set acc.type specific parameters:
  CLEAR: sd_saknr, sd_bukrs.
  CLEAR: so_wlsak, so_wlbuk.
  IF gd_wl_on IS INITIAL.
    READ TABLE sd_saknr INDEX 1.
    IF sy-subrc = 0 AND sd_saknr-sign = 'I' AND sd_saknr-option = 'EQ'
        AND NOT sd_saknr-low IS INITIAL.
      SET PARAMETER ID 'SAK' FIELD sd_saknr-low.
    ENDIF.
    READ TABLE sd_bukrs INDEX 1.
    IF sy-subrc = 0 AND sd_bukrs-sign = 'I' AND sd_bukrs-option = 'EQ'
        AND NOT sd_bukrs-low IS INITIAL.
      SET PARAMETER ID 'BUK' FIELD sd_bukrs-low.
    ENDIF.
    SET PARAMETER ID 'AVS' FIELD space.
    SET PARAMETER ID 'AVB' FIELD space.
  ELSE.
*   worklists switched on:
    SET PARAMETER ID 'AVS' FIELD pa_wlsak.
    SET PARAMETER ID 'AVB' FIELD pa_wlbuk.
    READ TABLE so_wlsak INDEX 1.
    IF sy-subrc = 0 AND so_wlsak-sign = 'I' AND so_wlsak-option = 'EQ'
        AND NOT so_wlsak-low IS INITIAL.
      SET PARAMETER ID 'SAK' FIELD so_wlsak-low.
    ENDIF.
    READ TABLE so_wlbuk INDEX 1.
    IF sy-subrc = 0 AND so_wlbuk-sign = 'I' AND so_wlbuk-option = 'EQ'
        AND NOT so_wlbuk-low IS INITIAL.
      SET PARAMETER ID 'BUK' FIELD so_wlbuk-low.
    ENDIF.
  ENDIF.

* Set parameters for reading from archiv
  gd_usear = sd_usear.
  gd_usedb = sd_usedb.
  gd_memor = 'X'.

ENDFORM.                    "set_parameters

*&---------------------------------------------------------------------*
*&      Form  SET_ACCT_AND_CCODE
*&---------------------------------------------------------------------*
FORM set_acct_and_ccode.

  REFRESH: sd_saknr, sd_bukrs, so_wlsak, so_wlbuk.
  CLEAR:   sd_saknr, sd_bukrs, so_wlsak, so_wlbuk,
           pa_wlsak, pa_wlbuk.

  IF gd_wl_on IS INITIAL.
    GET PARAMETER ID 'SAK' FIELD sd_saknr-low.
    IF sy-subrc = 0 AND NOT sd_saknr-low IS INITIAL.
      sd_saknr-sign   = 'I'.
      sd_saknr-option = 'EQ'.
      APPEND sd_saknr.
    ENDIF.
    GET PARAMETER ID 'BUK' FIELD sd_bukrs-low.
    IF sy-subrc = 0 AND NOT sd_bukrs-low IS INITIAL.
      sd_bukrs-sign   = 'I'.
      sd_bukrs-option = 'EQ'.
      APPEND sd_bukrs.
    ENDIF.
  ELSE.
*   worklists switched on:
    GET PARAMETER ID 'AVS' FIELD pa_wlsak.
    GET PARAMETER ID 'AVB' FIELD pa_wlbuk.
    IF pa_wlsak IS INITIAL.
      GET PARAMETER ID 'SAK' FIELD so_wlsak-low.
      IF sy-subrc = 0 AND NOT so_wlsak-low IS INITIAL.
        so_wlsak-sign   = 'I'.
        so_wlsak-option = 'EQ'.
        APPEND so_wlsak.
      ENDIF.
    ENDIF.
    IF pa_wlbuk IS INITIAL.
      GET PARAMETER ID 'BUK' FIELD so_wlbuk-low.
      IF sy-subrc = 0 AND NOT so_wlbuk-low IS INITIAL.
        so_wlbuk-sign   = 'I'.
        so_wlbuk-option = 'EQ'.
        APPEND so_wlbuk.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " SET_ACCT_AND_CCODE

*&---------------------------------------------------------------------*
*&      Form  exclude_pl_acounts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_pl_accounts .

  DATA: ld_lines TYPE i.

  IF  sy-calld = 'X' AND
    ( sy-tcode = 'FS10N' OR sy-tcode CP 'S+38' OR sy-tcode EQ 'SE80' ).

* make copy of sd_saknr
    gt_saknr = sd_saknr[].
    gt_bukrs = sd_bukrs[].

* Exclude PL accounts.
* check if worklist or not

    DESCRIBE TABLE sd_saknr LINES ld_lines.
    LOOP AT sd_saknr TRANSPORTING NO FIELDS
         WHERE sign NE 'I' OR
               option NE 'EQ'.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0 OR ld_lines = 0.
      SELECT        s~saknr t~bukrs s~gvtyp
               INTO TABLE gt_accoco
               FROM t001 AS t INNER JOIN ska1 AS s
                 ON t~ktopl =  s~ktopl
              WHERE t~bukrs IN sd_bukrs
                AND s~saknr IN sd_saknr.
    ELSE.                                       "worklist.
      SELECT        s~saknr t~bukrs s~gvtyp
               INTO  gd_accoco
               FROM t001 AS t INNER JOIN ska1 AS s
                 ON t~ktopl =  s~ktopl
                FOR ALL ENTRIES IN sd_saknr
              WHERE t~bukrs IN sd_bukrs
                AND s~saknr = sd_saknr-low.
        APPEND gd_accoco TO gt_accoco.
      ENDSELECT.
    ENDIF.

* PL account numbers remain in GT_ACCOCO
* all others are transfered into SD_SAKNR.
    REFRESH sd_saknr.
    CLEAR sd_saknr.
    sd_saknr-sign   = 'I'.
    sd_saknr-option = 'EQ'.
    LOOP AT gt_accoco INTO gd_accoco WHERE gvtyp = space.
      DELETE gt_accoco INDEX sy-tabix.
      sd_saknr-low = gd_accoco-saknr.
      APPEND sd_saknr.
    ENDLOOP.
    IF sd_saknr[] IS INITIAL.
      APPEND sd_saknr.
    ENDIF.

  ENDIF.
ENDFORM.                    " exclude_pl_acounts

*&--------------------------------------------------------------------*
*&      Form  include_pl_accounts
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM include_pl_accounts.
  DATA: sak_name   LIKE kna1-name1.

  IF  sy-calld = 'X' AND
    ( sy-tcode = 'FS10N' OR sy-tcode CP 'S+38' OR sy-tcode EQ 'SE80' ).
    LOOP AT gt_accoco INTO gd_accoco.
* prepare selection screen table:
      PERFORM sdf_selscreen.
* prepare dynamic selections:
      PERFORM sdf_dynamics.
* prepare it_h_t001 table for calculating special fields later.
      PERFORM t001_info_fill USING gd_accoco-bukrs.
* prepare callbacks for ldb_process:
      PERFORM fill_sdf_callback.
* export b0sg flags for ldb
      EXPORT b0sg TO MEMORY ID 'SDF_B0SG'.
* read ldb SDF:
      CALL FUNCTION 'LDB_PROCESS'
        EXPORTING
          ldbname                     = 'SDF'
          expressions                 = it_dyn_texpr[]
        TABLES
          callback                    = it_callback
          selections                  = it_selscreen
        EXCEPTIONS
          ldb_selections_error        = 1
          ldb_selections_not_accepted = 2
          free_selections_error       = 3
          callback_no_event           = 4
          callback_no_program         = 5
          callback_no_cbform          = 6
          OTHERS                      = 7.
      IF sy-subrc NE 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        PERFORM import_arch_from_memory.
        WRITE skat-txt50 TO sak_name.
        PERFORM fill_acct_table USING  'S'
                                       gd_accoco-saknr
                                       gd_accoco-bukrs
                                       space
                                       sak_name
                                       gd_accoco-saknr.
      ENDIF.
    ENDLOOP.

* restore accounts (for refresh of list).
    sd_saknr[] = gt_saknr.
    sd_bukrs[] = gt_bukrs.

  ENDIF.

ENDFORM.                    "include_pl_accounts

*&--------------------------------------------------------------------*
*&      Form  sdf_selscreen
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM sdf_selscreen.

  DATA: ld_lines TYPE sytabix.

  REFRESH: it_selscreen.
* fill selections: first single values & parameters
  it_selscreen-selname = 'SD_SAKNR'.
  it_selscreen-kind    = 'S'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = gd_accoco-saknr.
  APPEND it_selscreen.
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_BUKRS'.
  it_selscreen-kind    = 'S'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = gd_accoco-bukrs.
  APPEND it_selscreen.
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_OPOPT'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_opopt.
  APPEND it_selscreen.
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_APOPT'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_apopt.
  APPEND it_selscreen.
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_STIDA'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_stida.
  APPEND it_selscreen.
* now select option tables:
  LOOP AT sd_augdt.
    CLEAR it_selscreen.
    it_selscreen-selname = 'SD_AUGDT'.
    it_selscreen-kind    = 'S'.
    MOVE-CORRESPONDING sd_augdt TO it_selscreen.
    APPEND it_selscreen.
  ENDLOOP.
  LOOP AT sd_budat.
    CLEAR it_selscreen.
    it_selscreen-selname = 'SD_BUDAT'.
    it_selscreen-kind    = 'S'.
    MOVE-CORRESPONDING sd_budat TO it_selscreen.
    APPEND it_selscreen.
  ENDLOOP.

* modify SO_YRPER:
  gt_yrper = so_yrper[].
  DESCRIBE TABLE gt_yrper LINES ld_lines.
  IF ld_lines EQ 1 .
    READ TABLE gt_yrper INTO gd_yrper INDEX 1.
    IF gd_yrper-sign = 'I' AND gd_yrper-option = 'BT'.
      gd_yrper-low(4) = gd_yrper-high(4).
      MODIFY gt_yrper FROM gd_yrper INDEX 1.
    ENDIF.
  ENDIF.
  LOOP AT gt_yrper INTO gd_yrper.
    CLEAR it_selscreen.
    it_selscreen-selname = 'SO_YRPER'.
    it_selscreen-kind    = 'S'.
    MOVE-CORRESPONDING gd_yrper TO it_selscreen.
    APPEND it_selscreen.
  ENDLOOP.
  IF sy-subrc <> 0.
    sy-subrc = 0.
  ENDIF.

* Parameters and Select Options for Archives
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_USEDB'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_usedb.
  APPEND it_selscreen.
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_USEAR'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_usear.
  APPEND it_selscreen.
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_USEAS'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_useas.
  APPEND it_selscreen.
  IF NOT sd_files[] IS INITIAL.
    LOOP AT sd_files.
      CLEAR it_selscreen.
      it_selscreen-selname = 'SD_FILES'.
      it_selscreen-kind    = 'S'.
      MOVE-CORRESPONDING sd_files TO it_selscreen.
      APPEND it_selscreen.
    ENDLOOP.
  ENDIF.
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_MEMOR'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_memor.
  APPEND it_selscreen.
  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_IARCH'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_iarch.
  APPEND it_selscreen.

  CLEAR it_selscreen.
  it_selscreen-selname = 'SD_NOOAP'.
  it_selscreen-kind    = 'P'.
  it_selscreen-sign    = 'I'.
  it_selscreen-option  = 'EQ'.
  it_selscreen-low     = sd_nooap.
  APPEND it_selscreen.


ENDFORM.                               " SDF_SELSCREEN
*&--------------------------------------------------------------------*
*&      Form  sdf_dynamics
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM sdf_dynamics.

  DATA: rt_dyn_trange  TYPE rsds_trange.
  DATA: ld_rsds_expr TYPE rsds_expr,
        ld_rsdsexpr  TYPE rsdsexpr.

  REFRESH: it_dyn_texpr.
* get report dynamic selections:
  CALL FUNCTION 'RS_REFRESH_FROM_DYNAMICAL_SEL'
    EXPORTING
      curr_report        = g_repid
      mode_write_or_move = 'M'
    IMPORTING
      p_trange           = rt_dyn_trange
    EXCEPTIONS
      not_found          = 1
      wrong_type         = 2
      OTHERS             = 3.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.
* delete rt_dyn_trange where tablename ne 'BSIS'.                "740027
  CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
    EXPORTING
      field_ranges = rt_dyn_trange
    IMPORTING
      expressions  = it_dyn_texpr[].
  LOOP AT it_dyn_texpr INTO ld_rsds_expr WHERE tablename = 'BSIS'.
    LOOP AT ld_rsds_expr-expr_tab INTO ld_rsdsexpr
             WHERE fieldname = 'GJAHR' OR fieldname = 'MONAT'.
      IF ld_rsdsexpr-fieldname = 'GJAHR'.
        IF     ld_rsdsexpr-option = 'BT'
           AND ld_rsdsexpr-low NE ld_rsdsexpr-high.
          ld_rsdsexpr-option = 'EQ'.
          ld_rsdsexpr-low = ld_rsdsexpr-high.
          CLEAR ld_rsdsexpr-high.
          MODIFY ld_rsds_expr-expr_tab FROM ld_rsdsexpr.
        ENDIF.
      ELSEIF ld_rsdsexpr-fieldname = 'MONAT'.
        IF ld_rsdsexpr-option = 'BT'.
          ld_rsdsexpr-high = so_yrper-high+5.
          MODIFY ld_rsds_expr-expr_tab FROM ld_rsdsexpr.
        ENDIF.
      ENDIF.
    ENDLOOP.
    MODIFY it_dyn_texpr FROM ld_rsds_expr.
  ENDLOOP.

ENDFORM.                    "sdf_dynamics

*&--------------------------------------------------------------------*
*&      Form  fill_sdf_callback
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM fill_sdf_callback.
  REFRESH: it_callback.
  it_callback-ldbnode = 'BSIS'.
  it_callback-get     = 'X'.
  it_callback-cb_prog = c_repid_gl.
  it_callback-cb_form = 'CB_SDF_GET_BSIS'.
  APPEND it_callback.
ENDFORM.                    "fill_sdf_callback

*&--------------------------------------------------------------------*
*&      Form  cb_sdf_get_bsis
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->NAME       text
*      -->LS_BSIS    text
*      -->MODE       text
*      -->SELECTED   text
*---------------------------------------------------------------------*
FORM cb_sdf_get_bsis  USING  name     LIKE ldbn-ldbnode
                             ls_bsis  LIKE bsis
                             mode     TYPE c
                             selected TYPE c.
  bsis = ls_bsis.
  IF x_stop IS INITIAL.                                        "2342763
    perform import_arch_from_memory.
    perform pos_table_fill  changing  x_stop.
  ENDIF.                                                       "2342763
ENDFORM.                    "cb_sdf_get_bsis

*&---------------------------------------------------------------------*
*&     Include RFITEM_INC
*&---------------------------------------------------------------------*
INCLUDE rfitem_inc.
INCLUDE rfitem_inc_lir.                                     "n2199162